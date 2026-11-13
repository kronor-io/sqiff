{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Rewrites a table's deploy file so it creates the table as a database has it, keeping
-- the file's text wherever it already matches.
module Kronor.SchemaDiff.Rewrite (
    Rewrite (..),
    rewriteFile,
    fileDrift,
) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Kronor.SchemaDiff.Catalog (Address (..), Aspect (..), ObjectKey (..), Schema, render)
import Kronor.SchemaDiff.Diff (Change (..), diff)
import Kronor.SchemaDiff.Object (Object (..), QName (..), fromAddress, qualified, quoteIdent)
import Kronor.SchemaDiff.Recipe (ColumnDef (..), columnSql, parseColumn)
import Kronor.SchemaDiff.Script (splitStatements)
import Kronor.SchemaDiff.Sqitch (tidyConstraint, tidyIndex)
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set


data Rewrite = Rewrite
    { text :: Text
    , unplaced :: [Text]
    -- ^ Differences the rewrite leaves for a person, one line each.
    }


-- | What differs between the table the file creates and the one in the database, without
-- grants, validity, and the triggers, policies and indexes that live in files of their own.
fileDrift :: Set Text -> Schema -> Schema -> Map ObjectKey Change
fileDrift ownFiles fromFile live = Map.filterWithKey kept (diff same fromFile live)
  where
    same _ a b = a == b || plain a == plain b
    plain = T.intercalate "\n" . map defaultNotNull . T.lines . T.replace " not valid" "" . T.replace " NOT VALID" ""
    defaultNotNull l = case T.breakOn " constraint " l of
        (front, name)
            | "not null" `T.isPrefixOf` front
            , "_not_null" `T.isSuffixOf` T.dropWhileEnd isDigit (T.drop (T.length " constraint ") name) ->
                front
        _ -> l
    kept k c = k.aspect /= Privileges && not (elsewhere (fromAddress k.address) c)
    elsewhere o = \case
        Added _ -> case o of
            Trigger _ _ -> True
            Policy _ _ -> True
            Index q -> q.name `Set.member` ownFiles
            _ -> False
        _ -> False


-- | The file's text changed so that it creates the table as @live@ has it. @fromFile@ is
-- what the file creates now, @order@ the live column order, @ownFiles@ the indexes with
-- deploy files of their own.
rewriteFile :: QName -> Set Text -> [Text] -> Schema -> Schema -> Text -> Either Text Rewrite
rewriteFile table ownFiles order live fromFile original = do
    parts <- pieces original
    let changes = fileDrift ownFiles fromFile live
        definitions = [(fromAddress k.address, c) | (k, c) <- Map.toList changes, k.aspect == Definition]
        comments = Map.fromList [(k.address, c) | (k, c) <- Map.toList changes, k.aspect == Comment]
        statements = [(n, s, classify table (snd (splitLead s))) | (n, Stmt s) <- zip [0 :: Int ..] parts]
        indentOf n = statementIndent parts n
        settings = case lookup (Table table) definitions of
            Just (Changed fileDef liveDef) -> tableSettings table (T.lines fileDef) (T.lines liveDef)
            _ -> Nothing
        tableEdit = do
            (n, s, _) <- L.find (\(_, _, k) -> k == CreatesTable) statements
            let (lead, code) = splitLead s
                columns = [(c, ch) | (Column t c, ch) <- definitions, t == table]
                constraints = [(c, ch) | (TableConstraint t c, ch) <- definitions, t == table]
            (code', placed) <- editCreateTable table order columns constraints (settings >>= snd) code
            pure (n, lead <> code', placed)
        settingsPlaced = case settings of
            Just (alters, options) -> (null alters || isJust tableAt) && (isNothing options || any (\(_, _, p) -> Table table `Set.member` p) tableEdit)
            Nothing -> False
        indexEdits =
            [ (n, edit, Index q)
            | (n, s, CreatesIndex ix) <- statements
            , let q = QName table.schema ix
            , Just ch <- [lookup (Index q) definitions]
            , let (lead, _) = splitLead s
                  edit = case ch of
                    Changed _ def -> Just (lead <> indexCode (indentOf n) def)
                    _ -> Nothing
            ]
        commentEdits =
            [ (n, edit, addr)
            | (n, s, Comments addr) <- statements
            , Just ch <- [Map.lookup addr comments]
            , let (lead, code) = splitLead s
                  edit = case ch of
                    Changed _ new -> Just (lead <> withLiteral new code)
                    _ -> Nothing
            ]
        kept n = n `notElem` [m | (m, Nothing, _) <- indexEdits] && n `notElem` [m | (m, Nothing, _) <- commentEdits]
        lastOf p = L.lastMaybe [n | (n, _, k) <- statements, p k, kept n]
        tableAt = lastOf (== CreatesTable)
        indexAnchor = lastOf (\case CreatesIndex _ -> True; _ -> False) <|> tableAt
        commentAnchor = lastOf (\case Comments _ -> True; _ -> False) <|> indexAnchor
        newIndexes = [(q, def) | (Index q, Added def) <- definitions]
        newComments = [(addr, txt) | (addr, Added txt) <- Map.toList comments, isJust (commentTarget addr)]
        inserts =
            Map.fromListWith
                (flip (<>))
                ( [(a, alters) | Just a <- [tableAt], Just (alters, _) <- [settings], not (null alters)]
                    <> [(a, [indexCode (indentOf a) def]) | Just a <- [indexAnchor], (_, def) <- newIndexes]
                    <> [(a, [commentCode (indentOf a) addr txt]) | Just a <- [commentAnchor], (addr, txt) <- newComments]
                )
        handled =
            Set.unions
                [ maybe Set.empty (\(_, _, h) -> Set.map (ObjectKey Definition . objectAddress) h) tableEdit
                , Set.fromList [ObjectKey Definition (objectAddress o) | (_, _, o) <- indexEdits]
                , Set.fromList [ObjectKey Comment a | (_, _, a) <- commentEdits]
                , if isJust indexAnchor then Set.fromList [ObjectKey Definition (objectAddress (Index q)) | (q, _) <- newIndexes] else Set.empty
                , if isJust commentAnchor then Set.fromList [ObjectKey Comment a | (a, _) <- newComments] else Set.empty
                , if settingsPlaced then Set.singleton (ObjectKey Definition (Address "table" [table.schema, table.name] [])) else Set.empty
                ]
        replaced = Map.fromList ([(n, t) | Just (n, t, _) <- [tableEdit]] <> [(n, t) | (n, Just t, _) <- indexEdits] <> [(n, t) | (n, Just t, _) <- commentEdits])
        removed = Set.fromList [n | (n, Nothing, _) <- indexEdits] <> Set.fromList [n | (n, Nothing, _) <- commentEdits]
        out =
            concat
                [ case part of
                    Gap g -> [Out g]
                    Stmt s
                        | n `Set.member` removed -> [Gone]
                        | otherwise -> Out (Map.findWithDefault s n replaced) : concat [[Out [i|;\n\n#{indentOf n}|], Out c] | c <- Map.findWithDefault [] n inserts]
                | (n, part) <- zip [0 ..] parts
                ]
        unplaced = [describeChange k c | (k, c) <- Map.toList changes, k `Set.notMember` handled]
    pure Rewrite{text = renderOut out, unplaced}
  where
    objectAddress = \case
        Index q -> Address "index" [q.schema, q.name] []
        Column t c -> Address "table column" [t.schema, t.name, c] []
        TableConstraint t c -> Address "table constraint" [t.schema, t.name, c] []
        _ -> Address "" [] []


-- | For a table whose settings the file lacks, the statements that turn them on and the
-- storage options for @create table@; nothing when the file has a setting the table lacks.
tableSettings :: QName -> [Text] -> [Text] -> Maybe ([Text], Maybe Text)
tableSettings table file live = do
    let added = filter (`notElem` file) live
        options = L.find ("with (" `T.isPrefixOf`) added
        name = qualified table
        alters =
            [[i|alter table #{name} enable row level security|] | "row level security" `elem` added]
                <> [[i|alter table #{name} force row level security|] | "force row level security" `elem` added]
    guard (all (`elem` live) file)
    guard (all (\l -> l `elem` ["row level security", "force row level security"] || Just l == options) added)
    guard (isNothing options || not (any ("with (" `T.isPrefixOf`) file))
    pure (alters, options)


describeChange :: ObjectKey -> Change -> Text
describeChange k c =
    let name = render k.address <> if k.aspect == Comment then " (comment)" else ""
        what = case c of
            Added _ -> "in the database, not in the file" :: Text
            Dropped _ -> "in the file, not in the database"
            Changed _ _ -> "differs from the database"
     in [i|#{name}: #{what}|]


-- | The text between statements, and the statements without their semicolons.
data Part = Gap Text | Stmt Text


pieces :: Text -> Either Text [Part]
pieces original = splitStatements original >>= go original
  where
    go rest [] = Right [Gap rest]
    go rest (s : ss) = case T.breakOn s rest of
        (_, "") -> Left "a statement cannot be found in the file's text"
        (before, after) -> (\more -> Gap before : Stmt s : more) <$> go (T.drop (T.length s) after) ss


data Out = Out Text | Gone


-- | Joins the output; a removed statement takes its semicolon and line with it.
renderOut :: [Out] -> Text
renderOut = go ""
  where
    go acc = \case
        [] -> acc
        Gone : Out g : rest -> go (trimEnd acc) (Out (afterRemoved acc g) : rest)
        Gone : rest -> go acc rest
        Out t : rest -> go (acc <> t) rest
    trimEnd = T.dropWhileEnd (\c -> c == ' ' || c == '\t')
    afterRemoved acc g =
        let g1 = fromMaybe g (T.stripPrefix ";" g)
            (line, more) = T.breakOn "\n" g1
            g2 = if T.all isSpace line then T.drop 1 more else g1
         in if "\n\n" `T.isSuffixOf` trimEnd acc && "\n" `T.isPrefixOf` g2 then T.drop 1 g2 else g2


-- | The indentation a statement's code starts with.
statementIndent :: [Part] -> Int -> Text
statementIndent parts n = case drop n parts of
    Stmt s : _
        | let (lead, _) = splitLead s
        , not (T.null lead) ->
            T.takeWhileEnd (== ' ') lead
    _ -> case drop (n - 1) parts of
        Gap g : _ | n > 0 -> T.takeWhileEnd (== ' ') g
        _ -> ""


-- | A statement's leading comment lines and indentation, and its code.
splitLead :: Text -> (Text, Text)
splitLead = go ""
  where
    go acc t =
        let (ws, rest) = T.span isSpace t
         in if "--" `T.isPrefixOf` rest
                then let (line, more) = T.breakOn "\n" rest in go (acc <> ws <> line) more
                else (acc <> ws, rest)


data Kind = CreatesTable | CreatesIndex Text | Comments Address | OtherStatement
    deriving stock (Eq)


classify :: QName -> Text -> Kind
classify table code = case dropWords ["unlogged"] (drop 1 lower) of
    "table" : _ | take 1 lower == ["create"], tableName (dropWords ["if", "not", "exists"] (dropWords ["table"] (dropWords ["unlogged"] (drop 1 ws)))) == Just table -> CreatesTable
    _ -> classifyOther
  where
    classifyOther = case lower of
        "create" : _ | Just (ix, target) <- indexOf (drop 1 ws), target == Just table -> CreatesIndex ix
        "comment" : "on" : _ | Just addr <- commentOn (drop 2 ws), belongs addr -> Comments addr
        _ -> OtherStatement
    ws = T.words (codeOnly code)
    lower = map T.toLower ws
    tableName = \case
        t : _ -> qname (T.takeWhile (/= '(') t)
        [] -> Nothing
    indexOf ts = do
        rest <- stripWord "index" (dropWords ["unique"] ts)
        name : more <- Just (dropWords ["if", "not", "exists"] (dropWords ["concurrently"] rest))
        guard (T.toLower name /= "on")
        target : _ <- dropWords ["only"] <$> stripWord "on" more
        pure (unquote name, qname (T.takeWhile (/= '(') target))
    commentOn ts = do
        kind : rest <- Just ts
        let target = takeWhile ((/= "is") . T.toLower) rest
        case (T.toLower kind, map unquote . nameParts <$> target) of
            ("table", [[s, t]]) -> Just (Address "table" [s, t] [])
            ("column", [[s, t, c]]) -> Just (Address "table column" [s, t, c] [])
            ("index", [[s, n]]) -> Just (Address "index" [s, n] [])
            ("constraint", [[n], ["on"], [s, t]]) -> Just (Address "table constraint" [s, t, n] [])
            _ -> Nothing
    belongs addr = take 2 addr.objectNames == [table.schema, table.name] || addr.objectType == "index"
    qname t = case map unquote (nameParts t) of
        [s, n] -> Just (QName s n)
        [n] -> Just (QName "public" n)
        _ -> Nothing


dropWords :: [Text] -> [Text] -> [Text]
dropWords expected ts =
    let n = length expected
     in if map T.toLower (take n ts) == expected then drop n ts else ts


stripWord :: Text -> [Text] -> Maybe [Text]
stripWord w = \case
    t : rest | T.toLower t == w -> Just rest
    _ -> Nothing


-- | A dotted name's parts, split outside double quotes.
nameParts :: Text -> [Text]
nameParts = map T.pack . go [] False . T.unpack
  where
    go acc _ [] = [reverse acc]
    go acc quoted (c : rest)
        | c == '"' = go (c : acc) (not quoted) rest
        | c == '.' && not quoted = reverse acc : go [] False rest
        | otherwise = go (c : acc) quoted rest


unquote :: Text -> Text
unquote p = case T.stripPrefix "\"" p >>= T.stripSuffix "\"" of
    Just q -> T.replace "\"\"" "\"" q
    Nothing -> T.toLower p


data Region = Code | Quoted | Remark
    deriving stock (Eq)


-- | Each character with whether it is code, inside quotes, or inside a comment.
regions :: Text -> [(Char, Region)]
regions = code . T.unpack
  where
    code = \case
        [] -> []
        '-' : '-' : rest -> let (c, more) = break (== '\n') rest in map (,Remark) ('-' : '-' : c) <> code more
        '/' : '*' : rest -> ('/', Remark) : ('*', Remark) : block (1 :: Int) rest
        q : rest | q == '\'' || q == '"' -> (q, Quoted) : quoted q rest
        c : rest -> (c, Code) : code rest
    quoted q = \case
        [] -> []
        c : d : more | c == q && d == q -> (c, Quoted) : (d, Quoted) : quoted q more
        c : more | c == q -> (c, Quoted) : code more
        c : more -> (c, Quoted) : quoted q more
    block depth = \case
        [] -> []
        '*' : '/' : more -> ('*', Remark) : ('/', Remark) : if depth == 1 then code more else block (depth - 1) more
        '/' : '*' : more -> ('/', Remark) : ('*', Remark) : block (depth + 1) more
        c : more -> (c, Remark) : block depth more


-- | The text with its comments blanked out.
codeOnly :: Text -> Text
codeOnly t = T.pack [if r == Remark then ' ' else c | (c, r) <- regions t]


-- | The words outside quotes, comments and parentheses, lower case, with where they start.
topWords :: Text -> [(Int, Text)]
topWords t = go (0 :: Int) (zip [0 ..] (regions t))
  where
    go _ [] = []
    go depth xs@((k, (c, r)) : rest)
        | r /= Code = go depth rest
        | c == '(' = go (depth + 1) rest
        | c == ')' = go (depth - 1) rest
        | depth == 0 && (isAlpha c || c == '_') =
            let (w, more) = span (\(_, (ch, rr)) -> rr == Code && (isAlphaNum ch || ch == '_' || ch == '$')) xs
             in (k, T.toLower (T.pack (map (fst . snd) w))) : go depth more
        | otherwise = go depth rest


-- | A comment statement with its text replaced, keeping how the file quotes it.
withLiteral :: Text -> Text -> Text
withLiteral new code = case [k | (k, "is") <- topWords code] of
    k : _ ->
        let (sep, old) = T.span isSpace (T.drop (k + 2) code)
            tag = if "$" `T.isPrefixOf` old then Just ("$" <> T.takeWhile (/= '$') (T.drop 1 old) <> "$") else Nothing
         in T.take (k + 2) code <> sep <> literal tag new
    [] -> code


literal :: Maybe Text -> Text -> Text
literal tag t = case tag of
    Just q | not (q `T.isInfixOf` t) -> q <> t <> q
    _
        | "\n" `T.isInfixOf` t && not ("$comment$" `T.isInfixOf` t) -> "$comment$" <> t <> "$comment$"
        | otherwise -> "'" <> T.replace "'" "''" t <> "'"


commentTarget :: Address -> Maybe Text
commentTarget addr = case (addr.objectType, addr.objectNames) of
    ("table", [s, t]) -> Just ("table " <> qualified (QName s t))
    ("table column", [s, t, c]) -> Just ("column " <> qualified (QName s t) <> "." <> quoteIdent c)
    ("index", [s, n]) -> Just ("index " <> qualified (QName s n))
    ("table constraint", [s, t, n]) -> Just ("constraint " <> quoteIdent n <> " on " <> qualified (QName s t))
    _ -> Nothing


commentCode :: Text -> Address -> Text -> Text
commentCode indent addr t =
    let target = fromMaybe "" (commentTarget addr)
     in [i|comment on #{target} is\n#{indent}    #{literal Nothing t}|]


indexCode :: Text -> Text -> Text
indexCode indent def =
    let tidied = tidyIndex def
        guarded = fromMaybe tidied (asum [(new <>) <$> T.stripPrefix old tidied | (old, new) <- [("create unique index ", "create unique index if not exists "), ("create index ", "create index if not exists ")]])
        (front, back) = T.breakOn " on " guarded
     in if T.null back then guarded else front <> "\n" <> indent <> "    " <> T.drop 1 back


-- | A @create table@ statement's text up to its column list, the list's entries, and the
-- rest from the whitespace before the closing parenthesis.
data TableText = TableText
    { opening :: Text
    , elements :: [Element]
    , closing :: Text
    }


-- | One entry of the column list: what precedes it on its own lines, its text, and a
-- comment after it on its last line.
data Element = Element
    { lead :: Text
    , body :: Text
    , trail :: Text
    }


tableText :: Text -> Maybe TableText
tableText code = do
    let indexed = zip [0 :: Int ..] (regions code)
    open <- listToMaybe [n | (n, ('(', Code)) <- indexed]
    let step (depth, cs, close) (n, (c, r))
            | isJust close || r /= Code = (depth, cs, close)
            | c == '(' = (depth + 1, cs, close)
            | c == ')' = if depth == 1 then (depth, cs, Just n) else (depth - 1, cs, close)
            | c == ',' && depth == 1 = (depth, n : cs, close)
            | otherwise = (depth, cs, close)
        (_, commas, closeAt) = foldl' step (1 :: Int, [], Nothing) (drop (open + 1) indexed)
    close <- closeAt
    let cuts = open : reverse commas <> [close]
        raws = zipWith (\a b -> T.take (b - a - 1) (T.drop (a + 1) code)) cuts (drop 1 cuts)
        carried = map (fst . sameLineComment) (drop 1 raws) <> [""]
        rests = take 1 raws <> map (snd . sameLineComment) (drop 1 raws)
        parsed = map element rests
        closingSpace = maybe "" (\(_, _, _, ws) -> ws) (L.lastMaybe parsed)
    pure
        TableText
            { opening = T.take (open + 1) code
            , elements = [Element{lead, body, trail = own <> next} | ((lead, body, own, _), next) <- zip parsed carried]
            , closing = closingSpace <> T.drop close code
            }
  where
    sameLineComment r = case T.breakOn "\n" r of
        (line, rest) | "--" `T.isPrefixOf` T.stripStart line -> (line, rest)
        _ -> ("", r)
    element r =
        let (lead, rest) = splitLead r
            code' = T.stripEnd rest
            ws = T.drop (T.length code') rest
            (body, own) = endComment code'
         in (lead, body, own, ws)
    endComment t =
        let indexed = zip [0 :: Int ..] (regions t)
            lastLine = maybe (-1) fst (L.lastMaybe [x | x@(_, ('\n', _)) <- indexed])
         in case [n | (n, ('-', Remark)) <- indexed, n > lastLine] of
                n : _ -> let body = T.stripEnd (T.take n t) in (body, T.drop (T.length body) t)
                [] -> (t, "")


renderTable :: TableText -> Text
renderTable tt = tt.opening <> T.concat (zipWith piece [1 :: Int ..] tt.elements) <> tt.closing
  where
    count = length tt.elements
    piece k e = e.lead <> e.body <> (if k == count then "" else ",") <> e.trail


data ElementKind = ColumnElement Text [Text] | ConstraintElement (Maybe Text) | OtherElement


elementKind :: QName -> Text -> ElementKind
elementKind table body = case ws of
    w : n : _ | T.toLower w == "constraint" -> ConstraintElement (Just (unquote n))
    w : _ | T.toLower w == "primary" -> ConstraintElement (Just pkey)
    w : _ | T.toLower w == "unique" -> ConstraintElement (named "key" <$> columnList)
    w : _ | T.toLower w == "foreign" -> ConstraintElement (named "fkey" <$> columnList)
    w : _ | T.toLower w `elem` ["check", "exclude"] -> ConstraintElement Nothing
    w : _ | T.toLower w == "like" -> OtherElement
    w : rest ->
        let c = unquote w
            lower = map T.toLower rest
         in ColumnElement
                c
                ( [unquote n | (l, n) <- zip lower (drop 1 rest), l == "constraint"]
                    <> [pkey | "primary" `elem` lower]
                    <> [named "key" [c] | "unique" `elem` lower]
                    <> [named "fkey" [c] | "references" `elem` lower]
                    <> [named "check" [c] | "check" `elem` lower]
                )
    [] -> OtherElement
  where
    pkey = T.intercalate "_" [table.name, "pkey"]
    plain = codeOnly body
    ws = T.words (T.map (\ch -> if ch == '(' || ch == ')' || ch == ',' then ' ' else ch) plain)
    columnList = case T.splitOn "," (T.takeWhile (/= ')') (T.drop 1 (T.dropWhile (/= '(') plain))) of
        [""] -> Nothing
        cs -> Just (map (unquote . T.strip) cs)
    named suffix cs = T.intercalate "_" (table.name : cs) <> "_" <> suffix


-- | Applies the column and constraint differences to a @create table@ statement, with the
-- objects it placed; the column list keeps the file's order and gets new columns after it.
-- @options@ is a @with (...)@ clause to add.
editCreateTable :: QName -> [Text] -> [(Text, Change)] -> [(Text, Change)] -> Maybe Text -> Text -> Maybe (Text, Set Object)
editCreateTable table order columns constraints options code = do
    tt <- tableText code
    let indent = maybe "        " (T.takeWhileEnd (== ' ') . (.lead)) (listToMaybe tt.elements)
        newLead = "\n" <> indent
        kinds = [(e, elementKind table e.body) | e <- tt.elements]
        constraintLead = case L.lastMaybe [e | (e, ConstraintElement _) <- kinds] of
            Just e | "\n\n" `T.isPrefixOf` e.lead -> "\n" <> newLead
            _ -> newLead
        (beforeParen, fromParen) = T.breakOn ")" tt.closing
        closing = case options of
            Just w | not (any ((== "with") . snd) (topWords (T.drop 1 fromParen))) -> Just (beforeParen <> ") " <> w <> T.drop 1 fromParen)
            _ -> Nothing
        inline = Set.fromList [n | (_, ColumnElement _ ns) <- kinds, n <- ns]
        edit (e, kind) = case kind of
            ColumnElement c _ -> case lookup c columns of
                Just (Changed _ def) -> (Just e{body = columnLine c def <> inlineSuffix e.body}, [Column table c])
                Just (Dropped _) -> (Nothing, [Column table c])
                _ -> (Just e, [])
            ConstraintElement (Just n) | n `Set.notMember` inline -> case lookup n constraints of
                Just (Changed _ def) -> (Just e{body = constraintLine indent n def}, [TableConstraint table n])
                Just (Dropped _) -> (Nothing, [TableConstraint table n])
                _ -> (Just e, [])
            _ -> (Just e, [])
        edited = map edit kinds
        kept = [(e, isColumn k) | ((Just e, _), (_, k)) <- zip edited kinds]
        isColumn = \case ColumnElement _ _ -> True; _ -> False
        added = [(c, def) | c <- order, Just (Added def) <- [lookup c columns]]
        addedColumns = [Element newLead (columnLine c def) "" | (c, def) <- added]
        lastColumn = L.lastMaybe [n | (n, (_, True)) <- zip [0 :: Int ..] kept]
        withColumns = case lastColumn of
            Just n -> map fst (take (n + 1) kept) <> addedColumns <> map fst (drop (n + 1) kept)
            Nothing -> addedColumns <> map fst kept
        addedConstraints = [(n, def) | (n, Added def) <- constraints, n `Set.notMember` inline]
        elements = withColumns <> [Element constraintLead (constraintLine indent n def) "" | (n, def) <- addedConstraints]
        placed =
            Set.fromList (concatMap snd edited)
                <> Set.fromList [Column table c | (c, _) <- added]
                <> Set.fromList [TableConstraint table n | (n, _) <- addedConstraints]
                <> Set.fromList [Table table | isJust closing]
    pure (if Set.null placed then code else renderTable tt{elements, closing = fromMaybe tt.closing closing}, placed)


columnLine :: Text -> Text -> Text
columnLine c def =
    let parsed = parseColumn def
     in quoteIdent c <> " " <> columnSql parsed{colType = fromMaybe parsed.colType (T.stripPrefix "public." parsed.colType)}


-- | The inline constraints written after a column's type and options.
inlineSuffix :: Text -> Text
inlineSuffix body = case [k | (k, w) <- drop 1 (topWords body), w `elem` ["constraint", "primary", "unique", "check", "references"]] of
    k : _ -> " " <> T.strip (T.drop k body)
    [] -> ""


constraintLine :: Text -> Text -> Text -> Text
constraintLine indent n def =
    let tidied = fromMaybe def (T.stripPrefix "add constraint " (tidyConstraint [i|add constraint #{quoteIdent n} #{def}|]))
        broken = foldl' (\acc kw -> T.replace (" " <> kw <> " ") ("\n" <> indent <> "    " <> kw <> " ") acc) tidied ["foreign key", "references", "on delete", "on update"]
     in "constraint " <> if "foreign key" `T.isInfixOf` tidied then broken else tidied
