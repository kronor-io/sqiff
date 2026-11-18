{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Rewrites a table's deploy file so it creates the table as a database has it, keeping
-- the file's text wherever it already matches.
module Sqiff.Rewrite (
    Rewrite (..),
    rewriteFile,
    fileDrift,
) where

import Data.Char (isDigit, isSpace)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Sqiff.Definition hiding (tableText)
import Sqiff.Diff (Change (..), Changes, diff)
import Sqiff.Identifier
import Sqiff.Model
import Sqiff.Recipe (columnSql)
import Sqiff.Script (splitStatements)
import Sqiff.Sqitch.Units (tidyConstraint, tidyIndex)
import Sqiff.Sql (Region (..), codeOnly, regions, replaceInCode, topWords)
import Sqiff.Sql qualified as Sql


data Rewrite = Rewrite
    { text :: Text
    , unplaced :: [Text]
    -- ^ Differences the rewrite leaves for a person, one line each.
    }


-- | What differs between the table the file creates and the one in the database, without
-- grants, validity, and the triggers, policies and indexes that live in files of their own.
fileDrift :: Set Identifier -> Schema -> Schema -> Changes
fileDrift ownFiles fromFile live = Map.filterWithKey kept (diff same fromFile live)
  where
    same k a b = a == b || normalise k a == normalise k b
    -- A file cannot say whether a constraint is validated, nor name a not-null
    -- constraint the way PostgreSQL names it after a rename.
    normalise k = \case
        ColumnDefinition c -> ColumnDefinition c{nullability = plainNotNull c.nullability}
        ConstraintDefinition c | TableConstraint{} <- k.object -> ConstraintDefinition (withoutValidity c)
        def -> def
    plainNotNull = \case
        NotNull _ name -> NotNull Validated (mfilter (not . defaultNotNullName) name)
        Nullable -> Nullable
    defaultNotNullName n = "_not_null" `T.isSuffixOf` T.dropWhileEnd isDigit (nameText n)
    kept k c = k.aspect /= Privileges && not (elsewhere k.object c)
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
rewriteFile :: QName -> Set Identifier -> [Identifier] -> Schema -> Schema -> Text -> Either Text Rewrite
rewriteFile table ownFiles order live fromFile original = do
    parts <- pieces original
    let changes = fileDrift ownFiles fromFile live
        typedDefinitions = [(k.object, c) | (k, c) <- Map.toList changes, k.aspect == Definition]
        definitions = [(o, printed <$> c) | (o, c) <- typedDefinitions]
        comments = Map.fromList [(k.object, printed <$> c) | (k, c) <- Map.toList changes, k.aspect == Comment]
        statements = [(n, s, classify table (snd (splitLead s))) | (n, Stmt s) <- zip [0 :: Int ..] parts]
        indentOf n = statementIndent parts n
        settings = case lookup (Table table) typedDefinitions of
            Just (Changed (TableDefinition fileDef) (TableDefinition liveDef)) -> tableSettings table fileDef liveDef
            _ -> Nothing
        tableEdit = do
            (n, s, _) <- L.find (\(_, _, k) -> k == CreatesTable) statements
            let (lead, code) = splitLead s
                columns = [(c, ch') | (Column t c, ch) <- typedDefinitions, t == table, Just ch' <- [traverse asColumn ch]]
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
                [ maybe Set.empty (\(_, _, h) -> Set.map definitionKey (Set.filter (not . isTable) h)) tableEdit
                , Set.fromList [definitionKey o | (_, _, o) <- indexEdits]
                , Set.fromList [ObjectKey Comment a | (_, _, a) <- commentEdits]
                , if isJust indexAnchor then Set.fromList [definitionKey (Index q) | (q, _) <- newIndexes] else Set.empty
                , if isJust commentAnchor then Set.fromList [ObjectKey Comment a | (a, _) <- newComments] else Set.empty
                , if settingsPlaced then Set.singleton (definitionKey (Table table)) else Set.empty
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
    -- The table counts as handled only when its settings are placed, above.
    isTable = \case
        Table _ -> True
        _ -> False


-- | For a table whose settings the file lacks, the statements that turn them on and the
-- storage options for @create table@; nothing when the file has a setting the table lacks,
-- or other options than it.
tableSettings :: QName -> TableDef -> TableDef -> Maybe ([Text], Maybe Text)
tableSettings table file live = do
    let name = qualified table
        added setting = setting live && not (setting file)
        alters =
            [[i|alter table #{name} enable row level security|] | added (.rowSecurity)]
                <> [[i|alter table #{name} force row level security|] | added (.forceRowSecurity)]
    guard (file.unlogged == live.unlogged && file.partitionBy == live.partitionBy && file.partitionOf == live.partitionOf)
    -- The file may lack a setting the table has, not have one the table lacks.
    guard ((live.rowSecurity || not file.rowSecurity) && (live.forceRowSecurity || not file.forceRowSecurity))
    guard (null file.options || file.options == live.options)
    pure (alters, if file.options == live.options then Nothing else withOptions live.options)


describeChange :: ObjectKey -> Change a -> Text
describeChange k c =
    let name = render k.object <> if k.aspect == Comment then " (comment)" else ""
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
    go rest (s : ss) = case [(stmt, found) | stmt <- [s, snd (splitLead s)], let found = T.breakOn stmt rest, not (T.null (snd found))] of
        [] -> Left "a statement cannot be found in the file's text"
        (stmt, (before, after)) : _ -> (\more -> Gap before : Stmt stmt : more) <$> go (T.drop (T.length stmt) after) ss


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


data Kind = CreatesTable | CreatesIndex Identifier | Comments ObjectId | OtherStatement
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
        t : _ -> parseQName (T.takeWhile (/= '(') t)
        [] -> Nothing
    indexOf ts = do
        rest <- stripWord "index" (dropWords ["unique"] ts)
        name : more <- Just (dropWords ["if", "not", "exists"] (dropWords ["concurrently"] rest))
        guard (T.toLower name /= "on")
        target : _ <- dropWords ["only"] <$> stripWord "on" more
        pure (identifier (unquote name), parseQName (T.takeWhile (/= '(') target))
    commentOn ts = do
        kind : rest <- Just ts
        let target = takeWhile ((/= "is") . T.toLower) rest
        case (T.toLower kind, map unquote . nameParts <$> target) of
            ("table", [[s, t]]) -> Just (Table (qname s t))
            ("column", [[s, t, c]]) -> Just (Column (qname s t) (identifier c))
            ("index", [[s, n]]) -> Just (Index (qname s n))
            ("constraint", [[n], ["on"], [s, t]]) -> Just (TableConstraint (qname s t) (identifier n))
            _ -> Nothing
    belongs = \case
        Index _ -> True
        Table q -> q == table
        object -> owningTable object == Just table
    parseQName t = case map unquote (nameParts t) of
        [s, n] -> Just (qname s n)
        [n] -> Just (qname "public" n)
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
    go acc inQuotes (c : rest)
        | c == '"' = go (c : acc) (not inQuotes) rest
        | c == '.' && not inQuotes = reverse acc : go [] False rest
        | otherwise = go (c : acc) inQuotes rest


unquote :: Text -> Text
unquote p = case T.stripPrefix "\"" p >>= T.stripSuffix "\"" of
    Just q -> T.replace "\"\"" "\"" q
    Nothing -> T.toLower p


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
    Just q | fits q -> q <> t <> q
    _
        | "\n" `T.isInfixOf` t && fits "$comment$" -> "$comment$" <> t <> "$comment$"
        | otherwise -> Sql.literal t
  where
    fits q = not (q `T.isInfixOf` (t <> "$"))


commentTarget :: ObjectId -> Maybe Text
commentTarget = \case
    Table t -> Just ("table " <> qualified t)
    Column t c -> Just ("column " <> qualified t <> "." <> quoted c)
    Index ix -> Just ("index " <> qualified ix)
    TableConstraint t n -> Just ("constraint " <> quoted n <> " on " <> qualified t)
    _ -> Nothing


commentCode :: Text -> ObjectId -> Text -> Text
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


data ElementKind = ColumnElement Identifier [Identifier] | ConstraintElement (Maybe Identifier) | OtherElement


elementKind :: QName -> Text -> ElementKind
elementKind table body = case ws of
    w : n : _ | T.toLower w == "constraint" -> ConstraintElement (Just (identifier (unquote n)))
    w : _ | T.toLower w == "primary" -> ConstraintElement (Just pkey)
    w : _ | T.toLower w == "unique" -> ConstraintElement (named "key" <$> columnList)
    w : _ | T.toLower w == "foreign" -> ConstraintElement (named "fkey" <$> columnList)
    w : _ | T.toLower w `elem` ["check", "exclude"] -> ConstraintElement Nothing
    w : _ | T.toLower w == "like" -> OtherElement
    w : rest ->
        let c = unquote w
            lower = map T.toLower rest
         in ColumnElement
                (identifier c)
                ( [identifier (unquote n) | (l, n) <- zip lower (drop 1 rest), l == "constraint"]
                    <> [pkey | "primary" `elem` lower]
                    <> [named "key" [c] | "unique" `elem` lower]
                    <> [named "fkey" [c] | "references" `elem` lower]
                    <> [named "check" [c] | "check" `elem` lower]
                )
    [] -> OtherElement
  where
    -- The names PostgreSQL gives constraints declared without one.
    pkey = identifier (T.intercalate "_" [nameText table.name, "pkey"])
    plain = codeOnly body
    ws = T.words (T.map (\ch -> if ch == '(' || ch == ')' || ch == ',' then ' ' else ch) plain)
    columnList = case T.splitOn "," (T.takeWhile (/= ')') (T.drop 1 (T.dropWhile (/= '(') plain))) of
        [""] -> Nothing
        cs -> Just (map (unquote . T.strip) cs)
    named suffix cs = identifier (T.intercalate "_" (nameText table.name : cs) <> "_" <> suffix)


-- | Applies the column and constraint differences to a @create table@ statement, with the
-- objects it placed; the column list keeps the file's order and gets new columns after it.
-- @options@ is a @with (...)@ clause to add.
editCreateTable :: QName -> [Identifier] -> [(Identifier, Change ColumnDef)] -> [(Identifier, Change Text)] -> Maybe Text -> Text -> Maybe (Text, Set ObjectId)
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
        kept = keep "" (zip edited kinds)
        keep pending = \case
            [] -> []
            ((Just e, _), (_, k)) : rest -> (e{lead = pending <> e.lead}, isColumn k) : keep "" rest
            ((Nothing, _), (e, _)) : rest -> keep (pending <> separateComments e.lead) rest
        separateComments l = case T.breakOnEnd "\n\n" l of
            (before, _) | "--" `T.isInfixOf` before -> T.dropEnd 1 before
            _ -> ""
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


-- | A column's line in @create table@, its type without the @public.@ the catalog prints.
columnLine :: Identifier -> ColumnDef -> Text
columnLine c def =
    let unqualified = typeName (fromMaybe (typeText def.colType) (T.stripPrefix "public." (typeText def.colType)))
     in quoted c <> " " <> columnSql def{colType = unqualified}


-- | The inline constraints written after a column's type and options.
inlineSuffix :: Text -> Text
inlineSuffix body = case [k | (k, w) <- drop 1 (topWords body), w `elem` ["constraint", "primary", "unique", "check", "references"]] of
    k : _ -> " " <> T.strip (T.drop k body)
    [] -> ""


constraintLine :: Text -> Identifier -> Text -> Text
constraintLine indent n def =
    let tidied = fromMaybe def (T.stripPrefix "add constraint " (tidyConstraint [i|add constraint #{quoted n} #{def}|]))
        broken = foldl' (\acc kw -> replaceInCode (" " <> kw <> " ") ("\n" <> indent <> "    " <> kw <> " ") acc) tidied ["foreign key", "references", "on delete", "on update"]
     in "constraint " <> if "foreign key" `T.isInfixOf` tidied then broken else tidied
