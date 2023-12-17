import Data.Char
import Data.List
import Data.Maybe

-- https://www.markdownguide.org/basic-syntax/
data MDStruc = Content String                             -- Case 0
             | NestBetween MDStruc MDStruc MDStruc
             | NestLeft MDStruc MDStruc
             | NestRight MDStruc MDStruc                  -- Case 1
             | Heading Int MDStruc                        -- Case 2
             | Bold (Either String MDStruc)
             | Italics (Either String MDStruc)            -- Case 3
             | Monospace MDStruc | Code (Either String MDStruc)
                                                          -- Case 4
             | Hyperlink String String | Image String
                                                          -- Case 5
             | Hline | Return                             -- Case 6
             | Block (Maybe MDStruc)                      -- Case 7
             | OrdListStart MDStruc | OrdListItem MDStruc
             | UnordList MDStruc                          -- Case 8
             | NestedUnord MDStruc | NestedOrd MDStruc    -- Case 9
  deriving (Show, Eq)

data Token = ContentT String    -- Case 1
            | Hash              -- Case 2
            | Asterisk | UScore -- Case 3
            | Tick | Tab        -- Case 4
            | LBra | RBra | LPar | RPar | Quo
                                -- Case 5
            | ReturnT | Dash    -- Case 6
            | BlockQuote        -- Case 7
            | List Int | Dot | Plus
                                -- Case 8
            | Err String | PT MDStruc
                                -- Case 9
  deriving (Show, Eq)

-- Does not support Reference-style links

parseMarkdownLine :: String -> Either MDStruc String
parseMarkdownLine bs = lineParser (lineLexer bs)

lineLexer :: String -> [Token]
-- Case 0: Escape character
-- lineLexer ('\\':w:s)    | isSpace w = ContentT " " : lineLexer s
lineLexer ('\\':'\\':s) = ContentT "$\\backslash$" : lineLexer s
lineLexer ('\\':'#':s)  = ContentT "\\#" : lineLexer s
lineLexer ('\\':'*':s)  = ContentT "*"   : lineLexer s
lineLexer ('\\':'_':s)  = ContentT "\\_" : lineLexer s
lineLexer ('\\':'`':s)  = ContentT "`"   : lineLexer s
lineLexer ('\\':'[':s)  = ContentT "["   : lineLexer s
lineLexer ('\\':']':s)  = ContentT "]"   : lineLexer s
lineLexer ('\\':'(':s)  = ContentT "("   : lineLexer s
lineLexer ('\\':')':s)  = ContentT ")"   : lineLexer s
lineLexer ('\\':'"':s)  = ContentT "\""  : lineLexer s --"
lineLexer ('\\':'-':s)  = ContentT "-"   : lineLexer s
lineLexer ('\\':'>':s)  = ContentT "$>$" : lineLexer s
lineLexer ('\\':'<':s)  = ContentT "$<$" : lineLexer s
lineLexer ('\\':'.':s)  = ContentT "."   : lineLexer s
lineLexer ('\\':'+':s)  = ContentT "+"   : lineLexer s
-- Case 1: Text (Content) is actually lexed last
-- Case 2: Hashes
lineLexer ('#':s)   = Hash : lineLexer s
-- Case 3: Asterisks and Underscores
lineLexer ('*':s)   = Asterisk : lineLexer s
lineLexer ('_':s)   = UScore   : lineLexer s
-- Case 4: Backticks and Tab characters
lineLexer ('`':s)     = Tick : lineLexer s
lineLexer (a:b:c:d:s) | (isSpace a && isSpace b && isSpace c && isSpace d) = Tab  : lineLexer s
-- Case 5: Brackets, Parentheses, and Quotation Marks
lineLexer ('[':s)   = LBra : lineLexer s
lineLexer (']':s)   = RBra : lineLexer s
lineLexer ('(':s)   = LPar : lineLexer s
lineLexer (')':s)   = RPar : lineLexer s
lineLexer ('"':s)   = Quo  : lineLexer s
-- Case 6: Return and Dash characters
lineLexer (x:y:s)   | (isSpace x && isSpace y) = ReturnT : lineLexer s
lineLexer ('\\':'n':s) = ReturnT : lineLexer s -- \n
lineLexer ('-':s)   = Dash : lineLexer s
-- Case 7: > character
lineLexer ('>':s)   = BlockQuote : lineLexer s
-- Case 8: Integers, Dots, and Plus signs
lineLexer (n:'.':w:s) | isDigit n && isSpace w = List (digitToInt n) : lineLexer s
lineLexer (n:s)     | isDigit n = ContentT [n] : lineLexer s
lineLexer ('.':s)   = Dot  : lineLexer s
lineLexer ('+':s)   = Plus : lineLexer s
-- Case 1
lineLexer (x:s)     | isPunctuation x = ContentT [x] : lineLexer s
lineLexer (x:s)     | isSymbol x = ContentT ('\\' : [x]) : lineLexer s
lineLexer ('\\':s)       = ContentT "\\"   : lineLexer s
lineLexer (x:s)     | isSpace x = ContentT " " : lineLexer s
lineLexer r@(s:xs)  | isAlpha s = ContentT c : lineLexer t
    where (c,t) = span isAlpha r
-- Empty String
lineLexer ""        = []
-- Everything else :(
lineLexer (s:xs)    = (Err $ "Lexical error: " ++ [s]) : lineLexer xs

lineParser :: [Token] -> Either MDStruc String
lineParser ts = case lineSR [] (reverse $ lineSRC [] ts) of
  [PT t]  -> Left t
  []      -> Left $ Content ""
  [Err e] -> Right e
  s       -> Right ("Line Parsing error: " ++ (show s))

lineSRC :: [Token] -> [Token] -> [Token]
-- Text and Sentences (Content)
lineSRC (PT (Content t) : Dot : ts)              i = lineSRC (PT (Content ("." ++ t)) : ts) i
lineSRC (PT (Content c2) : PT (Content c1) : ts) i = lineSRC (PT (Content (c1 ++ c2)) : ts) i
lineSRC (ContentT c : ts)                        i = lineSRC (PT (Content c) : ts) i
-- Symbols in text
lineSRC (Hash : PT (Content t) : ts)       i = lineSRC (PT (Content (t ++ "\\#")) : ts) i
lineSRC (Dash : PT (Content t) : ts)       i = lineSRC (PT (Content (t ++ "-")) : ts) i
lineSRC (BlockQuote : PT (Content t) : ts) i = lineSRC (PT (Content (t ++ "$>$")) : ts) i
lineSRC (Dot : PT (Content t) : ts)        i = lineSRC (PT (Content (t ++ ".")) : ts) i
lineSRC (Plus : PT (Content t) : ts)       i = lineSRC (PT (Content (t ++ "+")) : ts) i
-- Shifting
lineSRC (Err e : ts) i = [Err e]
lineSRC st      (i:is) = lineSRC (i:st) is
lineSRC st          [] = st





lineSR :: [Token] -> [Token] -> [Token]
-- Headings (Hashes)
lineSR (ReturnT : PT (Content t) : Hash : Hash : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 6 (Content tb)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (Content t) : Hash : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 5 (Content tb)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (Content t) : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 4 (Content tb)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (Content t) : Hash : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 3 (Content tb)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (Content t) : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 2 (Content (tail t))) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (Content t) : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 1 (Content tb)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (NestLeft (Content t) r) : Hash : Hash : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 6 (NestLeft (Content tb) r)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (NestLeft (Content t) r) : Hash : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 5 (NestLeft (Content tb) r)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (NestLeft (Content t) r) : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 4 (NestLeft (Content tb) r)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (NestLeft (Content t) r) : Hash : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 3 (NestLeft (Content tb) r)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (NestLeft (Content t) r) : Hash : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 2 (NestLeft (Content tb) r)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (NestLeft (Content t) r) : Hash : ts)
                            i | isSpace (head t) = lineSR (PT (Heading 1 (NestLeft (Content tb) r)) : ts) i
                                                      where (ta,tb) = span isSpace t
lineSR (ReturnT : PT (NestBetween (Content t1) r (Content t2)) : Hash : Hash : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t1) = lineSR (PT (Heading 6 (NestBetween (Content tb) r (Content t2))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : PT (NestBetween (Content t1) r (Content t2)) : Hash : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t1) = lineSR (PT (Heading 5 (NestBetween (Content tb) r (Content t2))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : PT (NestBetween (Content t1) r (Content t2)) : Hash : Hash : Hash : Hash : ts)
                            i | isSpace (head t1) = lineSR (PT (Heading 4 (NestBetween (Content tb) r (Content t2))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : PT (NestBetween (Content t1) r (Content t2)) : Hash : Hash : Hash : ts)
                            i | isSpace (head t1) = lineSR (PT (Heading 3 (NestBetween (Content tb) r (Content t2))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : PT (NestBetween (Content t1) r (Content t2)) : Hash : Hash : ts)
                            i | isSpace (head t1) = lineSR (PT (Heading 2 (NestBetween (Content tb) r (Content t2))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : PT (NestBetween (Content t1) r (Content t2)) : Hash : ts)
                            i | isSpace (head t1) = lineSR (PT (Heading 1 (NestBetween (Content tb) r (Content t2))) : ts) i
                                                      where (ta,tb) = span isSpace t1
-- Italics AND Bold first (To catch 3 in a row before 2 in a row)
lineSR (Asterisk : PT (Bold c) : Asterisk : ts)
                            i = lineSR (PT (Italics (Right (Bold c))) : ts) i
lineSR (UScore : PT (Bold c) : UScore : ts)
                            i = lineSR (PT (Italics (Right (Bold c))) : ts) i
-- Bold
lineSR (Asterisk : PT (Italics c) : Asterisk : ts)
                            i = lineSR (PT (Bold c) : ts) i
lineSR (UScore : PT (Italics c) : UScore : ts)
                            i = lineSR (PT (Bold c) : ts) i
-- Italics
lineSR (Asterisk : PT c : Asterisk : ts)
                            i = lineSR (PT (Italics (Right c)) : ts) i
lineSR (UScore : PT c : UScore : ts)
                            i = lineSR (PT (Italics (Right c)) : ts) i
-- Nesting Content and Bold/Italics
lineSR (PT c2 : PT (Bold b) : PT c1 : ts)
                            i = lineSR (PT (NestBetween c1 (Bold b) c2) : ts) i
lineSR (PT c2 : PT (Italics b) : PT c1 : ts)
                            i = lineSR (PT (NestBetween c1 (Italics b) c2) : ts) i
lineSR (PT c2 : PT (Bold b) : ts)
                            i = lineSR (PT (NestRight (Bold b) c2) : ts) i
lineSR (PT c2 : PT (Italics b) : ts)
                            i = lineSR (PT (NestRight (Italics b) c2) : ts) i
lineSR (PT (Bold b) : PT c1 : ts)
                            i = lineSR (PT (NestLeft c1 (Bold b)) : ts) i
lineSR (PT (Italics b) : PT c1 : ts)
                            i = lineSR (PT (NestLeft c1 (Italics b)) : ts) i
-- Nesting the nesting!
lineSR (PT (Content t) : PT (NestBetween l1 l2 l3) : ts)
                            i = lineSR (PT (NestLeft (NestBetween l1 l2 l3) (Content t)) : ts) i
lineSR (PT (Content t) : PT (NestRight l1 l2) : ts)
                            i = lineSR (PT (NestLeft (NestRight l1 l2) (Content t)) : ts) i
lineSR (PT (Content t) : PT (NestLeft l1 l2) : ts)
                            i = lineSR (PT (NestLeft (NestLeft l1 l2) (Content t)) : ts) i
lineSR (PT (NestBetween l1 l2 l3) : PT (Content t) : ts)
                            i = lineSR (PT (NestRight (NestBetween l1 l2 l3) (Content t)) : ts) i
lineSR (PT (NestRight l1 l2) : PT (NestRight r1 r2) : ts)
                            i = lineSR (PT (NestRight (NestRight r1 r2) (NestRight l1 l2)) : ts) i
lineSR (PT (NestLeft l1 l2) : PT (NestLeft r1 r2) : ts)
                            i = lineSR (PT (NestLeft (NestLeft r1 r2) (NestLeft l1 l2)) : ts) i
-- Monospace
lineSR (Tick : PT t : Tick : ts)
                            i = lineSR (PT (Monospace t) : ts) i
lineSR (PT c2 : PT (Monospace c) : PT c1 : ts)
                            i = lineSR (PT (NestBetween c1 (Monospace c) c2) : ts) i
lineSR (PT c2 : PT (Monospace c) : ts)
                            i = lineSR (PT (NestRight (Monospace c) c2) : ts) i
lineSR (PT (Monospace c) : PT c1 : ts)
                            i = lineSR (PT (NestLeft c1 (Monospace c)) : ts) i
-- Code
lineSR (ReturnT : PT t : Tab : ts)
                            i = lineSR (PT (Code (Right t)) : ts) i
-- Hyperlinks & Images
lineSR (RPar : PT (Content url) : LPar : RBra : PT (Content text) : LBra : ts)
                            i = lineSR (PT (Hyperlink urlP text) : ts) i
                                  where (urlP,rest) = span (/= ' ') url
lineSR (PT (Hyperlink path alt) : PT (Content e) : ts)
                            i | last e == '!' = lineSR (PT (Image path) : ts) i
lineSR (RBra : LBra : ts)   i = lineSR (PT (Content ("[" ++ "]")) : ts) i
lineSR (RPar : LPar : ts)   i = lineSR (PT (Content ("(" ++ ")")) : ts) i
-- Nest those (^) in text
lineSR (PT c2 : PT (Image b) : PT c1 : ts)
                            i = lineSR (PT (NestBetween c1 (Image b) c2) : ts) i
lineSR (PT c2 : PT (Hyperlink a b) : PT c1 : ts)
                            i = lineSR (PT (NestBetween c1 (Hyperlink a b) c2) : ts) i
lineSR (PT c2 : PT (Image b) : ts)
                            i = lineSR (PT (NestRight (Image b) c2) : ts) i
lineSR (PT c2 : PT (Hyperlink a b) : ts)
                            i = lineSR (PT (NestRight (Hyperlink a b) c2) : ts) i
lineSR (PT (Image b) : PT c1 : ts)
                            i = lineSR (PT (NestLeft c1 (Image b)) : ts) i
lineSR (PT (Hyperlink a b) : PT c1 : ts)
                            i = lineSR (PT (NestLeft c1 (Hyperlink a b)) : ts) i
-- Hline
lineSR (ReturnT : Asterisk : Asterisk : Asterisk : ts)
                            i = lineSR (PT (Hline) : ts) i
lineSR (ReturnT : UScore : UScore : UScore : ts)
                            i = lineSR (PT (Hline) : ts) i
lineSR (ReturnT : Dash : Dash : Dash : ts)
                            i = lineSR (PT (Hline) : ts) i
-- There is a possibility here to create an hline at the end of a line
-- which is not supported by Markdown but is supported by LaTeX
-- so we won't try and stop it.

-- Block
lineSR (ReturnT : PT (NestLeft (NestLeft (Content t1) x) r) : BlockQuote : ts)
                            i | isSpace (head t1) = lineSR (PT (Block (Just (NestLeft (NestLeft (Content tb) x) r))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : PT (NestBetween (Content t1) r (Content t2)) : BlockQuote : ts)
                            i | isSpace (head t1) = lineSR (PT (Block (Just (NestBetween (Content tb) r (Content t2)))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : PT (NestLeft (Content t1) r) : BlockQuote : ts)
                            i | isSpace (head t1) = lineSR (PT (Block (Just (NestLeft (Content tb) r))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : PT (Content t1) : BlockQuote : ts)
                            i | isSpace (head t1) = lineSR (PT (Block (Just (Content tb))) : ts) i
                                                      where (ta,tb) = span isSpace t1
lineSR (ReturnT : BlockQuote : ts)
                            i = lineSR (PT (Block Nothing) : ts) i
lineSR (Quo : PT (Content t) : Quo : ts)
                            i = lineSR (PT (Content ("\"" ++ t ++ "\"")) : ts) i
-- Ordered
lineSR (ReturnT : PT t : List 1 : ts)
                            i = lineSR (PT (OrdListStart t) : ts) i
lineSR (ReturnT : PT t : List n : ts)
                            i = lineSR (PT (OrdListItem t) : ts) i
-- Unordered Lists
lineSR (ReturnT : PT (Content c) : Asterisk : ts)
                            i = lineSR (PT (UnordList (Content t)) : ts) i
                                  where (r,t) = span isSpace c
lineSR (ReturnT : PT (Content c) : Dash : ts)
                            i = lineSR (PT (UnordList (Content t)) : ts) i
                                  where (r,t) = span isSpace c
lineSR (ReturnT : PT (Content c) : Plus : ts)
                            i = lineSR (PT (UnordList (Content t)) : ts) i
                                  where (r,t) = span isSpace c
lineSR (ReturnT : PT e : PT (Content c) : Asterisk : ts)
                            i | c == " " = lineSR (PT (UnordList e) : ts) i
lineSR (ReturnT : PT e : PT (Content c) : Dash : ts)
                            i | c == " " = lineSR (PT (UnordList e) : ts) i
lineSR (ReturnT : PT e : PT (Content c) : Plus : ts)
                            i | c == " " = lineSR (PT (UnordList e) : ts) i
lineSR (ReturnT : PT t : Asterisk : ts)
                            i = lineSR (PT (UnordList t) : ts) i
lineSR (ReturnT : PT t : Dash : ts)
                            i = lineSR (PT (UnordList t) : ts) i
lineSR (ReturnT : PT t : Plus : ts)
                            i = lineSR (PT (UnordList t) : ts) i
-- Return character (two spaces) in text
lineSR (PT t : ReturnT : ts)
                            i = lineSR (PT t : PT (Content "  ") : ts) i
-- Unnesting the empty string
lineSR (PT (OrdListStart (NestLeft (NestLeft (NestLeft (Content "") l) s) w)) : ts)
                            i = lineSR (PT (OrdListStart (NestLeft (NestLeft l s) w)) : ts) i
lineSR (PT (Block (Just (NestLeft (Content "") t))) : ts)
                            i = lineSR (PT (Block (Just t)) : ts) i
lineSR (PT (UnordList (NestLeft (Content " ") t)) : ts)
                            i = lineSR (PT (UnordList t) : ts) i
lineSR (PT (OrdListStart (NestLeft (Content "") t)) : ts)
                            i = lineSR (PT (OrdListStart t) : ts) i
lineSR (PT (OrdListItem (NestLeft (Content "") t)) : ts)
                            i = lineSR (PT (OrdListItem t) : ts) i
-- Nested Lists
lineSR (PT (OrdListStart t) : Tab : Tab : ts)
                            i = lineSR (PT (NestedOrd (NestedOrd (OrdListStart t))) : ts) i
lineSR (PT (OrdListItem t) : Tab : Tab : ts)
                            i = lineSR (PT (NestedOrd (NestedOrd (OrdListItem t))) : ts) i
lineSR (PT (UnordList t) : Tab : Tab : ts)
                            i = lineSR (PT (NestedUnord (NestedUnord (UnordList t))) : ts) i
lineSR (PT (OrdListStart t) : Tab : ts)
                            i = lineSR (PT (NestedOrd (OrdListStart t)) : ts) i
lineSR (PT (OrdListItem t) : Tab : ts)
                            i = lineSR (PT (NestedOrd (OrdListItem t)) : ts) i
lineSR (PT (UnordList t) : Tab : ts)
                            i = lineSR (PT (NestedUnord (UnordList t)) : ts) i
-- Shifting
lineSR (Err e : ts) i = [Err e]
lineSR st      (i:is) = lineSR (i:st) is
lineSR st          [] = st



parseMarkdownFile :: [Either MDStruc String] -> [Either MDStruc String]
parseMarkdownFile = fileParserMediate []

fileParserMediate :: [MDStruc] -> [Either MDStruc String] -> [Either MDStruc String]
fileParserMediate mds [] = map Left (fileParser $ reverse mds)
fileParserMediate mds (x:xs) = case x of
    Right n -> Right n : fileParserMediate mds xs
    Left s  -> fileParserMediate (s:mds) xs

fileParser :: [MDStruc] -> [MDStruc]
fileParser = foldr merge []

merge :: MDStruc -> [MDStruc] -> [MDStruc]
-- Nested Code or BlockQuotes
merge x@(Code a)  (y@(Code b):ys)  = merge (mergeCodes  x y) ys
merge x@(Block a) (y@(Block b):ys) = merge (mergeBlocks x y) ys
-- Singly Nested Lists
merge x@(OrdListItem a)  (y@(OrdListItem b):ys) = merge (mergeOrdLists   x y) ys
merge x@(OrdListStart a) (y@(OrdListItem b):ys) = merge (mergeOrdLists   x y) ys
merge x@(UnordList a)    (y@(UnordList b):ys)   = merge (mergeUnordLists x y) ys
merge x@(NestedOrd a)    (y@(NestedOrd b):ys)   = merge (mergeNestedList x y) ys
merge x@(NestedUnord a)  (y@(NestedUnord b):ys) = merge (mergeNestedList x y) ys
-- Nested Nested Lists
merge x@(OrdListStart a)  (y@(NestedOrd b):ys)   = merge (mergeNestedList x y) ys
merge x@(OrdListStart a)  (y@(NestedUnord b):ys) = merge (mergeNestedList x y) ys
merge x@(OrdListItem a)   (y@(NestedOrd b):ys)   = merge (mergeNestedList x y) ys
merge x@(OrdListItem a)   (y@(NestedUnord b):ys) = merge (mergeNestedList x y) ys
merge x@(UnordList a)     (y@(NestedOrd b):ys)   = merge (mergeNestedList x y) ys
merge x@(UnordList a)     (y@(NestedUnord b):ys) = merge (mergeNestedList x y) ys
-- Base Case / Everything Else
merge x [] = [x]
merge x (y:ys) = x : y : ys

mergeBlocks :: MDStruc -> MDStruc -> MDStruc
mergeBlocks (Block (Just a)) (Block (Just b)) = Block (Just (Content ("    " ++ (convertToLaTeX a) ++ "\n" ++ "    " ++ (convertToLaTeX b))))
mergeBlocks (Block (Just a)) (Block Nothing)  = Block (Just (Content ("    " ++ (convertToLaTeX a) ++ "\n")))
mergeBlocks (Block Nothing)  (Block (Just b)) = Block (Just (Content ("\n"   ++ "    " ++ (convertToLaTeX b))))
mergeBlocks a _ = a

mergeCodes :: MDStruc -> MDStruc -> MDStruc
mergeCodes (Code (Right a)) (Code (Right b)) = Code (Left ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeCodes a _ = a

mergeOrdLists :: MDStruc -> MDStruc -> MDStruc
mergeOrdLists (OrdListItem a)  (OrdListItem b) = OrdListStart (Content ((convertToLaTeX a) ++ "\n" ++ "    \\item " ++ (convertToLaTeX b)))
mergeOrdLists (OrdListStart a) (OrdListItem b) = OrdListStart (Content ((convertToLaTeX a) ++ "\n" ++ "    \\item " ++ (convertToLaTeX b)))
mergeOrdLists a _ = a

mergeUnordLists :: MDStruc -> MDStruc -> MDStruc
mergeUnordLists (UnordList a) (UnordList b) = UnordList (Content ((convertToLaTeX a) ++ "\n" ++ "    \\item " ++ (convertToLaTeX b)))
mergeUnordLists a _ = a

mergeNestedList :: MDStruc -> MDStruc -> MDStruc
mergeNestedList (OrdListStart a) (NestedOrd b)   = OrdListStart (Content ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeNestedList (OrdListStart a) (NestedUnord b) = OrdListStart (Content ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeNestedList (OrdListItem a)  (NestedOrd b)   = OrdListItem  (Content ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeNestedList (OrdListItem a)  (NestedUnord b) = OrdListItem  (Content ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeNestedList (UnordList a)    (NestedOrd b)   = UnordList    (Content ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeNestedList (UnordList a)    (NestedUnord b) = UnordList    (Content ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeNestedList (NestedOrd a)    (NestedOrd b)   = NestedOrd    (Content ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeNestedList (NestedUnord a)  (NestedUnord b) = NestedUnord  (Content ((convertToLaTeX a) ++ "\n" ++ (convertToLaTeX b)))
mergeNestedList a _ = a

convertToLaTeX :: MDStruc -> String
convertToLaTeX t = case t of
    Content a          -> a
    NestBetween a b c  -> convertToLaTeX a       ++ convertToLaTeX b   ++ convertToLaTeX c
    NestLeft a b       -> convertToLaTeX a       ++ convertToLaTeX b
    NestRight a b      -> convertToLaTeX a       ++ convertToLaTeX b
    Heading 1 str      -> "\\section{"           ++ convertToLaTeX str ++ "}"
    Heading 2 str      -> "\\subsection{"        ++ convertToLaTeX str ++ "}"
    Heading 3 str      -> "\\subsubsection{"     ++ convertToLaTeX str ++ "}"
    Heading 4 str      -> "\\paragraph{"         ++ convertToLaTeX str ++ "}"
    Heading 5 str      -> "\\paragraph{"         ++ convertToLaTeX str ++ "}"
    Heading 6 str      -> "\\paragraph{"         ++ convertToLaTeX str ++ "}"
    Bold (Left str)    -> "\\textbf{"            ++ str                ++ "}"
    Bold (Right it)    -> "\\textbf{"            ++ convertToLaTeX it  ++ "}"
    Italics (Left str) -> "\\textit{"            ++ str                ++ "}"
    Italics (Right it) -> "\\textit{"            ++ convertToLaTeX it  ++ "}"
    Monospace mon      -> "\\texttt{"            ++ convertToLaTeX mon ++ "}"
    Code (Left str)    -> "\\begin{verbatim}\n"  ++ str                ++ "\n\\end{verbatim}"
    Code (Right md)    -> "\\begin{verbatim}\n"  ++ convertToLaTeX md  ++ "\n\\end{verbatim}"
    Hyperlink url str  -> "\\href{" ++ url ++ "}{" ++ str              ++ "}"
    Image ref          -> "\\includegraphics{"   ++ ref                ++ "}"
    Hline              -> "\\_\\hrulefill"
    Return             -> "\\\\"
    Block (Just md)    -> "\\enquote{"         ++ convertToLaTeX md ++ "}"
    OrdListStart md    -> "\\begin{enumerate}\n    \\item " ++ convertToLaTeX md ++ "\n\\end{enumerate}"
    OrdListItem md     -> "\\begin{enumerate}\n    \\item " ++ convertToLaTeX md ++ "\n\\end{enumerate}"
    UnordList md       -> "\\begin{itemize}\n    \\item "   ++ convertToLaTeX md ++ "\n\\end{itemize}"
    NestedOrd md       -> "\\begin{enumerate}\n    \\item " ++ convertToLaTeX md ++ "\n\\end{enumerate}"
    NestedUnord md     -> "\\begin{itemize}\n    \\item "   ++ convertToLaTeX md ++ "\n\\end{itemize}"

prelude = ["\\documentclass{article}","% Package for using images","    \\usepackage{graphicx}","","% Code to handle lists","    % Ensures nested ordered lists start at 1","    \\renewcommand{\\theenumii}{\\arabic{enumii}}","    \\renewcommand{\\labelenumii}{\\theenumii.}","","    % Ensures nested unordered lists are represented by a hollow circle like in Markdown","    \\renewcommand{\\labelitemii}{$\\circ$}","","% Remove section numbering as well","    \\renewcommand{\\thesection}{}","    \\renewcommand{\\thesubsection}{}","    \\renewcommand{\\thesubsubsection}{}","","% Package and additional code for handling hyperlink formatting","    \\usepackage{hyperref}","    \\hypersetup{","        colorlinks=true,","        linkcolor=black,","        filecolor=magenta,","        urlcolor=blue,","        pdftitle={Markdown Converted to LaTeX},","        pdfpagemode=FullScreen,","        }","    \\urlstyle{same}","    \\renewcommand{\\UrlFont}{\\normalfont\\itshape\\underline}","    \\usepackage[dvipsnames]{xcolor}","","% Package for handling block quotes","    \\usepackage{csquotes}","","\\begin{document}","","% This removes page numbers from every page,","% Markdown doesn't use them so we don't need them!","    \\pagestyle{empty}",""]

main :: IO ()
main = do
        putStrLn "Enter the filename including .md: "
        inputMarkdownFilename <- getLine
        markdownContent       <- readFile inputMarkdownFilename
        let markdownLines      = lines markdownContent
        let markdownStructures = map parseMarkdownLine markdownLines
        let markdownParsed     = parseMarkdownFile markdownStructures
        writeFile "output.tex" (unlines prelude)
        reformatEnv (concatMap lines (appendProcess markdownParsed))
        appendFile "output.tex" "\n\\end{document}"
        putStrLn "Done!"

reformatEnv :: [String] -> IO ()
reformatEnv []       = return ()
reformatEnv [x]      = appendFile "output.tex" x
reformatEnv (x:y:zs) | ((x == "\\end{enumerate}") && (y == "\\begin{enumerate}")) = reformatEnv zs
                     | ((x == "\\end{itemize}")   && (y == "\\begin{itemize}"))   = reformatEnv zs
                     | ((x == "\\end{enumerate}") && (y == "") && not (null zs) && (head zs == "\\begin{enumerate}")) = reformatEnv (tail zs)
                     | ((x == "\\end{itemize}")   && (y == "") && not (null zs) && (head zs == "\\begin{itemize}"))   = reformatEnv (tail zs)
                     | (x == "    \\item \\begin{itemize}")   = reformatEnv ("\\begin{itemize}"   : y : zs)
                     | (x == "    \\item \\begin{enumerate}") = reformatEnv ("\\begin{enumerate}" : y : zs)
                     | otherwise = do
                                     appendFile "output.tex" x
                                     appendFile "output.tex" "\n"
                                     reformatEnv (y:zs)


appendProcess :: [Either MDStruc String] -> [String]
appendProcess markdownStructures = concatMap processEach markdownStructures
  where
    processEach markdownStructure = case markdownStructure of
        Right e -> [("\\textbf{ERROR:} " ++ e)]
        Left s  -> [convertToLaTeX s, "\n"]



-- Tests!

test = and (lineLexerTests ++ lineParserTests ++ parseLineTests)

lineLexerTests = [
  lineLexer "Hello" == [ContentT "Hello"]
  , lineLexer " Hello" == [ContentT " ",ContentT "Hello"]
  , lineLexer " Hello " == [ContentT " ",ContentT "Hello",ContentT " "]
  , lineLexer "Hello " == [ContentT "Hello",ContentT " "]
  , lineLexer "# Hi" == [Hash,ContentT " ",ContentT "Hi"]
  , lineLexer "# Hi  " == [Hash,ContentT " ",ContentT "Hi",ReturnT]
  , lineLexer "## Hi  " == [Hash,Hash,ContentT " ",ContentT "Hi",ReturnT]
  , lineLexer "### Hi  " == [Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT]
  , lineLexer "#### Hi  " == [Hash,Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT]
  , lineLexer "##### Hi  " == [Hash,Hash,Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT]
  , lineLexer "###### Hi  " == [Hash,Hash,Hash,Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT]
  , lineLexer "####### Hi  " == [Hash,Hash,Hash,Hash,Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT]
  , lineLexer "**Hi**" == [Asterisk,Asterisk,ContentT "Hi",Asterisk,Asterisk]
  , lineLexer "_Hello_" == [UScore,ContentT "Hello",UScore]
  , lineLexer "`MDStruc`" == [Tick,ContentT "MDStruc",Tick]
  , lineLexer "    Tabbed!" == [Tab,ContentT "Tabbed",ContentT "!"]
  , lineLexer "[]" == [LBra,RBra]
  , lineLexer "[Hello]" == [LBra,ContentT "Hello",RBra]
  , lineLexer "[.*_-+!?]" == [LBra,Dot,Asterisk,UScore,Dash,Plus,ContentT "!",ContentT "?",RBra]
  , lineLexer "()" == [LPar,RPar]
  , lineLexer "\"To be or not to be\"" == [Quo,ContentT "To",ContentT " ",ContentT "be",ContentT " ",ContentT "or",ContentT " ",ContentT "not",ContentT " ",ContentT "to",ContentT " ",ContentT "be",Quo]
  , lineLexer "  " == [ReturnT]
  , lineLexer "Return  " == [ContentT "Return",ReturnT]
  , lineLexer "  Returned" == [ReturnT,ContentT "Returned"]
  , lineLexer "> Quote this  " == [BlockQuote,ContentT " ",ContentT "Quote",ContentT " ",ContentT "this",ReturnT]
  , lineLexer "1" == [ContentT "1"]
  , lineLexer "I like the number 3" == [ContentT "I",ContentT " ",ContentT "like",ContentT " ",ContentT "the",ContentT " ",ContentT "number",ContentT " ",ContentT "3"]
  , lineLexer "1. " == [List 1]
  , lineLexer "2003\\." == [ContentT "2",ContentT "0",ContentT "0",ContentT "3",ContentT "."]
  ]

lineParserTests = [
  lineParser [ContentT "Hello"] == Left (Content "Hello")
  , lineParser [ContentT " ",ContentT "Hello",ContentT " "] == Left (Content " Hello ")
  , lineParser [ContentT " ",ContentT "Hello"] == Left (Content " Hello")
  , lineParser [ContentT "Hello",ContentT " "] == Left (Content "Hello ")
  , lineParser [Hash,ContentT " ",ContentT "Hi"] == Right "Line Parsing error: [PT (Content \" Hi\"),Hash]"
  , lineParser [Hash,ContentT " ",ContentT "Hi",ReturnT] == Left (Heading 1 (Content "Hi"))
  , lineParser [Hash,Hash,ContentT " ",ContentT "Hi",ReturnT] == Left (Heading 2 (Content "Hi"))
  , lineParser [Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT] == Left (Heading 3 (Content "Hi"))
  , lineParser [Hash,Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT] == Left (Heading 4 (Content "Hi"))
  , lineParser [Hash,Hash,Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT] == Left (Heading 5 (Content "Hi"))
  , lineParser [Hash,Hash,Hash,Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT] == Left (Heading 6 (Content "Hi"))
  , lineParser [Hash,Hash,Hash,Hash,Hash,Hash,Hash,ContentT " ",ContentT "Hi",ReturnT] == Right "Line Parsing error: [PT (Heading 6 (Content \"Hi\")),Hash]"
  , lineParser [Hash,Hash,Hash,ContentT " ",ContentT "#",ReturnT] == Left (Heading 3 (Content "#"))
  , lineParser [Asterisk,Asterisk,ContentT "Hi",Asterisk,Asterisk] == Left (Bold (Right (Content "Hi")))
  , lineParser [UScore,ContentT "Hello",UScore] == Left (Italics (Right (Content "Hello")))
  , lineParser [Tick,ContentT "MDStruc",Tick] == Left (Monospace (Content "MDStruc"))
  , lineParser [Tab,ContentT "Tabbed",ContentT "!",ReturnT] == Left (Code (Right (Content "Tabbed!")))
  , lineParser [LBra,RBra] == Left (Content "[]")
  , lineParser [LBra,ContentT "Hello",RBra] == Right "Line Parsing error: [RBra,PT (Content \"Hello\"),LBra]"
  , lineParser [ContentT "[Hello]"] == Left (Content "[Hello]")
  , lineParser [LBra,Dot,Asterisk,UScore,Dash,Plus,ContentT "!",ContentT "?",RBra] == Right "Line Parsing error: [RBra,PT (Content \"!?\"),Plus,Dash,UScore,Asterisk,Dot,LBra]"
  , lineParser [ContentT "[.*_-+!?]"] == Left (Content "[.*_-+!?]")
  , lineParser [LPar,RPar] == Left (Content "()")
  , lineParser [Quo,ContentT "To",ContentT " ",ContentT "be",ContentT " ",ContentT "or",ContentT " ",ContentT "not",ContentT " ",ContentT "to",ContentT " ",ContentT "be",Quo] == Left (Content "\"To be or not to be\"")
  , lineParser [ReturnT] == Right "Line Parsing error: [ReturnT]"
  , lineParser [ContentT "Return",ReturnT] == Right "Line Parsing error: [ReturnT,PT (Content \"Return\")]"
  , lineParser [ReturnT,ContentT "Returned"] == Right "Line Parsing error: [PT (Content \"Returned\"),PT (Content \"  \")]"
  , lineParser [BlockQuote,ContentT " ",ContentT "Quote",ContentT " ",ContentT "this",ReturnT] == Left (Block (Just (Content "Quote this")))
  , lineParser [ContentT "1"] == Left (Content "1")
  , lineParser [ContentT "I",ContentT " ",ContentT "like",ContentT " ",ContentT "the",ContentT " ",ContentT "number",ContentT " ",ContentT "3"] == Left (Content "I like the number 3")
  , lineParser [List 1,ContentT "Hi",ReturnT] == Left (OrdListStart (Content "Hi"))
  , lineParser [ContentT "2",ContentT "0",ContentT "0",ContentT "3",Dot] == Left (Content "2003.")
  ]

parseLineTests = [
  -- Basic text
  parseMarkdownLine "Hello" == Left (Content "Hello")
  , parseMarkdownLine "Hello World" == Left (Content "Hello World")

  -- Simple Bold and Italics
  , parseMarkdownLine "*HI*" == Left (Italics (Right (Content "HI")))
  , parseMarkdownLine "**HI**" == Left (Bold (Right (Content "HI")))
  , parseMarkdownLine "***HI***" == Left (Italics (Right (Bold (Right (Content "HI")))))
  , parseMarkdownLine "_HI_" == Left (Italics (Right (Content "HI")))
  , parseMarkdownLine "__HI__" == Left (Bold (Right (Content "HI")))
  , parseMarkdownLine "___HI___" == Left (Italics (Right (Bold (Right (Content "HI")))))
  , parseMarkdownLine "*_HI_*" == Left (Bold (Right (Content "HI")))
  , parseMarkdownLine "_*HI*_" == Left (Bold (Right (Content "HI")))
  , parseMarkdownLine "_**HI**_" == Left (Italics (Right (Bold (Right (Content "HI")))))
  , parseMarkdownLine "*_*HI*_*" == Left (Italics (Right (Bold (Right (Content "HI")))))
  , parseMarkdownLine "**_HI_**" == Left (Italics (Right (Bold (Right (Content "HI")))))

  -- Nested Styling
  , parseMarkdownLine "__*Mixed* Styling__" == Left (Bold (Right (NestRight (Italics (Right (Content "Mixed"))) (Content " Styling"))))
  , parseMarkdownLine "**Bold _Italic_**" == Left (Bold (Right (NestLeft (Content "Bold ") (Italics (Right (Content "Italic"))))))
  , parseMarkdownLine "_Italic **Bold**_" == Left (Italics (Right (NestLeft (Content "Italic ") (Bold (Right (Content "Bold"))))))
  , parseMarkdownLine "**_BoldItalic_**" == Left (Italics (Right (Bold (Right (Content "BoldItalic")))))

  -- Escaped Characters
  , parseMarkdownLine "\\*Not Bold\\*" == Left (Content "*Not Bold*")
  , parseMarkdownLine "Escaped \\# Hashtag" == Left (Content "Escaped \\# Hashtag")

  -- Mixed Content within Lists
  , parseMarkdownLine "1. Item 1  " == Left (OrdListStart (Content "Item 1"))
  , parseMarkdownLine "- Item  " == Left (UnordList (Content "Item"))
  , parseMarkdownLine "+ ![Alt text](/path/to/img.jpg)  " == Left (UnordList (Image "/path/to/img.jpg"))
  , parseMarkdownLine "- [Link](http://example.com)  " == Left (UnordList (Hyperlink "http://example.com" "Link"))

  -- Blockquotes with Nested Formatting
  , parseMarkdownLine "> **Bold Quote**  " == Left (Block (Just (Bold (Right (Content "Bold Quote")))))
  , parseMarkdownLine "> _Italic Quote_  " == Left (Block (Just (Italics (Right (Content "Italic Quote")))))

  -- Code Blocks and Inline Code
  , parseMarkdownLine "    code block  " == Left (Code (Right (Content "code block")))
  , parseMarkdownLine "`inline code`" == Left (Monospace (Content "inline code"))

  -- Headers with Different Styling
  , parseMarkdownLine "# Header with **Bold**  " == Left (Heading 1 (NestLeft (Content "Header with ") (Bold (Right (Content "Bold")))))

  -- Lists with Incorrect Numbers
  , parseMarkdownLine "1. First  " == Left (OrdListStart (Content "First"))

  -- Images with Alt Text
  , parseMarkdownLine "![Alt text](/path/to/img.jpg)" == Left (Image "/path/to/img.jpg")

  -- Horizontal Rules
  , parseMarkdownLine "---  " == Left (Hline)
  , parseMarkdownLine "***  " == Left (Hline)
  , parseMarkdownLine "___  " == Left (Hline)

  -- Edge Cases
  , parseMarkdownLine "Unclosed *Italic" == Right "Line Parsing error: [PT (Content \"Italic\"),Asterisk,PT (Content \"Unclosed \")]"
  , parseMarkdownLine "No space-List" == Left (Content "No space-List")

  ]
