--
-- Simple XML parser.
--

module XML where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

type Tag = String

-- A character entity.
data Entity = LT_E | GT_E | AMP_E
   deriving (Show)

-- A single element of an XML document.
data Elem =
    TextE String     -- raw text
  | EntE Entity      -- entity
  | FormE Tag [Elem] -- tagged data
  deriving (Show)

----------------------------------------------------------------------
-- Simple parsers.
----------------------------------------------------------------------

-- Parse an entity.
parseEntity :: Parser Entity
parseEntity = 
  char '&' >>
  ((string "lt" >> return LT_E)
   <|> (string "gt" >> return GT_E)
   <|> (string "amp" >> return AMP_E))
  <?> "entity"

-- Parse some text not containing any tags or entities.
parseText :: Parser String
parseText = 
  many1 (noneOf ['<', '>', '&'])
  <?> "text"

-- Parse a tag name, which is a nonempty sequence of letters,
-- all alphabetic characters or digits (no symbols), either
-- lower-case or upper-case.
parseTag :: Parser Tag
parseTag = 
  many1 alphaNum
  <?> "tag"

----------------------------------------------------------------------
-- Form parsers.
----------------------------------------------------------------------

-- Parse a single tagged form.
parseTagged :: Parser (Tag, [Elem])
parseTagged = do
  char '<'
  tag <- parseTag
  char '>'
  xs <- many parseElem
  string ("</" ++ tag ++ ">")
  return (tag, xs)
  <?> "tagged form"

-- Parse a single XML expression.
parseElem :: Parser Elem
parseElem = 
  try (parseTagged >>= return . (\(x, ys) -> FormE x ys))
  <|> (parseEntity >>= return . EntE)
  <|> (parseText >>= return . TextE)
  <?> "XML element"

-- Parse a series of elements from a string representing the entire contents of
-- a file.
parseElemsFromFile :: Parser [Elem]
parseElemsFromFile = do
  xs <- many parseElem
  eof
  return xs 
  <?> "file of XML elements"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print an XML element.
ppXML :: Int -> Elem -> String
ppXML i (TextE t)  = indent i ++ "Text[\n" ++ t ++ "\n" ++ indent i ++ "]\n"
ppXML i (EntE e)   = indent i ++ "Entity[" ++ show e ++ "]"
ppXML i (FormE t es) = 
  indent i ++ "Form: {" ++ t ++ "}[\n" 
  ++ concatMap (\s -> ppXML (i + 2) s ++ "\n") es
  ++ indent i ++ "]{/" ++ t ++ "}\n"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpXML :: FilePath -> IO ()
runPpXML f = do
  p <- parseFromFile parseElemsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppXML 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpXML "test.xml"

