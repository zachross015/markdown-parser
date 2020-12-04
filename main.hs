import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator (choice)

data Markdown = Heading Int String
              | Italic String
              | Bold String
              | Plain Char
              deriving (Show)

tryParsers :: [GenParser Char st Markdown] -> GenParser Char st Markdown
tryParsers = foldr (\a b -> try a <|> b) (fail "Initial Failure")

headingLevelParser :: Int -> GenParser Char st Markdown
headingLevelParser level = do
    count level (char '#')
    skipMany1 (char ' ')
    str <- manyTill anyToken (char '\n')
    return (Heading level str)
headingParser = tryParsers . map headingLevelParser $ [1..6]

boldParser :: GenParser Char st Markdown
boldParser = do 
    let surround = (string "**") <|> (string "__")
    surround
    text <- manyTill anyToken surround
    if text == "" 
       then fail "Didn't find text between Bold strings" 
    else return (Bold text)

italicParser :: GenParser Char st Markdown
italicParser = do 
    let surround = (string "*") <|> (string "_")
    surround
    text <- manyTill anyToken surround
    if text == "" 
       then fail "Didn't find text between italic strings" 
    else return (Italic text)

plainParser :: GenParser Char st Markdown
plainParser = do
    c <- anyToken
    return (Plain c)

markdownParser :: GenParser Char st [Markdown]
markdownParser = many . tryParsers $ [headingParser, boldParser, italicParser, plainParser]

parseMarkdown :: String -> Either ParseError [Markdown]
parseMarkdown = parse markdownParser "unknown"

main = do
    print $ parseMarkdown "## hellow world \n**hello world** goodbye"

