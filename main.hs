import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator (choice)

data Markdown = Heading Int String
              | Italic String
              | Bold String
              | Plain Char
              | TextBlock [Markdown]
              | Structure [Markdown]
              | Empty

instance Show Markdown where 
    show (Heading i s) = concat ["<h", show i, ">", s, "</h", show i, ">"]
    show (Italic s) = concat ["<i>", s, "</i>"]
    show (Bold s) = concat ["<b>", s, "</b>"]
    show (Plain c) = c:""
    show (TextBlock xs) = "<p>" ++ (concatMap show xs) ++ "</p>"
    show (Structure md) = concatMap show md
    show Empty = ""

-- |The 'tryAll' function does what 'choice' is supposed to do, that is is tries
-- each individual parser from left to right and returns the result of the first
-- successful parse.
tryAll :: [GenParser Char st a] -> GenParser Char st a
tryAll = foldr (\a b -> try a <|> b) (fail "Initial Failure")

manyTill1 :: GenParser Char st a -> GenParser Char st b -> GenParser Char st [a]
manyTill1 p q = (:) <$> p <*> manyTill p q

-- |End of Block parser 
eob :: GenParser Char st String
eob = try $ string "\n\n"

eof' :: GenParser Char st String
eof' = eof >> return []

whiteNoise :: GenParser Char st Markdown
whiteNoise = space >> return Empty

-- |The 'headingLevelParser' takes as input the level of the header (1-6) and 
-- returns the heading string for that level.
headingLevelParser :: Int -> GenParser Char st Markdown
headingLevelParser level = do
    count level (char '#')
    skipMany1 (char ' ')
    str <- manyTill anyChar (string "\n" <|> eof')
    return (Heading level str)
headingParser = tryAll . map headingLevelParser $ [1..6]

boldParser :: GenParser Char st Markdown
boldParser = do 
    let surround = (string "**") <|> (string "__")
    surround
    text <- manyTill anyChar (surround <|> lookAhead eob)
    if text == "" 
       then fail "Didn't find text between Bold strings" 
    else return (Bold text)

italicParser :: GenParser Char st Markdown
italicParser = do 
    let surround = (string "*") <|> (string "_")
    surround
    text <- manyTill anyChar (surround <|> lookAhead eob)
    if text == "" 
       then fail "Didn't find text between italic strings" 
    else return (Italic text)

plainParser :: GenParser Char st Markdown
plainParser = Plain <$> (tryAll [space >> return ' ', anyChar])

textBlockParser :: GenParser Char st Markdown
textBlockParser = do
    t <- manyTill1 (tryAll [boldParser, italicParser, plainParser]) (eob <|> eof')
    return $ TextBlock t

markdownParser :: GenParser Char st Markdown
markdownParser = Structure <$> (many . tryAll $ [whiteNoise, headingParser, textBlockParser])

parseMarkdown :: String -> Either ParseError Markdown
parseMarkdown = parse markdownParser "unknown"

main = do
    print $ parseMarkdown "## hellow world"
    print $ parseMarkdown " hellow world\n"
    print $ parseMarkdown "## hellow world \n**hello world\n\n** *goodbye*\n\n"

