main :: IO ()
main = do
        putStrLn "Input file: "
        ifile <- getLine
        putStrLn "Output file: "
        ofile <- getLine
        input <- readFile ifile
        writeFile ofile (markdownToHTML input)
        putStrLn "Converting successful\n"

markdownToHTML :: String -> String
markdownToHTML input =
    let linesOfText = lines input
        convertedLines = convertLines linesOfText
        body           = unlines convertedLines
    in unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "<meta charset=\"UTF-8\">"
        , "<title>Converted Markdown</title>"
        , "</head>"
        , "<body>"
        , body
        , "</body>"
        , "</html>"
        ]

-- Process whole input line-by-line, handling lists
convertLines :: [String] -> [String]
convertLines [] = []
convertLines (('-':' ':listItem) : xs) =
    let (items, restLines) = span isListItem xs
        firstItem = "<li>" ++ convertEmphasis listItem ++ "</li>"
        allItems = firstItem : map toLi items
    in ["<ul>"] ++ allItems ++ ["</ul>"] ++ convertLines restLines
convertLines (line:rest) = convert line : convertLines rest

-- Check if a line is a list item
isListItem :: String -> Bool
isListItem ('-':' ':_) = True
isListItem _ = False -- Note: _ is a wildcard pattern which means "anything else"


-- Turn a list line into <li>
toLi :: String -> String
toLi ('-':' ':rest) = "<li>" ++ convertEmphasis rest ++ "</li>"
toLi other = other

-- Sort markdown elements by line
convert :: String -> String

-- Heading conversions
convert ('#':' ':rest)  = "<h1>" ++ rest ++ "</h1>"
convert ('#':'#':' ':rest) = "<h2>" ++ rest ++ "</h2>"
convert ('#':'#':'#':' ':rest) = "<h3>" ++ rest ++ "</h3>"
convert ('#':'#':'#':'#':' ':rest) = "<h4>" ++ rest ++ "</h4>"
convert ('#':'#':'#':'#':'#':' ':rest) = "<h5>" ++ rest ++ "</h5>"
convert ('#':'#':'#':'#':'#':'#':' ':rest) = "<h6>" ++ rest ++ "</h6>"

-- Plain text conversion
convert "" = ""
convert text = "<p>" ++ convertEmphasis text ++ "</p>"

-- Emphasis conversion
convertEmphasis :: String -> String
convertEmphasis [] = []
convertEmphasis ('*':'*':rest) =
    let (boldText, remaining) = spanUntilDoubleAsterisk rest
    in "<strong>" ++ boldText ++ "</strong>" ++ convertEmphasis remaining
convertEmphasis ('_':rest) =
    let (italicText, remaining) = spanUntilUnderscore rest
    in "<em>" ++ italicText ++ "</em>" ++ convertEmphasis remaining
convertEmphasis (currentChar:rest) = currentChar : convertEmphasis rest

spanUntilDoubleAsterisk :: String -> (String, String)
spanUntilDoubleAsterisk [] = ([], [])
spanUntilDoubleAsterisk ('*':'*':rest) = ([], rest)
spanUntilDoubleAsterisk (currentChar:restOfString) =
    let (collectedText, remainingText) = spanUntilDoubleAsterisk restOfString
    in (currentChar : collectedText, remainingText)

spanUntilUnderscore :: String -> (String, String)
spanUntilUnderscore [] = ([], [])
spanUntilUnderscore ('_':rest) = ([], rest)
spanUntilUnderscore (currentChar:restOfString) =
    let (collectedText, remainingText) = spanUntilUnderscore restOfString
    in (currentChar : collectedText, remainingText)