-- Markdown to HTML converter

-- Dit is de main functie van de converter. Wanneer je de code runt zal je eerst een prompt krijgen om een input en output file te geven.
-- De input file moet markdown tekst bevatten en de output file zal de geconverteerde HTML tekst bevatten.
-- Je moet dus wel eerst de input en output file aanmaken voordat je de code runt.
-- Voor het gemak van het uitvoeren heb ik al een input file toegevoegd met de naam "input.md" en een lege output file met de naam "output.html".
main :: IO ()
main = do
        putStrLn "Input file: "
        ifile <- getLine
        putStrLn "Output file: "
        ofile <- getLine
        input <- readFile ifile
        writeFile ofile (markdownToHTML input)
        putStrLn "Converting successful"


-- Markdown converter
-- Wanneer de main functie het nieuwe bestand gaat schrijven, roept hij deze functie aan om de markdown tekst om te zetten naar HTML.
markdownToHTML :: String -> String
markdownToHTML input =
    let linesOfText = lines input -- De lines functie splitst de input string op in een lijst van strings, waarbij elke string een regel tekst is.
        convertedLines = convertLines linesOfText -- Hier worden de regels omgezet naar HTML.
        body           = unlines convertedLines -- Vervolgens worden de omgezette regels weer samengevoegd tot één string.
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
        ] -- Hier wordt de HTML boilerplate toegevoegd. En aangegeven waar de body van de HTML moet komen te staan.

-- List conversion
-- Deze functie converteert elke regel in de lijst van strings naar HTML, door middel van recursie.
convertLines :: [String] -> [String] -- De input is een lijst van strings (regels) en de output is ook een lijst van strings (omgezette regels).
convertLines [] = [] -- Basis geval van de recursie. Als de lijst leeg is, is de output ook een lege lijst.
convertLines (('-':' ':listItem) : rest) = -- Als de regel begint met "- ", wordt deze regel als een lijst item gezien.
    let (items, restLines) = span isListItem rest -- Span zorgt ervoor dat alle volgende regels die ook lijst items zijn worden verzameld in een de lijst items. De rest van de regels worden opgeslagen in restLines.
        firstItem = "<li>" ++ convertEmphasis listItem ++ "</li>" -- Het eerste lijst item wordt apart geconverteerd omdat deze al in de eerste regel zit.
        allItems = firstItem : map toLi items -- Alle lijst items worden omgezet naar HTML.
    in ["<ul>"] ++ allItems ++ ["</ul>"] ++ convertLines restLines -- Vervolgens worden de omgezette lijst items tussen <ul> en </ul> tags geplaatst. Daarna wordt de rest van de regels ook omgezet naar HTML door middel van recursie.
convertLines (line:rest) = convertSingleLine line : convertLines rest -- Als de regel geen lijst item is, wordt deze omgezet naar HTML met de convertSingleLine functie.

-- Deze functie controleert of een regel een lijst item is.
isListItem :: String -> Bool
isListItem ('-':' ':_) = True
isListItem _ = False -- Note: _ is een wildcard pattern dat betekend "alles wat niet hierboven staat".

-- Deze functie converteert een lijst item naar HTML.
toLi :: String -> String
toLi ('-':' ':rest) = "<li>" ++ convertEmphasis rest ++ "</li>"
toLi other = other

-- Single line conversion
-- In deze functie worden headings en paragrafen omgezet naar HTML. Aangezien een regel maar één van deze dingen kan zijn, is er geen overlap.
convertSingleLine :: String -> String

-- Heading conversions
convertSingleLine ('#':' ':rest)  = "<h1>" ++ rest ++ "</h1>" -- In het geval dat een regel begint met "# ", wordt deze omgezet naar een h1 heading. Hetzelfde geldt voor de andere heading levels.
convertSingleLine ('#':'#':' ':rest) = "<h2>" ++ rest ++ "</h2>"
convertSingleLine ('#':'#':'#':' ':rest) = "<h3>" ++ rest ++ "</h3>"
convertSingleLine ('#':'#':'#':'#':' ':rest) = "<h4>" ++ rest ++ "</h4>"
convertSingleLine ('#':'#':'#':'#':'#':' ':rest) = "<h5>" ++ rest ++ "</h5>"
convertSingleLine ('#':'#':'#':'#':'#':'#':' ':rest) = "<h6>" ++ rest ++ "</h6>"

-- Plain text conversion
convertSingleLine "" = "" -- Als de regel leeg is, wordt er ook een lege string teruggegeven.
convertSingleLine text = "<p>" ++ convertEmphasis text ++ "</p>" -- In de paragrafen kunnen wel emphasis tags voorkomen, dus die worden ook omgezet via de convertEmphasis functie.

-- Emphasis conversion
-- In deze functie worden emphasis tags omgezet naar HTML. Dit kan zowel bold als italics zijn.
convertEmphasis :: String -> String
convertEmphasis [] = []
convertEmphasis ('*':'*':rest) = -- Wanneer er "**" wordt gevonden, wordt de tekst tot de volgende "**" als bold gemarkeerd.
    let (boldText, remaining) = spanUntilDoubleAsterisk rest -- De spanUntilDoubleAsterisk functie verzamelt alle tekst tot de volgende "**" en splitst dit van de bold tekst.
    in "<strong>" ++ boldText ++ "</strong>" ++ convertEmphasis remaining -- Hier wordt de bold tekst tussen <strong> tags geplaatst en wordt de rest van de tekst ook omgezet via recursie.
convertEmphasis ('_':rest) =
    let (italicText, remaining) = spanUntilUnderscore rest -- Hetzelfde geldt voor "_" en italics. Alleen wordt hier <em> gebruikt in plaats van <strong> en een andere span functie aangeroepen.
    in "<em>" ++ italicText ++ "</em>" ++ convertEmphasis remaining
convertEmphasis (currentChar:rest) = currentChar : convertEmphasis rest -- Als er geen emphasis tags worden gevonden, wordt het huidige karakter toegevoegd aan de output en wordt de rest van de tekst ook omgezet via recursie.

-- Deze functie verzamelt alle tekst tot de volgende "**".
spanUntilDoubleAsterisk :: String -> (String, String) -- Als output geeft deze functie de verzamelde tekst en de rest van de tekst terug.
spanUntilDoubleAsterisk [] = ([], [])
spanUntilDoubleAsterisk ('*':'*':rest) = ([], rest) -- Wanneer er "**" wordt gevonden, stopt de verzameling en wordt de rest van de tekst teruggegeven.
spanUntilDoubleAsterisk (currentChar:restOfString) = -- Als er geen "**" wordt gevonden, wordt het huidige karakter toegevoegd aan de verzamelde tekst en wordt de rest van de tekst ook verzameld via recursie.
    let (collectedText, remainingText) = spanUntilDoubleAsterisk restOfString
    in (currentChar : collectedText, remainingText)

-- Deze functie verzamelt alle tekst tot de volgende "_".
-- Deze methode werkt hetzelfde als de bovenstaande functie, maar dan voor italics. Het is mij niet gelukt om één functie te maken die voor beide werkt. Dus vandaar heb ik twee aparte functies gemaakt.
spanUntilUnderscore :: String -> (String, String)
spanUntilUnderscore [] = ([], [])
spanUntilUnderscore ('_':rest) = ([], rest)
spanUntilUnderscore (currentChar:restOfString) =
    let (collectedText, remainingText) = spanUntilUnderscore restOfString
    in (currentChar : collectedText, remainingText)