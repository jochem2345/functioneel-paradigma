-- pattern List3 :: a -> a -> a -> [a]
-- pattern List3 a b c = [a, b, c]

converter text = do
    let chapter = text ++ " - Chapter 1"
    putStrLn chapter

convert :: String -> String
-- Sort markdown elements by line


-- Heading conversions
convert ('#':' ':rest)  = "<h1>" ++ rest ++ "</h1>"
convert ('#':'#':' ':rest) = "<h2>" ++ rest ++ "</h2>"
convert ('#':'#':'#':' ':rest) = "<h3>" ++ rest ++ "</h3>"
convert ('#':'#':'#':'#':' ':rest) = "<h4>" ++ rest ++ "</h4>"
convert ('#':'#':'#':'#':'#':' ':rest) = "<h5>" ++ rest ++ "</h5>"
convert ('#':'#':'#':'#':'#':'#':' ':rest) = "<h6>" ++ rest ++ "</h6>"

-- Emphasis conversion
-- Iets met bold en italic
convert ('*':'*':rest) = "<strong>" ++ rest ++ "</strong>"

-- Unordered List conversion
convert ('-':' ':rest) = "<ul><li>" ++ rest ++ "</li></ul>"

-- Plain text conversion
convert text = "<p>" ++ text ++ "</p>"
