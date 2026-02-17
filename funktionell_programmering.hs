--Uppgift 1: Sträng Funktioner
            -- a) mulString
mulString :: Integer -> String -> String
mulString n str
    | n <= 0 = ""
    | null str = ""
    | otherwise = str ++ mulString (n - 1) str



           -- b) addChar 
addChar :: Char -> String -> String
addChar _ " " = " "                     -- Vi returnerar tom sträng om vi har en tom sträng.
addChar _ [x] = [x]                     -- vi returnerar första bokstanen om vi har bara den.  
addChar c (x:xs) = x : c : addChar c xs -- vi adderar  till första bokstaven och sen vi gör
                                        -- en rekursiv anrop för resten av strängen.


-- Uppgift 2: gissinspelet
gissinspelet :: Integer -> IO()
gissinspelet secretNumber = do
    putStrLn "Guess a number between 1 and 99:"
    guess <- getLine
    let number = read guess :: Integer
    if number < 1 || number > 99
        then do
            putStrLn "You must guess a number between 1 and 99."
            
            gissinspelet secretNumber
        else if number < secretNumber
            then do
                putStrLn "Too low!"
                gissinspelet secretNumber
            else if number > secretNumber
                then do 
                    putStrLn "Too high!"
                    gissinspelet secretNumber
                else do 
                    putStrLn "Correct!"

--Uppgift 3: översättning C till Haskell
f :: [Int] -> [Int]
f lst = go lst 0
    where 
        go [] _ = []
        go (x:xs) i
            | x >= 24 = i : go xs (i + 1)
            | otherwise = go xs (i + 1)


--Uppgift 4: Listor med siffror

maxTwoDigit :: [Int] -> Int
maxTwoDigit lst = maximum (getAllPairs lst)


getAllPairs :: [Int] -> [Int]
getAllPairs [] = []
getAllPairs [x] = [x] 
getAllPairs (x:xs) = map (makeNumber x) xs ++ getAllPairs xs
    where 
        makeNumber a b = a * 10 + b



--Uppgift 5: Listskyffling (2p)
skyffla :: [a] -> [a]
skyffla [] = []
skyffla lst = oddElements lst ++ skyffla (evenElements lst)
  where
    -- Tar varannat element (1:a, 3:e, 5:e, etc)
    oddElements [] = []
    oddElements [x] = [x]
    oddElements (x:_:xs) = x : oddElements xs
    
    -- Tar de andra elementen (2:a, 4:e, 6:e, etc)
    evenElements [] = []
    evenElements [x] = []
    evenElements (_:y:xs) = y : evenElements xs


main :: IO ()
main = do 
    -- testar mulString funktionen
    putStrLn (mulString 2 "hej")
    putStrLn ("###############")
    putStrLn (mulString 0 "hej")
    putStrLn ("###############")
    putStrLn (mulString (-2) "hej")
    putStrLn ("###############")
    putStrLn (mulString 3 "")

    -- Testar addChar funktionen
    putStrLn (addChar ' ' "hej")
    putStrLn (addChar 'x' "hej")
    putStrLn (addChar 'a' "hej olle")
    putStrLn (addChar 'a' " ")

    -- Test gissinspelet
    gissinspelet 63 

    -- Testar f funktionen
    putStrLn (show (f [19,24,12,38,59,9]))

    -- Testar maxTwoDigit funktionen
    putStrLn (show (maxTwoDigit [1, 8, 3, 5, 1, 2, 3, 7, 9 ]))
    putStrLn (show (maxTwoDigit [ 8 ]))
    
    -- Testar skyffla funktionen
    putStrLn "### Testar skyffla ###"
    putStrLn (show (skyffla [1..9]))
    putStrLn (show (skyffla [-3,1,9,100,92,111,-321,42]))
    putStrLn (skyffla "Hello, World!")

