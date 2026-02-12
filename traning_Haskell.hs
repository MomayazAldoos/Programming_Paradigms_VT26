isEven :: Int -> Bool
isEven x = mod x 2 == 0

myMax :: Integer -> Integer -> Integer
myMax x y = if x > y then x else y 


--recursion repetition
repet :: Integer -> String -> String
repet number lst 
    | number <= 0 = ""
    | null lst = ""
    | otherwise = lst ++ " " ++ repet (number - 1) lst


-- adding character after each character in a string
addChar :: Char -> String -> String
addChar c xs 
    | null xs = ""
    | otherwise = (head xs) : c : addChar c (tail xs)



-- Create a guissning game
gissningGame :: Integer -> IO()
gissningGame secretNumber = do
    putStrLn "Guess a number between 1 and 99:"
    guess <- getLine
    let number = read guess :: Integer
    if number < 1 || number > 99
        then do
            putStrLn "You must guess a number between 1 and 99."
            gissningGame secretNumber
        else if number < secretNumber
            then do
                putStrLn "Too low!"
                gissningGame secretNumber
            else if number > secretNumber
                then do
                    putStrLn "Too high!"
                    gissningGame secretNumber
                else do
                    putStrLn "Correct!"

-- Creating a funtion that returns the biggest two digits of an array 
-- applying the help function makeNumber.

biggestTwoDigit :: [Int] -> Int
biggestTwoDigit ls = maximum (getAllPairs ls)


getAllPairs :: [Int] -> [Int]
getAllPairs [] = []
getAllPairs [x] = [x] 
getAllPairs (x:xs) = map (makeNumber x) xs ++ getAllPairs xs
    where 
        makeNumber a b = a * 10 + b


-- shuffling a list depending on the position of the elements.

shuffling :: [elements] -> [elements]
shuffling [] = []
shuffling [x] = [x]
shuffling ls = oddElements ls ++ shuffling (evenElements ls)
    where 
        -- extract odd elements depending on their position
        oddElements [] = []
        oddElements [x] = [x]
        oddElements (x:_:xs) = x : oddElements xs
        -- extract even elements depending on their position
        evenElements [] = []
        evenElements [x] = []
        evenElements (_:y:ys) = y : evenElements ys


main :: IO()
main = do
    putStrLn (show (isEven 4)) 
    putStrLn (show (isEven 5)) 
    putStrLn(show (myMax 3 5))
    putStrLn (repet 3 "You are doing great! Never stop, you will make it ")
    putStrLn (addChar '-' "Hello")
    putStrLn(addChar ' ' "Momayaz")
    gissningGame 42
    putStrLn (show (biggestTwoDigit [8, 2, 3, 4, 7]))
    putStrLn (show (biggestTwoDigit [81, 2, 3, 4, 9]))
    putStrLn (shuffling("Hello, World!"))
    putStrLn (show(shuffling([1,2,3,4,5,6,7,8,9])))

