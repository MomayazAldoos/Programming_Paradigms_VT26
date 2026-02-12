# Programming Paradigms VT2026 - Lab 4

## Functional Programming in Haskell

This lab demonstrates fundamental functional programming concepts using Haskell through the implementation of various functions in `funktionell_programmering.hs`.

## 📁 Main File

`funktionell_programmering.hs` - Complete Haskell implementation with all lab functions

## 🎯 Functions Implemented

### 1. String Multiplication (`mulString`)
Repeats a string n times using recursion.
```haskell
mulString :: Integer -> String -> String
mulString 3 "hej"  -- Result: "hejhejhej"
```

### 2. Character Insertion (`addChar`)
Inserts a character between each character in a string.
```haskell
addChar :: Char -> String -> String
addChar 'x' "hej"  -- Result: "hxexj"
```

### 3. Guessing Game (`gissinspelet`)
Interactive number guessing game with input validation.
```haskell
gissinspelet :: Integer -> IO()
```

### 4. List Processing (`f`)
Processes a list and returns indices where elements are >= 24.
```haskell
f :: [Int] -> [Int]
f [19,24,12,38,59,9]  -- Result: [1,3,4]
```

### 5. Maximum Two-Digit Number (`maxTwoDigit`)
Finds the maximum two-digit number that can be formed from list elements.
```haskell
maxTwoDigit :: [Int] -> Int
maxTwoDigit [1,8,3,5,1,2,3,7,9]  -- Result: 89
```

### 6. List Shuffling (`skyffla`)
Performs a specific shuffling pattern on lists.
```haskell
skyffla :: [a] -> [a]
skyffla [1,2,3,4,5,6,7,8]  -- Result: [1,3,5,7,2,6,4,8]
```

## 🚀 How to Run

### Prerequisites
- Install Haskell: https://www.haskell.org/downloads/
- Or use GHCi (Glasgow Haskell Compiler Interactive)

### Running the Code

#### Option 1: Run the Complete Program
```bash
runhaskell funktionell_programmering.hs
```

#### Option 2: Interactive GHCi
```bash
ghci funktionell_programmering.hs
```
Then test individual functions:
```haskell
*Main> skyffla [1,2,3,4]
[1,3,2,4]
*Main> addChar '-' "hello"
"h-e-l-l-o"
*Main> maxTwoDigit [1,8,3,5]
85
```

## 📊 Expected Output

When you run `runhaskell funktionell_programmering.hs`, you'll see:
- String multiplication examples
- Character insertion demonstrations  
- Interactive guessing game
- List processing results
- Maximum two-digit calculations
- List shuffling examples

## 🎮 Interactive Features

The program includes an interactive guessing game (`gissinspelet`). When you run the main program, you'll be prompted to guess numbers between 1-99.

## 📝 Sample Output

```
"hejhej"
###############
""
###############
""
###############
""
"h e j"
"hxexj"
"haejaeoalaleaeja"
" "
Guess a number between 1 and 99:
[1,3,4]
89
8
### Testar skyffla ###
[1,3,5,7,9,2,6,4,8]
[-3,1,100,111,-321,92,9,42]
"Hlo ol!e,rdlW"
```

## 🔧 Technical Notes

- **Language**: Haskell (functional programming paradigm)
- **Key Concepts**: Recursion, pattern matching, guards, higher-order functions
- **List Processing**: Demonstrates various list manipulation techniques
- **I/O Operations**: Shows how to handle user input in functional programming

## 📚 Learning Objectives

- Understand recursive function design
- Master pattern matching and guards
- Learn list processing techniques
- Practice functional programming principles
- Explore Haskell's type system

## 🤝 Contributing

This is a lab assignment. Feel free to explore the code and experiment with different inputs to better understand functional programming concepts!

---
*Created for Programming Paradigms Course VT2026*