module Lib where
import System.Random
--import Data.List
import System.IO
import System.Directory

toList :: [Char] -> Int -> [Char] -> [[Char]]
toList allWords index curWord
    | index >= length (allWords) = curWord : []
    | allWords !! index == '\n' = curWord : toList allWords (index + 1) [] 
    | otherwise = toList allWords (index + 1) (curWord ++ [allWords !! index])

filterWordsByN :: Foldable t => Int -> [t a] -> [t a]
filterWordsByN n listOfWords = [x | x <- listOfWords, (length x) == n] ---filters all of the words with a given length "n" in the list of words

---TODO
---picks a random word of the dictionary with length of n
pickAword :: [a] -> Int -> a
pickAword allWordsWithLengthN index = allWordsWithLengthN !! index 
        
---Regime "Game"

letterIsInTheWord :: (Foldable t, Eq a) => a -> t a -> Bool
letterIsInTheWord letter word = elem letter word ----checks if a letter is in the word      

letterIsInTheRightPlace :: Eq a => a -> [a] -> Int -> Bool
letterIsInTheRightPlace letter word index = (word !! index) == letter ----checks if a letter is in the right index in the word          

letterIsGreen :: Eq a => a -> [a] -> Int -> Bool
letterIsGreen letter word index =
    if (letterIsInTheWord letter word == True) && (letterIsInTheRightPlace letter word index) == True
        then True
        else False  ----checks if the letter is in the word and is in the right place      ///green

letterIsYellow :: Eq a => a -> [a] -> Int -> Bool
letterIsYellow letter word index =
        if ((letterIsInTheWord letter word) == True && (letterIsInTheRightPlace letter word index) == False)
        then True
        else False  ----if the letter exists in the word and is not at the right index, then returns True, else False

letterIsGrey :: (Foldable t, Eq a) => a -> t a -> Bool
letterIsGrey letter word =
    if ((letterIsInTheWord letter word) == False)
        then True
        else False ---- if the letter does not exist in the word, then the letter is grey->True, else False

----Returns a list of colors: "grey" if the current letter doesnt exist in the word
----"yellow" if the current letter exists in the word but is not in the right place
----"green" if the current letter exists in the word and is in the right place
listOfColors :: Eq a => [a] -> [a] -> [String]
listOfColors wordWithLengthN guessedWord =
    returnList wordWithLengthN guessedWord 0
    where
        returnList wordWithLengthN guessedWord index 
            | index == (length guessedWord) = []
            | letterIsGreen (guessedWord !! index) wordWithLengthN index == True = "green" : returnList wordWithLengthN guessedWord (index + 1)
            | letterIsYellow (guessedWord !! index) wordWithLengthN index ==True = "yellow" : returnList wordWithLengthN guessedWord (index + 1)
            | letterIsGrey (guessedWord !! index) wordWithLengthN  == True = "grey" : returnList wordWithLengthN guessedWord (index + 1)


allGreen [] = True ----Given a list of strings, checks if every one of them is green
allGreen (x:xs) =
    if (x == "green")
        then allGreen xs
        else False

----Easy mode

wordIsInDictionary :: (Foldable t, Eq a) => a -> t a -> Bool
wordIsInDictionary word allWords = elem word allWords ----Checks if the played word is in the dictionary


---- Да направя функция, която приема (лист от стрингове)- вече използвани думи, (дума)- току що използвана (ако думата съществува в листа, не прави нищо) дума -> добавя думата в листа ->(за 2.)
---- Да направя функция, която приема (лист от букви), буква - добавя буквата към листа (ако не съществува) -> ще се използва за grey, yellow файловете. -> (за 3. и 5.)
----Given a list and a word -> adds the word to the list if the word is not already in the list, otherwise returns the list
addWordOrLetter :: Eq a => [a] -> a -> [a]
addWordOrLetter usedWords word =  
     if not (elem word usedWords) 
        then word : usedWords
        else usedWords 



---- Да направя функция, която приема (лист от букви), дума -> ако всички букви не присъстват в думата, връща False иначе True -> (за 6.)
---- Given a guessed word , list and a word, returns a list of all the letters in the guessed word that are yellow and does not exist in the list

allYellowsNotInList guessedword word lst =
    check guessedword word lst 0
    where
        check guessedWord word lst index
            | (null lst) = []
            | (null guessedWord) = lst
            | (letterIsYellow (head guessedWord) word index) && ((elem [(head guessedWord)] lst) == True) = (check (tail guessedWord) word (removeALetter lst [(head guessedWord)]) (index + 1))
            | otherwise = check (tail guessedWord) word lst (index + 1)


---- given a list of tuples (All of the green tuples) and a word, returns all of the letters that are known to be green, but aren't in the right place.
allKnownGreensAreInWord word listOfTuples
    | null listOfTuples = []
    | [(word !! (snd (head listOfTuples)))] /= (fst (head listOfTuples)) = (head listOfTuples) : allKnownGreensAreInWord word (tail listOfTuples) 
    | otherwise = allKnownGreensAreInWord word (tail listOfTuples) 

---- Да направя функция, която приема (лист от букви), буква -> премахва дадената буква от листа от букви -> (за 7.)
----(трябва lst да е от видя  ["a", "b", "c", "d"], a не от видя ['a', 'b', 'c', 'd'])
----removes the first instance of a letter in a list
removeALetter :: Eq a => [a] -> a -> [a]
removeALetter lst letter =
    (removeSingleLetter lst letter 0)
    where
        removeSingleLetter lst letter flag
            | (null lst) = []
            | (head lst) == letter && (flag == 0) = (removeSingleLetter (tail lst) letter 1)
            | otherwise = (head lst) : (removeSingleLetter (tail lst) letter flag)



---- Да направя функция, която приема (лист от кортежи), буква, индекс -> прави кортеж от буквата и индекса и го добавя към листа от кортежи ->(за 7.)
addToTuple :: [(a, b)] -> a -> b -> [(a, b)]
addToTuple tuples letter index = (letter, index) : tuples


---Creates a list from a given string seperated by space
makeAList fromFile cur index
    | (listed fromFile cur index) == [""] = []
    | otherwise = (listed fromFile cur index)
    where
        listed fromFile cur index
            | index >= (length fromFile) = cur : []
            | fromFile !! index == ' ' = cur : makeAList fromFile [] (index + 1)
            | otherwise = makeAList fromFile (cur ++ [fromFile !! index]) (index + 1)


---Creates a list of tuples from a given a string
makeAListOfGreens fromFile
    | fromFile == "" = []
    | otherwise = map (\x -> ((fst x), (read (snd x) :: Int))) (makeTuples fromFile 0)
    where
        makeTuples fromFile index
            | (index + 2) >= ((length fromFile) - 1) = [([fromFile !! index], [fromFile !! (index + 1)])]
            | otherwise = ([(fromFile !! index)] , [(fromFile !! (index + 1))]) : (makeTuples fromFile (index + 2))


---Makes a string from a given list of strings
makeAString lst
    | null lst = []
    | null (tail lst) = (head lst)
    | otherwise = (head lst) ++ " " ++ (makeAString (tail lst))


makeAStringFromTuples (x:xs)
    | null xs = (fst x) ++ (show (snd x))
    | otherwise = (fst x) ++ (show (snd x)) ++ (makeAStringFromTuples xs)


---Given a word and a list, checks if any elements in the word exists in the list
existsInList word lst
    | null word = []
    | elem [(head word)] lst = [(head word)] : existsInList (tail word) lst
    | otherwise = existsInList (tail word) lst


---Given a guessed word and a word, returns a list of all the grey letters 
returnGrays guessedWord word
    | null guessedWord = []
    | letterIsGrey (head guessedWord) word = [(head guessedWord)] : returnGrays (tail guessedWord) word
    | otherwise = returnGrays (tail guessedWord) word 

---Given a guessed word and a word, returns a list of all the yellow letters
returnYellows guessedWord word =
    allYellows guessedWord word 0
    where 
        allYellows guessedWord word index
            | null guessedWord = []
            | letterIsYellow (head guessedWord) word index = [(head guessedWord)] : allYellows (tail guessedWord) word (index + 1)
            | otherwise = allYellows (tail guessedWord) word (index + 1)

---Given a guessed word and a word, returns a list of tuples of all the green letters and their locations
returnGreens guessedWord word =
    allGreens guessedWord word 0
    where
        allGreens guessedWord word index
            | null guessedWord = []
            | letterIsGreen (head guessedWord) word index = ([(head guessedWord)], index) : allGreens (tail guessedWord) word (index + 1)
            | otherwise = allGreens (tail guessedWord) word (index + 1)


---Given two lists, merges them with no duplicates
mergeNoDups lst1 lst2 
    | (null lst2) = lst1
    | (elem (head lst2) lst1) = mergeNoDups lst1 (tail lst2)
    | otherwise = mergeNoDups ((head lst2) : lst1) (tail lst2)


----1.Програмата ще пита потребителя в main за думата му. Ако тази дума не се среща в речника -> ще изпише думата не се среща в речника
----2.Програмата ще добави думата към файл с вече използвани думи.
----3.Програмата ще минава през всеки отделен отговор и ще записва в друг файл всички букви, които са били изведени като "grey"
----4.ако в следващите познати думи се използва някоя от тези букви да се изведе съобщение
----5.Програмата ще минава през всеки отделен отговор и ще записва в друг файл всички букви, които са били изведени като "yellow"
----6.ако в следващите познати думи не присъстват всички думи от списъка за yellow, то да се изведе съобщение
----7.ако при следващия вход някоя от тези букви е "green", то трябва да се махне от файла с "yellow" и да се добави като кортеж в списъка с "green"
----8.Програмата ще има файл в който ще има списък от кортежи, където до всяка буква от думата ще има индекса, в който тя е била позната за "green"
----9.ако при някой ход буквата не е на същия индекс, а е на друг трябва да се изведе текст за това.

main :: IO ()
main = do
    putStrLn "What is the length 'n' of the word?"
    en <- getLine
    let n = (read en :: Int)
    dictionary <- openFile "words.txt" ReadMode
    contents <- hGetContents dictionary
    ---print(filterWordsByN n (toList contents 0 ""))
    randNum <- randomRIO (0, (length (filterWordsByN n (toList contents 0 "")) - 1  :: Int))
    print (filterWordsByN n (toList contents 0 ""))
    putStrLn "Choose a gamemode -> game or helper"
    gamemode <- getLine
    if (gamemode == "game") then do
        putStrLn "pick difficulty -> standart, easy or expert"
        difficulty <- getLine
        if(difficulty == "standart") then do
            putStrLn "When you are done playing, type exit!"
            playStandart (toList contents 0 "") n randNum
            else if (difficulty == "easy") then do
                putStrLn "When you are done playing, type exit!"
                playEasy [] [] [] (pickAword (filterWordsByN n (toList contents 0 "")) randNum) (filterWordsByN n (toList contents 0 "")) n
                
                else putStrLn "TODO"
        else putStrLn "TODO"

{-  
    writeFile "allGreens.txt" ""
    writeFile "allGrey.txt" ""
    writeFile "allYellows.txt" ""
    removeFile "allGreens.txt"
    renameFile "greens2.txt" "allGreens.txt"

    removeFile "allGrey.txt"
    renameFile "greys2.txt" "allGrey.txt"

    removeFile "allYellows.txt"
    renameFile "yellows2.txt" "allYellows.txt"
-}
    where 
        playStandart contents n randNum = do 
            putStrLn "Guess a word with that length"
            guessedWord <- getLine
            if(guessedWord == "exit")
                then putStrLn "Goodbye!"
                else
                    if (((length guessedWord) > n) || ((length guessedWord) < n))
                        then do
                            putStrLn "Guessed a word with invalid length! Try again!"
                            playStandart contents n randNum
                        else
                            if (allGreen (listOfColors (pickAword (filterWordsByN n contents) randNum ) guessedWord ))
                                then putStrLn "You won!"
                                else do
                                    print (listOfColors (pickAword (filterWordsByN n contents) randNum ) guessedWord )
                                    playStandart contents n randNum
        playEasy greensLst yellowsLst greysLst pickedWord dict n = do
            putStrLn "Guess a word with that length"
            print (pickedWord)
            guessedWord <- getLine
            
            if(guessedWord == "exit")
                then do
                    putStrLn "Goodbye!"
                else do
                    if (((length guessedWord) > n) || ((length guessedWord) < n))
                        then do
                            putStrLn "Guessed a word with invalid length! Try again!"
                            playEasy greensLst yellowsLst greysLst pickedWord dict n 
                        else
                            if(not (wordIsInDictionary guessedWord dict))
                                then do
                                    putStrLn "The guessed word is not in the dictionary. Try again!"
                                    playEasy greensLst yellowsLst greysLst pickedWord dict n 
                                else do 
                                    if (allGreen (listOfColors pickedWord guessedWord))
                                        then do 
                                            putStrLn "You won!"
                                        else do
                                            if((existsInList guessedWord greysLst) /= [])
                                                then do
                                                    print (existsInList guessedWord greysLst)
                                                    print ("The letters above are already known to be grey")
                                                else print ('\n')
                                            if ((allYellowsNotInList guessedWord pickedWord yellowsLst) /= [])
                                                then do
                                                    print(allYellowsNotInList guessedWord pickedWord yellowsLst)
                                                    print ("The letters above are known to be yellow and are not in your word!")
                                                    
                                                else print ('\n')
                                            
                                            if ((allKnownGreensAreInWord guessedWord greensLst) /= [])
                                                then do
                                                    print(allKnownGreensAreInWord guessedWord greensLst)
                                                    print ("The letters and indexes above are known to be green and are not green in your word!")
                                                    else print ('\n')
                                            
                                            print(listOfColors pickedWord guessedWord)

                                            
                                            writeFile "allGrey.txt" (makeAString (mergeNoDups greysLst (returnGrays guessedWord pickedWord)))
                                            writeFile "allYellows.txt" (makeAString (mergeNoDups yellowsLst (returnYellows guessedWord pickedWord)))
                                            writeFile "allGreens.txt" (makeAStringFromTuples (mergeNoDups greensLst (returnGreens guessedWord pickedWord)))

                                            greyContents <- readFile "allGrey.txt"
                                            yellowContents <- readFile "allYellows.txt"
                                            greenContents <- readFile "allGreens.txt"

                                            playEasy (makeAListOfGreens greenContents) (makeAList yellowContents [] 0) (makeAList greyContents [] 0) pickedWord dict n
                                            




                                            



                                            
                                            
                                            
                                            
                                            
                                    




    

---в easy mode:
---Пита потребителя за input///
---ако думата която е вкарал потребителя не е в речника, да изкара съобщение и да loop-ne (да поиска нов вход)///
---да прочете нещата от allGrey.txt и да ги вкара в променлива -> да премине през думата и ако някоя от буквите в думата съществува в allGrey.txt да изпише че тази буква вече е използвана///
---да прочете allYellows и да ги вкара в променлива -> да премине през променливата и ако всички букви от нея съществуват в познатата дума да не прави нищо, иначе да изведе съобщение за всяка жълта буква,
---която не присъства в думата.///
---да прочете allGreens и да ги вкара в променлива -> да премине през променливата и за всеки кортеж да провери индекса на всяка буква в думата, ако не съвпадат -> да изведе съобщение за
---съответната буква.
---ОТВАРЯ ВСИЧКО И ЧЕТЕ ПРЕДИ ДА ВЛЕЗЕ В PLEYEASY ФУНКЦИЯТА, ТЯ САМО ПИШЕ ВЪВ ФАЙЛОВЕТЕ
---пускам функцията минавам през всяка буква в думата и проверявам цвета и (с letterIsGreen, yellow...) в зависимост коя е, проверявам дали съществува в променливата на съответния файл и
---ако не съществува я добавям най- отзад на листа. Ако letterIsGreen и кортежа на буквата и индекса и не съществуват в листа със зелени кортежи я добавям, иначе не.
---презаписвам всеки файл с новите стойности на листовете им.
---Когато играчът познае думата, изтривам съдържаниетпо на всеки файл и го затварям. Ако играчът напише exit изтривам съдържанието на всеки файл и го затварям.

---LEARNING HASKELL WEEK07 INPUT/OUTPUT 23:45


---ако е Helper -> ...




