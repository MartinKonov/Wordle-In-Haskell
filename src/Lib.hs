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
            | (letterIsInTheWord (head guessedWord) word) && ((elem [(head guessedWord)] lst) == True) = (check (tail guessedWord) word (removeALetter lst [(head guessedWord)]) (index + 1))
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


makeAStringFromTuples [] = ""
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


---Given three lists and a word,  returns a list of all the indices of letters in the word that don't exist in any of the lists
allLettersInLists greenLst greyLst yellowLst word =
    helper greenLst greyLst yellowLst word 0
    where
        helper greenLst greyLst yellowLst word index
            | index >= (length word) = []
            | elem ([word !! index], index) greenLst = helper greenLst greyLst yellowLst word (index + 1)
            | (elem ([word !! index]) greyLst) || (elem ([word !! index]) yellowLst) = helper greenLst greyLst yellowLst word (index + 1)
            | otherwise = index : helper greenLst greyLst yellowLst word (index + 1)   


---Given a list of true colors, a list of indices (where a lie is possible) and 3 random numbers (either 0 or 1), returns a list of colors, where all of the valid indices are a lie.
listOfLIES listOfTrueColors listWhereToLie zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey =
    helper listOfTrueColors listWhereToLie zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey 0
    where
        helper listOfTrueColors listWhereToLie zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey index
            | index >= (length listOfTrueColors) = []
            | (null listWhereToLie) = (listOfTrueColors !! index) : helper listOfTrueColors listWhereToLie zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey (index + 1)
            | (head listWhereToLie) == index = (returnLie listOfTrueColors index zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey) : (helper listOfTrueColors (tail listWhereToLie) zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey (index + 1))
            | otherwise = (listOfTrueColors !! index) : helper listOfTrueColors listWhereToLie zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey (index + 1)
        returnLie listOfTrueColors curIndex zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey
            | (listOfTrueColors !! curIndex) == "green" = ["grey", "yellow"] !! zeroOrOneForGreen
            | (listOfTrueColors !! curIndex) == "yellow" = ["green", "grey"] !! zeroOrOneForYellow
            | (listOfTrueColors !! curIndex) == "grey" = ["green", "yellow"] !! zeroOrOneForGrey


----1.Програмата ще пита потребителя в main за думата му. Ако тази дума не се среща в речника -> ще изпише думата не се среща в речника
----2.Програмата ще добави думата към файл с вече използвани думи.
----3.Програмата ще минава през всеки отделен отговор и ще записва в друг файл всички букви, които са били изведени като "grey"
----4.ако в следващите познати думи се използва някоя от тези букви да се изведе съобщение
----5.Програмата ще минава през всеки отделен отговор и ще записва в друг файл всички букви, които са били изведени като "yellow"
----6.ако в следващите познати думи не присъстват всички думи от списъка за yellow, то да се изведе съобщение
----7.ако при следващия вход някоя от тези букви е "green", то трябва да се махне от файла с "yellow" и да се добави като кортеж в списъка с "green"
----8.Програмата ще има файл в който ще има списък от кортежи, където до всяка буква от думата ще има индекса, в който тя е била позната за "green"
----9.ако при някой ход буквата не е на същия индекс, а е на друг трябва да се изведе текст за това.

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

---LEARNING HASKELL WEEK07 INPUT/OUTPUT 23:45

---в expert mode:
---Ще избира случайно число между 0 и 5 -> това число ще определя на кой ход програмата ще излъже.///
---Ако на този ход, всяка буква от познатата дума съществува в allGreens, allGrey, allYellows, то тогава не лъжем на този ход -> програмата работи като в easy mode (но не подсказва), но
---обръщам булев флаг на 1, който казва, че на следващия ход трябва да се излъже. Ако не може да се излъже пак, то пак работи като в easy mode (но не подсказва), но булевия флаг си остава на 1.///
---Ако излъже успешно, то булевия флаг се обръща на 0 и програмата продължава до края без да излъже.
---Когато програмата ще лъже ->
---Пуска познатата дума във функция, която приема думата, greensLst, greysLst, yellowsLst, и избраната дума от програмата. Връща лист от местата на буквите, които не се срещат и в трите листа.//
---Ако е празен, то променям флага на 1 и си минава на easy mode до края на хода., Ако не е празен:///
---Пускам функция, която приема listOfColors (истинските цветове на думата) и листа от местата на буквите,които не се срещат в 3те листа.
---Функцията минава през истинския вход и на индексите, в които буквите не съществуват в листовете, ги променя. (Ако истинския цвят на буквата е green, то избира случайно между
---grey и yellow и го записва на мястото на green...) -> този отговор не се записва във файловете.


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
        putStrLn "When you are done playing, type exit!"
        if(difficulty == "standart") then do
            playStandart (pickAword (filterWordsByN n (toList contents 0 "")) randNum) n 
            else if (difficulty == "easy") then do
                playEasy [] [] [] (pickAword (filterWordsByN n (toList contents 0 "")) randNum) (filterWordsByN n (toList contents 0 "")) n
                else if (difficulty == "expert") then do
                    lieOnMove <- randomRIO (1, 6 :: Int)
                    playExpert [] [] [] (pickAword (filterWordsByN n (toList contents 0 "")) randNum) (filterWordsByN n (toList contents 0 "")) n 0 lieOnMove
                    else do 
                        print ("We don't have that difficulty yet. Try again!")
                        main
        else putStrLn "TODO"
    where 
        playStandart pickedWord n = do 
            putStrLn "Guess a word with that length"
            guessedWord <- getLine
            if(guessedWord == "exit")
                then putStrLn "Goodbye!"
                else
                    if (((length guessedWord) > n) || ((length guessedWord) < n))
                        then do
                            putStrLn "Guessed a word with invalid length! Try again!"
                            playStandart pickedWord n
                        else
                            if (allGreen (listOfColors pickedWord guessedWord ))
                                then putStrLn "You won!"
                                else do
                                    print (listOfColors pickedWord guessedWord)
                                    playStandart pickedWord n
        playEasy greensLst yellowsLst greysLst pickedWord dict n = do
            putStrLn "Guess a word with that length"
            guessedWord <- getLine
            
            if(guessedWord == "exit")
                then do
                    putStrLn "The word was:"
                    print (pickedWord)
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
                                                else return ()
                                            if ((allYellowsNotInList guessedWord pickedWord yellowsLst) /= [])
                                                then do
                                                    print(allYellowsNotInList guessedWord pickedWord yellowsLst)
                                                    print ("The letters above are known to be yellow and are not in your word!")
                                                    
                                                else return ()
                                            
                                            if ((allKnownGreensAreInWord guessedWord greensLst) /= [])
                                                then do
                                                    print(allKnownGreensAreInWord guessedWord greensLst)
                                                    print ("The letters and indices above are known to be green and are not green in your word!")
                                                    else return ()
                                            
                                            print(listOfColors pickedWord guessedWord)

                                            writeFile "helpGrey.txt" (makeAString (mergeNoDups greysLst (returnGrays guessedWord pickedWord)))
                                            writeFile "helpYellow.txt" (makeAString (mergeNoDups yellowsLst (returnYellows guessedWord pickedWord)))
                                            writeFile "helpGreen.txt" (makeAStringFromTuples (mergeNoDups greensLst (returnGreens guessedWord pickedWord)))

                                            removeFile "allGreens.txt"
                                            removeFile "allGrey.txt"
                                            removeFile "allYellows.txt"

                                            renameFile "helpGrey.txt" "allGrey.txt"
                                            renameFile "helpYellow.txt" "allYellows.txt"
                                            renameFile "helpGreen.txt" "allGreens.txt"

                                            greyContents <- readFile "allGrey.txt"
                                            yellowContents <- readFile "allYellows.txt"
                                            greenContents <- readFile "allGreens.txt"
                                            playEasy (makeAListOfGreens greenContents) (makeAList yellowContents [] 0) (makeAList greyContents [] 0) pickedWord dict n

                                {-
                                            writeFile "allGrey.txt" (makeAString (mergeNoDups greysLst (returnGrays guessedWord pickedWord)))
                                            writeFile "allYellows.txt" (makeAString (mergeNoDups yellowsLst (returnYellows guessedWord pickedWord)))
                                            writeFile "allGreens.txt" (makeAStringFromTuples (mergeNoDups greensLst (returnGreens guessedWord pickedWord)))

                                            greyContents <- readFile "allGrey.txt"
                                            yellowContents <- readFile "allYellows.txt"
                                            greenContents <- readFile "allGreens.txt"
                                -}                                         
        playExpert greensLst yellowsLst greysLst pickedWord dict n curMove lieOnMove = do
            
            putStrLn "Guess a word with that length"
            guessedWord <- getLine
            if(guessedWord == "exit")
                then do
                    putStrLn "The word was:"
                    print (pickedWord)
                    putStrLn "Goodbye!"
                    else do
                        if (((length guessedWord) > n) || ((length guessedWord) < n))
                            then do
                                putStrLn "Guessed a word with invalid length! Try again!"
                                playExpert greensLst yellowsLst greysLst pickedWord dict n curMove lieOnMove
                                else do
                                    if (allGreen (listOfColors pickedWord guessedWord))
                                        then
                                            putStrLn "You won!"
                                            else do
                                                if (((curMove == lieOnMove)) && ((allLettersInLists greensLst greysLst yellowsLst guessedWord) /= []))
                                                    then do
                                                        zeroOrOneForGreen <- randomRIO (0, 1 :: Int)
                                                        zeroOrOneForYellow <- randomRIO (0, 1 :: Int)
                                                        zeroOrOneForGrey <- randomRIO (0, 1 :: Int)
                                                        print (listOfLIES (listOfColors pickedWord guessedWord) (allLettersInLists greensLst greysLst yellowsLst guessedWord) zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey)
                                                        playExpert greensLst yellowsLst greysLst pickedWord dict n (curMove+1) (-1)
                                                        else do
                                                            print(listOfColors pickedWord guessedWord)   
                {-
                                                            writeFile "allGrey.txt" (makeAString (mergeNoDups greysLst (returnGrays guessedWord pickedWord)))
                                                            writeFile "allYellows.txt" (makeAString (mergeNoDups yellowsLst (returnYellows guessedWord pickedWord)))
                                                            writeFile "allGreens.txt" (makeAStringFromTuples (mergeNoDups greensLst (returnGreens guessedWord pickedWord)))

                                                            greyContents <- readFile "allGrey.txt"
                                                            yellowContents <- readFile "allYellows.txt"
                                                            greenContents <- readFile "allGreens.txt"
                -}
                                                            writeFile "helpGrey.txt" (makeAString (mergeNoDups greysLst (returnGrays guessedWord pickedWord)))
                                                            writeFile "helpYellow.txt" (makeAString (mergeNoDups yellowsLst (returnYellows guessedWord pickedWord)))
                                                            writeFile "helpGreen.txt" (makeAStringFromTuples (mergeNoDups greensLst (returnGreens guessedWord pickedWord)))

                                                            removeFile "allGreens.txt"
                                                            removeFile "allGrey.txt"
                                                            removeFile "allYellows.txt"

                                                            renameFile "helpGrey.txt" "allGrey.txt"
                                                            renameFile "helpYellow.txt" "allYellows.txt"
                                                            renameFile "helpGreen.txt" "allGreens.txt"

                                                            greyContents <- readFile "allGrey.txt"
                                                            yellowContents <- readFile "allYellows.txt"
                                                            greenContents <- readFile "allGreens.txt"
                                                            
                                                            if (curMove == lieOnMove) 
                                                                then
                                                                    playExpert (makeAListOfGreens greenContents) (makeAList yellowContents [] 0) (makeAList greyContents [] 0) pickedWord dict n (curMove + 1) (lieOnMove + 1)
                                                                    else
                                                                        playExpert (makeAListOfGreens greenContents) (makeAList yellowContents [] 0) (makeAList greyContents [] 0) pickedWord dict n (curMove + 1) lieOnMove


---идея -> всичие read и write да го правя в playNormal и като викна playNormal, той да вика playExpert вместо да връща нещо.

---ако е Helper -> ...




