module Lib where
import System.Random
import System.IO
import System.Directory


---Given a string of words, seperated by '\n', creates a list of those words.
toList :: [Char] -> Int -> [Char] -> [[Char]]
toList allWords index curWord
    | index >= length (allWords) = curWord : []
    | allWords !! index == '\n' = curWord : toList allWords (index + 1) [] 
    | otherwise = toList allWords (index + 1) (curWord ++ [allWords !! index])


---Filters all of the words with a given length "n" in the list of words
filterWordsByN n listOfWords = [x | x <- listOfWords, (length x) == n] 


---Picks a word of the dictionary with length of n
pickAword :: [a] -> Int -> a
pickAword allWordsWithLengthN index = allWordsWithLengthN !! index 
        

---Checks if a given letter is in the word      
letterIsInTheWord letter word = elem letter word 


---Checks if a letter is in the right index in the word
letterIsInTheRightPlace :: Eq a => a -> [a] -> Int -> Bool
letterIsInTheRightPlace letter word index = (word !! index) == letter 


---Checks if the letter is in the word and is in the right place
letterIsGreen :: Eq a => a -> [a] -> Int -> Bool
letterIsGreen letter word index =
    if (letterIsInTheWord letter word == True) && (letterIsInTheRightPlace letter word index) == True
        then True
        else False  


---If the letter exists in the word and is not at the right index, then returns True, else False
letterIsYellow :: Eq a => a -> [a] -> Int -> Bool
letterIsYellow letter word index =
        if ((letterIsInTheWord letter word) == True && (letterIsInTheRightPlace letter word index) == False)
        then True
        else False  


---If the letter does not exist in the word, then the letter is grey->True, else False
letterIsGrey letter word =
    if ((letterIsInTheWord letter word) == False)
        then True
        else False 


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


---Given a list of strings, checks if every one of them is green
allGreen :: [String] -> Bool
allGreen [] = True 
allGreen (x:xs) =
    if (x == "green")
        then allGreen xs
        else False


---Checks if the played word is in the dictionary
wordIsInDictionary word allWords = elem word allWords


----Given a list and a word -> adds the word to the list if the word is not already in the list, otherwise returns the list
addWordOrLetter :: Eq a => [a] -> a -> [a]
addWordOrLetter usedWords word =  
     if not (elem word usedWords) 
        then word : usedWords
        else usedWords 


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
allKnownGreensAreInWord :: Eq a => [a] -> [([a], Int)] -> [([a], Int)]
allKnownGreensAreInWord word listOfTuples
    | null listOfTuples = []
    | [(word !! (snd (head listOfTuples)))] /= (fst (head listOfTuples)) = (head listOfTuples) : allKnownGreensAreInWord word (tail listOfTuples) 
    | otherwise = allKnownGreensAreInWord word (tail listOfTuples) 


----removes the first instance of a letter in a list
removeALetter :: Eq a => [a] -> a -> [a]
removeALetter lst letter =
    (removeSingleLetter lst letter 0)
    where
        removeSingleLetter lst letter flag
            | (null lst) = []
            | (head lst) == letter && (flag == 0) = (removeSingleLetter (tail lst) letter 1)
            | otherwise = (head lst) : (removeSingleLetter (tail lst) letter flag)



---Given a list of tuples, a letter and an index, creates a tuple of the letter and the index, and adds it to the list of tuples
addToTuple :: [(a, b)] -> a -> b -> [(a, b)]
addToTuple tuples letter index = (letter, index) : tuples


---Creates a list from a given string seperated by space
makeAList :: [Char] -> [Char] -> Int -> [[Char]]
makeAList str cur index
    | (listed str cur index) == [""] = []
    | otherwise = (listed str cur index)
    where
        listed str cur index
            | index >= (length str) = cur : []
            | str !! index == ' ' = cur : makeAList str [] (index + 1)
            | otherwise = makeAList str (cur ++ [str !! index]) (index + 1)


---Creates a list of tuples from a given a string
makeAListOfGreens :: String -> [([Char], Int)]
makeAListOfGreens fromFile
    | fromFile == "" = []
    | otherwise = map (\x -> ((fst x), (read (snd x) :: Int))) (makeTuples fromFile 0)
    where
        makeTuples fromFile index
            | (index + 2) >= ((length fromFile) - 1) = [([fromFile !! index], [fromFile !! (index + 1)])]
            | otherwise = ([(fromFile !! index)] , [(fromFile !! (index + 1))]) : (makeTuples fromFile (index + 2))


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
returnYellows :: Eq a => [a] -> [a] -> [[a]]
returnYellows guessedWord word =
    allYellows guessedWord word 0
    where 
        allYellows guessedWord word index
            | null guessedWord = []
            | letterIsYellow (head guessedWord) word index = [(head guessedWord)] : allYellows (tail guessedWord) word (index + 1)
            | otherwise = allYellows (tail guessedWord) word (index + 1)


---Given a guessed word and a word, returns a list of tuples of all the green letters and their locations
returnGreens :: Eq a => [a] -> [a] -> [([a], Int)]
returnGreens guessedWord word =
    allGreens guessedWord word 0
    where
        allGreens guessedWord word index
            | null guessedWord = []
            | letterIsGreen (head guessedWord) word index = ([(head guessedWord)], index) : allGreens (tail guessedWord) word (index + 1)
            | otherwise = allGreens (tail guessedWord) word (index + 1)


---Given two lists, merges them with no duplicates
mergeNoDups :: Eq a => [a] -> [a] -> [a]
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
            | elem index (map (\x->(snd x)) greenLst) = helper greenLst greyLst yellowLst word (index + 1)
            | elem ([word !! index], index) greenLst = helper greenLst greyLst yellowLst word (index + 1)
            | (elem ([word !! index]) greyLst) || (elem ([word !! index]) yellowLst) = helper greenLst greyLst yellowLst word (index + 1)
            | otherwise = index : helper greenLst greyLst yellowLst word (index + 1)   


---Given a list of true colors, a list of indices (where a lie is possible) and 3 random numbers (each either 0 or 1), returns a list of colors, where all of the valid indices are a lie.
listOfLIES :: [String] -> [Int] -> Int -> Int -> Int -> [String]
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



---HELPER MODE FUNCTIONS:

---Given a dictionary (allWords), answer list (example -> ["yellow", "grey", "green"]) and a word, filters all of the words in the dictionary that do not contradict(possibleWord)
---with the answer list.
filterDict :: Eq a => [[a]] -> [String] -> [a] -> [[a]]
filterDict allWords curAnswerLst curGuess
    | null allWords = []
    | possibleWord curAnswerLst (head allWords) curGuess 0 == True = (head allWords) : (filterDict (tail allWords) curAnswerLst curGuess)
    | otherwise = filterDict (tail allWords) curAnswerLst curGuess
    where
        ---Given an answer list, current word, guess and an index, checks for every letter in the current word, if it condtradicts with the answer list. If the letter on index dies not
        ---contradict with the answer list, calls recursively with incremented index. If the letter does contradict with the answer list => the current word contradicts with the answer list, 
        ---returns False. If the index >= length of the answer list, then all of the letters in the word do not contradict with the answer list => the current word is valid.
        possibleWord answerLst curWord guess index
            | index >= (length answerLst) = True
            | (answerLst !! index) == "green" = (if ((curWord !! index) == (guess !! index))
                                                    then
                                                        (possibleWord answerLst curWord guess (index + 1))
                                                        else
                                                            False)
            | (answerLst !! index) == "yellow" = (if (caseYellow (guess !! index) curWord guess index)
                                                    then 
                                                        (possibleWord answerLst curWord guess (index + 1))
                                                        else
                                                            False)
            | (answerLst !! index) == "grey" = (if (caseGrey (guess !! index) curWord)
                                                    then (possibleWord answerLst curWord guess (index + 1))
                                                        else False)

        ---Finds if the letter exists in the current word and not in the index of the current word
        caseYellow letter curWord guess index
            | letter == (curWord !! index) = False
            | findLetter letter curWord == True = True
            | otherwise = False
            where
                findLetter letter curWord
                    | null curWord = False
                    | letter == (head curWord) = True
                    | otherwise = findLetter letter (tail curWord)
        ---If the letter exists in the current word, then it's not grey, returns False. Otherwise True
        caseGrey letter curWord
            | null curWord = True
            | letter == (head curWord) = False
            | otherwise = caseGrey letter (tail curWord)

     


---Given two lists, (bestScore = 0) and (bestWord = "") returns the word with the best score.
bestGuess lst1 lst2 bestScore bestWord
    | null lst1 = bestWord
    | (evalWord (head lst1) lst2) > bestScore = bestGuess (tail lst1) lst2 (evalWord (head lst1) lst2) (head lst1)
    | otherwise = bestGuess (tail lst1) lst2 bestScore bestWord
    where
        ---Given a guess and a list, sums the wordScore of the guess and every word in the list
        evalWord guess lst
            | null lst = 0
            | otherwise = (wordScore guess (head lst) 0) + (evalWord guess (tail lst))

        ---Given a guess(word), word and an index, for every letter in the word, checks:
        ---if the letter is green in guess, adds 2 and recursively calls the function with incremented index.
        ---if the letter is yellow in guess, adds 1 and recursively calls the function with incremented index.
        ---if the letter is not green nor yellow, doesn't add anything and calls the function with incremented index.
        ---if the index is equal to the length of the word or the guess -> returns 0
        wordScore guess word index
            | index == (length word) = 0
            | index == (length guess) = 0
            | (letterIsGreen (word !! index) guess index) = 2 + (wordScore guess word (index + 1))
            | (letterIsYellow (word !! index) guess index) = 1 + (wordScore guess word (index + 1))
            | otherwise = (wordScore guess word (index + 1))


---Validates the result list
validate resultLst
    | null resultLst = True
    | ((head resultLst) /= "green" && (head resultLst) /= "yellow" && (head resultLst) /= "grey") = False
    | otherwise = validate (tail resultLst)




main :: IO ()
main = do
    dictionary <- openFile "words.txt" ReadMode
    contents <- hGetContents dictionary
    putStrLn "Choose a gamemode -> game or helper"
    gamemode <- getLine
    if (gamemode == "game") then do
        putStrLn "What is the length 'n' of the word?"
        wordLength <- getLine
        let n = (read wordLength :: Int)
        print (filterWordsByN n (toList contents 0 ""))
        randNum <- randomRIO (0, (length (filterWordsByN n (toList contents 0 "")) - 1  :: Int))
        putStrLn "pick a difficulty -> standart, easy or expert"
        difficulty <- getLine
        putStrLn "To stop playing, type exit!"
        if(difficulty == "standart") then do
            playStandart (pickAword (filterWordsByN n (toList contents 0 "")) randNum) n 
            else if (difficulty == "easy") then do
                playEasy [] [] [] (pickAword (filterWordsByN n (toList contents 0 "")) randNum) (filterWordsByN n (toList contents 0 "")) n
                else if (difficulty == "expert") then do
                    lieOnMove <- randomRIO (1, 6 :: Int)
                    playExpert [] [] [] (pickAword (filterWordsByN n (toList contents 0 "")) randNum) (filterWordsByN n (toList contents 0 "")) n 0 lieOnMove
                    else do 
                        putStrLn "We don't have that difficulty yet. Try again!"
                        main
        else if (gamemode == "helper") then do
            putStrLn "pick a difficulty -> standart or expert"
            difficulty <- getLine
            putStrLn "To stop playing, type exit!"
            putStrLn "What is the length 'n' of the word?"
            wordLength <- getLine
            let n = (read wordLength :: Int)
            if (difficulty == "standart") then do
                playHelper (filterWordsByN n (toList contents 0 "")) n
                else if (difficulty == "expert") then do
                    playHelperExpert [] (filterWordsByN n (toList contents 0 "")) n
                    else do 
                        putStrLn "We don't have that difficulty yet. Try again!"
                        main
            else do 
                putStrLn "We don't have that gamemode yet. Try again!"
                main
    where 
        playStandart pickedWord n = do 
            putStrLn "Guess a word with that length"
            guessedWord <- getLine
            if(guessedWord == "exit")
                then do
                putStrLn "Goodbye!"
                putStrLn "The word was:"
                print (pickedWord)
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
                                            playEasy (mergeNoDups greensLst (returnGreens guessedWord pickedWord)) (mergeNoDups yellowsLst (returnYellows guessedWord pickedWord)) (mergeNoDups greysLst (returnGrays guessedWord pickedWord)) pickedWord dict n
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
                                                            if (curMove == lieOnMove) 
                                                                then
                                                                    playExpert (mergeNoDups greensLst (returnGreens guessedWord pickedWord)) (mergeNoDups yellowsLst (returnYellows guessedWord pickedWord))  (mergeNoDups greysLst (returnGrays guessedWord pickedWord)) pickedWord dict n (curMove + 1) (lieOnMove + 1)
                                                                    else
                                                                        playExpert (mergeNoDups greensLst (returnGreens guessedWord pickedWord)) (mergeNoDups yellowsLst (returnYellows guessedWord pickedWord)) (mergeNoDups greysLst (returnGrays guessedWord pickedWord)) pickedWord dict n (curMove + 1) lieOnMove
        playHelper possibleWords n = do
            putStrLn "Possible words:"
            print (possibleWords)
            putStrLn "is it:"
            print (bestGuess (possibleWords) (possibleWords) 0 "")
            putStrLn "Enter result:"
            result <- getLine

            if(result == "yes" || result == "exit" || allGreen (makeAList result "" 0))
                then do
                    putStrLn "Thank you for playing!"
                    else if ((length (makeAList result "" 0) > n || length(makeAList result "" 0) < n) || (not (validate (makeAList result "" 0))))
                        then do
                            putStrLn "Invalid input! Try again!"
                            playHelper possibleWords n
                            else if (filterDict possibleWords (makeAList result "" 0) (bestGuess (possibleWords) (possibleWords) 0 "") == [])
                                then do
                                    putStrLn "The result you have entered doesn't match any of the words in the dictionary. Try again!"
                                    playHelper possibleWords n
                                    else do
                                        playHelper (filterDict possibleWords (makeAList result "" 0) (bestGuess (possibleWords) (possibleWords) 0 "")) n
        playHelperExpert possibleWordsInEveryMove possibleWords n = do
            putStrLn "Possible words:"
            print (possibleWords)
            putStrLn "is it:"
            print (bestGuess (possibleWords) (possibleWords) 0 "")
            putStrLn "Enter result:"
            result <- getLine

            if(result == "yes" || result == "exit" || allGreen (makeAList result "" 0))
                then do
                    putStrLn "Thank you for playing!"
                    else if ((length (makeAList result "" 0) > n || length(makeAList result "" 0) < n) || (not (validate (makeAList result "" 0))))
                        then do
                            putStrLn "Invalid input! Try again!"
                            playHelperExpert possibleWordsInEveryMove possibleWords n
                            else if (filterDict possibleWords (makeAList result "" 0) (bestGuess (possibleWords) (possibleWords) 0 "") == [])
                                then do
                                    hasLied (tail (reverse possibleWordsInEveryMove)) (head (reverse possibleWordsInEveryMove)) n
                                    else do
                                        playHelperExpert (possibleWordsInEveryMove ++ [possibleWords]) (filterDict possibleWords (makeAList result "" 0) (bestGuess (possibleWords) (possibleWords) 0 "")) n
            where
                hasLied possibleWordsInEveryMove curPossibleWords n = do
                    putStrLn "Possible words:"
                    print (curPossibleWords)
                    putStrLn "is it:"
                    print (bestGuess (curPossibleWords) (curPossibleWords) 0 "")
                    putStrLn "Enter result:"
                    result <- getLine
                    if(result == "yes" || result == "exit" || allGreen (makeAList result "" 0))
                        then do
                            putStrLn "Thank you for playing!"
                            else if ((length (makeAList result "" 0) > n || length(makeAList result "" 0) < n) || (not (validate (makeAList result "" 0))))
                                then do
                                    putStrLn "Invalid input! Try again!"
                                    hasLied possibleWordsInEveryMove curPossibleWords n
                                    else if (filterDict curPossibleWords (makeAList result "" 0) (bestGuess (curPossibleWords) (curPossibleWords) 0 "") == [])
                                        then do
                                            if(null possibleWordsInEveryMove)
                                                then do
                                                    putStrLn "The result you have entered doesn't match any of the words in the dictionary. Try again!"
                                                    hasLied possibleWordsInEveryMove curPossibleWords n
                                                else do
                                                    hasLied (tail possibleWordsInEveryMove) (head possibleWordsInEveryMove) n
                                            else do
                                                hasLied possibleWordsInEveryMove (filterDict curPossibleWords (makeAList result "" 0) (bestGuess (curPossibleWords) (curPossibleWords) 0 "")) n
