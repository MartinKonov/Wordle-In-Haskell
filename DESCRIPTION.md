# Wordle
### Martin Konov 45804 INF
---
## Main logic

> The main functions loads a dictionary (words.txt) of the 1000 most common words in English.  
> [Link to dictionary](https://www.ef.com/wwen/english-resources/english-vocabulary/top-1000-words/)  
> Asks the user to choose a gamemode
>
> OPTIONS:
>  - game
>  - helper
>
> The user inputs the desired gamemode and is asked to choose the length of the word, that will be played
>
> OPTIONS:  
> 
> - $n \in \mathbb{N}$ 
> - $n \in {1 ... 14}$ (The longest word in the dictionary is 14 letters long)


> ## If the player picks gamemode "game":  
> The program picks a random word with the chosen length from the dictionary.
> 
>  The user is asked to choose a difficulty
>
> OPTIONS:
>
>   - standart
>   - easy
>   - expert
>
>
> ## Description of standart difficulty:  
>  - The player is asked to guess a word with the given length "n"  
>  - The playStandart function is called.
> - If the player types "exit", the program stops the game and shows the word, that it picked.  
> - The program checks if the guessed word is of length "n", if it isn't - the player is prompted to guess again.  
> - The function displays a list of colors (example -> ["green", "grey", "yellow"]) for every letter, (using (4.) from function descriptions). If the list of colors is all green (example -> ["green", "green", "green"], the program displays a message "You won!" and exits. Otherwise the playStandart function is called recursively.
> 
>
> ## Description of easy difficulty:
>
>  - The playEasy function is called with 3 lists  
> greenLst -> a list of tuples ([a], Int), where [a] is a letter and Int is an index of the letter in the word.  
> yellowsLst -> a list of letters  
> greysLst -> a list of letters
> - If the player types "exit", the program stops the game and shows the word, that it picked.  
> - The program checks if the guessed word is of length "n", if it isn't - the player is prompted to guess again.
> - If the played word doesn't exist in the dictionary, the function displays a message and calls playEasy recursively with the same arguments.
> - Checks if the played word is the correct answer, if it is - displays a message "You won!" and exits.
> - Otherwise, checks if any of the letters that are known to be grey, yellow or grey exist in the guessed word, (using (5.) from function descriptions) and prints those who do not meet the conditions of the task.
> - Displays a list of colors (using (4.) from function descriptions).
> - Calls the playEasy function recursively with changed lists (using (6.) (7.) from function descriptions)
>
> ## Description of expert difficulty:
> - A random number between 1 and 6 is picked (lieOnMove) which indicates on which move the program is going to try to lie.
> - The playExpert function is called with 3 lists (same lists used in the easy difficulty).
> - If the player types "exit", the program stops the game and shows the word, that it picked.  
> - The program checks if the guessed word is of length "n", if it isn't - the player is prompted to guess again.
> - Checks if the played word is the correct answer, if it is - displays a message "You won!" and exits.
> - If the current move is equal to (lieOnMove) and ((8.) from function descriptions)  
> returns a non- empty list:  
>   - The program picks three random numbers (each 0 or 1) and calls ((9.) from function descriptions).
>   - Calls the playExpert function with changed lists (using (6.) (7.) from function descriptions) and the (lieOnMove) number is set to -1, so the function doesn't try to lie again.
> - Displays a list of colors (using (4.) from function descriptions).
> - If the current move is equal to (lieOnMove) but ((8.) from function descriptions) returns an empty list, then the playExpert function is called recursively with changed lists (using (6.) (7.) from function descriptions) and the (lieOnMove) number is incremented by 1, so the program attempts to lie again on the next move.
> - If the current move is not equal to (lieOnMove), then the playExpert function is called recursively with changed lists (using (6.) (7.) from function descriptions)

> ## If the player picks gamemode "helper": 
>
>  The user is asked to choose a difficulty
>
> OPTIONS:
>
>   - standart
>   - expert
>
> ## Description of standart difficulty:
> - The playHelper function is called with a list (possibleWords) which in the beginning is all of the words in the dictionary that have a length of "n" ((1.) (2.) from functions descriptions)
> - The program displays all of the possible words, and guesses an optimal word, that could eliminate the largest number of possible words. The guess is determined by using a simple scoring system -> ((11.) from function descriptions).
> - If the program has guessed the correct word, the player could type a list of green colors  
>  (example -> green green green), or "yes" or "exit"
> - Otherwise, the player inputs a sequence of colors (example -> green yellow grey) 
> - If none of the words in the dictionary match the given list of colors (using (10.) from function descriptions), a message is displayed and the playHelper function is called with the same parameters.
> - Oftherwise the playHelper function is called with filtered (possibleWords) ((10.) from function descriptions).
>
> ## Description of expert difficulty:
> - The playHelperExpert function is called with a list (possibleWordsInEveryMove) and (possibleWords).
> - The playHelperExpert function operates exactly like the playHelper function with a few differences:
>   - When the player inputs an answer (example -> green yellow grey) (and the answer has valid length and is not "yes", "exit" or all green (example -> green green green)), (possibleWords) is added to (possibleWordsInEveryMove) as a new element and the playHelperExpert function is called with the new (possibleWordsInEveryMove) is the first argument and the filtered (possibleWords) as the second (using (10.) from function descriptions).
>   - If the player hasn't lied in any of the moves, then the program would eventually guess the correct word
>   - If the player has lied ==> the (possibleWords) list would eventualy become empty (because none of the words match the given answers). When the (possibleWords) list becomes empty, the hasLied helper function is called.
>
> - The hasLied function is called with tail of the reversed (possibleWordsInEveryMove) list as the first argument and the head of the reversed (possibleWordsInEveryMove) as the second argument (curPossibleWords).
>   - The hasLied function operates exactly as the playHelper function with (curPossibleWords) as the (possibleWords) list. Every time, the player gives an answer, the (curPossibleWords) list is filtered (using (10.) from function descriptions) and a new guess is given.
>   - If the (curPossibleWords) list becomes empty at some point, the hasLied function is called with the tail of (possibleWordsInEveryMove) as the first argument and the head of (possibleWordsInEveryMove) as the second argument.
>   - The algorithm repeats until the user types "exit", the program guesses the word (and the user types "yes" or green list (example -> green green green)), or the (possibleWordsInEveryMove) list becomes empty, in which case the user has lied more than once or the picked word from the user doesn't exist in the dictionary.


### letter coloring logic
  - In "game" gamemode, the case where the program picks a word like "after" and the player guesses with a word that has two of the same letter, like "again", the program should print ["green", "grey", "yellow", "grey", "grey"], since in the task description it is said, that if the letter exists in the word, but is not in the correct position, it is yellow, and not grey.

---
## Most important functions descriptions

1.  
    ```
    toList allWords index curWord

    - Given a string of words, seperated by '\n', creates a list of those words.
    ```
2. 
    ```
    filterWordsByN n listOfWords

    - Filters all of the words with a given length "n" in the list of words
    ```
3.  ```
    pickAword allWordsWithLengthN index

    - Picks a word of the dictionary with length of n on index - index
    ```
4. ```
    listOfColors wordWithLengthN guessedWord

    - Returns a list of colors: "grey" if the current letter doesnt exist in the word
     "yellow" if the current letter exists in the word but is not in the right place
     "green" if the current letter exists in the word and is in the right place
    ```
5. ```
    existsInList word lst
    - Given a word and a list, checks if any element in the word exists in the list, returns all the letters that do not exist in the list.

    allYellowsNotInList guessedword word lst
    - Given a list of letters, a guessed word and a word, for every letter in the guessed word, checks if it exists in the list and in the word, if it does - removes the letter from the list. Returns a list of letters that don't meet these conditions.

    allKnownGreensAreInWord word listOfTuples
    - Given a list of tuples (All of the green tuples) and a word, returns all of the letters that are known to be green, but aren't in the right place.
    ```
6.  ```
    returnGrays guessedWord word
    - Given a guessed word and a word, returns a list of all the grey letters

    returnYellows guessedWord word
    - Given a guessed word and a word, returns a list of all the yellow letters

    returnGreens guessedWord word
    - Given a guessed word and a word, returns a list of tuples of all the green letters and their locations
    ```
7. ```
    mergeNoDups lst1 lst2
    - Given two lists, merges them with no duplicates
    ```
8. ```
    allLettersInLists greenLst greyLst yellowLst word
    - Given three lists and a word,  returns a list of all the indices of letters in the word that don't exist in any of the lists
    ```
9. ```
    listOfLIES listOfTrueColors listWhereToLie zeroOrOneForGreen zeroOrOneForYellow zeroOrOneForGrey
    - Given a list of true colors, a list of indices (where a lie is possible) and 3 random numbers (each either 0 or 1), returns a list of colors, where all of the valid indices are a lie.
    ```
10. ```
    filterDict allWords curAnswerLst curGuess
    - Given a dictionary (allWords), answer list (example -> ["yellow", "grey", "green"]) and a word, filters all of the words in the dictionary that do not contradict(possibleWord) with the answer list, using 3 helper functions:

    10.1
        possibleWord answerLst curWord guess index

        -Given an answer list, current word, guess and an index, checks for every letter in the current word, if it condtradicts with the answer list. If the letter on index does not contradict with the answer list, calls recursively with incremented index.
        If the letter does contradict with the answer list => the current word contradicts with the answer list => returns False.  
        If the index >= length of the answer list, then all of the letters in the word do not contradict with the answer list => the current word is valid.
    
    10.2
        caseYellow letter curWord guess index
        - Finds if the letter exists in the current word and not in the index of the current word
    
    10.3
        caseGrey letter curWord
        - If the letter exists in the current word, then it's not grey, returns False. Otherwise True
11. ```
    bestGuess lst1 lst2 bestScore bestWord
    - Given two lists, (bestScore = 0) and (bestWord = "") returns the word with the best score using 2 helper functions:

    11.1
        evalWord guess lst
        - Given a guess and a list, sums the wordScore of the guess and every word in the list
    
    11.2
        wordScore guess word index
        -Given a guess(word), word and an index, for every letter in the word, checks:
            1. if the letter is green in guess, adds 2 and recursively calls the function with     incremented index.
            2. if the letter is yellow in guess, adds 1 and recursively calls the function with incremented index.
            3. if the letter is not green nor yellow, doesn't add anything and calls the function with incremented index.
            4. if the index is equal to the length of the word or the guess -> returns 0
    ```

---


[Task description](https://github.com/MartinKonov/Wordle-In-Haskell/blob/main/README.md)

[Github repository](https://github.com/MartinKonov/Wordle-In-Haskell)