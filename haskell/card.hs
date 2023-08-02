--Author: Xiaoli Liu 1400168; <xiaoli3@student.unimelb.edu.au>

{-| Purpose: 
      This file aims to provide the Haskell type GameState  
      and functions feedback, initialGuess, nextGuess to the 
      main card game program.
-}

{-|
The Mastermind card version is a game for two players, 
one guesser one answerer. The game flow as following:
1.Answerer pick answer(only 2,3,or 4 cards)
2.Guesser start guess
3.Answerer give feedback based on the Guesser's guess
4.Guesser have next guess based on feedback
5.Repeat step 3,4 until guesser find the correct answer 
-}


module Proj2 
    (-- * Proj2 importers
     -- | There is a smater importer,
     -- "initialGuess" to start first guess(step 2)
     -- "feedback" to return the answerer feedback(step 3)
     -- "nextGuess" to return a new guess(step 4)
    feedback, 
    initialGuess,
    nextGuess, 
    GameState
    -- * Proj2 exporters
    ) where

--Library imported:
import Card 
import Data.List

-------------------------------------------
-- |type GameState, store the list of the probaly guesses
type GameState = [[Card]]

-- |feedback aims to get the feedback from the guess and the answer
--input target and guess(both are the list of the cards), 
--return the feedback, feedback is 5 integers in one tuple.
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback target guess  
    = (correct_cards, lower_ranks, correct_ranks,
       higher_ranks,correct_suits)
    where 
    correct_cards = length( target \\ (target \\ guess))
    
    lower_ranks   = length([rank|
                            rank<- target_ranks,
                            rank < minimum guess_ranks])
                         
    correct_ranks = length( target_ranks \\ (target_ranks \\ guess_ranks))
    
    higher_ranks  = length([rank|
                            rank<- target_ranks,
                            rank > maximum guess_ranks])   
                            
    correct_suits = length(target_suits \\ (target_suits \\ guess_suits))
    
    target_ranks  = getranks target
    guess_ranks   = getranks guess
    target_suits  = getsuits target
    guess_suits   = getsuits guess

-- |get the ranks of the set of the cards
--input cards and output their ranks
getranks :: [Card] -> [Rank]
getranks cards =  [rank|Card suit rank <- cards]

-- |get the suits of the set of the cards
--input cards and output their suits
getsuits :: [Card] -> [Suit]
getsuits cards =  [suit|Card suit rank <- cards]
 
{-|
run the first guess and initial game state
input the number of cards players want to choose
if players choose 2 cards, use 6C,D10 
                  3 cards for 5C,8D,JH
                  4 cards for 4C,7D,8H,QS
as first guess to eleminate the most incorrect guesses
output the tuple of first guess and initial game state
-}
initialGuess :: Int -> ([Card],GameState)
initialGuess no_card 
    | no_card == 2 
              = ([Card Club R6, Card Diamond R10],initial_State)
    | no_card == 3 
              = ([Card Club R5, Card Diamond R8,
                  Card Heart Jack],initial_State)
    | no_card == 4 
              = ([Card Club R4, Card Diamond R7,
                 Card Heart R8, Card Spade Queen],initial_State)
    where 
    initial_State = get_initial_state no_card cards
    cards = [minBound..maxBound]::[Card]
{-|
get all the probilities of the guesses
input the number of card the players want to select and a set of cards
output all the probilities of the guesses in one list
-}
get_initial_state :: Int -> [Card] -> [[Card]]
get_initial_state 0 _ = [[]]
get_initial_state _ [] = []
get_initial_state n (c1:deck) = 
     let remainingCombos = get_initial_state n deck  
         combosWithC1 = map (c1:) (get_initial_state (n-1) deck) 
     in combosWithC1 ++ remainingCombos 

{-|
take the tuple of last guess, last game state also last feedback as input
output tuple of new_guess and new_GameState
new_GameState is all possible guesses from remaining 
    possible guesses that generate the same feedback as the answer
new_guess is the guess which has the most probility to 
eliminate most of the remaining cards
when new_GameState is too bigger, to reduce the time,
new_guess will only be the first guess in the guess list
-}
nextGuess :: ([Card],GameState) 
          -> (Int,Int,Int,Int,Int) 
          -> ([Card],GameState)     

nextGuess (last_guess,last_GameState) last_feedback
         = (new_guess,new_GameState)
         where 
         new_GameState = [guess|guess <- last_GameState, 
                          feedback guess last_guess  == last_feedback]
         
         new_guess     = 
             if length new_GameState > 3000
                 then head new_GameState
             else get_NewGuess new_GameState

{-|
get the best guess from the possible guesses list
input possible guesses list
output guess
1.function create list of tuple (guess,possible guesses)
2.calculate all feedbacks for one guess and other guesses
generate the list of tuple(guess,[feedback1..])
3.use all feedbacks calculate the guess's expeced value 
4.rank all guesses by their expected values
The guesses with the minimum expected value is the best guess for next turn
-}
get_NewGuess :: GameState -> [Card]       
get_NewGuess posb_guess = newGuess
        where 
        guess_answers   = [(guess,posb_guess) 
                           |guess <- posb_guess] 
                           
        guess_feedbacks = [(guess, getFeedbacks guess posb_guess)
                          |(guess, posb_guess) <- guess_answers]
                          
        expValue_guess  = [(getExpValue guessFeedBacks,guess)
                          |(guess,guessFeedBacks) <- guess_feedbacks]
        
        (_ ,newGuess)   = head $ sortOn fst expValue_guess

{-|
get all feedbacks for one guess and other posible guesses
input the guess and other posible guesses
output the list of the feedbacks
after get the feedbacks, sort and group them
-}
getFeedbacks :: [Card] -> GameState -> [[(Int,Int,Int,Int,Int)]]
getFeedbacks guess answers = group $ sort $ feedbacks
           where feedbacks = [feedback answer guess
                             |answer <- answers]

{-|
calculate the expected value by feedback list
input feedback list
output expected value
expected value is the sum of the squares of the 
group sizes divided by the sum of the group sizes.
-}
getExpValue ::[[(Int,Int,Int,Int,Int)]] -> Int
getExpValue feedbacks = div sqrSumSizes sumSizes
      where 
      sqrSumSizes = sum $ map((^2) .length) feedbacks
      sumSizes  = sum $ map length feedbacks
             
