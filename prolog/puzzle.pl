%Author: Xiaoli Liu 1400168; <xiaoli3@student.unimelb.edu.au>

%Purpose: This program aims to complete the Fill-in puzzle.

% This file start from a main function called puzzle_solution(Puzzle,WordList),
% which will complete the Fill-in puzzle with the word in WordList.

% Fill-in puzzle is a matrix with several solid, pre-filed and empty squares,
% we could not modify the solid and pre-filled squares.
% The aim of this game is to use the given wordlist to fill all empty squares 
% without break any word and match the pre-filled character, 
% each word appears in WordList should only be used once.

% The input WordList is in matrix form, each row represent a word, 
% characters in word(row) are seperated by ','
% eg.WordList = [[h,a,t],[b,a,g]].

% The input puzzle has at most one solution(fill with WordList).
% we use matrix to represent the puzzle:

% puzzle = 
% [['#','h','#'],
%  [ _ , _ , _],
%  ['#' ,_, '#']]

%'#' is the solid square we can not fill,
% characters like'h' is the pre-filled square,
% _ represent the empty square in puzzle,
% unbound variable in prolog and waitting for filling.

% Firstly, program will get all slots in the puzzle.
% A slot is the lists of ordered squares.
% eg. ['#','h','#'], this list represent a row in the puzzle above, 
% each element in it represents a square in the puzzle, 
% and slot we grab in this row is like:['h'], and the slots we can get from 
% the puzzle above is like: [['h'],[_,_,_],[_]].
% After get the slots from rows, we transpose the puzzzle and  
% get the slots form the column:
% [[_],[h,_,_],[_]], hence, the total slots in the puzzle is:
% [['h'],[_,_,_],[_],[_],[h,_,_],[_]].
% However, our aim is to fill slots with word, [] represent only one character
% that should not considered as slot, 
% hence we delete the lists with length not larger than 1,
% the result of the total slot's list is:[[_,_,_],[h,_,_]].
% That's mean we need to fill two words in this pzzle.

% Secondly, the program will sort the slots by the number of matched words, 
%this number represent how many words can be filled in this slot.
% In this step, we use the match number as the key,
% use key sort approach to find the best slot:
% eg. after find the match number(use wordlist [[h,a,t],[b,a,g]]), 
% we will get: [2-[_,_,_],1-[h,_,_]], for clearly show the concept, 
% we assign unbound variables' name:[2-[X,Y,Z],1-[h,U,V]]
% after sort by key, we will get:[1-[h,U,V],2-[X,U,Z]]
% 1-[h,U,V] means this slot can only be filled by word 'hat'
% 2-[X,U,Z] means this slot can be filled by both 'hat' and 'bag'

% Thirdly, the program will pick the best slot which 
% has the minimum fillable words, and fill the slot.
% eg.with the sorted slot list we got from last step, 
% 1-[h,U,V] is the current best slot, it can be only filled with word 'hat',
% prolog will unified U = a, V = t. 
% we fill it then remove this slot from our slots' list,
% also this used word from word list,
% and update our key to find the next best slot:
% after update, our slot list become: 
% [1-[X,a,Z]](U already bounded to a), 
% which means this slot can only unified with word 'bag'.

% Finally, the program repeat the third step to fill all the slots,
% unified all unbound variables in the matrix.
% after all slots filled with all variable bounded, 
% the Fill-in puzzle completed.

% Libiary used
:- ensure_loaded(library(clpfd)).

% puzzle_solution(+Puzzle, +WordList)
% complete the Fill-in puzzle.
% 1.get_all_slots(Puzzle,RowSlots) 
%    get all the slots of rows' from the puzzle.
% 2.transpose(Puzzle,PuzzleT)
%    get the transposed puzzle, puzzleT.
% 3.get_all_slots(PuzzleT,ColSlots)
%    get all the slots of cols' from the puzzle(rows in puzzleT).
% 4.append(RowSlots,ColSlots,Slots),
%    combine two slot's list to get total slots' list.
% 5.filling(Slots,WordList),
%    fill slots in Slots with the word in Wordlist.
puzzle_solution(Puzzle, WordList) :-
    get_all_slots(Puzzle,RowSlots),
    transpose(Puzzle,PuzzleT),
    get_all_slots(PuzzleT,ColSlots),
    append(RowSlots,ColSlots,Slots),
    filling(Slots,WordList).


% get_all_slots(+Puzzle,-Slots)
% inpur Puzzle, get all slots from the puzzle,output slot list Slots.
% 1. use maplist to apply find_row_slot for each row
% 2. use append/2 to flatten the RowSlots list
% 3. filter the slots with length more than one.
get_all_slots(Puzzle,Slots):-
    maplist(find_row_slot,Puzzle,RowSlots),
    append(RowSlots,Row_Flatten_Slots),
    filter_long_sublists(Row_Flatten_Slots,Slots).

% find_row_slot(+Row,-Slots)
% find slots in one row, input Row, output its slots.
% This function tranverse each element in the row, 
% when meet the empty square or pre-filled square,
% append it to WordAcc,
% when meet '#', append accumulated Wordacc into the accumulated slot list
% and initialize the Wordacc to [](means a slot end).
% eg. Row = ['#',h,_,'#'], return Slots: [[],[h,_],[]].
find_row_slot(Row,Slots):-
    find_row_slot(Row,[],[],Slots).

find_row_slot([],WordAcc,Restslot,Slots):-
    append(Restslot,[WordAcc],Slots).

find_row_slot([H|Tail],WordAcc,Restslot,Slots):-
    (H == '#' ->  
    append([WordAcc],Restslot,Restslot1),
    find_row_slot(Tail,[],Restslot1,Slots)
    ;
    append(WordAcc, [H], Acc1),
    find_row_slot(Tail,Acc1,Restslot,Slots)
    ). 

% filter_long_sublists(+List, -Filtered)
% use include filter list with length >1. 
filter_long_sublists(List, Filtered) :-
    include(long_list, List, Filtered).

% is true if length of list > 1.
long_list(List) :-
    length(List, Len),
    Len > 1.

% filling(+Slots, +WordList)
% This function aims to fill all slots in the Slots list with the WordList.
% 1.use fitNum_Slot to get the fitnumber-slot pairs
%    like: [2-[_,_,_],1-[h,_,_]], mentioned before
% 2.sort fitnumber-slot list by key(fitnumber),
%    [1-[h,_,_],2-[_,_,_]]
% 3.fill the best slot(with minimum fit number)
%    [h,_,_] = [h,a,t]
%    delete filled slot and used word
% 4.reset the slots without key.(delete key)
%    [[_,a,_]]
% repeat until all slots filled.
filling([], _).
filling(Slots, WordList):-
    fitNum_Slot(Slots,WordList,Result),
    keysort(Result, Sorted),
    Sorted = [_-Best_slot|RestSorted],
    select(Best_slot, WordList, Rest_words),
    reset_slot_list(RestSorted, Rest_slots),
    filling(Rest_slots, Rest_words).

% fitNum_Slot(+Slot_list,+Word_list, -Result)
% use fit_number to find the number of words fitted in slot.
% convert slots to key-value pairs,key is the fit_num.
% eg. input [[_,_,_],[h,_,_]],output [2-[_,_,_],1-[h,_,_]]
fitNum_Slot(Slots,WordList,Result):-
    fitNum_Slot(Slots,WordList,[], Result).
fitNum_Slot([], _, Result, Result).
fitNum_Slot([Slot|Total_slot_list],Word_list, Acc, Result):-
    fit_number(Slot,Word_list,FitNumber),
    fitNum_Slot(Total_slot_list,Word_list,[FitNumber-Slot|Acc],Result).

% fit_number(+Slot,+WorldList,-FitNumber)
% find the fit number of the slot with word in WordList.
% if slot can be unified with the word, acc+=1.
% eg. input Slot = [h,_,_], WordList = [[h,a,t],[b,a,g]],output 1
fit_number(Slot,Word_list,FitNumber):-
    fit_number(Slot,Word_list,0,FitNumber).
fit_number(_,[],Acc,Acc).
fit_number(Slot,[Word|Total_word_list],Acc,Result):- 
    
    (Slot \= Word
    -> fit_number(Slot,Total_word_list,Acc,Result)
    ;  Acc2 is Acc +1,
    fit_number(Slot,Total_word_list,Acc2,Result)).

% reset_slot_list(+Rest, -Rest2)
% convert slots to slots without '-'
% eg. input [2-[_,_,_]] ,output [[_,_,_]]
reset_slot_list([], []).
reset_slot_list([_-S|Rest], [S|Rest2]) :-
    reset_slot_list(Rest, Rest2).
