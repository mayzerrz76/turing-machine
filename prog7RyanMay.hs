-------------------------------------------------------------
--------------------Programming Task 7-----------------------
-------------------------------------------------------------
--  Name:  RYAN MAY                                        --
--  Email: rmay03@syr.edu                                  --
-------------------------------------------------------------
import Turing

---------------------------------------------------------------------------------------------------------
-- PROBLEM #1
-- Purpose: This turing machine takes a string comprised of only 'a' and 'h' characters.  The TM will then place
--          a '1' at the end of the original string for each time the sequence "haah" occurs. Note that the end of
--          an "haah" occurrence may mark the beginning of another one.

-- Hi-Level Description: This TM will begin in the start state, with the tape head pointing at the left most char of the string.
--          If the machine sees an 'h' it will move to the sawH state, otherwise, TM saw an 'a' and will stay in start.  When in 
--          the saw H state, if an 'a' is seen move on to the sawHA state, otherwise TM saw an 'h' and returns back to sawH, because the
--          sequence was disrupted. When in the sawHA state, if an 'a' is seen move to the sawHAA state, otherewise TM saw an 'h' and returns 
--          back to sawH, because the sequence was disrupted.  When in the sawHAA state, if we see an 'h' we have found an occurence of our sequence
--          and TM changes the 'h' to an 'X' as a placeholder goes to the findEnd state, otherwise an 'a' was seen and we return to start.  When
--          in the findEnd state, the TM is looking for the first blank, which should be the end of the string, a 1 is placed in place of the blank,
--          for any other char, the TM just moves past it. When a 1 is placed at the end of the string, the TM enters the goback state, in which case
--          it searches for the 'X' placeholder, and when it finds the 'X' it will replace it with the original 'h' it was placeholding for, the TM
--          will then return to the sawH state, because the end of a sequence also marks the beginning of another possible sequence.  When the TM reaches
--          either a 1 or a blank in start,sawH,sawHA, or sawHAA, there is no instruction for the turing machine and it terminates, this should only happen
--          when each char in the original string was evaluated for the sequence.

-- Definition:
haahTM :: Prog
haahTM = [ 
           (("start",'a'),('g',Rght,"start")),
           (("start",'h'),('h',Rght,"sawH")),

           (("sawH",'h'),('h',Rght,"sawH")),
           (("sawH",'a'),('a',Rght,"sawHA")),

           (("sawHA",'h'),('h',Rght,"sawH")),
           (("sawHA",'a'),('a',Rght,"sawHAA")),

           (("sawHAA",'h'),('X',Rght,"findEnd")),
           (("sawHAA",'a'),('a',Rght,"start")),

           (("findEnd",'a'),('a',Rght,"findEnd")),
           (("findEnd",'h'),('h',Rght,"findEnd")),
           (("findEnd",'1'),('1',Rght,"findEnd")),
           (("findEnd",' '),('1',Lft,"goback")),

           (("goback",'a'),('a',Lft,"goback")),
           (("goback",'h'),('h',Lft,"goback")),
           (("goback",'1'),('1',Lft,"goback")),
           (("goback",'X'),('h',Rght,"sawH"))
         ]



---------------------------------------------------------------------------------------------------------
-- PROBLEM #2
-- Purpose: This turing machine will take a string comprised of 'd', 'e', and 'i' chars.  It will then parse the string and determine
--          if the number of 'e's in the string is divisible by 4.  Note that 0 is divisible by four so a string with no 'e's should be
--          divisible by 4.  The result of this turing machine will be a single character, either a Y or an N, which indicates Yes amt of e's 
--          is divisible by 4, or No amt of e's was not divisible by 4 (respectively).

-- Hi-Level Description: This TM begins with the leftmost character of a string of 'd's, 'i's, and 'e's.  The TM begins in the divisible state
--          because initially the TM has encountered 0 e's.  The TM then traverses the string to the right, analyzing and deleting each character
--          as it traverses.  The TM only changes state when it encounters an 'e' character.  Because the TM does not care about the total number
--          of e's, but rather just if that total is divisible by 4, the TM only has 4 different states. Every 4 e's, starting from 0, will make the
--          total diisible by 4. The other 3 states, not1, not2, and not3, happen when (total # of e's) / 4 has a remainder of 1, 2, or 3 respectively.
--          So, as e's are encountered the states move as such, [divisible -> not1 -> not2 -> not3 -> divisible -> not1 -> ...)  When the TM reaches the
--          end of the string, it will have deleted the contents of the string, and will be in 1 of 4 states, not1, not2, not3, or divisible. If the TM is
--          reaches end of string in divisible state, a 'Y' char is written to the tape and execution ends. On the contrary, if the TM reaches end of string 
--          in not1, not2, or not3 state, an 'N' char will be written to the tape, and execution ends.



-- Definition:
divFourTM :: Prog
divFourTM = [ 
           (("divisible",'d'),(' ',Rght,"divisible")),
           (("divisible",'i'),(' ',Rght,"divisible")),
           (("divisible",'e'),(' ',Rght,"not1")),
           (("divisible",' '),('Y',Rght,"finish")),

           (("not1",'d'),(' ',Rght,"not1")),
           (("not1",'i'),(' ',Rght,"not1")),
           (("not1",'e'),(' ',Rght,"not2")),
           (("not1",' '),('N',Rght,"finish")),

           (("not2",'d'),(' ',Rght,"not2")),
           (("not2",'i'),(' ',Rght,"not2")),
           (("not2",'e'),(' ',Rght,"not3")),
           (("not2",' '),('N',Rght,"finish")),

           (("not3",'d'),(' ',Rght,"not3")),
           (("not3",'i'),(' ',Rght,"not3")),
           (("not3",'e'),(' ',Rght,"divisible")),
           (("not3",' '),('N',Rght,"finish"))
         ]


---------------------------------------------------------------------------------------------------------
-- PROBLEM #3
-- Purpose: This turing machine takes a string comprised of only 'b' and 'g' characters.  The TM then orders the
--          string so that all the b's in the string occur before the g's (i.e. all b's at beginning of string, all
--          g's at the end of the string. The result will be the in order string.

-- Hi-Level Description: This TM begins at the left most character, in the start state. Because we want b's to come first, whenever
--          a 'b' is encountered in the start state, the TM will just move to the next char. However, if a 'g' is encountered the TM
--          will place a 'G' placeholder in place of the 'g' and proceed to the switch state.  In the switch state the TM is moving 
--          past 'g's searching for the first occurence of 'b', when this happens the 'b' will be replaced by a 'g', and then the TM will
--          enter the switchBack state to traverse to the left to find the 'G' placeholder to turn into a 'b' and return back to start state. 
--          However, if a blank is reached while in switch mode, that means there are no 'b's that come after the 'g' we are trying to switch. 
--          In that case, the cleanup state is entered to cleanup the 'G' placeholder. 


-- Definition:
orderBGsTM :: Prog
orderBGsTM = [ 
           (("start",'b'),('b',Rght,"start")),
           (("start",'g'),('G',Rght,"switch")),

           (("switch",'b'),('g',Lft,"switchBack")),
           (("switch",'g'),('g',Rght,"switch")),
           (("switch",' '),(' ',Lft,"cleanup")),

           (("switchBack",'G'),('b',Rght,"start")),
           (("switchBack",'b'),('b',Lft,"switchBack")),
           (("switchBack",'g'),('g',Lft,"switchBack")),

           (("cleanup",'b'),('b',Lft,"cleanup")),
           (("cleanup",'g'),('g',Lft,"cleanup")),
           (("cleanup",'G'),('g',Lft,"cleanup"))
         ]

---------------------------------------------------------------------------------------------------------
-- PROBLEM #4
-- Purpose: This turing machine takes a string comprised of only '1', '2', and '3' characters.  The turing machine then
--          determines if the string contains an equal number of '1's and '2's in the string.  Note that when the string
--          contains no '1's or '2's, that the amount of each is 0, and therefor their amounts are equal.  The TM will return
--          just one char, either a 'Y' or a 'N', which indicates whether or not the amount of 1's and 3's are equal. (Y - yes there
--          were an equal amount, or N - there werent an equal amount)

-- Hi-Level Description: This turing machine starts at the leftmost character of a string of 1's 2's and 3's. The TM will traverse the string to
--          the right, moving past 2's, when the TM encounters a 1 or a 3, it will proceed to saw1 or saw3 state, respectively, and mark the spot with
--          a 'C' as a placeholder. When in the saw1 state, the TM will traverse the string to the right, looking for an occurence of a 3, skipping over
---         1's and 2's.  If the end of string is reached without finding a 3, then the amount of 1's and 3's are not equal, and the notEqual state is entered.
--          If 3 does occur in the saw1 state, it will be marked by an X, and will enter the marchBack state to return to the 'C' placeholder.  The saw3 state
--          works similarly to the saw1 state, i.e. looking for for occurence of 1, if 1 is not found, enter notEqual state, if 1 is found mark it as an X and 
--          enter marchBack state to return to 'C' placeholder.  The marchBack state simply traverses to the left skipping over chars until it reaches 'C', which
--          it replaces with an 'X', and moves back to the start state. The notEqual state mentioned above will remove all chars (1,2,3,C,X) and when a blank is reached
--          will write an 'N' and the program will terminate. The idea is that when the amount of 1's and 3's are equal, the TM should reach the end of the string in the
--          start state, given that each 1 and 3 has been matched and replaced by X's (i.e. there is no 3 that has no corresponding 1, and vice versa). So when a blank is 
--          encountered in the start state it will enter the equal state, which works similarly to the notEqual state, removing all characters(1,2,3,X - C is not included
--          because there should be no C if there are an equal amount), and writing a Y to the tape, then terminating.

-- Definition:
equalNumsTM :: Prog
equalNumsTM = [ 

           (("start",'1'),('C',Rght,"saw1")),
           (("start",'2'),('2',Rght,"start")),
           (("start",'3'),('C',Rght,"saw3")),
           (("start",'X'),('X',Rght,"start")),
           (("start",' '),(' ',Lft,"equal")),

           (("saw1",'1'),('1',Rght,"saw1")),
           (("saw1",'X'),('X',Rght,"saw1")),
           (("saw1",'2'),('2',Rght,"saw1")),
           (("saw1",'3'),('X',Lft,"marchBack")),
           (("saw1",' '),(' ',Lft,"notEqual")),

           (("saw3",'1'),('X',Lft,"marchBack")),
           (("saw3",'X'),('X',Rght,"saw3")),
           (("saw3",'2'),('2',Rght,"saw3")),
           (("saw3",'3'),('3',Rght,"saw3")),
           (("saw3",' '),(' ',Lft,"notEqual")),

           (("marchBack",'1'),('1',Lft,"marchBack")),
           (("marchBack",'2'),('2',Lft,"marchBack")),
           (("marchBack",'3'),('3',Lft,"marchBack")),
           (("marchBack",'X'),('X',Lft,"marchBack")),
           (("marchBack",'C'),('X',Rght,"start")),

           (("notEqual",'1'),(' ',Lft,"notEqual")),
           (("notEqual",'2'),(' ',Lft,"notEqual")),
           (("notEqual",'3'),(' ',Lft,"notEqual")),
           (("notEqual",'X'),(' ',Lft,"notEqual")),
           (("notEqual",'C'),(' ',Lft,"notEqual")),
           (("notEqual",' '),('N',Lft,"finish")),

           (("equal",'1'),(' ',Lft,"equal")),
           (("equal",'2'),(' ',Lft,"equal")),
           (("equal",'3'),(' ',Lft,"equal")),
           (("equal",'X'),(' ',Lft,"equal")),
           (("equal",' '),('Y',Lft,"finish"))
         ]