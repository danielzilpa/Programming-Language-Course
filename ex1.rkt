#lang pl

#|
question 1:
personal description of the solution: The function receives an array of strings as input and returns string (the first word that ends with "pl")
or flase value (if no such word exists).
The function make a few tests on the input (by "cond" - we learn that it is a syntactic sugar of the "if" condition):
1. If this is an empty list - return false.
2. If the first word length is less than 2 - move to the next word in the list*
3. If the word ends with "pl" - return this word and if not - move to the next word in the list*

*Moving on to the next word in the list - We learned that lists in this language are recursive - a list is a pair whose first organ
is anything and the second organ is the continuation of the list.
If this is the last organ we will send an empty list and so we end with the first condition.
otherwise, the first word on the next call is actually the second word on the list and so on.

The main difficulties was to understanding the compilation errors of the workspace and to figuring out how to cross the words in the list,
I solved it by repeating the material of the lesson and so I remembered the definition of a listת
I invest something like 1 hour to solving it,
I had to consult others about understanding the compilation errors of the workspace.
|#

(: plSuffixContained : (Listof String) → (U String #f))
(define (plSuffixContained list)
  (cond [(null? list) #f] ;;The first test
        [(< (string-length (first list)) 2) (plSuffixContained (rest list))] ;;The second test
        [(equal? (substring (first list) (- (string-length (first list)) 2) (string-length (first list))) "pl") (first list)] ;;The third test
        [else (plSuffixContained (rest list))])) ;;If the third test fails

;;plSuffixContained tests:
(test (plSuffixContained '()) => false)
(test (plSuffixContained '("")) => false)
(test (plSuffixContained '("p")) => false)
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => false)
(test (plSuffixContained '("yyyt" "TplT" "plTT" "PLPL" "plplpl")) => "plplpl")
(test (plSuffixContained '("yyyt" "pl" "plTT" "PLPL")) => "pl")
(test (plSuffixContained '("1danipl" "yyyt" "" "plTT" "PLPL" "2danipl")) => "1danipl")
(test (plSuffixContained '("danipl")) => "danipl")
(test (plSuffixContained '("pl")) => "pl")

#|
question 2.1:
personal description of the solution: The function receives an array of coefficients (numbers) as input and returns the polynomial (in a reversed order of
coefficients).
This solution use tail-recursion (detailed explanation on the solution adjacent to the code below).

The main difficulty was to try cover all the cases,
I solved it by listing many kinds of possible polynomials,
I invest something like 3 hour to solving it,
I had to consult others about using built-in functions in the language like "string-append" and "number->string".
|#

(: write-poly : (Listof Number) → String)
(define (write-poly list)
  (reversed-pol list ""))

#|
This is the "help" function of the tail recursion.
The variable pol - holds the updated polynomial until that moment and every new call there is calculation that updates him
(the calculation is not done at the end of calls like in a regular recursion),
once the stopping condition is reached, we simply return it.
|#
(: reversed-pol : (Listof Number) String → String)
(define (reversed-pol list pol)
  (if(null? list) ;;stopping condition
   pol ;;return the polynomial once the stopping condition is reached
   (reversed-pol (rest list) (update-pol-string list pol)))) ;;if there are more numbers in the list update pol and recursively call the function with it and the rest of the list

#|
This function update the polynomial according to the current coefficient.
|#     
(: update-pol-string : (Listof Number) String → String)
(define (update-pol-string list pol)
  (cond [(equal? pol "") (first-number list pol)] ;;if pol is empty - this is the first coefficient
        [(= (length (rest list)) 1) (before-the-last-number list pol)] ;;if there is one additional number in the list - this is the penultimate coefficient
        [(null? (rest list)) (last-number list pol)] ;;if the rest of the list is empty - this is the last coefficient
        [else (any-number list pol)])) ;;otherwise - this is some coefficient in the middle of the polynomial 

(: first-number : (Listof Number) String → String)
(define (first-number list pol)
  (cond [(= (first list) 0) pol] ;;if there is a single coefficient and he is 0 - the polynomial is an empty string
        [(null? (rest list)) (number->string (first list))] ;;if there is a single coefficient - the polynomial is this number
        [(= (length (rest list)) 1) (string-append pol (string-append (number->string (first list)) "x"))] ;;if this is the one factor before the last one - it is unnecessary to wrieh "x^1" so only "x"
        [else (string-append pol (string-append (number->string (first list)) (string-append "x^" (number->string (- (length list) 1)))))])) ;;otherwise - the x-power will be the opposite

(: before-the-last-number : (Listof Number) String → String)
(define (before-the-last-number list pol)
  (cond [(> (first list) 0) (string-append pol (string-append "+" (string-append (number->string (first list)) "x")))] ;;if the coefficient of x is positive - add the plus sign to it
        [(< (first list) 0) (string-append pol (string-append (number->string (first list)) "x"))] ;;if the coefficient of x is negative
        [else pol])) ;;otherwise - he is 0 and he reset this part so we dont need to writh him as part of the polynomial

(: last-number : (Listof Number) String → String)
(define (last-number list pol)
  (cond [(> (first list) 0) (string-append pol (string-append "+" (number->string (first list))))] ;;if the free organ is positive - add the plus sign to it
        [(< (first list) 0) (string-append pol (number->string (first list)))] ;;if the free organ is negative
        [else pol])) ;;otherwise - he is 0 and he reset this part so we dont need to writh him as part of the polynomial
  
(: any-number : (Listof Number) String → String)
(define (any-number list pol)
  (cond [(> (first list) 0) (string-append pol (string-append "+" (string-append (number->string (first list)) (string-append "x^" (number->string (- (length list) 1))))))] ;;if positive - add the plus sign and the x power to it
        [(< (first list) 0) (string-append pol (string-append (number->string (first list)) (string-append "x^" (number->string (- (length list) 1)))))] ;;if negative - add the x power to it
        [else pol])) ;;otherwise - he is 0 and he reset this part so we dont need to writh him as part of the polynomial

;;write-poly tests:
(test (write-poly '()) => "")
(test (write-poly '(3)) => "3")
(test (write-poly '(-3)) => "-3")
(test (write-poly '(0)) => "")
(test (write-poly '(6 1)) => "6x+1")
(test (write-poly '(6 -1)) => "6x-1")
(test (write-poly '(-6 2)) => "-6x+2")
(test (write-poly '(-6 -9.4)) => "-6x-9.4")
(test (write-poly '(0 2)) => "2")
(test (write-poly '(0 -2)) => "-2")
(test (write-poly '(2 0)) => "2x")
(test (write-poly '(-2 0)) => "-2x")
(test (write-poly '(0 0)) => "")
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '(-3 0.2 6)) => "-3x^2+0.2x+6")
(test (write-poly '(0 2 6)) => "2x+6")
(test (write-poly '(-3 0 6)) => "-3x^2+6")
(test (write-poly '(1 -2 0)) => "1x^2-2x")
(test (write-poly '(0 0 6)) => "6")
(test (write-poly '(3.1 0 0)) => "3.1x^2")
(test (write-poly '(0 2 0)) => "2x")
(test (write-poly '(0 0 0)) => "")
(test (write-poly '(-1 8 9 10)) => "-1x^3+8x^2+9x+10")
(test (write-poly '(7 -8 9 -10)) => "7x^3-8x^2+9x-10")
(test (write-poly '(0 8 9 10)) => "8x^2+9x+10")
(test (write-poly '(-705 0 9 10)) => "-705x^3+9x+10")
(test (write-poly '(7 56 0 10)) => "7x^3+56x^2+10")
(test (write-poly '(7 8 9 0)) => "7x^3+8x^2+9x")
(test (write-poly '(0 0 9 -10.5)) => "9x-10.5")
(test (write-poly '(7 0 0 -3.9)) => "7x^3-3.9")
(test (write-poly '(7 8.2 0 0)) => "7x^3+8.2x^2")
(test (write-poly '(0 8 -6 0)) => "8x^2-6x")
(test (write-poly '(0 -212 0 0)) => "-212x^2")
(test (write-poly '(0 0 0 0)) => "")

#|
question 2.2:
personal description of the solution: The function receives an array of coefficients (numbers) and x-value as input and returns the result of the
polynomial (in a reversed order of coefficients).
This solution use tail-recursion (detailed explanation on the solution adjacent to the code below).

The main difficulty was to find out how to calculate the power operation,
I solved it by search it on the Internet,
I invest something like 0.5 hour to solving it,
I didn't have to consult with others about this solution.
|#
(: compute-poly : Number (Listof Number) → Number)
(define (compute-poly xvalue list)
  (cp-cal-reversed-pol xvalue list 0)) ;;in the beginning the answer is 0.

(: cp-cal-reversed-pol : Number (Listof Number) Number → Number)
(define (cp-cal-reversed-pol xvalue list ans)
  (if(null? list) ;;stopping condition
   ans ;;return the answer once the stopping condition is reached
   (cp-cal-reversed-pol xvalue (rest list) (cp-update-ans xvalue list ans)))) ;;if there are more numbers in the list continue to calculate and recursively call to the function with it and the rest of the list.

(: cp-update-ans : Number (Listof Number) Number → Number) ;;this function update the answer according to the current elemet in the polynomial.
(define (cp-update-ans xvalue list ans)
  (+ ans (* (first list) (expt xvalue (- (length list) 1))))) ;;return the updated answer by calculating this way: (ans = ans + cuurent element) and the cuurent element = cuurent coefficient * x-value ^ cuurent power.

;;compute-poly tests:
(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3)) => 3)
(test (compute-poly 2 '(-6)) => -6)
(test (compute-poly -2 '(3.1 -6)) => -12.2)
(test (compute-poly 2 '(0 6)) => 6)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly -2 '(3 2 6)) => 14)
(test (compute-poly 2 '(3 0 6)) => 18)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly -3 '(4 3 2 3)) => -84)
(test (compute-poly 2 '(4.3 3 2 5)) => 55.4)
(test (compute-poly 3 '(0 3 2 8)) => 41)

#|
question 3:
personal description of the solution: I have defined a new type called "KeyStack" with 2 constructors: EmptyKS and Push.
In addition, 2 functions that receive as input an object of this type and perform operations on it (detailed explanation adjacent to the code below).

The main difficulty was to understand how to do the pop operation (how to return the keyed-stack without its first value),
I solved it by asked others,
I invest something like 1 hour to solving it,
I had to consult others about the pop operation.
|#
(define-type KeyStack ;;new type called "KeyStack"
  [EmptyKS] ;;empty constructor - to create an empty stack
  [Push Symbol String KeyStack]) ;;push constructor - to add value to the stack

(: search-stack : Symbol KeyStack → (U String #f)) ;;search operation
(define (search-stack key stack)
  (cases stack ;;this function is used to check which constructor the object "stack" was created from
    [(EmptyKS) #f] ;;if the stack is empty return false
    [(Push k v s) (if (equal? k key) ;;else (if pushed values into the stack) - check if the keys are equal
                      v ;;if they are equal - return the key value
                      (search-stack key s))])) ;;if not - search the rest of the stack (with the next key and so on)

(: pop-stack : KeyStack → (U KeyStack #f)) ;;pop operation
(define (pop-stack stack)
  (cases stack ;;this function is used to check which constructor the object "stack" was created from
    [(EmptyKS) #f] ;;if the stack is empty return false
    [(Push k v s) s]));;else (if pushed values into the stack) - return the rest of the stack (without the first value - pop operation)) 

;;KeyStack tests:
(test (EmptyKS) => (EmptyKS))
(test (Push 'd "DD" (EmptyKS)) => (Push 'd "DD" (EmptyKS)))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'a (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (Push 'a "A" (EmptyKS))) => (EmptyKS))
(test (pop-stack (EmptyKS)) => #f)

#|
question 4:
personal description of the solution: adjacent to the code below.

The main difficulty was to understand how "is-odd?" and "is-even?" functions operates together,
I solved it by doing a few sample runs on the page,
I invest something like 1 hour to solving it,
I didn't have to consult with others about this solution.

"is-odd?" and "is-even?" functions calls each other at a time with a smaller number by 1 until one receives the number 0.
When "is-odd?" function accepts the 0, it returns a false because 0 is an even number and vice versa.
In this way, the function return values will be passed back (according to the "call stack") until eventually the first function that was called returns the answer.
For example: the functions calls: is-odd?(3) -> is-even?(2) -> is-odd?(1) -> is-even?(0)
             the return values accorsing to the path in the "call stack": true -> true -> true -> true.
|#
(: is-odd? : Natural -> Boolean) ;;This function receives a natural number as input and returns whether it is an odd number (true) or not (false).
(define (is-odd? x) ;;x = the natural number we get as input
 (if (zero? x) ;;check if x equal to 0
 false ;;if equal return false because 0 is an even number
 (is-even? (- x 1)))) ;;else, check if the number-1 is an even number (the idea is that if it does - that means that this number is necessarily an odd number since follow numbers are always the opposite in their pairs)

(: is-even? : Natural -> Boolean) ;;This function receives a natural number as input and returns whether it is an even number (true) or not (false).
(define (is-even? x) ;;x = the natural number we get as input
 (if (zero? x) ;;check if x equal to 0
 true ;;if equal return true because 0 is an even number
 (is-odd? (- x 1)))) ;;else, check if the number-1 is an odd number (the idea is that if it does - that means that this number is necessarily an even number since follow numbers are always the opposite in their pairs)

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

#|
This function receive another function (accepts some type A and returns a boolean value) - this function represents the "condition"
and a list of the same type A as input and checks whether all the list arguments fulfill the "condition" and returns a boolean value accordingly.
All (A) means that "A" should be the same type in both.
|#
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
(define (every? pred lst) ;;pred = the "condition" function. lst = the list
 (or (null? lst) ;;check if the list is empty or (if the first argument in the list fulfills the condition (pred return true on him) and does the rest of the list fulfill the condition by recursively call to this function (every?)) - in this way we cheak all the list
 (and (pred (first lst))
 (every? pred (rest lst)))))

#|
An example for the usefulness of this polymorphic function.
This function receive list of natural numbers as input, checks whether all the list numbers are even numbers and returns a boolean value accordingly.
checks all the list by "every?" function while "is-even?" is the "condition" function - if it return "true" for all the list numbers the function "every?" return true.
|#
(: all-even? : (Listof Natural) -> Boolean)
(define (all-even? lst) ;;lst = trhe input list
 (every? is-even? lst)) ;;call "every?" with "is-even?" function and the list

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))

#|
This function does the same operation as "every?" function - for 2 conditions and 2 lists at the same time.
All the first list arguments should fulfill the "condition" of the first function and all the second list arguments should fulfill the "condition" of the second function.
Receive 2 functions that represents the 2 "conditions" and 2 lists and returns a boolean value.
All (A B) means that "A" and "B" can be different, but "A" should be the same type in both and "B" should be the same type in both.
|#
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
(define (every2? pred1 pred2 lst1 lst2) ;;pred1 = the first "condition" function. pred2 = the second "condition" function. lst1 = the first list. lst2 = the second list.  
 (or (null? lst1) ;;both lists assumed to be of same length - so basically check if the 2 lists are empty
 (and (pred1 (first lst1)) ;;check if the lists are empty or (if the first argument in the fisrt list fulfills the first condition (pred1 return true on him) and if the fisrt argument in the second list fulfills the second condition (pred2 return true on him) and does the rest of the lists fulfill the conditions by recursively call to this function (every2?)) - in this way we cheak all the lists
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))