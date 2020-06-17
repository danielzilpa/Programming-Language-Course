; The ROL BNF and Parsing code:
#lang pl

#|
question 1.1:
personal description of the solution: This solution include a BNF grammar for the ROL language,
and three examples for ROL words and how they are derived from this BNF.
The main difficulty was to understand the ROL language - the meaning of each of these parts: ROL, RegE and Bits.
I solved it by consulting with other students.
I invest something like 3 hour to solving it.
I had to consult others about the meaning of each of these parts: ROL, RegE and Bits - Olga Mazo helped me.
|#

;;Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;;The actual interpreter
#|
BNF for the RegE language:

<ROL> ::= { reg-len = <Num> <RegE> }

<RegE> ::= <Bits>
         | { and <RegE> <RegE> }
         | { or <RegE> <RegE> }
         | { shl <RegE> }

<Bits> ::= <BIT> | <BIT> <Bits>

3 examples for ROL program codes and the appropriate three derivation processes:

"{ reg-len = 3 {or {shl {1 0 1}}{shl {1 0 1}}}}" ->
<ROL> ->
{ reg-len = 3 <RegE> } ->
{ reg-len = 3 { or <RegE> <RegE> } } ->
{ reg-len = 3 { or { shl <RegE> } { shl <RegE> } } } ->
{ reg-len = 3 { or { shl <Bits> } { shl <Bits> } } } ->
{ reg-len = 3 { or { shl 1 <Bits> } { shl 1 <Bits> } } } ->
{ reg-len = 3 { or { shl 1 0 <Bits> } { shl 1 0 <Bits> } } } ->
{ reg-len = 3 { or { shl 1 0 1 } { shl 1 0 1 } } }


"{ reg-len = 2 { and {or {shl {1 0}} {shl {1 0}}} {1 0}}}" ->
<ROL> ->
{ reg-len = 2 <RegE> } ->
{ reg-len = 2 { and <RegE> <RegE> } } ->
{ reg-len = 2 { and { or <RegE> <RegE> } <Bits> } } ->
{ reg-len = 2 { and { or { shl <RegE> } { shl <RegE> } } 1 <Bits> } } ->
{ reg-len = 2 { and { or { shl <Bits> } { shl <Bits> } } 1 0 } } ->
{ reg-len = 2 { and { or { shl 1 <Bits> } { shl 1 <Bits> } } 1 0 } } ->
{ reg-len = 2 { and { or { shl 1 0 } { shl 1 0 } } 1 0 } }


"{ reg-len = 4 { or {or {shl {1 0 0 1}} {1 0 0 1}} {1 0 0 1}}}" ->
<ROL> ->
{ reg-len = 4 <RegE> } ->
{ reg-len = 4 { or <RegE> <RegE> } } ->
{ reg-len = 4 { or { or <RegE> <RegE> } <Bits> } } ->
{ reg-len = 4 { or { or { shl <RegE> } <Bits> } 1 <Bits> } } ->
{ reg-len = 4 { or { or { shl <Bits> } 1 <Bits> } 1 0 <Bits> } } ->
{ reg-len = 4 { or { or { shl 1 <Bits> } 1 0 <Bits> } 1 0 0 <Bits>} } ->
{ reg-len = 4 { or { or { shl 1 0 <Bits> } 1 0 0 <Bits>} 1 0 0 1} } ->
{ reg-len = 4 { or { or { shl 1 0 0 <Bits>} 1 0 0 1} 1 0 0 1} } ->
{ reg-len = 4 { or { or { shl 1 0 0 1} 1 0 0 1} 1 0 0 1} } ->

|#

#|
question 1.2:
personal description of the solution: Building the ROL language parser.
The main difficulty was to understand the need and purpose of the "parse-sexpr-RegL" function.
I solved it by consulting with other students.
I invest something like 2 hour to solving it.
I had to consult others about the "parse-sexpr-RegL" function and the location of each of the required tests.
|#

;;RegE abstract syntax trees
(define-type RegE
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Shl RegE])

;;Next is a technical function that converts (casts)
;;(any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List) ;;this function get list of any and return list of bits.
(define (list->bit-list lst) ;; to cast a list of bits as a bit-list
 (cond [(null? lst) null] ;;if the input list is empty - return null
       [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))] ;;if the first element in the list is 1 create pair - the first element is 1 and the second by recursive call to function with the rest of the list 
       [else (cons 0 (list->bit-list (rest lst)))])) ;;else create pair - the first element is 0 and the second by recursive call to function with the rest of the list. 

#|
parse-sexpr function.
recive sexpr - word in ROL language as input.
Return RegE (abstract syntax trees).
|#
(: parse-sexpr : Sexpr -> RegE)
(define (parse-sexpr sexpr) ;; to convert the main s-expression into ROL
 (match sexpr
  [(list reg-len = (number: n) Sexpr) ;;if sexpr is from this structure (reg-len =  number sexpr)
   (if (> n 0) (parse-sexpr-RegL Sexpr n) (error 'parse-sexpr "list lengt should be at least one ~s" sexpr))] ;;if the number is greater than 0 - call "parse-sexpr-RegL" function, else drop an error.
  [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) ;;if sexpr is not from this structure - drop an error.

#|
parse-sexpr-RegL function.
recive sexpr - the B part from "{ reg-len = len B}" and Number - the reg lenght.
Return RegE (abstract syntax trees).
|#
(: parse-sexpr-RegL : Sexpr Number -> RegE)
(define (parse-sexpr-RegL sexpr reg-len) ;;to convert s-expressions into RegEs
 (match sexpr
  [(list (and a (or 1 0)) ... ) ;;if sexpr is from this structure - list where each element is 0 or 1.
   (if (= (length a) reg-len) (Reg(list->bit-list a)) (error 'parse-sexpr "wrong number of bits in ~s" a))] ;;if the list lenght equal to reg-len - this is [Reg Bit-List].
  [(list 'and lhs rhs) ;;if sexpr is from this structure - in length 3 and the first word is "and"
   (And (parse-sexpr-RegL lhs reg-len) (parse-sexpr-RegL rhs reg-len))] ;;[And RegE RegE] - the two RegEs in recursive call to function
  [(list 'or lhs rhs) ;;if sexpr is from this structure - in length 3 and the first word is "or"
   (Or (parse-sexpr-RegL lhs reg-len) (parse-sexpr-RegL rhs reg-len))] ;;[Or RegE RegE] - the two RegEs in recursive call to function
  [(list 'shl hs) ;;if sexpr is from this structure - in length 2 and the first word is "shl"
   (Shl (parse-sexpr-RegL hs reg-len))] ;;[Shl RegE] - RegEs in recursive call to function
  [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) ;;else - if sexpr not match to any of these templates - drop an error.

#|
parse function.
recive string as input - Words in the ROL language are of the form "{ reg-len = len B}"
(where len is a natural number, and B is the sequence of Register operations to be performed and the registers they should be applied to).
Return RegE (abstract syntax trees).
|#
(: parse : String -> RegE)
(define (parse str) ;;parses a string containing a RegE expression to a RegE AST
 (parse-sexpr (string->sexpr str))) ;;send to "parse-sexpr" function the input as sexpr (sexpr is an existing recursive type in racket).

;;tests:
(test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse "{ reg-len = 1 {1}}") => (Reg '(1)))
(test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 1 {and {shl {1}} {shl {0}}}}") => (And (Shl (Reg '(1))) (Shl (Reg '(0)))))
(test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 4 { and {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (And (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{ reg-len = 2 { or {or {shl {1 0}} {1 0}} {1 0}}}") => (Or (Or (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))


(test (parse "{ reg-len = 2 {1 0 0 0}}") =error> "wrong number of bits in")
(test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")

(test (parse "{ reg-len = 0 {}}") =error> "list lengt should be at least one")
(test (parse "{ reg-len = -3 {1 0 0 0}}") =error> "list lengt should be at least one")
(test (parse "{ reg-len = 0 {or {1 1 1 1} {0 1 1}}}") =error> "list lengt should be at least one")

(test (parse "{ 4 {1 0 0 0}}") =error> "bad syntax in")
(test (parse "{ reg-ln = 6}") =error> "bad syntax in")
(test (parse "{ reg-len 3 {or {1 1 1 1} {0 1 1}}}") =error> "bad syntax in")

(test (parse "{ reg-len = 4 {{1 1 1 1} {1 1 1 1}}}") =error> "bad syntax in")
(test (parse "{ reg-len = 2 {shl {1 1} {1 1}}}") =error> "bad syntax in")

#|
question 2:
personal description of the solution: enhance the AE language with a memory functionality by adding set and get operators.
The main difficulty was to understand the question and how to build the BNF grammar.
I solved it by tracking and dismantling many examples and asked more students.
I invest something like 4 hour to solving it.
I had to consult others about the BNF grammar structure.
I try to understand the question with Olga Mazo.

a:
The problem that is demonstrated by this example is that we can reach a state of ambigious results.
We learned in a class that the grammar needs to be set so that there is no state that we can reach different meanings for the same word,
and here it does not happen - you can refer to 1 or 2 first.

b:
This can be solved by defining a specific MAE semantics - set the tree to open only to one side for example on the right.

<AE> ::= <num> 
       | {+ <AE> <AE>} 
       | {- <AE> <AE>} 
       | {* <AE> <AE>} 
       | {/ <AE> <AE>}

<MAE> ::= {seq <AE>}
        | {seq <SET>}

<HELPER> ::= {set <GET>} 
          |  {set <GET>} <HELPER> 

<GET> ::= get 
       | <num> 
       | {+ <GET> <GET>} 
       | {- <GET> <GET>} 
       | {* <GET> <GET>} 
       | {/ <GET> <GET>} 

<SET> ::= {set <AE>} <GET> 
       |  {set <AE>} <HELPER> <GET> 


1)314112103 ->
{seq <AE>} ->
{seq {- <AE> <AE>}} ->
{seq {- <num> <num>}} ->
{seq {- 31 103}}

2)314112103 ->
{seq <SET>} ->
{seq {set <AE>} <GET>} ->
{seq {set {/ <AE> <AE>}} {* <GET> <GET>}} ->
{seq {set {/ <num> <num>}} {* <num> <num>}} ->
{seq {set {/ 41 121}} {* 314 21}}

3)314112103 ->
{seq <SET>} ->
{seq {set <AE>} <HELPER> <GET>} ->
{seq {set {+ <AE> <AE>}} <HELPER> <num>} ->
{seq {set {+ <num> <num>}} <HELPER> <num>} ->
{seq {set {+ <num> <num>}} {set <GET>} <num>} ->
{seq {set {+ <num> <num>}} {set {* <GET> <GET>}} <num>} ->
{seq {set {+ <num> <num>}} {set {* <num> <num>}} <num>} ->
{seq {set {+ 411 10}} {set {* 31 121}} 210}
|#

#|
question 3:
personal description of the solution: calculate sum of the squares of numbers using foldl function.
The main difficulty was to understand the foldl function.
I solved it by read the explanation several times.
I invest something like 30 minutes to solving it.
I had to consult others - I helped Olga Mazo to think about the solution.
|#

#|
sum-of-squares function.
recive list of numbers as input,
return number which is the sum of the squares of all of the numbers in the list.
|#
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares l)
  (foldl + 0 (map num-square l))) ;;the function is +, the init value is 0 and map will call to num-square func with each number in the list

#|
num-square function.
recive number as input,
return number which is the squares of the number.
|#
(: num-square : Number -> Number)
(define (num-square n)
  (* n n))

;;tests:
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 -2 3)) => 14)
(test (sum-of-squares '(-1 -1 -3)) => 11)
(test (sum-of-squares '(1 2 3 0 1)) => 15)
(test (sum-of-squares '(3)) => 9)
(test (sum-of-squares '(-5)) => 25)
(test (sum-of-squares '(0 0)) => 0)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(0.5 0.5 1)) => 1.5)

#|
question 4:
personal description of the solution: various actions on binary tree (more above the code).
The main difficulty was to understand what does tree-fold function need to do.
I solved it by see students' questions and answers in the course's whatsapp group.
I invest something like 5 hour to solving it.
I had to consult others about tree-fold function.
Olga Mazo helped me to think about the solution
|#

(define-type BINTREE ;;new type called "BINTREE".
  [Node BINTREE BINTREE] ;;Node constructor - has 2 sub-trees.
  [Leaf Number]) ;;Leaf constructor - has a number.

#|
tree-map function.
recive function f and a binary tree as input,
return a tree with the same shape but using f(n) for values in its leaves.
|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f tree) 
  (cases tree 
   [(Node b1 b2) (Node (tree-map f b1) (tree-map f b2))] ;;if this is node - call the function recursively with each of its sub trees.
   [(Leaf n) (Leaf (f n))])) ;;if this is leaf - create leaf with f(n) value.

;;tree-map tests:
(test (tree-map sub1 (Node (Node (Leaf 5) (Node (Leaf 7)(Leaf 9))) (Leaf -2))) => (Node (Node (Leaf 4) (Node (Leaf 6)(Leaf 8))) (Leaf -3)))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 (Node (Leaf 0)(Leaf 3.2))) => (Node (Leaf 1) (Leaf 4.2)))
(test (tree-map sub1 (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))
(test (tree-map sub1 (Leaf 0)) => (Leaf -1))
(test (tree-map num-square (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => (Node (Leaf 1) (Node (Leaf 4) (Leaf 9))))

#|
tree-fold function.
recive the combiner function, the leaf function, and the BINTREE as input,
this function is the equivalent of the swissarmy-knife tool that foldl is for lists and recive some input type A.
|#
(: tree-fold : All (A) (A A -> A) (Number -> A) BINTREE -> A)
(define (tree-fold f1 f2 tree)
  (cases tree 
   [(Node b1 b2) (f1 (tree-fold f1 f2 b1) (tree-fold f1 f2 b2))] ;;if this is node - call the function recursively with each of its sub trees and the functions.
   [(Leaf n) (f2 n)])) ;;if this is leaf - call f2 func with the leaf value.

;;tree-fold tests:
(test (tree-fold + add1 (Node (Node (Leaf 5) (Node (Leaf 7)(Leaf 9))) (Leaf -2))) => 23)
(test (tree-fold + add1 (Node (Leaf 1) (Node (Leaf 0)(Leaf -1)))) => 3)
(test (tree-fold + add1 (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => 9)
(test (tree-fold + sub1 (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => 3)
(test (tree-fold - add1 (Node (Leaf 0)(Leaf 3.2))) => -3.2)
(test (tree-fold - sub1 (Leaf 0)) => -1)

;;flattens a binary tree to a list of its values in left-to-right order
(: tree-flatten : BINTREE -> (Listof Number))
(define (tree-flatten tree)
 (tree-fold (inst append Number) (inst list Number) tree))

;;tree-flatten tets:
(test (tree-flatten (Node (Node (Leaf 5) (Node (Leaf 7)(Leaf 9))) (Leaf -2))) => '(5 7 9 -2))
(test (tree-flatten (Node (Leaf -2) (Node (Node (Leaf 7)(Leaf 9)) (Leaf 5)))) => '(-2 7 9 5))
(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))) => '(1 2 3))
(test (tree-flatten (Node (Node (Leaf 2)(Leaf 3)) (Leaf 1))) => '(2 3 1))
(test (tree-flatten (Node (Leaf 0)(Leaf 3.2))) => '(0 3.2))
(test (tree-flatten (Leaf 0)) => '(0))

#|
tree-reverse function.
recive BINTREE as input,
return BINTREE - a tree that is the input tree mirror image.
|#
(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse tree)
 (tree-fold switch-nodes Leaf tree))

#|
switch-nodes function.
recive two BINTREES as input,
return BINTREE - a tree with switch nodes.
|#
(: switch-nodes : BINTREE BINTREE -> BINTREE)
(define (switch-nodes ftree stree)
 (Node stree ftree))

;;tree-reverse tests:
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))
              (tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))) => true)

(test (equal? (reverse (tree-flatten (Node (Leaf -2) (Node (Node (Leaf 7)(Leaf 9)) (Leaf 5)))))
              (tree-flatten (tree-reverse (Node (Leaf -2) (Node (Node (Leaf 7)(Leaf 9)) (Leaf 5)))))) => true)

(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))
              (tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))) => true)

(test (equal? (reverse (tree-flatten (Node (Node (Leaf 0)(Leaf -3)) (Leaf 1))))
              (tree-flatten (tree-reverse (Node (Node (Leaf 0)(Leaf -3)) (Leaf 1))))) => true)

(test (equal? (reverse (tree-flatten (Node (Leaf 0)(Leaf 3.2))))
              (tree-flatten (tree-reverse (Node (Leaf 0)(Leaf 3.2))))) => true)

(test (equal? (reverse (tree-flatten (Leaf 1)))
              (tree-flatten (tree-reverse (Leaf 1)))) => true)
