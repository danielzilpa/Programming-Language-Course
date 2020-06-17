#lang pl 03

#|
Part a:
personal description of the solution:
In this part of the assignment we expanded the WAE language (renamed MUAWE) and added the action sqrt to it
In our implementation of sqrt we return two results (one positive and one negative)ץ
(A detailed explanation of each section appears alongside the code)

The main difficulty was to understand the expected result in each section
we solved it by asking other students.
We invest something like 6 hours to solving it.
|#

 #| BNF for the MUWAE language:
       <MUWAE> ::= <num>
               | { + <MUWAE> <MUWAE> }
               | { - <MUWAE> <MUWAE> }
               | { * <MUWAE> <MUWAE> }
               | { / <MUWAE> <MUWAE> }
               | { with { <id> <MUWAE> } <MUWAE> }
               | <id>
               | { sqrt <MUWAE> } ;;sqrt is working on one MUWAE
 |#

;; MUWAE abstract syntax trees
(define-type MUWAE
  [Num  (Listof Number)]
  [Add  MUWAE MUWAE]
  [Sub  MUWAE MUWAE]
  [Mul  MUWAE MUWAE]
  [Div  MUWAE MUWAE]
  [Id   Symbol]
  [With Symbol MUWAE MUWAE]
  [Sqrt MUWAE]) ;;sqrt is working on one MUWAE


;;In this function we changed the returned value to be a list becauese we added the sqrt action - this fuction must return 2 values
(: parse-sexpr : Sexpr -> MUWAE)
;; to convert s-expressions into MUWAE
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num (list n))]
    [(symbol: name) (Id name)]
    [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'sqrt hs) (Sqrt (parse-sexpr hs))] ;;if it is from this from (sqrt) - send to sqrt constructor the result from the parse-sexpr recursive call 
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse : String -> MUWAE)
  ;; parses a string containing a MUWAE expression to a MUWAE AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  #| Formal specs for `subst':
     (`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        {√ E}    [v/x]        = {√ E [v/x]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
  |#

(: subst : MUWAE Symbol MUWAE -> MUWAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
 (cases expr
   [(Num n) expr]
   [(Add l r) (Add (subst l from to) (subst r from to))]
   [(Sub l r) (Sub (subst l from to) (subst r from to))]
   [(Mul l r) (Mul (subst l from to) (subst r from to))]
   [(Div l r) (Div (subst l from to) (subst r from to))]
   [(Sqrt hs) (Sqrt (subst hs from to))] ;;if it is from this from (sqrt) - send to sqrt constructor the result from the subst recursive call
   [(Id name) (if (eq? name from) to expr)]
   [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]))

  #| Formal specs for `eval':
       eval(N)         = N
       eval({+ E1 E2}) = eval(E1) + eval(E2)
       eval({- E1 E2}) = eval(E1) - eval(E2)
       eval({* E1 E2}) = eval(E1) * eval(E2)
       eval({/ E1 E2}) = eval(E1) / eval(E2)
       eval({sqrt E})  = √ (eval (E))  
       eval(id)        = error!
       eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
  |#

;;In this function we send to bin-op the result of the operator on the results of the recursives calls
(: eval : MUWAE -> (Listof Number))
;; evaluates MUWAE expressions by reducing them to numbers
(define (eval expr)
 (cases expr
   [(Num n) n]
   [(Add l r) (bin-op + (eval l) (eval r))]
   [(Sub l r) (bin-op - (eval l) (eval r))]
   [(Mul l r) (bin-op * (eval l) (eval r))]
   [(Div l r) (bin-op / (eval l) (eval r))]
   [(Sqrt hs) (sqrt+ (eval hs))] ;;if it is from this from (sqrt) - send to sqrt the result from the eval recursive call 
   [(With bound-id named-expr bound-body)
      (eval (subst bound-body
                    bound-id
                    (Num (eval named-expr))))]
   [(Id name) (error 'eval "free identifier: ~s" name)]))


;; a version of `sqrt' that takes a list of numbers, and return a list
;; with twice the elements, holding the two roots of each of the inp
;;; throws an error if any input is negative.
(: sqrt+ : (Listof Number) -> (Listof Number))
(define (sqrt+ ns)
(cond
  [(null? ns) ns] ;;if the list is empty - return it
  [(< (first ns) 0) (error 'sqrt+ "`sqrt' requires a nonnegative input: ~s" ns)] ;;if the first element in the list is smaller than 0 - error
  [else
   (cond
     [(null? (rest ns)) (if (= (first ns) 0) (list 0 0) (list (sqrt(first ns)) (* -1 (sqrt(first ns)))))] ;;if we reached to the last element in the list - calc the sqrt of the number
     [else (append (list (sqrt(first ns)) (* -1 (sqrt(first ns)))) (sqrt+ (rest ns)))])])) ;;if it is not the last element - calc the sqrt of the number and send the rest of the list to this function 
   
(: run : String -> (Listof Number))
 ;; evaluate a MUWAE program contained in a string
(define (run str)
  (eval (parse str)))

;; applies a binary numeric function on all combinations of numbers from
;; the two input lists, and return the list of all of the results
(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number) -> (Listof Number))
;; applies a binary numeric function on all combinations of numbers from
;; the two input lists, and return the list of all of the results
(define (bin-op op ls rs)
(: helper : Number (Listof Number) -> (Listof Number))
(define (helper l rs)
(: f : Number -> Number)
(define (f input)
 (op l input)) ;;active the function op with l and input
 (map f rs)) ;;map activates the function f on all elements of the list rs
 (if (null? ls) null (append (helper (first ls) rs) (bin-op op (rest ls) rs)))) 

;;tests part A:
(test (run "5") => '(5))  
(test (run "{+ 5 5}") => '(10))
(test (run "{with {x {+ 5 5}} {+ x x}}") => '(20))
(test (run "{with {x 5} {+ x x}}") => '(10))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
(test (run "{with {x 5} {with {y x} y}}") => '(5))
(test (run "{with {x 5} {with {x x} x}}") => '(5))
(test (run "{with {x 5} {* x {with {y 3} x}}}") => '(25))
(test (run "{with {x 5} {/ x {with {y 3} x}}}") => '(1))
(test (run "{sqrt 25}") => '(5 -5))
(test (run "{sqrt -25}")  =error> "`sqrt' requires a nonnegative input")
(test (run "{with {x 1} y}") =error> "free identifier")
(test (run "{sqrt 9}") => '(3 -3))
(test (run "{sqrt 1}") => '(1 -1))
(test (run "{sqrt 0}") => '(0 0))
(test (run "{sqrt -1}") =error> "`sqrt' requires a nonnegative input")
(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))
(test (run "{with {x 5} { x {with {y 3} x}}}")  =error> "bad syntax in")
(test (run "{with { 5} {+ x {with {y 3} x}}}")  =error> "bad `with' syntax in")
(test (run "{with {x {+ 20 5}} {sqrt x}}") => '(5 -5))
(test (run "{with {x {sqrt 36}} {+ x {with {x 3} 10}}}") => '(16 4))
(test (run "{with {x 4} {sqrt x}}") => '(2 -2))
(test (run "{with {x 6} {with {y {sqrt 49}} {+ y y}}}") => '(14 0 0 -14))
(test (run "{+ {with {x 6} {+ x x}} {with {y {sqrt 49}} {+ y y}}}") => '(26 12 12 -2))

#|
Part b:
personal description of the solution:
We wrote the function freeInstanceList, it consumes an abstract syntax tree (WAE) and returns null if there are no free
instance, and a list of all the free instances otherwise.
To solve this problem, we used Tom's #6 practice
We invest something like 1 hour to solving it.
|#

#|
<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>

|#

(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE])


(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#


(: substW : WAE Symbol WAE -> WAE)
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
           (substW named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substW bound-body from to)))]))

;;This function recieves WAE and returns list of Symbol
(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
  (cases expr
      [(NumW n) '()] ;;if it NumW - return empty list
      [(AddW l r) (append (freeInstanceList l) (freeInstanceList r))] ;;if it AddW form - append the freeInstanceList recursive call results of l and r 
      [(SubW l r) (append (freeInstanceList l) (freeInstanceList r))] ;;if it SubW form - append the freeInstanceList recursive call results of l and r
      [(MulW l r) (append (freeInstanceList l) (freeInstanceList r))] ;;if it MulW form - append the freeInstanceList recursive call results of l and r
      [(DivW l r) (append (freeInstanceList l) (freeInstanceList r))] ;;if it DivW form - append the freeInstanceList recursive call results of l and r
      [(WithW bound-id named-expr bound-body) ;;if if WithW form 
       (append (freeInstanceList named-expr) ;;check if there are free instance in name-expr
           (freeInstanceList (substW bound-body bound-id (NumW 0))))] ;;set 0 (random number) and check if there are free instance
      [(IdW name) (list name)] ;;return the symbol
    ))


;;tests part B:
(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (WithW 'x (NumW 2) (MulW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 2) (DivW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{with {z 4} {with {z 7} {- z {* z z}}}}")) => '())















