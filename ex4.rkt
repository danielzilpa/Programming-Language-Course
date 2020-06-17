#|
In this task we expand the FLANG language to also treat conditionals. We added if expressions,
binary operators <, >, =, the unary operator (Boolean to Boolean) not, and the Boolean values True and False
The assignment solution took us something like 7 hours.
The main difficulty was understanding how to correctly write the new grammar conditions and how to address them in all the functions we
were asked to complete.
To resolve this issue, we wrote the code according to how we thought it was correct, ran the tests and fixed the errors.
|#

;; The Flang interpreter

#lang pl 04

#| The grammar:
<FLANG> ::= <num> ;; Rule 1
| { + <FLANG> <FLANG> } ;; Rule 2
| { - <FLANG> <FLANG> } ;; Rule 3
| { * <FLANG> <FLANG> } ;; Rule 4
| { / <FLANG> <FLANG> } ;; Rule 5
| { with { <id> <FLANG> } <FLANG> } ;; Rule 6
| <id> ;; Rule 7
| { fun { <id> } <FLANG> } ;; Rule 8
| { call <FLANG> <FLANG> } ;; Rule 9
|  True  ;; add rule for True ;; Rule 10
|  False  ;; Rule 11
| { = <FLANG> <FLANG> } ;; add rule for = ;; Rule 12
| { > <FLANG> <FLANG> } ;; Rule 13
| { < <FLANG> <FLANG> } ;; Rule 14
| { not <FLANG> }— ;; Rule 15
| { if {<FLANG>} {then-do <FLANG>} {else-do <FLANG>}} ;; add rule 16 for (the above) if
expressions


  Evaluation rules:

    subst:
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {call E1 E2}[v/x]     = {call E1[v/x] E2[v/x]}
      {fun {y} E}[v/x]      = {fun {y} E[v/x]}           ; if y =/= x
      {fun {x} E}[v/x]      = {fun {x} E}

     B[v/x] = B ;; B is Boolean
     {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
     {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
     {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
     { not E}[v/x] = {not E[v/x]}
     {if Econd {then-do Edo} {else-do Eelse}}[v/x] = {if Econd[v/x] {then-do Edo[v/x]} {else-do Eelse[v/x]}}

    eval:
      eval(N)            = N
      eval({+ E1 E2})    = eval(E1) + eval(E2)  \ if both E1 and E2
      eval({- E1 E2})    = eval(E1) - eval(E2)   \ evaluate to numbers
      eval({* E1 E2})    = eval(E1) * eval(E2)   / otherwise error!
      eval({/ E1 E2})    = eval(E1) / eval(E2)  /
      eval(id)           = error!
      eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
      eval(FUN)          = FUN ; assuming FUN is a function expression
      eval({call E1 E2}) = eval(Ef[eval(E2)/x]) if eval(E1)={fun {x}Ef}
                         = error!               otherwise
  |#

  (define-type FLANG
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG]
    ;;we've added new variants to the language:
    [Bool Boolean] 
    [Bigger FLANG FLANG]
    [Smaller FLANG FLANG]
    [Equal FLANG FLANG]
    [Not FLANG]
    [If FLANG FLANG FLANG])

  (: parse-sexpr : Sexpr -> FLANG)
  ;; to convert s-expressions into FLANGs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n) (Num n)]
      ['True (Bool true)] ;;if it from the form True - send it to Bool variant
      ['False (Bool false)] ;;if it from the form False - send it to Bool variant
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name)) body)
          (Fun name (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
      [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))] ;;if it from the form = lhs rhs - send to Equal variant the recursive call from parse-sexpr on lhs and rhs 
      [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))] ;;if it from the form > lhs rhs - send to Bigger variant the recursive call from parse-sexpr on lhs and rhs 
      [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))] ;;if it from the form < lhs rhs - send to Smaller variant the recursive call from parse-sexpr on lhs and rhs 
      [(list 'not exp) (Not (parse-sexpr exp))] ;;if it from the form not exp - send to Not variant the recursive call from parse-sexpr on exp
      [(cons 'if more) ;;if it from the form if :
          (match more
            [(list hs (list 'then-do yes) (list 'else-do no)) ;;if it from the form list - hs is the if condition , yes - what to do if the condition is true , no - else 
            (If (parse-sexpr hs) (parse-sexpr yes) (parse-sexpr no))]
            [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])] ;;error in if
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

          
  (: parse : String -> FLANG)
  ;; parses a string containing a FLANG expression to a FLANG AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  (: subst : FLANG Symbol FLANG -> FLANG)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Fun bound-id bound-body)
       (if (eq? bound-id from) 
         expr 
         (Fun bound-id (subst bound-body from to)))]
      [(Bool b) expr] ;;it it bool - return the expr 
      [(Equal l r) (Equal (subst l from to) (subst r from to))] ;;if it from the form = lhs rhs - send to Equal variant the recursive call from subst on lhs and rhs
      [(Bigger l r) (Bigger (subst l from to) (subst r from to))] ;;if it from the form > lhs rhs - send to Bigger variant the recursive call from subst on lhs and rhs
      [(Smaller l r) (Smaller (subst l from to) (subst r from to))] ;;if it from the form < lhs rhs - send to Smaller variant the recursive call from subst on lhs and rhs
      [(Not x) (Not(subst x from to))] ;;if it from the form Not x - send to Not variant the recursive call from subst on x
      [(If cond yes no)(If (subst cond from to) (subst yes from to) (subst no from to))])) ;;if it from the form if - send to If variant the recursive call from subst on yes and no

;; The following function is used in multiple places below,
;; hence, it is now a top-level definition
(: Num->number : FLANG -> Number)
;; gets a FLANG -- presumably a Num variant -- and returns the
;; unwrapped number
(define (Num->number e)
(cases e
[(Num n) n]
[else (error 'Num->number "expected a number, got: ~s" e)]))

(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
(Num (op (Num->number expr1) (Num->number expr2))))

(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; gets a Racket Boolean binary operator (on numbers), and applies it
;; to two `Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
(Bool (op (Num->number expr1) (Num->number expr2)))) ;;return boolean - op on the number of expr1 and expr2

(: flang->bool : FLANG -> Boolean)
;; gets a Flang E (of any kind) and returns a its appropiate
;; Boolean value -- which is true if and only if E does not
;; represent false
;; Remark: the `flang->bool` function will also be top-level
;; since it's used in more than one place.
(define (flang->bool e)
(cases e
[(Bool x) (if (equal? x true) true false)] ;;check if x = true - if yes - return true, otherwise - false
[else true])) ;;else return false


  (: eval : FLANG -> FLANG)
  ;; evaluates FLANG expressions by reducing them to *expressions*
  (define (eval expr)
    (cases expr
      [(Num n) expr]
      [(Add l r) (arith-op + (eval l) (eval r))]
      [(Sub l r) (arith-op - (eval l) (eval r))]
      [(Mul l r) (arith-op * (eval l) (eval r))]
      [(Div l r) (arith-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (eval named-expr)))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Fun bound-id bound-body) expr]
      [(Call fun-expr arg-expr)
       (let([fval (eval fun-expr)])
         (cases fval
           [(Fun bound-id bound-body)
            (eval (subst bound-body
                         bound-id
                         (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s" fval)]))]
       [(Bool b) expr] ;;it it bool - return the expr 
       [(Bigger l r) (logic-op > (eval l) (eval r))] ;;if it from the form > l r - send to Bigger variant the recursive call from eval on l and r
       [(Smaller l r) (logic-op < (eval l) (eval r))] ;;if it from the form < l r - send to Smaller variant the recursive call from eval on l and r
       [(Equal l r) (logic-op = (eval l) (eval r))]  ;;if it from the form > l r - send to Equal variant the recursive call from eval on l and r
       [(If l m r) ;;if it from the from If - it have 3 elements
         (let ([x (eval l)]) ;;l = condotion so x will be true or false
           (if (flang->bool x) (eval m) (eval r)))] ;;if x = true - m , else - r
       [(Not exp) (Bool(not(flang->bool(eval exp))))] ;;if it from the form Not - return the boolean from pl not on exp
       ))


  (: run : String -> (U Number Boolean FLANG))
  ;; evaluate a FLANG program contained in a string
  (define (run str)
    (let ([result (eval (parse str))])
      (cases result
        [(Num n) n] ;;it it Num - return if
        [(Bool x) x] ;;it it Bool - return if 
        [else result]))) ;;else return result



  ;; tests
  (test (run "{call {fun {x} {+ x 1}} 4}")
        => 5)
  (test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
        => 4)
    (test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
        => 7)

   (test (run "True") => true)
   (test (run "{not True}") => false)
   (test (run "{> 3 44}") => false)
   (test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
   (test (run "{with {x 8}
                {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
   (test (run "{with {x 0}
               {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
   (test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
   (test (run "{with {c True}
             {if c {then-do {> 2 1}} {else-do 2}}}")
             => true)
   (test (run "{with {foo {fun {x}
       {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
          => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
   (test (run "{with {x 0}
         {if {> x 0} {/ 2 x} x}}")
               =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
   (test (run "true") =error> "eval: free identifier: true")
   (test (run "{< false 5}") =error> "eval: free identifier: false")
   (test (run "{< False 5}")
           =error> "Num->number: expected a number, got: #(struct:Bool #f)")

   (test (run "{with {x}
                {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") =error> "bad `with' syntax in (with (x) (if (> x 0) (then-do (/ 2 x)) (else-do x)))")

   (test (run "{with {foo {fun {}
       {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
          =error> "bad `fun' syntax in (fun () (if (< x 2) (then-do x) (else-do (/ x 2))))")
  (test (run "{call {fun {x} {* x 1}} 4}")
        => 4)
  (test (run "{call {fun {x} {= x 1}} 4}")
        => false)
  (test (run "{>  44}") =error> "bad syntax in (> 44)")
  (test (run "{with {add3 {fun {x} {- x 3}}}
                {call add3 1}}")
        => -2)
   (test (run "{with {x 0}
               {if {< x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
   (test (run "{call {fun {x}
    {if {not{not x}} {then-do False} {else-do True}}} True}")
     => false)
   (test (run "{with {sub1 {fun {y} {- y 2}}} {with {sub1 {fun {y} {* y 3}}} {with {y 4}  {call y {call add2 y}}}}}")=error>
          "eval: `call' expects a function, got: #(struct:Num 4)")
