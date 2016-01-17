;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 17_7_Exetended_Exercise_Evaluating_Scheme) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp")))))
;; Exercise 17.7.1
;; Extend the data definition of exercise 14.4.1 so that we can represent
;; the application of a user-defined function to an expression such as 
;; (f (+ 1 1)) or (* 3 (g 2)). The application should be represented as a 
;; structure with two fields. The first field contains the name of the 
;; function, the second one the representation of the argument expression.  

(define-struct add (left right))
(define-struct mul (left right))
(define-struct app (name param))

;; A Scheme-Expression (SE) is either
;;  a Number; representing a Number
;;  a Symbol: representing a Variable
;;  (make-add l r); representing (+ l r); where l,r are SE
;;  (make-mul l r); representing (* l r); where l,r are SE
;;  (make-app n p); representing (f a); where f is a Symbol, a is SE

;; Exercise 17.7.2
;; Provide a structure definition and a data definition for definitions. 
;; Recall that a function definition has three essential attributes:
;; 1. the function's name,
;; 2. the parameter name, and
;; 3. the function's body.

;; This suggests the introduction of a structure with three fields. 
;; The first two contain symbols, the last one a representation of the 
;; function's body, which is an expression.

(define-struct def (name arg body))
;; (make-def n p b); is a SE representing (define (f a) b)); where
;; f is a Symbol representing the function's name
;; a is a Symbol representing the function's arg name
;; b is a SE representing the function's body

;; A Scheme-Expression (SE) is either
;;  a Number; representing a Number
;;  a Symbol: representing a Variable
;;  (make-add l r); representing (+ l r); where l,r are SE
;;  (make-mul l r); representing (* l r); where l,r are SE
;;  (make-app n a); representing (f e); where f is a Symbol, e is a SE
;;  (make-def n p b); is a SE representing (define (f a) b)); where
;;    f is a Symbol representing the function's name
;;    a is a Symbol representing the function's parameter name
;;    b is a SE representing the function's body

#|
Examples:
1. (define (f x) (+ 3 x))
   (make-def 'f 'x (make-add 3 'x))
2. (define (g x) (* 3 x))
   (make-def 'g 'x (make-mul 3 'x))
3. (define (h u) (f (* 2 u)))
(make-def 'h 'u (make-app 
                 'f 
                 (make-mul 2 'u)))
4. (define (i v) (+ (* v v) (* v v)))
(make-def 'i 'v (make-add
                 (make-mul 'v 'v)
                 (make-mul 'v 'v)))
5. (define (k w) (* (h w) (i w)))
(make-def 'k 'w (make-mul (make-app 'h 'w)
                          (make-app 'i 'w)))
|#

;; Exercise 17.7.3
;; Develop evaluate-with-one-def. The function consumes (the 
;; representation of) a Scheme expression and (the representation of) a 
;; single function definition, P.
;; The remaining expressions from exercise 14.4.1 are evaluated as 
;; before. For (the representation of) a variable, the function signals
;; an error. For an application of the function P, evaluate-with-one-def
;; 1. evaluates the argument;
;; 2. substitutes the value of the argument for the function parameter in 
;;    the function's body; and 
;; 3. evaluates the new expression via recursion.
;; Here is a sketch:
;; (evaluate-with-one-def (subst ... ... ...) 
;;		       a-fun-def)
;; For all other function applications, evaluate-with-one-def signals an 
;; error. 

(define (numeric? se)
  (cond [(number? se) true]
        [(symbol? se) false]
        [(add?    se) (and (numeric? (add-left se)) 
                           (numeric? (add-right se)))]
        [(mul?    se) (and (numeric? (mul-left se)) 
                           (numeric? (mul-right se)))]
        [(app?    se) false]))

(define APP1 (make-app 'f 1))
(define DEF1 (make-def 'f 'x (make-add 'x 'x)))

(define APP2 (make-app 'f (make-mul 'x 1)))

(check-expect (subst (def-arg DEF1) (app-param APP1) (def-body DEF1))
              (make-add 1 1))

(check-expect (subst 'x 1 APP2)
              (make-app 'f (make-mul 1 1)))

(define (subst v n se)
  (cond [(number? se) se]
        [(symbol? se) (if (symbol=? v se)
                          n
                          se)]
        [(add?    se) (make-add (subst v n (add-left  se))
                                (subst v n (add-right se)))]
        [(mul?    se) (make-mul (subst v n (mul-left  se))
                                (subst v n (mul-right se)))]
        [(app?    se) (make-app (app-name se)
                                (subst v n (app-param se)))]))

(check-error (evaluate-with-one-def 
              (make-app 'h 1)
              (make-def 'f 'x (make-add 'x 'x)))
             "evaluate-with-one-def: function not defined.")

(check-expect (evaluate-with-one-def 
               (make-app 'f 1)
               (make-def 'f 'x (make-add 'x 'x)))
              (+ 1 1))

(define (evaluate-with-one-def se fd)
  (cond [(number? se) se]
        [(symbol? se) 
         (error evaluate-with-one-def "variables not supported yet.")]
        [(add?    se) (+ (evaluate-with-one-def (add-left  se) fd)
                         (evaluate-with-one-def (add-right se) fd))]
        [(mul?    se) (* (evaluate-with-one-def (mul-left  se) fd)
                         (evaluate-with-one-def (mul-right se) fd))]
        [(app?    se)
         (if (symbol=? (app-name se) (def-name fd))
             (evaluate-with-one-def
              (subst (def-arg   fd)
                     (app-param se)
                     (def-body  fd))
              fd)
             (error 'evaluate-with-one-def "function not defined."))]))

;; Exercise 17.7.4
;; Develop the function evaluate-with-defs. The function consumes (the 
;; representation of) a Scheme expression and a list of (representations 
;; of) function definitions, defs. The function produces the number that 
;; DrScheme would produce if we were to evaluate the actual Scheme 
;; expression in the Interactions window and if the Definitions window 
;; contained the actual definitions.
;; The remaining expressions from exercise 14.4.1 are evaluated as before.
;; For an application of the function P, evaluate-with-defs
;; 1. evaluates the argument;
;; 2. looks up the definition of P in defs;
;; 3. substitutes the value of the argument for the function parameter in
;;    the function's body; and
;; 4. evaluates the new expression via recursion.
;; Like DrScheme, evaluate-with-defs signals an error for a function 
;; application whose function name is not on the list and for (the
;; representation of) a variable. 

(define DEFS
  (list (make-def 'circle-area 'r (make-mul 3.14
                                            (make-mul 'r 'r)))
        (make-def 'add5 'n (make-add 'n 5))))

(check-expect (evaluate-with-defs 
               (make-app 'circle-area 
                         10) 
               DEFS)
              (* 3.14 (* 10 10)))

(check-expect (evaluate-with-defs 
               (make-app 'circle-area 
                         (make-add 5 5)) 
               DEFS)
              (* 3.14 (* 10 10)))

(check-expect (evaluate-with-defs 
               (make-app 'circle-area 
                         (make-app 'add5 5)) 
               DEFS)
              (* 3.14 (* 10 10)))

(define (evaluate-with-defs se lodef)
  (local [(define (lookup s lodef)
            (cond [(empty? lodef)
                   (error 'evaluate-with-defs "function not defined.")]
                  [(symbol=? s (def-name (first lodef))) (first lodef)]
                  [else 
                   (lookup s (rest lodef))]))
          (define (evaluate se lodef)
            (cond [(number? se) se]
                  [(symbol? se) 
                   (error evaluate-with-one-def "variables not supported yet.")]
                  [(add?    se) (+ (evaluate-with-one-def (add-left  se) lodef)
                                   (evaluate-with-one-def (add-right se) lodef))]
                  [(mul?    se) (* (evaluate-with-one-def (mul-left  se) lodef)
                                   (evaluate-with-one-def (mul-right se) lodef))]
                  [(app?    se)
                   (local [(define def (lookup (app-name se) lodef))]
                     (evaluate-with-defs
                      (subst (def-arg  def)
                             (evaluate-with-defs (app-param se) lodef)
                             (def-body  def))
                      lodef))]))]
    
    (evaluate se lodef)))

#|
Kopie von 14.4.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 14.4 Extended Exercise: Evaluating Scheme

;; Exercise 14.4.1
;; Provide a data definition for the representation of Scheme expressions.
;; Then translate the following expressions into representations:

;; A Scheme-Expression (SE) is either
;;  a Number; representing a Number
;;  a Symbol: representing a Variable
;;  (make-add l r); representing (+ l r); where l,r are SE
;;  (make-mul l r); representing (* l r); where l,r are SE

;;(+ 10 -10)
(make-add 10 -10)

;;(+ (* 20 3) 33)
(make-add (make-mul 20 30) 33)

;;(* 3.14 (* r r))
(make-mul 3.14 (make-mul 'r 'r))

;;(+ (* 9/5 c) 32)
(make-add (make-mul 9/5 'c) 32)

;;(+ (* 3.14 (* o o)) (* 3.14 (* i i)))
(make-add (make-mul 3.14 (make-mul 'o 'o)) 
          (make-mul 3.14 (make-mul 'i 'i)))

;; Exercise 14.4.2
;; Develop the function numeric?, which consumes (the representation of)
;; a Scheme expression and determines whether it is numeric. 

;; SE -> Boolean
;; consumes a SE; produces true if it is numeric.
(check-expect (numeric? 1) true)
(check-expect (numeric? (make-add 1 0)) true)
(check-expect (numeric? (make-add 1 (make-mul 1 1))) true)
(check-expect (numeric? (make-add (make-add 1 1) 0)) true)
(check-expect (numeric? 'i) false)
(check-expect (numeric? (make-add 1 'i)) false)
(check-expect (numeric? (make-add 'i 1)) false)
(check-expect (numeric? (make-add 1 (make-mul 1 'i))) false)
(check-expect (numeric? (make-add (make-add 'i 1) 0)) false)

(define (numeric? se)
  (cond [(number? se) true]
        [(symbol? se) false]
        [(add?    se) (and (numeric? (add-left se)) 
                           (numeric? (add-right se)))]
        [(mul?    se) (and (numeric? (mul-left se)) 
                           (numeric? (mul-right se)))]))

;; Exercise 14.4.3
;; Provide a data definition for numeric expressions. 
;; Develop the function evaluate-expression. The function consumes 
;; (the representation of) a numeric Scheme expression and computes its 
;; value. When the function is tested, modify it so it consumes all kinds
;; of Scheme expressions; the revised version raises an error when it
;; encounters a variable

;; A Numeric-Expression (NE) is either
;; 1. Number, or
;; 2. (make-add l r); where l,r are NE, or
;; 3. (make-mul l r); where l,r are NE

;; NE -> Number
;; consumes a NE and computes its value
(check-expect (evaluate-expression 0) 0)
(check-expect (evaluate-expression (make-add 1 2)) 3)
(check-expect (evaluate-expression (make-mul 2 3)) 6)
(check-expect (evaluate-expression 
               (make-add (make-mul 2 3)
                         (make-add 3 1)))
              (+ (* 2 3)
                 (+ 3 1)))
;(check-expect (evaluate-expression (make-add 1 'i)) 
;              "evaluate-expression: variables not supported yet.")

(define (evaluate-expression ne)
  (cond [(number? ne) ne]
        [(symbol? ne) 
         (error 'evaluate-expression "variables not supported yet.")]
        [(add?    ne) (+ (evaluate-expression (add-left ne))
                         (evaluate-expression (add-right ne)))]
        [(mul?    ne) (* (evaluate-expression (mul-left ne))
                         (evaluate-expression (mul-right ne)))]))

;; Exercise 14.4.4
;; When people evaluate an application (f a) they substitute a for f's
;; parameter in f's body. More generally, when people evaluate expressions
;; with variables, they substitute the variables with values.
;; Develop the function subst. The function consumes (the representation 
;; of) a variable (V), a number (N), and (the representation of) a Scheme
;; expression. It produces a structurally equivalent expression in which 
;; all occurrences of V are substituted by N.

;; Symbol Number SE -> SE
;; consumes (the representation of) a variable (V), a number (N), and 
;; (the representation of) a Scheme expression. 
;; produces a structurally equivalent expression in which all occurrences 
;; of V are substituted by N.
(check-expect (subst 'i 5 (make-add (make-add 'i (make-mul 'i 'i)) 3))
              (make-add (make-add 5 (make-mul 5 5)) 3))
(check-expect (subst 'i 5 (make-add (make-add 'i (make-mul 'x 'i)) 3))
              (make-add (make-add 5 (make-mul 'x 5)) 3))
(check-expect (evaluate-expression 
               (subst 'i 5 
                      (make-add (make-add 'i (make-mul 'i 'i)) 3)))
              (evaluate-expression
               (make-add (make-add 5 (make-mul 5 5)) 3)))

(define (subst v n se)
  (cond [(number? se) se]
        [(symbol? se) (if (symbol=? v se)
                          n
                          se)]
        [(add?    se) (make-add (subst v n (add-left  se))
                                (subst v n (add-right se)))]
        [(mul?    se) (make-mul (subst v n (mul-left  se))
                                (subst v n (mul-right se)))]))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 17.8 Equality and Testing

;; (listof Number) (listof Number) -> Boolean
;; consumes two lists; produces true iff both contains the same items.
(check-expect (list=? empty empty) true) 
(check-expect (list=? empty (cons 1 empty)) false)
(check-expect (list=? (cons 1 empty) empty) false)
(check-expect (list=? (cons 1 (cons 2 (cons 3 empty))) 
                      (cons 1 (cons 2 (cons 3 empty))))
              true)
(check-expect (list=? (cons 1 (cons 2 (cons 3 empty)))
                      (cons 1 (cons 3 empty)))
              false)

(define (list=? lona lonb)
  (cond [(empty? lona) (empty? lonb)]
        [else
         (and (cons? lonb)
              (= (first lona) (first lonb))
              (list=? (rest lona) (rest lonb)))]))

;; Exercise 17.8.4
;; Develop contains-same-numbers. The function determines whether two 
;; lists of numbers contain the same numbers, regardless of the ordering.
;; Thus, for example,

(check-expect (contains-same-numbers (list 1 2 3) (list 3 2 1)) true)
(check-expect (contains-same-numbers (list 1 2) (list 3 2 1)) false)
(check-expect (contains-same-numbers (list 1 2 3) (list 3 2)) false)
(check-expect (contains-same-numbers (list 1 2 3) '()) false)
(check-expect (contains-same-numbers '() (list 3 2 1)) false)

;; (listof Number) (listof Number) -> Boolean
;; consumes two lists; produces true iff both contain the same numbers.
(define (contains-same-numbers a b)
  (and (all-in? a b)
       (all-in? b a)))

;; (listof Number) (listof Number) -> Boolean
;; consumes two lists; produces true iff all items in a are in b.
(define (all-in? a b)
  (cond [(empty? a) true]
        [else
         (and (one-in? (first a) b)
              (all-in? (rest a) b))]))

;; Number (listof Number) -> Boolean
;; consumes a Number and a list of Number; produces true iff the given
;; Number is member of the list.
(define (one-in? n lon)
  (cond [(empty? lon) false]
        [else
         (or (= n (first lon))
             (one-in? n (rest lon)))]))

;; Exercise 17.8.5
;; The class of numbers, symbols, and booleans are sometimes called atoms

;; An atom is either
;; a number
;; a boolean
;; a symbol

;; Develop the function list-equal?, which consumes two lists of atoms 
;; and determines whether they are equal.
(check-expect (list-equal? '() '()) true)
(check-expect (list-equal? '(1 2) '(1 2)) true)
(check-expect (list-equal? '(1 2) '(2 1)) false)
(check-expect (list-equal? '(1 2 3) '(2 1)) false)
(check-expect (list-equal? '(1 2) '(2 1 1)) false)
(check-expect (list-equal? '(a b) '(a b)) true)
(check-expect (list-equal? '(a b c) '(a b)) false)
(check-expect (list-equal? '(a b) '(a b b)) false)
(check-expect (list-equal? (list true false) (list true false)) true)
(check-expect (list-equal? (list false true) (list true false)) false)
(check-expect (list-equal? (list true false) (list false true)) false)
(check-expect (list-equal? '(1 2) '(1 a)) false)

(define (list-equal? a b)
  (cond [(empty? a) (empty? b)]
        [else
         (and (cons? b)
              (atom=? (first a) (first b))
              (list-equal? (rest a) (rest b)))]))

(define (atom=? a b)
  (cond [(number?  a) (and (number?  b) (=         a b))]
        [(boolean? a) (and (boolean? b) (boolean=? a b))]
        [(symbol?  a) (and (symbol?  b) (symbol=?  a b))]))

#|
(define (list-equal? a b)
  (and (all-atoms-in a b)
       (all-atoms-in b a)))

(define (all-atoms-in a b)
  (cond [(empty? a) true]
        [else
         (and (atom-in (first a) b)
              (all-atoms-in (rest a) b))]))

(define (atom-in a loa)
  (cond [(empty? loa) false]
        [else
         (or (equal-atom a (first loa))
             (atom-in a (rest loa)))]))

(define (equal-atom a b)
  (cond [(number? a) (and (number?  b) (= a b))]
        [(boolean? a) (and (boolean? b) (= a b))]
        [(symbol? a) (and (symbol?  b) (symbol=? a b))]))
|#

;; A Web-Page (WP) is either
;; 1. empty
;; 2. (cons s wp)  ; where s is Symbol and wp is WP; or
;; 3. (cons ewp wp); where both are WP

;; Web-Page Web-Page -> Boolean
;; determine whether the two pages have the same shape and contain the
;; same symbols in the same order.
(define (web=? a b)
  (cond [(empty? a) (empty? b)]
        [(symbol? (first a))
         (and (cons?    b)
              (symbol?  (first b))
              (symbol=? (first a) (first b))
              (web=? (rest a) (rest b)))]
        [else
         (and (cons? b)
              (cons? (first b))
              (web=? (first a) (first b))
              (web=? (rest a) (rest b)))]))

;; Exercise 17.8.6
;; Draw the table based on the data definition for simple Web pages. 
;; Develop (at least) one example for each of the nine cases. 
;; Test web=? with these examples.  

#|
                 | a
                 | empty  | (cons s wp)                         | (cons ewp wp)
--------------------------------------------------------------------------------------------------
b  empty         | 1      | 2                                   | 3
                 | true   | false                               | false
   -----------------------------------------------------------------------------------------------
   (cons s wp)   | 4      | 5                                   | 6
                 | false  | (and (symbol=? (first a)            | false
                 |        |        (symbol=? (first b)          |
                 |        |        (web=?    (rest a) (rest b)) |
   -----------------------------------------------------------------------------------------------
   (cons ewp wp) | 7      | 8                                   | 9
                 | false  | false                               | (and (web=? (first a) (first b))
                 |        |                                     |      (web=? (rest  a) (rest  b)))

|#

;Examples as Tests:
;Combination numbers are listed in the table above.
;combination 1
(check-expect (web=? empty empty)
              true)

;combination 2
(check-expect (web=? (list 'Hello 'World) empty)
              false)

;combination 3
(check-expect
 (web=? (list (list 'Goodbye)) empty)
 false)

;combination 4
(check-expect
 (web=? empty (list 'Homework))
 false)

;combination 5
(check-expect
 (web=? (list 'Hello 'World) (list 'Homework))
 false)

;combination 5
(check-expect
 (web=? (list 'Homework) (list 'Homework))
 true)

;combination 6
(check-expect
 (web=? (list (list 'Goodbye)) (list 'Homework))
 false)

;combination 7
(check-expect
 (web=? empty (list (list 'Solutions)))
 false)

;combination 8
(check-expect
 (web=? (list 'Hello 'World) (list (list 'Solutions)))
 false)

;combination 9
(check-expect
 (web=? (list (list 'Solutions)) (list (list 'Solutions)))
 true)

;combination 9
(check-expect
 (web=? (list (list 'Goodbye)) (list (list 'Solutions)))
 false)

;; Exercise 17.8.7
;; Develop the function posn=?, which consumes two binary posn structures and 
;; determines whether they are equal.

;; Posn Posn -> Boolean
;; consumes two binary posn structures and determines whether they are equal.
(check-expect (posn=? (make-posn 1 1) (make-posn 1 1)) true)
(check-expect (posn=? (make-posn 1 1) (make-posn 1 2)) false)
(check-expect (posn=? (make-posn 1 1) (make-posn 2 1)) false)
(check-expect (posn=? (make-posn 1 2) (make-posn 1 1)) false)
(check-expect (posn=? (make-posn 2 1) (make-posn 1 1)) false)
(check-expect (posn=? (make-posn 2 1)               1) false)
(check-expect (posn=? 1               (make-posn 1 1)) false)
(check-expect (posn=? 1                             1) false)

(define (posn=? a b)
  (and (posn? a) (posn? b)
       (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))

;; Exercise 17.8.8
;; Develop the function tree=?, which consumes two binary trees and determines 
;; whether they are equal. 

;; A BTree (Binary Tree) is either
;;  false; representing empty node or leaf; or
;,  (make-node i l r); where i is Number, l,r are BTree

(define-struct node (item left right))

;; BTree BTree -> Boolean
(define (tree=? a b)
  (cond [(false? a) (false? b)]
        [else
         (and (node? b)
              (=      (node-item  a) (node-item  b))
              (tree=? (node-left  a) (node-left  b))
              (tree=? (node-right a) (node-right b)))]))

;Examples as Tests:
(check-expect
 (tree=? false false)
 true)

(check-expect
 (tree=? (make-node 12 
                    false
                    false)
         (make-node 12 
                    false
                    false))
 true)

(check-expect
 (tree=? false false)
 true)

(check-expect
 (tree=? (make-node 12 
                    false
                    false)
         (make-node 18 
                    false
                    false))
 false)

(check-expect
 (tree=?
  (make-node 12 
             (make-node 35
                        false
                        false)
             (make-node 65
                        (make-node 89
                                   false
                                   false)
                        false))
  (make-node 12
             (make-node 35
                        false
                        false)
             (make-node 65
                        (make-node 89
                                   false
                                   false)
                        false))) 
 true)

(check-expect
 (tree=?
  (make-node 12 
             (make-node 35
                        false
                        false)
             (make-node 65
                        (make-node 89
                                   false
                                   false)
                        false))
  (make-node 12
             (make-node 35
                        false
                        false)
             (make-node 65
                        (make-node 89
                                   (make-node 7
                                              false
                                              false)
                                   false)
                        false))) 
 false)

;; Exercise 17.8.9
;; Consider the following two, mutually recursive data definitions:
;; A Slist is either
;;  empty
;;  (cons s sl) where s is a Sexpr and sl is a Slist.
;;
;; A Sexpr is either
;;  a number
;;  a boolean
;;  a symbol
;;  a Slist
;; Develop the function Slist=?, which consumes two Slists and determines whether
;; they are equal. Like lists of numbers, two Slists are equal if they contain 
;; the same item at analogous positions. 

;Examples as Tests:
(check-expect
 (slist=?
  empty
  empty)
 true)

(check-expect
 (slist=?
  empty
  (list 'w 6 6 false))
 false)

(check-expect
 (slist=?
  (list 'w 6 6 false)
  empty)
 false)

(check-expect
 (slist=?
  (list 'w 6 6 false)
  (list 'w 6 6 false))
 true)

(check-expect
 (slist=?
  (list 'w 6 6 false)
  (list 'w 6 false))
 false)

(check-expect
 (slist=?
  (list 'w 6 6 false (list true 'a 7 'b))
  (list 'w 6 6 false (list true 'a 7 'b)))
 true)

(check-expect
 (slist=?
  (list 'w 6 6 false (list true 'a 7 'b))
  (list 'w 6 (list true 'a 7 'b) 6 false ))
 false)

(check-expect
 (slist=?
  (list 'w (list 5) 6 false)
  (list 'w (list 5) 6 false))
 true)

(define (slist=? a b)
  (cond [(empty? a) (empty? b)]
        [else
         (and (cons? b)
              (sexpr=? (first a) (first b))
              (slist=? (rest  a) (rest  b)))]))

;;sexpr=?: Sexpr Sexpr -> boolean
;;consumes two Sexprs and returns true
;;if they are equal, false otherwise
;Examples as Tests:
(check-expect
 (sexpr=? 5 5)
 true)

(check-expect
 (sexpr=? 5 8)
 false)

(check-expect
 (sexpr=? 5 true)
 false)

(check-expect
 (sexpr=? 5 'hello)
 false)

(check-expect
 (sexpr=? false false)
 true)

(check-expect
 (sexpr=? false 8)
 false)

(check-expect
 (sexpr=? false 'hello)
 false)

(check-expect
 (sexpr=? false 8)
 false)

(check-expect
 (sexpr=? 'hello 'hello)
 true)

(check-expect
 (sexpr=? 'hello 8)
 false)

(check-expect
 (sexpr=? 'hello true)
 false)

(check-expect
 (sexpr=? 'hello 'goodbye)
 false)

(define (sexpr=? a b)
  (cond [(number? a)  (and (number? b)
                           (= a b))]
        [(boolean? a) (and (boolean? b)
                           (boolean=? a b))]
        [(symbol? a)  (and (symbol? b)
                           (symbol=? a b))]
        [else
         (and (or (empty? b) (cons? b))
              (slist=? a b))]))

;; Exercise 17.8.10
;; Define a test function for replace-eol-with from section 17.1 using equal? and
;; formulate the examples as test cases using this function. 

(define (test-replace-eol-with a b e)
  (equal? (replace-eol-with a b) e))

;; replace-eol-with : list-of-numbers list-of-numbers  ->  list-of-numbers
;; to construct a new list by replacing empty in alon1 with alon2
(check-expect (test-replace-eol-with '()  '()    '())      true)
(check-expect (test-replace-eol-with '()  '(1 2) '(1 2))   true)
(check-expect (test-replace-eol-with '(a) '(1 2) '(a 1 2)) true)

(define (replace-eol-with alon1 alon2)
  (cond [(empty? alon1) alon2]
        [else
         (cons (first alon1)
               (replace-eol-with (rest alon1) alon2))]))

;; Exercise 17.8.11
;; Define the function test-list-pick, which manages test cases for the list-pick
;; function from section 17.3. Formulate the examples from the section as test 
;; cases using test-list-pick.

;; Kopie von 17.3:
;; (listof Symbol) N[>= 1] -> Symbol | false
;; produces the nth Symbol; sginals an error if not found.

(define (list-pick los n)
  (cond
    [(and (= n 1) (empty? los)) false]
    [(and (= n 1) (cons?  los)) (first los)]
    [(and (> n 1) (empty? los)) false]
    [(and (> n 1) (cons?  los)) (list-pick (rest los) (sub1 n))]))

(define (test-list-pick lox n e)
  (if (not (equal? (list-pick lox n) e))
      (error 'test-list-pick "missmatch")
      true))

(test-list-pick empty 1     false)
(test-list-pick (list 'a) 1 'a)
(test-list-pick empty 3     false)
(test-list-pick (list 'a) 3 false)

;; Exercise 17.8.12
;; Define test-interpret, which tests interpret-with-defs from exercise 17.7.4, 
;; using equal?. Reformulate the test cases using this function.

#|
Kopie von 17.7.4
(define DEFS
  (list (make-def 'circle-area 'r (make-mul 3.14
                                            (make-mul 'r 'r)))
        (make-def 'add5 'n (make-add 'n 5))))

(check-expect (evaluate-with-defs 
               (make-app 'circle-area 
                         10) 
               DEFS)
              (* 3.14 (* 10 10)))

(check-expect (evaluate-with-defs 
               (make-app 'circle-area 
                         (make-add 5 5)) 
               DEFS)
              (* 3.14 (* 10 10)))

(check-expect (evaluate-with-defs 
               (make-app 'circle-area 
                         (make-app 'add5 5)) 
               DEFS)
              (* 3.14 (* 10 10)))
|#

(define (test-evaluate-with-defs se lod ese)
  (if (not (equal? (evaluate-with-defs se lod) ese))
      "test-evaluate-with-defs: evaluation faild."
      "Test passed."))

(test-evaluate-with-defs (make-app 'circle-area 10) 
                         DEFS
                         (* 3.14 (* 10 10)))

(test-evaluate-with-defs (make-app 'circle-area 
                                   (make-add 5 5)) 
                          DEFS
                         (* 3.14 (* 10 10)))

(test-evaluate-with-defs (make-app 'circle-area 
                                   (make-app 'add5 5)) 
                          DEFS
                          (* 3.14 (* 10 10)))