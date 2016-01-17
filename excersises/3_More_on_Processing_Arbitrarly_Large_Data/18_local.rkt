;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 18_locale) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp")))))
;; Exercise 18.1.12
;; Consider the following function definition:

;; maxi : non-empty-lon  ->  number
;; to determine the largest number on alon
(define (maxi alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else (cond
            [(> (first alon) (maxi (rest alon))) (first alon)]
            [else (maxi (rest alon))])]))

;; Both clauses in the nested cond-expression compute (maxi (rest an-inv)), 
;; which is therefore a natural candidate for a local-expression. Test both 
;; versions of maxi with

(define L (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 5))

(check-expect (maxi L) 20)

;; (lisof Number) -> Number
;; produces the largest Number of the given listof Number
;; Assmume: the list contains at least one Number.
(check-expect (maxi-v1 L) 20)

(define (maxi-v1 lon)
  (cond [(empty? (rest lon)) (first lon)]
        [else
         (local [(define n (maxi-v1 (rest lon)))]
           (if (> (first lon) n)
               (first lon)
               n))]))

;; Exercise 18.1.13
;; Develop the function to-blue-eyed-ancestor. The function consumes a family tree 
;; (ftn) (see section 14.1) and produces a list that explains how to get to a 
;; blue-eyed ancestor. If there is no blue-eyed ancestor, the function produces 
;; false.

;; A path is a list of 'father and 'mother, which we call a direction.
;; Here are the two data definitions:

;; A direction is either
;;  the symbol 'father or
;;  the symbol 'mother .
;; A path is either
;;  empty or
;; (cons s los) where s is a direction and los is a path.

;; The empty path indicates that a-ftn has 'blue in the eyes field. If the first 
;; item is 'mother, we may search in the mother's family tree for a blue-eyed 
;; ancestor using the rest of the path. Similarly, we search in the father's 
;; family tree if the first item is 'father and use the rest of the path for 
;; further directions.

;; Examples:
;; (to-blue-eyed-ancestor Gustav) produces (list 'mother) for the family tree 
;;in figure 35;
;; (to-blue-eyed-ancestor Adam) produces false in the same setting; and
;; if we added (define Hal (make-child Gustav Eva 'Gustav 1988 'hazel)) then 
;; (to-blue-eyed-ancestor Hal) would yield (list 'father 'mother).

;; Build test cases from these examples. Formulate them as boolean expressions, 
;; using the strategy of section 17.8. 

;; Kopie from 14.1
;; A Family-Tree-Node (FTN) is either 
;; - empty, or
;; - (make-child father mother name date eyes)

(define-struct child (father mother name date eyes))
;; A Child is a Structure (make-child f m n d e)
;; where f, m are ftn; and n, e are symbols; and d is a Natural

;; Oldest Generation:
(define Carl    (make-child empty empty 'Carl 1926 'green))
(define Bettina (make-child empty empty 'Bettina 1926 'green))

;; Middle Generation:
(define Adam    (make-child Carl  Bettina 'Adam 1950 'yellow))
(define Dave    (make-child Carl  Bettina 'Dave 1955 'black))
(define Eva     (make-child Carl  Bettina 'Eva  1960 'blue))
(define Fred    (make-child empty empty   'Fred 1966 'pink))

;; Youngest Generation:
(define Gustav  (make-child Fred   Eva   'Gustav 1988 'brown))
(define Linda   (make-child empty  empty 'Linda  1999 'green)) 
(define Hal     (make-child Gustav Linda 'Hal    2005 'hazel)) 

;; to-blue-eyed-ancestor : ftn  ->  path or false 
;; to compute the path from a-ftn tree to a blue-eyed ancestor
(check-expect (to-blue-eyed-ancestor Gustav) (list 'mother))
(check-expect (to-blue-eyed-ancestor Adam)   false)
(check-expect (to-blue-eyed-ancestor Hal)    (list 'father 'mother))

(define (to-blue-eyed-ancestor ftn) 
  (cond [(empty? ftn) false]
        [else
         (if (symbol=? 'blue (child-eyes ftn)) 
             empty
             (local [(define mother-blue (to-blue-eyed-ancestor (child-mother ftn)))
                     (define father-blue (to-blue-eyed-ancestor (child-father ftn)))]
               (cond [(and (false? mother-blue) (false? father-blue)) false]
                     [else
                      (if (not (false? mother-blue))
                          (cons 'mother mother-blue)
                          (cons 'father father-blue))])))]))

;; Exercise 18.1.14
;; Discuss the function find from exercise 15.3.4 in terms of backtracking. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kopie von Musterlösung

(define-struct wp (header body))

;; A Web-Page (WP) is a structure (make-wp h p); where
;; h is a Symbol and p is a Web-Document

;; A Web-Document (DOC) is either
;; 1. empty,
;; 2. (cons s p); where s is a Symbol and p a DOC
;; 3. (cons w p); where w is a WP and p is a DOC

(define WP1 (make-wp 'a-h empty))
(define WP2 (make-wp 'b-h (cons 'B-b empty)))
(define WP3 (make-wp 'c-h '(c-b1 c-b2)))
(define WP4 (make-wp 'd-h (list 'd-b1
                                (make-wp 'd1-h '(d1-b1 d1-b2)) 
                                'd-b2)))

(check-expect (find-v1 'a     WP1) false)
(check-expect (find-v1 'B-b   WP2) '(b-h))
(check-expect (find-v1 'a     WP2) false)
(check-expect (find-v1 'c-b2  WP3) '(c-h))
(check-expect (find-v1 'd1-b2 WP4) '(d-h d1-h))
(check-expect (find-v1 'd-b2  WP4) '(d-h))
(check-expect (find-v1 'x     WP4) false)

;; Symbol WP -> (listof Symbol) | false
(define (find-v1 s wp)
  (append/false (list (wp-header wp))
                (find-doc-v1 s (wp-body wp))))

;; DOC -> (listof Symbol) | false
(define (find-doc-v1 s d)
  (cond [(empty? d) false]
        [(symbol? (first d))
         (if (symbol=? s (first d))
             empty
             (find-doc-v1 s (rest d)))]
        [else
         (local [(define headers (find-v1 s (first d)))]
           (if (not (false? headers))
               headers
               (find-doc-v1 s (rest d))))]))

;; ((listof Symbol) | false) ((listof Symbol) | false) -> (listof Symbol) | false)
(define (append/false a b)
  (cond [(false? b) false]
        [else      (append a b)]))

;; Exercise 18.1.15
;; Consider the following function definition:

;; Inventory -> Inventory
;; to create an inventory from an-inv for all those items that cost less than $1
#|
(define (extract1 i)
  (cond [(empty? i) empty]
        [else
         (cond [(<= (ir-price (first i)) 1.00)
                (cons (first i) (extract1 (rest i)))]
               [else
                (extract1 (rest i))])]))
|#

;; Both clauses in the nested cond-expression extract the first item from an-inv and both compute 
;; (extract1 (rest an-inv)). Introduce a local-expression for these expressions


#|
(define (extract1 i)
  (cond [(empty? i) empty]
        [else
         (local [(define first1 (first i))
                 (define extrat1-rest (extract1 (rest i)))]
           ;--------------------------------------------
           (cond [(<= (ir-price first1) 1.00) (cons first1 extract1-rest)]
                 [else                        extract1-rest]))]))
|#