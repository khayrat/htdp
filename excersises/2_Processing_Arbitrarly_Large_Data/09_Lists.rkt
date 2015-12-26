;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 9_Lists) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp")))))
;; Exercise 9.1.4
;; Provide a data definition for lists of two symbols. Then develop the function
;; contains-2-doll?, which consumes a list of two symbols and determines whether one 
;; of them is 'doll

(define L1 (cons 'a
                 (cons 'b empty)))

(define L2 (cons 'a
                 (cons 'doll empty)))

(define L3 (cons 'doll
                 (cons 'doll empty)))

(check-expect (contains-2-doll? L1) false)
(check-expect (contains-2-doll? L2) false)
(check-expect (contains-2-doll? L3) true)

(define (contains-2-doll? los)
  (and (symbol=? 'doll
                 (first los))
       (symbol=? 'doll
                 (first (rest los)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct pair (left right))

(define (my-first lox)
  (pair-left lox))

(define (my-rest lox)
  (pair-right lox))

(define (my-cons? lox)
  (pair? lox))

(define (my-cons x lox)
  (cond [(empty? lox)   (make-pair x lox)]
        [(my-cons? lox) (make-pair x lox)]
        [else (error 'my-cons "list as second argument expected.")]))

(define ML1 (my-cons 1
                     (my-cons 2
                              empty)))
(check-expect (my-first ML1) 1)
(check-expect (my-first (my-rest ML1)) 2)
(check-expect (my-cons? ML1) true)
(check-expect (empty? (my-rest (my-rest ML1))) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 9.5.2
;; Develop the function how-many-symbols, which consumes a list of symbols and produces 
;; the number of items in the list.
;; Develop the function how-many-numbers, which counts how many numbers are in a list 
;; of numbers. How do how-many-symbols and how-many-numbers differ?

;; (listof Symbol) -> Number
;; consumes a list of symbols and produces the number of items in the list.
(check-expect (how-many-symbols empty) 0)
(check-expect (how-many-symbols (cons 'a empty)) 1)
(check-expect (how-many-symbols (cons 'a (cons 'b empty))) 2)

(define (how-many-symbols los)
  (cond [(empty? los) 0]
        [else
         (+ 1 (how-many-symbols (rest los)))]))

;; (listf Number) -> Number
;; consumes a list of numbers and produces the number of items in the list.
(check-expect (how-many-numbers empty) 0)
(check-expect (how-many-numbers (cons 2 empty)) 1)
(check-expect (how-many-numbers (cons 2 (cons 3 empty))) 2)

(define (how-many-numbers los)
  (cond [(empty? los) 0]
        [else
         (+ 1 (how-many-numbers (rest los)))]))

#|
The only difference is the data they process and the function names.
|#

;; Exercise 9.5.4
;; Develop the function check-range1?, which consumes a list of 
;; temperature measurements (represented as numbers) and checks whether
;; all measurements are between 5oC and 95oC.

;; (listOf Number) -> Boolean
(check-expect (check-range1? empty) true)
(check-expect (check-range1? (list 1)) false)
(check-expect (check-range1? (list 5 10 95)) true)
(check-expect (check-range1? (list 5 10 96)) false)
(check-expect (check-range1? (list 4 10 95)) false)

(define (check-range1? lon)
  (cond [(empty? lon) true]
        [else
         (and (<= 5  (first lon))
              (>= 95 (first lon))
              (check-range1? (rest lon)))]))

;; Generalize the function to check-range?, which consumes a list of 
;; temperature measurements and a legal interval and checks whether all 
;; measurements are within the legal interval. 

;; (listOf Number) Number Number -> Boolean
;; consumes a list of temperature measurements and a legal interval
;; produces true, if all measurements are within the legal interval. 
(check-expect (check-range? 5 95 empty) true)
(check-expect (check-range? 5 95 (list 1)) false)
(check-expect (check-range? 5 95 (list 5 10 95)) true)
(check-expect (check-range? 5 95 (list 5 10 96)) false)
(check-expect (check-range? 5 95 (list 4 10 95)) false)

(define (check-range? l h lon)
  (cond [(empty? lon) true]
        [else
         (and (<= l  (first lon))
              (>= h (first lon))
              (check-range? l h (rest lon)))]))

;; Exercise 9.5.3
;; Develop the function dollar-store?, which consumes a list of prices (numbers) and 
;; checks whether all of the prices are below 1.
;; (listof Number) Number -> Boolean
(check-expect (dollar-store? empty 1) true)
(check-expect (not (dollar-store? (cons .75 (cons 1.95 (cons .25 empty)))
                                  1)) 
              true)
(check-expect (dollar-store? (cons .15 (cons .05 (cons .25 empty)))
                             1) 
              true)
(check-expect (dollar-store? (cons .15 (cons 1.05 (cons .25 empty)))
                             1) 
              false)

(define (dollar-store? lod n)
  (cond [(empty? lod) true]
        [else
         (cond [(>= (first lod) n) false]
               [else (dollar-store? (rest lod) n)])]))

;; Generalize the function so that it consumes a list of prices (numbers) 
;; and a threshold price (number) and checks that all prices in the list 
;; are below the threshold.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 9.5.5
;; Develop the function convert. It consumes a list of digits and produces the 
;; corresponding number. The first digit is the least significant, and so on.

;; (listof Number) -> Number
;; consumes a list of digits and produces the corresponding number. 
;; The first digit is the least significant
(check-expect (convert empty) 0)
(check-expect (convert (cons 0 empty)) 0)
(check-expect (convert (cons 1 empty)) 
              (+ 1
                 (* 10 0)))
(check-expect (convert (cons 3 (cons 4 empty))) 
              (+ 3
                 (+ (* 10  4)
                    (* 100 0))))
(check-expect (convert (cons 3 (cons 4 (cons 6 empty)))) 
              (+ 3
                 (+ (* 10  4)
                    (+ (* 100  6)
                       (* 1000 0)))))

(define (convert lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (* 10 (convert (rest lon))))]))

;; Also develop the function check-guess-for-list. It implements a general version of 
;; the number-guessing game of exercise 5.1.3. The function consumes a list of digits,
;; which represents the player's guess, and a number, which represents the randomly 
;; chosen and hidden number. Depending on how the converted digits relate to target,
;; check-guess-for-list produces one of the following three answers: 
;; 'TooSmall, 'Perfect, or 'TooLarge.

;; (listof Number) Number -> Symbol
;; consumes a list of digits and a number, produces one of the following three answers: 
;; 'TooSmall, 'Perfect, or 'TooLarge.
(check-expect (check-guess-for-list (cons 5 empty) 5) 'Perfect)
(check-expect (check-guess-for-list (cons 5 empty) 4) 'TooLarge)
(check-expect (check-guess-for-list (cons 5 empty) 6) 'TooSmall)
(check-expect (check-guess-for-list (cons 5 (cons 3 empty)) 35) 'Perfect)

(define (check-guess-for-list lon n)
  (cond [(< (convert lon) n) 'TooSmall]
        [(> (convert lon) n) 'TooLarge]
        [else                'Perfect]))

;; The rest of the game is implemented by guess.ss. To play the game, use the
;; teachpack to guess.ss and evaluate the expression

;; (guess-with-gui-list 5 check-guess-for-list)
;; after the functions have been thoroughly developed. 

;(guess-with-gui-list 5 check-guess-for-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 9.5.6
;; Develop the function delta, which consumes two price lists, that is, lists of numbers.
;; The first represents the inventory at the beginning of a time period, the second one
;; the inventory at the end. The function outputs the difference in value. If the value of
;; the inventory has increased, the result is positive; if the value has decreased, 
;; it is negative.

;; (lisof Number) (listof Number) -> Number
;; consumes two price lists, that is, lists of numbers. outputs the difference in value.
;; Assumes: Both list have the same length.
(check-expect (delta empty empty) 0)
(check-expect (delta (cons 1 empty) (cons 1 empty))  0)
(check-expect (delta (cons 1 empty) (cons 0 empty)) -1)
(check-expect (delta (cons 0 empty) (cons 1 empty))  1)
(check-expect (delta (cons 1 (cons 5 empty)) (cons 2 (cons 6 empty))) 2)

(define (delta lon-a lon-b)
  (cond [(empty? lon-a) 0]
        [else
         (+ (- (first lon-b) (first lon-a))
            (delta (rest lon-a) (rest lon-b)))]))

;; Exercise 9.5.7
;; Define the function average-price. It consumes a list of toy prices and computes 
;; the average price of a toy. The average is the total of all prices divided by the
;; number of toys.

;; (listof Number) -> Number
;; consumes a list of toy prices and computes the average price of a toy
;; Assumes the list is not empty.
(check-expect (average (cons 1 empty)) 1)
(check-expect (average (cons 2 (cons 6 empty))) (/ (+ 2 6) 2))

(define (average lon)
  (/ (sum lon)
     (count lon)))

;; (listof Numer) -> Number
;; consumes a list of Numbers, produces the sum of them
(check-expect (sum empty) 0)
(check-expect (sum (cons 1 empty)) 1)
(check-expect (sum (cons 1 (cons 10 empty))) 11)

(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))

;; (listof X) -> Number
;; consumes a list of items; produces the number of items.
(check-expect (count empty) 0)
(check-expect (count (cons 1 empty)) 1)
(check-expect (count (cons 1 (cons 10 empty))) 2)

(define (count lox)
  (cond [(empty? lox) 0]
        [else
         (add1 (count (rest lox)))]))

;; Iterative Refinement: First develop a function that works on non-empty lists. 
;; Then produce a checked function (see section 7.5) that signals an error when the 
;; function is applied to an empty list.

;; (listof Number) -> Number
;; consumes a list of toy prices and computes the average price of a toy
(check-expect (average-v1 (cons 1 empty)) 1)
(check-expect (average-v1 (cons 2 (cons 6 empty))) (/ (+ 2 6) 2))
;(check-expect (average-v1 empty) "avarage-v1: emtpy list!")

(define (average-v1 lon)
  (cond [(empty? lon) (error 'avarage-v1 "emtpy list!")]
        [else
         (/ (sum lon)
            (count lon))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 9.5.8
;; Develop the function draw-circles, which consumes a posn p and a list of numbers. 
;; Each number of the list represents the radius of some circle. The function draws 
;; concentric red circles around p on a canvas, using the operation draw-circle. 
;; Its result is true, if it can draw all of them; otherwise an error has occurred and 
;; the function does not need to produce a value.

;; Posn (listof Number) -> Boolean
;; consumes a posn p and a list of numbers. Each number represents the radius of a circle.
;; draws concentric red circles around p on a canvas

(define (draw-circles p lon)
  (cond [(empty? lon) true]
        [else (and (draw-circle  p (first lon) 'red)
                   (draw-circles p (rest lon)))]))

;; Use the teachpack draw.ss; create the canvas with (start 300 300). Recall that 
;; draw.ss provides the structure definition for posn (see section 7.1).  

(start 300 300)
(draw-circles (make-posn 150 150)
              (cons 10 
                    (cons 30 
                          (cons 50 
                                (cons 70 
                                      (cons 90 
                                            (cons 110 
                                                  (cons 130 empty))))))))