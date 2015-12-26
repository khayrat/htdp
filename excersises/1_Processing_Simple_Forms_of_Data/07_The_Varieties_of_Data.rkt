;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 7_The_Varieties_of_Data) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp")))))
;; Exercise 7.2.1
;; Develop structure and data definitions for a collection of zoo animals. 
;; The collection includes
;; * spiders, whose relevant attributes are the number of remaining legs 
;;   (we assume that spiders can lose legs in accidents) and the space they need in 
;;   case of transport;
;; * elephants, whose only attributes are the space they need in case of transport;
;; * monkeys,  whose attributes are intelligence and space needed for transportation.

;; Then develop a template for functions that consume zoo animals.
;; Develop the function fits?. The function consumes a zoo animal and the volume of 
;; a cage. It determines whether the cage is large enough for the animal.

;; Data definitions
(define-struct spider (legs space))
;; Spider has legs and requires space

(define-struct elephant (space))
;; Elephant requires space

(define-struct monkey (iq space))
;; Monkey has iq and requires space

(define-struct animal (a))
;; Animal is on of
;; * spider, elephant, monkey

;; Spider -> ???
;; Template for Spider
(define (fn-for-spider s)
  (... (spider-legs  s) 
       (spider-space s)))

;; Elephant -> ???
;; Template for Elephant
(define (fn-for-elephant e)
  (... (elephant-space e)))

;; Monkey -> ???
;; Template for Monkey
(define (fn-for-monkey)
  (... (monkey-iq   m)
       (monkey-space m)))

;; Animal -> ???
;; Template for Animal
(define (fn-for-animal a)
  (cond [(spider? a)   (fn-for-spider   a)]
        [(elephant? a) (fn-for-elephant a)]
        [(monkey? a)   (fn-for-monkey   a)]))

;; Animal Number -> Boolean
;; fits?. The function consumes a zoo animal and the volume of 
;; a cage. It determines whether the cage is large enough for the animal.
(check-expect (fit? (make-spider 8 2) 1) false)
(check-expect (fit? (make-spider 8 2) 2) true)
(check-expect (fit? (make-elephant 10) 9) false)
(check-expect (fit? (make-elephant 10) 10) true)
(check-expect (fit? (make-monkey 100 5) 4) false)
(check-expect (fit? (make-monkey 100 5) 5) true)

(define (fit? a v)
  (cond [(spider? a)   (spider-fit?   a v)]
        [(elephant? a) (elephant-fit? a v)]
        [(monkey? a)   (monkey-fit?   a v)]))

;; Spider Number -> Boolean
;; consumes a Spider and Number (interp. as Volume); determines whether Spider fits in 
;; given Volume.
(define (spider-fit? s v)
  (>= v (spider-space s)))

;; Elephant Number -> Boolean
;; consumes a Elephant and Number (interp. as Volume); determines whether Elephant fits in 
;; given Volume.
(define (elephant-fit? e v)
  (>= v (elephant-space e)))

;; Monkey Number -> Boolean
;; consumes a Monkey and Number (interp. as Volume); determines whether Monkey fits in 
;; given Volume.
(define (monkey-fit? m v)
  (>= v (monkey-space m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 7.3.1
;; Modify the two versions of perimeter so that they also process rectangles. 
;; For our purposes, the description of a rectangle includes its upper-left corner, 
;; its width, and its height

;; Data Definitions:
(define-struct circle (center radius))
;; A circle is a structure:
;;          (make-circle p s)
;;    where p is a posn, s a number;

(define-struct square (nw length))
;; A square is a structure:
;;          (make-square p s)
;;    where p is a posn, s a number.

(define-struct rectangle (nw w l))
;; A Rectangle is a structure:
;;   (make-rectangle p w l)
;; where p is a posn, w, l are Number representing the widht and length
;; of the rectangle.

;; A shape is either
;; 1. a circle, or
;; 2. a square. 
;; 3. a rectangle

;; Final Definitions: 
;; perimeter : shape  ->  number
;; to compute the perimeter of a-shape
(check-within (perimeter (make-circle (make-posn 100 100) 10))
              (* 2 pi 10)
              0.00001)

(check-expect (perimeter (make-square (make-posn 100 100) 10))
              (* 4 10))

(check-expect (perimeter (make-rectangle (make-posn 100 100) 10 20))
              (* 2 (+ 10 20)))
 
(define (perimeter a-shape)
  (cond
    [(circle? a-shape)
     (perimeter-circle a-shape)]
    [(square? a-shape)
     (perimeter-square a-shape)]
     [(rectangle? a-shape)
     (perimeter-rectangle a-shape)]))

;; perimeter-circle : circle  ->  number
;; to compute the perimeter of a-circle
(define (perimeter-circle a-circle)
  (* (* 2 (circle-radius a-circle)) pi))

;; perimeter-square : square  ->  number
;; to compute the perimeter of a-square
(define (perimeter-square a-square)
  (* (square-length a-square) 4))

;; perimeter-rectangle : rectangle  ->  number
;; to compute the perimeter of a-rectangle
(define (perimeter-rectangle a-rectangle)
  (* 2 ( + (rectangle-w a-rectangle)
           (rectangle-l a-rectangle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 7.5.3
;; Take a look at these structure and data definitions:

(define-struct vec (x y))
;; A speed-vector (vec) is a structure: 
;;   (make-vec x y)  
;; where both x and y are positive numbers.

;; Develop the function checked-make-vec, which should be understood as a 
;; checked version of the primitive operation make-vec. It ensures that the arguments
;; to make-vec are positive numbers, and not just arbitrary numbers. In other words,
;; checked-make-vec enforces our informal data definition. 

;; Number Number -> Vec | error
(check-expect (checked-make-vec 0 0) 
              (error 'checked-make-vec "x,y should be positivie numbers"))
(check-expect (checked-make-vec 0 1) 
              (error 'checked-make-vec "x,y should be positivie numbers"))
(check-expect (checked-make-vec 1 0) 
              (error 'checked-make-vec "x,y should be positivie numbers"))
(check-expect (checked-make-vec 1 1) 
              (make-vec 1 1))

(define (checked-make-vec x y)
  (cond [(and (> x 0) (> y 0)) (make-vec x y)]
        [else (error 'checked-make-vec "x,y should be positivie numbers")]))

(check-expect (checked-make-vec 0 0) 
              (error 'checked-make-vec "x,y should be positivie numbers"))
