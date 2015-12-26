;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 11_Natural_Numbers) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")))))
;; Exercise 11.2.1
;; Generalize hellos to repeat, which consumes a natural number n and a 
;; symbol and produces a list with n occurrences of the symbol.

;;Number Symbol -> (listof Symbol)
(check-expect (repeat 0 'a) empty)
(check-expect (repeat 3 'a) (list 'a 'a 'a))

(define (repeat n s)
  (cond [(zero? n) empty]
        [else
         (cons s (repeat (sub1 n) s))]))

;; Exercise 11.2.2
;; Develop the function tabulate-f, which tabulates the values of

;; f : number  ->  number
(define (f x)
  (+ (* 3 (* x x)) 
     (+ (* -6 x)
        -1)))
;; for some natural numbers. Specifically, it consumes a natural number
;; n and produces a list of n posns. The first one combines n with (f n), 
;; the second one n-1 with (f n-1), etc.  

;; Number -> (listOf Posn)
(check-expect (tabulate-f 0) empty)
(check-expect (tabulate-f 2)
              (list (make-posn 2 (f 2))
                    (make-posn 1 (f 1))))

(define (tabulate-f n)
  (cond [(zero? n) empty]
        [else
         (cons (make-posn n (f n))
               (tabulate-f (sub1 n)))]))

;; Exercise 11.2.3
;; Develop apply-n. The function consumes a natural number, n. It applies 
;; the function move from exercise 10.3.6 n times to FACE, the list of 
;; shapes from exercise 10.3.1. Each application should translate the
;; shape by one pixel. The purpose of the function is to simulate a
;; continuously moving shape on a canvas, the last missing piece of the 
;; extended exercise 10.3.

;; Number -> Picture
(define (apply-n n)
  (cond [(zero? n) FACE]
        [else
         (move-picture 1 (apply-n (sub1 n)))]))

;; Imported

;; Exercise 10.3.1
;; Provide a data definition that describes the class of lists of shapes. 
;; The class of shapes was defined in exercise 7.4.1.

;; Data Definitions:

(define-struct circle (center radius color))
;; A circle is a structure:
;;          (make-circle p r c)
;;    where p is a posn, r a number, c is a Symbol repr. the color;

(define-struct square (nw length))
;; A square is a structure:
;;          (make-square p s)
;;    where p is a posn, s a number.

(define-struct rectangle (nw w l c))
;; A Rectangle is a structure:
;;   (make-rectangle p w l)
;; where p is a posn, w, l are Number representing the widht and length
;;       c is the color
;; of the rectangle.

;; A Shape is either
;; 1. a circle, or
;; 2. a square. 
;; 3. a rectangle

;; ListofShape is either
;; empty, or
;; (cons Shape ListofShape)


#|
Create a sample list that represents the face of figure 10.3.6 and name it 
FACE. Its basic dimensions are gathered in the following table:

shape	position	size(s)	color
circle	(50,50)	40	red
rectangle	(30,20)	5 × 5	blue
rectangle	(65,20)	5 × 5	blue
rectangle	(40,75)	20 × 10	red
rectangle	(45,35)	10 × 30	blue

The table assumes a canvas of size 300 by 100.
Develop the template fun-for-losh, which outlines functions that consume 
a list-of-shapes. 
|#

(define FACE (list (make-circle    (make-posn 50 50) 50    'red)
                   (make-rectangle (make-posn 30 20) 5 5   'blue)
                   (make-rectangle (make-posn 65 20) 5 5   'blue)
                   (make-rectangle (make-posn 40 75) 20 10 'blue)
                   (make-rectangle (make-posn 45 35) 10 30 'blue)))
#| Template
(define (fn-for-los los)
  (cond [(empty? los) ...]
        [else
         ... (fn-for-shape (first los))
         (fn-for-los (rest los))]))

(define (fn-for-shape s)
  (cond [(circle? s)    (fn-for-circle s)]
        [(square? s)    (fn-for-square s)]
        [(rectangle? s) (fn-for-rectangle s)]))
|#

;; Exercise 10.3.2
;; Use the template fun-for-losh to develop the function draw-losh.
;; It consumes a list-of-shapes, draws each item on the list, and returns 
;; true. Remember to use (start n m) to create the canvas before the 
;; function is used. 

(define (draw-losh los)
  (cond [(empty? los) true]
        [else
         (and (draw-shape (first los))
              (draw-losh (rest los)))]))

(define (draw-shape s)
  (cond [(circle? s)    (draw-a-circle s)]
        [(square? s)    (draw-a-rectangle s)]
        [(rectangle? s) (draw-a-rectangle s)]))

;; Rectangle -> Boolean
(define (draw-a-rectangle r)
  (draw-solid-rect (rectangle-nw r)
                   (rectangle-w  r)
                   (rectangle-l  r) 
                   (rectangle-c  r))) 

;; Circle -> Boolean
(define (draw-a-circle c)
  (draw-circle (circle-center c)
               (circle-radius c)
               (circle-color  c)))
#|
(start 300 300)
(draw-los FACE)
|#
;; Exercise 10.3.3
;; Use the template fun-for-losh to develop translate-losh. The function 
;; consumes a list-of-shapes and a number delta. The result is a list of 
;; shapes where each of them has been moved by delta pixels in the x 
;; direction. The function has no effect on the canvas.  

;; (listof Shape) Number -> (listof Shape)
(define (translate-losh los n)
  (cond [(empty? los) empty]
        [else
         (cons (translate-shape (first los) n)
               (translate-losh  (rest los)  n))]))

;; from 7_The_Varieties_of_Data_7_4.rkt

(define (translate-shape s n)
  (cond [(circle?    s) (translate-circle    s n)]
        [(rectangle? s) (translate-rectangle s n)]))


;; Rectangle Number -> Rectangle
(define (translate-rectangle r n)
  (make-rectangle (make-posn (+ n (posn-x (rectangle-nw r)))
                             (posn-y (rectangle-nw r)))
                  (rectangle-w r)
                  (rectangle-l r)
                  (rectangle-c r)))

;; Circle Number -> Circle
;; consumes a circle structure and a number delta. The result is a circle whose center 
;; is delta pixels to the right of the input
(define (translate-circle c delta)
  (make-circle (make-posn (+ (posn-x (circle-center c)) delta)
                          (posn-y (circle-center c)))
               (circle-radius c)
               (circle-color c)))


;; Exercise 10.3.4
;; Use the template fun-for-losh to develop clear-losh. The function 
;; consumes a list-of-shapes, erases each item on the list from the 
;; canvas, and returns true.

;; (listof Shape) -> Boolean
(define (clear-losh los)
  (cond [(empty? los) true]
        [else
         (and (clear-shape (first los))
              (clear-losh (rest los)))]))

;; from 7.4
;; Shape -> Boolean
;; consumes a shape, ereases it from the canvas, and returns true
(define (clear-shape s)
  (cond [(circle?    s) (clear-a-circle    s)]
        [(rectangle? s) (clear-a-rectangle s)]))

;; Circle -> Boolean
;; consumes a circle structure and clears the corresponding circle on the canvas.  
(define (clear-a-circle c)
  (clear-circle (circle-center c)
                (circle-radius c)))

;; Rectangle -> Boolean
(define (clear-a-rectangle r)
  (clear-solid-rect (rectangle-nw r)
                    (rectangle-w  r)
                    (rectangle-l  r))) 

#|
(start 300 300)
(clear-losh FACE)
|#

;; Exercise 10.3.5
;; Develop the function draw-and-clear-picture. It consumes a picture. 
;; Its effect is to draw the picture, sleep for a while, and to clear the 
;; picture.

;; Picture -> Boolean
(define (draw-and-clear-picture p)
  (and (draw-losh p)
       (sleep-for-a-while 1/25) 
       (clear-losh p)))

#|
(start 500 100)
(draw-and-clear-picture FACE)
|#

;; Exercise 10.3.6
;; Develop the function move-picture. It consumes a number (delta) and a
;; picture. It draws the picture, sleeps for a while, clears the picture
;; and then produces a translated version. The result should be moved by 
;; delta pixels.

;; Number Picture -> Picture
(define (move-picture n p)
  (cond [(draw-and-clear-picture p) (translate-picture n p)]))

(define (translate-picture n p)
  (translate-losh p n))

#|
|#

(start 500 100)
(apply-n 250)
(stop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 11.2.4
;; Lists may contain lists that contain lists and so on. Here is a data
;; definition that takes this idea to an extreme:
;; A deep-list is either
;;  s where s is some symbol or
;;  (cons dl empty), where dl is a deep list.

(define DL0 'a)
(define DL1 (cons DL0 empty))
(define DL2 (cons DL1 empty))
(define DL3 (cons DL2 empty))

;; 1.
;; Develop the function depth, which consumes a deep list and determines 
;; how many times cons was used to construct it.

;; DeepList -> Number
(check-expect (depth DL0) 0)
(check-expect (depth DL1) 1)
(check-expect (depth DL2) 2)
(check-expect (depth DL3) 3)

(define (depth dl)
  (cond [(symbol? dl) 0]
        [else
         (add1 (depth (first dl)))]))

;; 2.
;; Develop the function make-deep, which consumes a symbol s and a natural
;; number and produces a deep list containing s and constructed with n 
;; conses.

;; Symbol Number -> DeepList
(check-expect (depth (make-deep 'a 0)) 0)
(check-expect (depth (make-deep 'a 1)) 1)
(check-expect (depth (make-deep 'a 2)) 2)
(check-expect (make-deep 'a 0) DL0)
(check-expect (make-deep 'a 1) DL1)
(check-expect (make-deep 'a 2) DL2)
(check-expect (make-deep 'a 3) DL3)

(define (make-deep s n)
  (cond [(zero? n) s]
        [else
         (cons (make-deep s (sub1 n))
               empty)]))