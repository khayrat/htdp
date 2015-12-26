;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 10_3_Extended_Exercise) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")))))
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
       (sleep-for-a-while 1/2) 
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

;; Picture Number -> Picture
(define (move-picture n p)
  (cond [(draw-and-clear-picture p) (translate-picture n p)]))

(define (translate-picture n p)
  (translate-losh p n))

#|
Test the function with expressions like these:
|#

#|

(start 500 100)

(draw-losh
 (move-picture 10 
               (move-picture 10 
                             (move-picture 10
                                           (move-picture 10 FACE)))))

(stop)
|#

#|
This moves FACE (see exercise 10.3.1) by 10, 23, and -5 pixels in the x
direction. 

When the function is fully tested, use the teachpack arrow.ss and evaluate
the expression:


(start 500 100)
(control-left-right FACE 100 move-picture draw-losh)

|#

#|
The last one creates a graphical user interface that permits users to move
the shape FACE by clicking on arrows. The shape then moves in increments 
of 100 (right) and -100 (left) pixels. The teachpack also provides arrow
controls for other directions. Use them to develop other moving pictures.
|#