;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 7_The_Varieties_of_Data_7_4) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp")))))
;; from 6_Structures.rkt:

;; Data Defintion

;; A Circle is a structure: (make-circle center radius color)
;; where center is a Posn   represending the center of the circle
;;       radius is a Number representing the radius of the circle
;;       color  is a Symbor representing the color of the circle
(define-struct circle (center radius color))

;; Examples
(define C1 (make-circle (make-posn 100 100) 40 'red))

;; Develop the template fun-for-circle, which outlines a function that consumes 
;; circles. Its result is undetermined. 

#|
;; Template
;; Circle -> ???
(define (fn-for-circle c)
  ... (circle-center c) ...
  ... (circle-radius c) ...
  ... (circle-color c)  ...)
|#

;; Circle -> Boolean
(define (draw-a-circle c)
  (draw-circle (circle-center c)
               (circle-radius c)
               (circle-color  c)))

;; Circle Number -> Circle
;; consumes a circle structure and a number delta. The result is a circle whose center 
;; is delta pixels to the right of the input
(define (translate-circle c delta)
  (make-circle (make-posn (+ (posn-x (circle-center c)) delta)
                          (posn-y (circle-center c)))
               (circle-radius c)
               (circle-color c)))

;; Circle -> Boolean
;; consumes a circle structure and clears the corresponding circle on the canvas.  
(define (clear-a-circle c)
  (clear-circle (circle-center c)
                (circle-radius c)))

;; Circle -> Boolean
(define (draw-and-clear-circle c)
  (and (draw-a-circle c)
       (sleep-for-a-while 1/2)
       (clear-a-circle c)))

;; move-circle : number number circle  ->  circle
;; to draw and clear a circle, translate it by delta pixels
(define (move-circle delta a-circle)
  (cond
    [(draw-and-clear-circle a-circle) (translate-circle a-circle delta)]
    [else a-circle]))

;; Data definition
(define-struct rectangle (ul-corner width height color))
;; A rectangle is characterized by four pieces of information: 
;; its upper-left corner, its width, its height, and its fill color.
;; The first is a posn structure, the second and third quantities are plain numbers, 
;; and the last one is a color.

;; Rectangle -> ???
;; Template for Rectangle.
(define (fn-for-rectangle r)
  (... (rectangle-ul-corner r)
       (rectangle-widht     r)
       (rectangle-height    r)
       (rectangle-color     r)))

;; Rectangle -> Boolean
(define (draw-a-rectangle r)
  (draw-solid-rect (rectangle-ul-corner r)
                   (rectangle-width     r)
                   (rectangle-height    r) 
                   (rectangle-color     r))) 

;; Rectangle Number -> Rectangle
(define (translate-rectangle r n)
  (make-rectangle (make-posn       (+ n (posn-x (rectangle-ul-corner r)))
                                   (posn-y (rectangle-ul-corner r)))
                  (rectangle-width  r)
                  (rectangle-height r)
                  (rectangle-color  r)))

;; Rectangle -> Boolean
(define (clear-a-rectangle r)
  (clear-solid-rect (rectangle-ul-corner r)
                    (rectangle-width     r)
                    (rectangle-height    r))) 

;; move-rectangle : number rectangle  ->  rectangle
;; to draw and clear a rectangle, translate it by delta pixels
(define (move-rectangle delta a-rectangle)
  (cond
    [(draw-and-clear-rectangle a-rectangle) 
     (translate-rectangle a-rectangle delta)]
    [else a-rectangle]))

;; Rectangle -> Boolean
(define (draw-and-clear-rectangle r)
  (and (draw-a-rectangle r)
       (sleep-for-a-while 1/2)       
       (clear-a-rectangle r)))


;; Exercise 7.4.1
;; Provide a data definition for a general class of shapes. The class should at least 
;; subsume the classes of colored circles and rectangles from section 6.6.

;; A Shape is s is either
;; a Circle or Rectangle

;; Develop the template fun-for-shape, which outlines functions that consume shapes. 

;; Shape -> ???
;; Template for Shape
#|
(define (fn-for-shape s)
  (cond [(circle?    s) (fn-for-circle    s)]
        [(rectangle? s) (fn-for-rectangle s)]))
|#

;; Exercise 7.4.2
;; Use the template fun-for-shape to develop draw-shape. The function consumes a shape 
;; and draws it on the canvas. 

;; Shape -> Boolean
;; consumes a Shape and draw it on the canvas

(define (draw-shape s)
  (cond [(circle?    s) (draw-a-circle    s)]
        [(rectangle? s) (draw-a-rectangle s)]))
#|
(start 300 300)
(draw-shape (make-circle    (make-posn 100 100) 30 'red))
(draw-shape (make-rectangle (make-posn 100 100) 30 50 'red))
|#

;; Exercise 7.4.3
;; Use the template fun-for-shape to develop translate-shape. The function consumes a 
;; shape and a number delta, and produces a shape whose key position is moved by delta 
;; pixels in the x direction.

;; Shape Number -> Shape
;; consumes a shape and a number delta, and produces a shape whose key position is moved
;; by delta 
;; pixels in the x direction.
(check-expect (translate-shape (make-circle (make-posn 100 100) 20 'red) 110)
              (make-circle (make-posn (+ 100 110) 100) 20 'red))
(check-expect (translate-shape (make-rectangle (make-posn 100 100) 20 50 'red) 60)
              (make-rectangle (make-posn (+ 100 60) 100) 20 50 'red))

(define (translate-shape s n)
  (cond [(circle?    s) (translate-circle    s n)]
        [(rectangle? s) (translate-rectangle s n)]))

;; Exercise 7.4.4
;; Use the template fun-for-shape to develop clear-shape. The function consumes a shape,
;; erases it from the canvas, and returns true.

;; Shape -> Boolean
;; consumes a shape, ereases it from the canvas, and returns true
(define (clear-shape s)
  (cond [(circle?    s) (clear-a-circle    s)]
        [(rectangle? s) (clear-a-rectangle s)]))
#|
(start 300 300)
(draw-shape (make-circle    (make-posn 100 100) 30 'red))
(draw-shape (make-rectangle (make-posn 100 100) 30 50 'red))
(clear-shape (make-circle    (make-posn 100 100) 30 'red))
(clear-shape (make-rectangle (make-posn 100 100) 30 50 'red))
|#

;; Exercise 7.4.5
;; Develop the function draw-and-clear-shape. The function consumes a shape, draws it, 
;; sleeps for a while, and clears it. If all the effects work out, it produces true

;; Shape -> Boolean
(define (draw-and-clear-shape s)
  (cond [(circle?    s) (draw-and-clear-circle    s)]
        [(rectangle? s) (draw-and-clear-rectangle s)]))

#|
(start 300 300)
(draw-and-clear-shape (make-circle    (make-posn 100 100) 30 'red))
(draw-and-clear-shape (make-rectangle (make-posn 100 100) 30 50 'red))
|#

;; Exercise 7.4.6
;; Develop move-shape, which moves a shape across the canvas. The function consumes
;; a number (delta) and a shape. The function should draw-and-clear the shape and 
;; return a new shape that has been translated by delta pixels. Use this function 
;; several times to move a shape across the canvas.

;; Number Number -> Shape
(define (move-shape n s)
  (cond [(draw-and-clear-shape s) (translate-shape s n)]
        [else s]))

(define S1 (make-circle (make-posn 100 100) 30 'red))
(define S2 (make-rectangle (make-posn 100 100) 30 50 'red))

#|
(start 300 300)
(move-shape 30
 (move-shape 30
  (move-shape 30
   (move-shape 30
    (move-shape 30 
                S1)))))
;                S2)))))
|#
 
 