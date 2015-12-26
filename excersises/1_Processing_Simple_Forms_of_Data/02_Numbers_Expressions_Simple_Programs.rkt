;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp")))))
;; 2.2.1
;; Number -> Number
;; computes the celsius equivalent of fahrenheit
(check-expect (fahrenheit->celsius 32) 0)
(check-expect (fahrenheit->celsius 212) 100)
(check-expect (fahrenheit->celsius -40) -40)

(define (fahrenheit->celsius f)
  (* (- f 32) (/ 5 9)))

;(convert-gui Fahrenheit->Celsius)
;(convert-repl Fahrenheit->Celsius)

;; 2.2.3
;; Number Number -> Number
;; consumes the length of a triangle's side and the perpendicular height. 
;; produces the area of the triangle.
(check-expect (triangle 5 10) (/ (* 5 10) 2))

(define (triangle s h)
  (/ (* s h) 2))

;; 2.2.4
;; consumes three digits, starting with the least significant digit
;; produces the corresponding number.
(check-expect (convert3 1 2 3) 321)

(define (convert3 a b c)
  (+ a
     (* 10 b)
     (* 100 c)))

;; 2.2.5
;; Number -> Number
;; produces n**2 + 10
(check-expect (f1 2) (+ (sqr 2) 10))

(define (f1 n)
  (+ (sqr n) 10))

;; Number -> Number
;; produces (1/2) * n**2 + 20
(check-expect (f2 2) (+ (* 1/2 (sqr 2)) 10))

(define (f2 n)
  (+ (* 1/2 (sqr 2)) 10))

;; Number -> Number
;; produces 2 - (1/n)
(check-expect (f3 2) (- 2 1/2))

(define (f3 n)
  (- 2 (/ 1 n)))

;; 2.3.1
;; Number -> Number
;; determines the tax on the gross pay. the tax-rate is fix: 15%
(check-expect (tax 100) (* .15 100))

(define (tax p)
  (* .15 p))

;; Number -> Number
;; determines the net pay from the number of hours worked. 
;; Assume an hourly rate of $12
(check-expect (netpay 20) (- (* 20 12)
                             (tax (* 20 12))))

(define (netpay h)
  (- (* h 12)
     (tax (* h 12))))

;; 2.3.2
;; Number Number Number Number -> Number
;; consumes four numbers: the number of pennies, nickels, dimes, and quarters
;; produces the amount of money
(check-expect (sum-coins 10 5 13 3)
              (+ (* 1/100 10)
                 (* 1/20 5)
                 (* 1/10 13)
                 (* 1/4 3)))

;(define (sum-coins p n d q) 0); stub

(define (sum-coins p n d q)
  (+ (* 1/100 p)
     (* 1/20  n)
     (* 1/10  d)
     (* 1/4   q)))

;; 2.3.3
;; consumes the number of attendees; produces how much income the attendees produce
;; Each customer pays $5. Every performance costs $20, plus $.50 per attendee
(check-expect (total-profit 0) -20)
(check-expect (total-profit 1) (- (* 1 5) (+ 20 (* 1 .5))))
(check-expect (total-profit 100) (- (* 100 5) (+ 20 (* 100 .5))))

;(define (total-profit a) 0); stub

(define (total-profit a)
  (- (* a 5) (+ 20 (* a .5))))

;; 2.4.4
(define (somef x)
  (sin x x))