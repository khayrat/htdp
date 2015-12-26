;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |5_Symbolic Information|) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")))))
;; Exercise 5.1.1
(define (reply s)
  (cond
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))

;(reply 'HowAreYou?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 5.1.2.   
;; Develop the function check-guess. It consumes two numbers, guess and target. 
;; Depending on how guess relates to target, the function produces one of the following
;; three answers: 'TooSmall, 'Perfect, or 'TooLarge.

;; Number Number -> Symbol
;; consumes guess and target. 
;; Depending on how guess relates to target, produces one of: 
;; 'TooSmall, 'Perfect, or 'TooLarge.
(check-expect (check-guess 10 10) FIT)
(check-expect (check-guess 1  10) SMALL)
(check-expect (check-guess 10  1) LARGE)

(define SMALL 'TooSmall)
(define FIT   'Perfect)
(define LARGE 'TooLarge)

(define (check-guess g t)
  (cond
    [(< g t) SMALL]
    [(= g t) FIT]
    [(> g t) LARGE]))

;(guess-with-gui check-guess)

;; Exercise 5.1.3
;; check-guess3 consumes three digits and a number. The first digit is the least 
;; significant, the third one is the most significant. The number is called target and 
;; represents the randomly chosen number. 
;; Depending on the three digits, relates to target check-guess3 produces one of:
;; 'TooSmall, 'Perfect, or 'TooLarge.

;; Digit Digit Digit Numer -> Symbol
;; consumes three digits and a number. The first digit is the least significant
;; produces one of:
;; 'TooSmall, 'Perfect, or 'TooLarge.

(check-expect (check-guess3 '0 '0 '1 100) FIT)
(check-expect (check-guess3 '3 '2 '1 310) SMALL)
(check-expect (check-guess3 '0 '0 '1  99) LARGE)

(define (check-guess3 d1 d2 d3 target)
  (check-guess (digits->number d1 d2 d3) target))

(define (digits->number d1 d2 d3)
  (+    d1
     (* d2 10)
     (* d3 100)))

;(guess-with-gui-3 check-guess3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 5.1.5.   Develop the function check-color. It implements a key portion of
;; a color guessing game. One player picks two colors for two squares; we call those 
;; targets. The other one tries to guess which color is assigned to which square; 
;; they are guesses. The first player's response to a guess is to check the colors and 
;; to produce one of the following answers:
;; 'Perfect, if the first target is equal to the first guess and the second target is
;;           equal to the second guess;
;; 'OneColorAtCorrectPosition, if the first guess is equal to the first target or the 
;;           second guess is equal to the second target;
;; 'OneColorOccurs, if either guess is one of the two targets; and
;; 'NothingCorrect, otherwise.

;; consumes four colors; a color is a symbol, say, 'red. 
;; The first two arguments to check-color are "targets" the latter two are "guesses"
;; produces one of the four answers.
(check-expect (check-color 'red 'green 'red  'green)  'Perfect)
(check-expect (check-color 'red 'green 'red  'blue)   'OneColorAtCorrectPosition)
(check-expect (check-color 'red 'green 'blue 'green)  'OneColorAtCorrectPosition)
(check-expect (check-color 'red 'green 'blue 'red)    'OneColorOccurs)
(check-expect (check-color 'red 'green 'green 'blue)  'OneColorOccurs)
(check-expect (check-color 'red 'green 'yellow 'blue) 'NothingCorrect)

(define (check-color t1 t2 g1 g2)
  (cond
    [(and (symbol=? t1 g1) (symbol=? t2 g2)) 'Perfect]
    [(or  (symbol=? t1 g1) (symbol=? t2 g2)) 'OneColorAtCorrectPosition]
    [(or  (symbol=? t1 g2) (symbol=? t2 g1)) 'OneColorOccurs]
    [else 'NothingCorrect]))

(master check-color)