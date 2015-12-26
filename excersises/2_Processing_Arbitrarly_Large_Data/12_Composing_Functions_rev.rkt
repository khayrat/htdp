;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 12_Composing_Functions_rev) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")))))
;; (Listof Number) -> (ListOfNumber)
;; sort the given list of Number the smallest first
(check-expect (ins-sort empty) empty)
(check-expect (ins-sort (list 3 2 5)) (list 5 3 2))

(define (ins-sort lon)
  (cond [(empty? lon) empty]
        [else
         (insert (first lon)
                 (ins-sort (rest lon)))]))

;; Number (listOf Number) -> (ListOf Number)
;; insert the given number in the lon. The lon is already sorted.
(check-expect (insert 1 empty) (cons 1 empty))
(check-expect (insert 2 (list 4 1)) (list 4 2 1))

(define (insert n lon)
  (cond [(empty? lon) (cons n empty)]
        [else 
         (if (<= n (first lon))
             (cons (first lon) (insert n (rest lon)))
             (cons n lon))]))

;; Exercise 12.2.1
;; Develop a program that sorts lists of mail messages by date. 
;; Mail structures are defined as follows:

(define-struct mail (from date message))

;; A mail-message is a structure: 
;; (make-mail name n s) 
;;   where name is a string, n is a number, and s is a string.

;; (Listof Mail) -> (Listof Mail)
;; sorts lists of mail messages by date.
(check-expect (sort-mail-by-date empty) empty)
(check-expect (sort-mail-by-date (list (make-mail "a" 20151010 "b")
                                       (make-mail "a" 20141010 "b")
                                       (make-mail "a" 20161010 "b")
                                       (make-mail "a" 20131010 "b")))
              (list (make-mail "a" 20161010 "b")
                    (make-mail "a" 20151010 "b")
                    (make-mail "a" 20141010 "b")
                    (make-mail "a" 20131010 "b")))

(define (sort-mail-by-date lom)
  (cond [(empty? lom) empty]
        [else
         (insert-mail-date (first lom) (sort-mail-by-date (rest lom)))]))

;; Mail (Listof Mail) -> (Listof Mail)
;; inserts the given mail at the right position in the sorted by date
;; mails. The sort order is nwest first.
(check-expect (insert-mail-date (make-mail "a" 20151010 "b") empty)
              (cons (make-mail "a" 20151010 "b") empty))

(check-expect (insert-mail-date (make-mail "a" 20151010 "b")
                                (list (make-mail "a" 20161010 "b")
                                      (make-mail "a" 20141010 "b")
                                      (make-mail "a" 20131010 "b")))
              (list (make-mail "a" 20161010 "b")
                    (make-mail "a" 20151010 "b")
                    (make-mail "a" 20141010 "b")
                    (make-mail "a" 20131010 "b")))

(define (insert-mail-date m lom)
  (cond [(empty? lom) (cons m empty)]
        [else
         (if (>= (mail-date m) (mail-date (first lom)))
             (cons m lom)
             (cons (first lom)
                   (insert-mail-date m (rest lom))))]))

;; Also devel(op a program that sorts lists of mail messages by name. 
;; To compare two strings alphabetically, use the string<? primitive.

;; (Listof Mail) -> (Listof Mail)
;; sorts lists of mail messages by name.
(check-expect (sort-mail-by-name empty) empty)
(check-expect (sort-mail-by-name (list (make-mail "zz" 20151010 "b")
                                       (make-mail "ac" 20141010 "b")
                                       (make-mail "ab" 20161010 "b")
                                       (make-mail "aa" 20131010 "b")))
              (list (make-mail "aa" 20131010 "b")
                    (make-mail "ab" 20161010 "b")
                    (make-mail "ac" 20141010 "b")
                    (make-mail "zz" 20151010 "b")))

(define (sort-mail-by-name lom)
  (cond [(empty? lom) empty]
        [else
         (insert-mail-name (first lom) (sort-mail-by-name (rest lom)))]))

;; Mail (Listof Mail) -> (Listof Mail)
;; inserts the given mail at the right position in the sorted by name
;; mails. The sort order is nwest first.
(check-expect (insert-mail-name (make-mail "a" 20151010 "b") empty)
              (cons (make-mail "a" 20151010 "b") empty))

(check-expect (insert-mail-name (make-mail "ab" 20151010 "b")
                                (list (make-mail "aa" 20161010 "b")
                                      (make-mail "ac" 20141010 "b")
                                      (make-mail "zz" 20131010 "b")))
              (list (make-mail "aa" 20161010 "b")
                    (make-mail "ab" 20151010 "b")
                    (make-mail "ac" 20141010 "b")
                    (make-mail "zz" 20131010 "b")))

(define (insert-mail-name m lom)
  (cond [(empty? lom) (cons m empty)]
        [else
         (if (string<? (mail-from m) (mail-from (first lom)))
             (cons m lom)
             (cons (first lom)
                   (insert-mail-name m (rest lom))))]))

;; Exercise 12.2.2

;; Number (Listof Number) -> Boolean
;; prduces true, iff the given Number is in the LON
(define (search n lon)
  (cond [(empty? lon) false]
        [else
         (or (= n (first lon)) (search n (rest lon)))]))

;; Develop the function search-sorted, which determines whether a number 
;; occurs in a sorted list of numbers. The function must take advantage of
;; the fact that the list is sorted.

;; Number (Listof Number) -> Boolean
;; prduces true, iff the given Number is in the LON
;; LON is sorted least first
(check-expect (search-sorted 1 empty) false)
(check-expect (search-sorted 2 (list 1 2 3 4 6)) true)
(check-expect (search-sorted 5 (list 1 2 3 4 6)) false)


(define (search-sorted n lon)
  (cond [(empty? lon) false]
        [else
         (cond [(= n (first lon)) true]
               [(< n (first lon)) false]
               [(> n (first lon)) (search-sorted n (rest lon))])]))

;; Terminology: The function search-sorted conducts a LINEAR SEARCH.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Polygon is eiher
;; - (cons p empty) where p is a Posn, or
;; - (cons p lop)   where p is a Posn and lop is (Listof Posn)

;; Polygon -> true
(define (draw-polygon p)
  (connect-points (cons (last p) p)))

;; (Listof Posn) -> true
(define (connect-points lop)
  (cond [(empty? (rest lop)) true]
        [else
         (and (draw-solid-line (first lop) (second lop) 'red)
              (connect-points (rest lop)))]))

;; (Listof Posn) -> Posn
;; produces the last element of a List of Posn.
;; Assume list is not empty
(check-expect (last (list 1 2 3)) 3)

(define (last p)
  (cond [(empty? (rest p)) (first p)]
        [else
         (last (rest p))]))

;; xercise 12.3.1
;; Modify draw-polygon so that it adds the first item of a-poly to its end.
;; This requires a different auxiliary function: add-at-end. 

;; Polygon -> true
(define (draw-polygon-v1 p)
  (connect-points (add-at-end (first p) p)))

;; Posn (ListOf Posn) -> (Listof Posn)
;; adds the given point to the end of the list
(check-expect (add-at-end (make-posn 1 2) empty)
              (list (make-posn 1 2)))
(check-expect (add-at-end (make-posn 1 2) (list (make-posn 2 3)
                                                (make-posn 4 5)))
              (list (make-posn 2 3)
                    (make-posn 4 5)
                    (make-posn 1 2)))

(define (add-at-end p lop)
  (cond [(empty? lop) (cons p empty)]
        [else
         (cons (first lop)
               (add-at-end p (rest lop)))]))

;; Exercise 12.3.2
;; Modify connect-dots so that it consumes an additional posn structure to
;; which the last posn is connected.

;; Posn (Listof Posn) -> true
(define (connect-dots p lop)
  (cond [(empty? (rest lop)) (draw-solid-line p (first lop))]; p)]
        [else
         (and (draw-solid-line (first lop) (second lop) 'red)
              (connect-dots p (rest lop)))]))

;; Then modify draw-polygon to use this new version of connect-dots.

;; Polygon -> true
(define (draw-polygon-v2 p)
  (connect-dots (first p) p))

#|
(start 300 300)
(draw-polygon-v2 (list (make-posn 100 100)
                       (make-posn 100 200)
                       (make-posn 200 200)                 
                       (make-posn 200 100)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A word is either
;;  empty, or
;;  (cons a w) where a is a symbol ('a, 'b, ..., 'z) and w is a word.

(define W1 empty)
(define W2 (list 'h 'u 'h 'u))

;; Exercise 12.4.1
;; Formulate the data definition for lists of words. Systematically make
;; up examples of words and lists of words

;; A list of Words is either
;;   empty, or
;;   (cons Word low), where low is (Listof Word)

(define LW0 empty)
(define LW1 (list empty))
(define LW2 (list (list 'h 'l 'l 'o)
                  (list 'a 'd 'a 'm)))



;; Exercise 12.4.2
;; Develop the function insert-everywhere/in-all-words. 
;; It consumes a symbol and a list of words. The result is a list of words 
;; like its second argument, but with the first argument inserted between 
;; all letters and at the beginning and the end of all words of the second 
;; argument.

;; Hint: Reconsider the example from above. We stopped and decided that we 
;; needed to insert 'd into the words (cons 'e (cons 'r empty)) and 
;; (cons 'r (cons 'e empty)). The following is therefore a natural 
;; candidate:
#|
(insert-everywhere/in-all-words 'd
  (cons (cons 'e (cons 'r empty))
    (cons (cons 'r (cons 'e empty))
      empty)))
|#
;; for the ``function examples'' step. Keep in mind that the second input 
;; corresponds to the sequence of (partial) words ``er'' and ``re''.
;; Also, use the Scheme operation append, which consumes two lists and 
;; produces the concatenation of the two lists

;; Word -> (listof Word)
;; to create a list of all rearrangements of the letters in a-word
(check-expect (arrangements empty) 
              (list empty))
(check-expect (arrangements (list 'a)) 
              (list (list 'a)))
(check-expect (arrangements (list 'a 'b)) 
              (list (list 'a 'b)
                    (list 'b 'a)))

(define (arrangements w)
  (cond [(empty? w) (list empty)]; -> (listof empty Word)
        [else
         (insert-everywhere/in-all-words (first w) 
                                         (arrangements (rest w)))]))

;; Symbol (listof Word) -> (listof Word)
;; insert the given Symbol into all words between all possible letters and
;; at the beginning and end.
(check-expect (insert-everywhere/in-all-words 'a empty)
              empty)
(check-expect (insert-everywhere/in-all-words 'a (list empty))
              (list (list 'a)))
(check-expect (insert-everywhere/in-all-words 'a (list (list 'b)))
              (list (list 'a 'b)
                    (list 'b 'a)))
(check-expect (insert-everywhere/in-all-words 'a 
                                              (list (list 'c 'd)))
              (list (list 'a 'c 'd)
                    (list 'c 'a 'd)
                    (list 'c 'd 'a)))

(define (insert-everywhere/in-all-words s low)
  (cond [(empty? low) empty]
        [else
         (append (insert-everywhere s (first low))
                 (insert-everywhere/in-all-words s (rest low)))]))

;; Symbol Word -> (Listof Word)
;; each Word in the produced LOW is the given Word with the given Symbol
;; insert between all possible letters and at the beginning and end.
(check-expect (insert-everywhere 'a empty) 
              (list (list 'a)))
(check-expect (insert-everywhere 'a (list 'b))
              (list (list 'a 'b)
                    (list 'b 'a)))
(check-expect (insert-everywhere 'a (list 'b 'c))
              (list (list 'a 'b 'c)
                    (list 'b 'a 'c)
                    (list 'b 'c' a)))

(define (insert-everywhere s w)
  (cond [(empty? w) (list (list s))]
        [else
         (cons (cons s w) 
               (add-at-front (first w) 
                             (insert-everywhere s (rest w))))]))

;; Symbol (listof Word) -> (listof Word)
;; add the given Symbol to the front of each Word in the LOW
(check-expect (add-at-front 'a empty)
              empty)
(check-expect (add-at-front 'a (list empty)) 
              (list (list 'a)))
(check-expect (add-at-front 'a (list (list 'b 'c)))
              (list (list 'a 'b 'c)))
(check-expect (add-at-front 'a (list (list 'b 'c)
                                     (list 'x 'y)))
              (list (list 'a 'b 'c)
                    (list 'a 'x 'y)))

(define (add-at-front s low)
  (cond [(empty? low) empty]
        [else
         (cons (cons s (first low))
               (add-at-front s  (rest low)))]))