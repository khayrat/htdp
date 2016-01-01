;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 15_Mutually_Referential_Data_Definitions) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")))))
(define-struct parent (loc name date eyes))
;; A Parent is a structure (make-parent loc n d e)
;; where loc is a ListOfChildren, n, e are Symobls and d is a Number

;; ListOfChildren is either
;; empty or
;; (cons p loc); where p is a Parent and loc is a ListOfChildren

;; Youngest Generation:
(define Gustav (make-parent empty 'Gustav 1988 'brown))
(define Mike (make-parent empty 'Mike   1988 'blue))

(define Fred&Eva (list Gustav Mike))

;; Middle Generation:
(define Adam (make-parent empty    'Adam 1950 'yellow))
(define Dave (make-parent empty    'Dave 1955 'black))
(define Eva  (make-parent Fred&Eva 'Eva  1965 'blue))
(define Fred (make-parent Fred&Eva 'Fred 1966 'pink))

(define Carl&Bettina (list Adam Dave Eva))

;; Oldest Generation:
(define Carl    (make-parent Carl&Bettina 'Carl    1926 'green))
(define Bettina (make-parent Carl&Bettina 'Bettina 1926 'green))

;; Parent -> Boolean
;; Deterimine whether a parent or any of its descendants have 'blue eyes.
(check-expect (blue-eyed-descendant? Gustav) false)
(check-expect (blue-eyed-descendant? Eva)    true)
(check-expect (blue-eyed-descendant? Carl)   true)

(define (blue-eyed-descendant? p)
  (cond [(symbol=? 'blue (parent-eyes p)) true]
        [else
         (blue-eyed-children? (parent-loc p))]))

;; (listof Parent) -> Boolean
(define (blue-eyed-children? loc)
  (cond [(empty? loc) false]
        [else
         (or (blue-eyed-descendant? (first loc))
             (blue-eyed-children? (rest loc)))]))

;; Exercise 15.1.2
;; Develop the function how-far-removed. It determines how far a blue-eyed
;; descendant, if one exists, is removed from the given parent. If the
;; given parent has blue eyes, the distance is 0; if eyes is not blue but
;; at least one its children's eyes are, the distance is 1; and so on.
;; If no descendant of the given parent has blue eyes, the function 
;; returns false when it is applied to the corresponding family tree. 
(check-expect (how-far-removed Gustav) false)
(check-expect (how-far-removed Eva)    0)
(check-expect (how-far-removed Carl)   1)

;; Parent -> Natural | false
;; consumes a (Family-Tree) Parent; determines how far a blue-eyed
;; descendant, if one exists, is removed from the given parent. If the
;; given parent has blue eyes, the distance is 0; if eyes is not blue but
;; at least one its children's eyes are, the distance is 1; and so on.
;; If no descendant of the given parent has blue eyes, the function 
;; returns false when it is applied to the corresponding family tree.
(define (how-far-removed p)
  (cond [(symbol=? 'blue (parent-eyes p)) 0]
        [else
         (how-far-removed-children (parent-loc p))]))

;; (lisof Parent) -> Number | false
(define (how-far-removed-children loc)
  (cond [(empty? loc) false]
        [else
         (if (not (false? (how-far-removed (first loc))))
             (add1 (how-far-removed (first loc)))
             (how-far-removed-children (rest loc)))]))

;; Parent -> Natural | false
;; consumes a Parent yields the nearest descendant with blue eyes or false.
(check-expect (how-far-removed-nearest Gustav) false)
(check-expect (how-far-removed-nearest Eva)    0)
(check-expect (how-far-removed-nearest Carl)   1)

(define (how-far-removed-nearest p)
  (cond [(symbol=? 'blue (parent-eyes p)) 0]
        [else
         (min/false (parent-loc p))]))

;; (listof Parent) -> Natural | false
(define (min/false loc)
  (cond [(empty? loc) false]
        [else
         (min--false (false/add1 (how-far-removed-nearest (first loc)))
                     (min/false (rest loc)))]))

;; (Natural | false) -> Natural | false
(define (false/add1 nf)
  (cond [(number? nf) (add1 nf)]
        [else false]))

;; (Natural | false) (Natural | false) -> (Natural | false)
(define (min--false af bf)
  (cond [(false? af) bf]
        [(false? bf) af]
        [(< af bf) af]
        [else bf]))

;; Musterlösung

;; Parent -> Natural | false
;; consumes a Parent yields the nearest descendant with blue eyes or false.
(check-expect (how-far-removed-v1 Gustav) false)
(check-expect (how-far-removed-v1 Eva)    0)
(check-expect (how-far-removed-v1 Carl)   1)

(define (how-far-removed-v1 p)
  (cond [(symbol=? 'blue (parent-eyes p)) 0]
        [else
         (add1/false (how-far-removed-children-v1 (parent-loc p)))]))

;; (listof Parent) -> (Natural | false)
(define (how-far-removed-children-v1 loc)
  (cond [(empty? loc) false]
        [else
         (min/false-v1 (how-far-removed-v1          (first loc))
                       (how-far-removed-children-v1 (rest loc)))]))

;; (Natural | false) (Natural | false) -> (Natural | false)
(define (min/false-v1 a b)
  (cond [(false? a) b]
        [(false? b) a]
        [(< a b)    a]
        [else       b]))

;; (Natural | false) -> (Natural | false)
(define (add1/false nf)
  (cond [(number? nf) (add1 nf)]
        [else         false]))

;; Exercise 15.1.3
;; Develop the function count-descendants, which consumes a parent and 
;; produces the number of descendants, including the parent.

;; Parent -> Natural
;; consumes a parent; produces the number of descendants, including
;; the parent.
(check-expect (count-descendants Gustav) 1)
(check-expect (count-descendants Carl)   6)

(define (count-descendants p)
  (add1 (count-descendants-children (parent-loc p))))

;; (listof Parent) -> Natural
(define (count-descendants-children loc)
  (cond [(empty? loc) 0]
        [else
         (+ (count-descendants          (first loc))
            (count-descendants-children (rest loc)))]))

;; Develop the function count-proper-descendants, which consumes a parent
;; and produces the number of proper descendants, that is, all nodes in
;; the family tree, not counting the parent.

;; Parent -> Natural
;; consumes a parent; produces the number of descendants, including
;; the parent.
(check-expect (count-proper-descendants Gustav) 0)
(check-expect (count-proper-descendants Carl)   5)

(define (count-proper-descendants p)
  (count-descendants-children (parent-loc p)))

;; Exercise 15.1.4
;; Develop the function eye-colors, which consumes a parent and produces a
;; list of all eye colors in the tree. An eye color may occur more than 
;; once in the list.
;; Hint: Use the Scheme operation append, which consumes two lists and 
;; produces the concatenation of the two lists.

#|
;; Youngest Generation:
(define Gustav (make-parent empty 'Gustav 1988 'brown))
(define Mike (make-parent empty 'Mike   1988 'blue))

(define Fred&Eva (list Gustav Mike))

;; Middle Generation:
(define Adam (make-parent empty    'Adam 1950 'yellow))
(define Dave (make-parent empty    'Dave 1955 'black))
(define Eva  (make-parent Fred&Eva 'Eva  1965 'blue))
(define Fred (make-parent Fred&Eva 'Fred 1966 'pink))

(define Carl&Bettina (list Adam Dave Eva))

;; Oldest Generation:
(define Carl    (make-parent Carl&Bettina 'Carl    1926 'green))
(define Bettina (make-parent Carl&Bettina 'Bettina 1926 'green))

|#
;; Parent -> (listof Symbol)
;; consumes a parent and produces a list of all eye colors in the tree.
;; An eye color may occur more than 
;; once in the list.
(check-expect (eye-colors Gustav) '(brown))
(check-expect (eye-colors Carl)   '(green yellow black blue brown blue))

(define (eye-colors p)
  (cons (parent-eyes p)
        (eye-colors-children (parent-loc p))))

;; (listof Parent) -> (listof Symbol)
(define (eye-colors-children loc)
  (cond [(empty? loc) empty]
        [else
         (append (eye-colors (first loc))
                 (eye-colors-children (rest loc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extended Exercise: More on Web-Pages

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

#|                           
;; Templates

;; WP -> ???
(define (fn-for-wp wp)
  ... (wp-header wp)...
  ... (fn-for-doc (wp-body wp) ...))

;; DOC -> ???
(define (fn-for-doc d)
  (cond [(empty? d) ...]
        [(symbol? (first d))
         ... (first d) ...
         ... (fn-for-doc (rest d)) ...]
        [else
         ... (fn-for-wp (first d)) ...
             (fn-for-doc (rest d)) ...]))
|#

;; Exercise 15.3.1
;; Develop the function size, which consumes a Web page and produces the
;; number of symbols (words) it contains.

;; WP -> Natural
;; consumes a Web page and produces the number of symbols it contains.
(check-expect (size WP1) 1)
(check-expect (size WP2) 2)
(check-expect (size WP3) 3)
(check-expect (size WP4) 6)

(define (size wp)
  (add1 (size-doc (wp-body wp))))

;; DOC -> Natural
(define (size-doc d)
  (cond [(empty? d) 0]
        [(symbol? (first d)) (add1 (size-doc (rest d)))]
        [else
         (+ (size     (first d))
            (size-doc (rest d)))]))

;; Exercise 15.3.2.
;; Develop the function wp-to-file. The function consumes a Web page and 
;; produces a list of symbols. The list contains all the words in a body
;; and all the headers of embedded Web pages. The bodies of immediately 
;; embedded Web pages are ignored.
#|
(define WP1 (make-wp 'a-h empty))
(define WP2 (make-wp 'b-h (cons 'B-b empty)))
(define WP3 (make-wp 'c-h '(c-b1 c-b2)))
(define WP4 (make-wp 'd-h (list 'd-b1
                                (make-wp 'd1-h '(d1-b1 d1-b2)) 
                                'd-b2)))
|#
;; WP -> (listof Symbol)
;; consumes a Web page and produces a list of symbols. The list contains 
;; all the words in a body and all the headers of embedded Web pages. 
;; The bodies of immediately embedded Web pages are ignored.
(check-expect (wp-to-file WP1) '())
(check-expect (wp-to-file WP2) '(B-b))
(check-expect (wp-to-file WP3) '(c-b1 c-b2))
(check-expect (wp-to-file WP4) '(d-b1 d1-h d-b2))

(define (wp-to-file wp)
    (doc-to-file (wp-body wp)))

;; DOC -> (listof Symbol)
(define (doc-to-file d)
  (cond [(empty? d) empty]
        [(symbol? (first d))
         (cons (first d)
               (doc-to-file (rest d)))]
        [else
         (cons (wp-header   (first d))
               (doc-to-file (rest d)))]))

;; Exercise 15.3.3
;; Develop the function occurs. It consumes a symbol and a Web page and 
;; determines whether the former occurs anywhere in the latter, including
;; the embedded Web pages.

#|
(define WP1 (make-wp 'a-h empty))
(define WP2 (make-wp 'b-h (cons 'B-b empty)))
(define WP3 (make-wp 'c-h '(c-b1 c-b2)))
(define WP4 (make-wp 'd-h (list 'd-b1
                                (make-wp 'd1-h '(d1-b1 d1-b2)) 
                                'd-b2)))
|#

;; Symbol WP -> Boolean
;; consuems a Symbol and a WP; produces true if the symbol occurs in WP.
(check-expect (occurs 'a     WP1) false)
(check-expect (occurs 'B-b   WP2) true)
(check-expect (occurs 'a     WP2) false)
(check-expect (occurs 'c-b2  WP3) true)
(check-expect (occurs 'd1-b2 WP4) true)
(check-expect (occurs 'd-b2  WP4) true)

(define (occurs s wp)
    (occurs-doc s (wp-body wp)))

;; Symbol DOC -> Boolean
(define (occurs-doc s d)
  (cond [(empty? d) false]
        [(symbol? (first d))
         (or (symbol=?   s (first d))
             (occurs-doc s (rest d)))]
        [else
         (or (occurs     s (first d))
             (occurs-doc s (rest d)))]))

;; Exercise 15.3.4
;; Develop the program find. The function consumes a Web page and a symbol.
;; It produces false, if the symbol does not occur in the body of the page
;; or its embedded Web pages. If the symbol occurs at least once, it
;; produces a list of the headers that are encountered on the way to the
;; symbol.
;; Hint: Define an auxiliary like find that produces only true when a Web
;; page contains the desired word. Use it to define find. Alternatively, 
;; use boolean? to determine whether a natural recursion of find produced 
;; a list or a boolean. Then compute the result again. We will discuss
;; this second technique, called backtracking, in the intermezzo at the
;; end of this part. 

#|
(define WP1 (make-wp 'a-h empty))
(define WP2 (make-wp 'b-h (cons 'B-b empty)))
(define WP3 (make-wp 'c-h '(c-b1 c-b2)))
(define WP4 (make-wp 'd-h (list 'd-b1
                                (make-wp 'd1-h '(d1-b1 d1-b2)) 
                                'd-b2)))
|#

;; Symbol WP Symbol -> (listof Symbol) | false
;; consumes a Web page and a symbol.
;; It produces false, if the symbol does not occur in the body of the page
;; or its embedded Web pages. If the symbol occurs at least once, it
;; produces a list of the headers that are encountered on the way to the
;; symbol.

(check-expect (find 'a     WP1) false)
(check-expect (find 'B-b   WP2) '(b-h))
(check-expect (find 'a     WP2) false)
(check-expect (find 'c-b2  WP3) '(c-h))
(check-expect (find 'd1-b2 WP4) '(d-h d1-h))
(check-expect (find 'd-b2  WP4) '(d-h))
(check-expect (find 'x     WP4) false)

(define (find s wp)
  (if (not (false? (find-doc s (wp-body wp))))
      (cons (wp-header wp)
            (find-doc s (wp-body wp)))
      false))

;; DOC -> (listof Symbol) | false
(define (find-doc s d)
  (cond [(empty? d) false]
        [(symbol? (first d))
         (if (symbol=? s (first d))
             empty
             (find-doc s (rest d)))]
        [else
         (cons/false (find s (first d))
                     (find-doc s (rest d)))]))

;; ((listof Symbol) | false) ((listof Symbol) | false) -> (listof Symbol) | false
(define (cons/false a b)
  (cond [(false? a) b]
        [(false? b) a]
        [else
         (append a b)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Musterlösung
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
         (if (not (false? (find-v1 s (first d))))
             (find-v1 s (first d))
             (find-doc-v1 s (rest d)))]))

;; ((listof Symbol) | false) ((listof Symbol) | false) -> (listof Symbol) | false)
(define (append/false a b)
  (cond [(false? b) false]
        [else      (append a b)]))