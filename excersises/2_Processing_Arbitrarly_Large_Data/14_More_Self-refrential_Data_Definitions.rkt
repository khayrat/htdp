;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 14_More_Self-refrential_Data_Definitions) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")))))
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
(define Gustav  (make-child Fred Eva 'Gustav 1988 'brown))

;; Function Template
#|
;; FTN -> ???
(define (fn-for-ftn ftn)
  (cond [(empty? ftn) ...]
        [else
         ... (fn-for-child ftn) ...]))

;; Child -> ???
(define (fn-for-child c)
  ... (fn-for-ftn (child-father c)) ...
  ... (fn-for-ftn (child-mother c)) ...
  ... (child-name c) ...
  ... (child-date c) ...
  ... (child-eyes c) ...)
|#

(check-expect (blue-eyed-ancestor? Carl) false)
(check-expect (blue-eyed-ancestor? Gustav) true)

;; FTN -> Boolean
;; determine whether the given family-tree contains a child with blue eyes.
(define (blue-eyed-ancestor? ftn)
  (cond [(empty? ftn) false]
        [else
         (blue-eyed-child? ftn)]))

;; Child -> Boolean
(define (blue-eyed-child? c)
  (or (symbol=? 'blue (child-eyes c))
      (or (blue-eyed-ancestor? (child-father c))
          (blue-eyed-ancestor? (child-mother c)))))

;; Exercise 14.1.3
;; Develop count-persons. The function consumes a family tree node and 
;; produces the number of people in the corresponding family tree. 

;; FTN -> Number
;; consumes a family tree node
;; produces the number of people in the corresponding family tree.
(check-expect (count-persons empty)  0)
(check-expect (count-persons Carl)   1)
(check-expect (count-persons Gustav) 5)

(define (count-persons ftn)
  (cond [(empty? ftn) 0]
        [else
         (add1 (count-ancestors ftn))]))

;; Child -> Number
(define (count-ancestors c)
  (+ (count-persons (child-father c))
     (count-persons (child-mother c))))

;; Exercise 14.1.4
;; Develop the function average-age. It consumes a family tree node and 
;; the current year. It produces the average age of all people in the 
;; family tree. 

;; FTN Number -> Number
;; consumes a family tree node and the current year
;; produces the average age of all people in the family tree.
(check-expect (average-age empty 2015)  0)
(check-expect (average-age Carl  2015)  (- 2015 (child-date Carl)))
(check-expect (average-age Adam  2015) 
              (/ (+ (- 2015 (child-date Carl))
                    (- 2015 (child-date Bettina))
                    (- 2015 (child-date Adam)))
                 3))

(define (average-age ftn n)
  (cond [(empty? ftn) 0]
        [else
         (/ (age-childs ftn n)
            (count-persons ftn))]))

;; Child -> Number
(define (age-childs c n)
  (+ (- n (child-date c))
     (age-ancestors (child-father c) n)
     (age-ancestors (child-mother c) n)))

;; FTN Number -> Number
(define (age-ancestors ftn n)
  (cond [(empty? ftn) 0]
        [else
         (age-childs ftn n)]))


;; Exercise 14.1.5
;; Develop the function eye-colors, which consumes a family tree node and
;; produces a list of all eye colors in the tree. An eye color may occur
;; more than once in the list.

;; FTN -> (listof Symbol)
;; consumes a family tree node and
;; produces a list of all eye colors in the tree. An eye color may occur
;; more than once in the list.

(check-expect (eye-colors empty) empty)
(check-expect (eye-colors Carl) (list 'green))
(check-expect (eye-colors Adam) (list 'yellow 'green 'green))
(check-expect (eye-colors Gustav) 
              (list 'brown 'pink 'blue 'green 'green))

(define (eye-colors ftn)
  (cond [(empty? ftn) empty]
        [else
         (eye-colors-child ftn)]))

;; Child -> (listof Symbol)
(define (eye-colors-child c)
  (cons (child-eyes c)
        (append (eye-colors (child-father c))
                (eye-colors (child-mother c)))))

;; Exercise 14.1.6
;; Suppose we need the function proper-blue-eyed-ancestor?. It is like
;; blue-eyed-ancestor? but responds with true only when some proper 
;; ancestor, not the given one, has blue eyes.

;; FTN -> Boolean
;; determine whether the given family-tree contains a child with blue eyes.
;; responds with true only when some proper ancestor, not the given one, 
;; has blue eyes.
#|
;; Oldest Generation:
(define Carl    (make-child empty empty 'Carl 1926 'green))
(define Bettina (make-child empty empty 'Bettina 1926 'green))

;; Middle Generation:
(define Adam    (make-child Carl  Bettina 'Adam 1950 'yellow))
(define Dave    (make-child Carl  Bettina 'Dave 1955 'black))
(define Eva     (make-child Carl  Bettina 'Eva  1960 'blue))
(define Fred    (make-child empty empty   'Fred 1966 'pink))

;; Youngest Generation:
(define Gustav  (make-child Fred Eva 'Gustav 1988 'brown))
|#
(check-expect (proper-blue-eyed-ancestor? empty) false)
(check-expect (proper-blue-eyed-ancestor? Carl) false)
(check-expect (proper-blue-eyed-ancestor? Eva) false)
(check-expect (proper-blue-eyed-ancestor? Gustav) true)

(define (proper-blue-eyed-ancestor? ftn)
  (cond [(empty? ftn) false]
        [else
         (proper-blue-eyed-child? ftn)]))

;; Child -> Boolean
(define (proper-blue-eyed-child? c)
  (or (blue-eyed-ancestor? (child-father c))
      (blue-eyed-ancestor? (child-mother c))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct node (ssn name left right))
;; A Binary-Tree BT is either
;; - false; or
;; - (make-node ssn n l r); where ssn is a Number, n is a Symbol, and 
;;                          l,r are BTs.

(define BS1 (make-node 15 'd 
                       false
                       (make-node 24 'i false false)))
(define BS2 (make-node 15 'd
                       (make-node 87 'h false false)
                       false))

;; Exercise 14.2.1
;; Draw the two trees above in the manner of figure 38. Then develop
;; contains-bt. The function consumes a number and a BT and determines
;; whether the number occurs in the tree.

;; Number BT -> Boolean
;; consumes a number and a BT and determines whether the number 
;; occurs in the tree.
(check-expect (contains-bt  1 BS1) false)
(check-expect (contains-bt 15 BS1) true)
(check-expect (contains-bt 24 BS1) true)
(check-expect (contains-bt 15 BS2) true)
(check-expect (contains-bt 87 BS2) true)
(check-expect (contains-bt  1 BS2) false)

(define (contains-bt n bt)
  (cond [(false? bt) false]
        [else
         (or (= n (node-ssn bt))
             (contains-bt n (node-left  bt))
             (contains-bt n (node-right bt)))]))

;; Exercise 14.2.2
;; Develop search-bt. The function consumes a number n and a BT. If the
;; tree contains a node structure whose soc field is n, the function 
;; produces the value of the pn field in that node. Otherwise, the 
;; function produces false.

;; Number BT -> Symbol | false
;; consumes a number n and a BT. If BT contains a node whose ssn field is n,
;; produces the value of the n field in that node. Otherwise, false.
(check-expect (search-bt  1 BS1) false)
(check-expect (search-bt 15 BS1) 'd)
(check-expect (search-bt 24 BS1) 'i)
(check-expect (search-bt 15 BS2) 'd)
(check-expect (search-bt 87 BS2) 'h)
(check-expect (search-bt  1 BS2) false)

(define (search-bt n bt)
  (cond [(false? bt) false]
        [else
         (cond [(= n (node-ssn bt)) (node-name bt)]
               [else
                (if (not (false? (search-bt n (node-left bt))))
                    (search-bt n (node-left bt))
                    (search-bt n (node-right bt)))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;; A Binary-Seach-Tree BST is a BT:
;; - false is a BST
;; - (make-node sn n l r) is a BST if
;;   - a. l,r are BST
;;   - b. all ssn in l are smaller than sn, and
;;   - c. all ssn in r are larger tnan sn

;; Exercise 14.2.3
;; Develop the function inorder. It consumes a binary tree and produces 
;; a list of all the ssn numbers in the tree. The list contains the
;; numbers in the left-to-right order we have used above.

(define BS3
  (make-node 63 'a
             (make-node 29 'b
                        (make-node 15 'c
                                   (make-node 10 'd false false)
                                   (make-node 24 'e false false))
                        false)
             (make-node 89 'f
                        (make-node 77 'g false false)
                        (make-node 95 'h
                                   false
                                   (make-node 99 'i false false)))))

(define BS4
  (make-node 63 'a
             (make-node 29 'b
                        (make-node 15 'c
                                   (make-node 87 'd false false)
                                   (make-node 24 'e false false))
                        false)
             (make-node 89 'f
                        (make-node 33 'g false false)
                        (make-node 95 'h
                                   false
                                   (make-node 99 'i false false)))))

;; BT -> (listof Number)
;; consumes a binary tree and produces a list of all the ssn numbers in 
;; the tree. The numbers are in the left-to-right order.
(check-expect (inorder false) empty)
(check-expect (inorder BS3)
              (list 10 15 24 29 63 77 89 95 99))
(check-expect (inorder BS4)
              (list 87 15 24 29 63 33 89 95 99))

(define (inorder bt)
  (cond [(false? bt) empty]
        [else
         (append (inorder (node-left bt))
                 (list (node-ssn bt))
                 (inorder (node-right bt)))]))

;; Exercise 14.2.4
;; Develop search-bst. The function consumes a number n and a BST. If the 
;; tree contains a node structure whose soc field is n, the function 
;; produces the value of the pn field in that node. Otherwise, the 
;; function produces false. The function organization must exploit the BST
;; Invariant so that the function performs as few comparisons as necessary.
;; Compare searching in binary search trees with searching in sorted lists

;; Number BST -> Symbol | false
;; consumes a number n and a BST; produces name of the matching node. 
;; Otherwise, false
#|
(define BS3
  (make-node 63 'a
             (make-node 29 'b
                        (make-node 15 'c
                                   (make-node 10 'd false false)
                                   (make-node 24 'e false false))
                        false)
             (make-node 89 'f
                        (make-node 77 'g false false)
                        (make-node 95 'h
                                   false
                                   (make-node 99 'i false false)))))
|#
(check-expect (search-bst  1 BS3) false)
(check-expect (search-bst 63 BS3) 'a)
(check-expect (search-bst 29 BS3) 'b)
(check-expect (search-bst 15 BS3) 'c)
(check-expect (search-bst 10 BS3) 'd)
(check-expect (search-bst 24 BS3) 'e)
(check-expect (search-bst 89 BS3) 'f)
(check-expect (search-bst 77 BS3) 'g)
(check-expect (search-bst 95 BS3) 'h)
(check-expect (search-bst 99 BS3) 'i)
(check-expect (search-bst 100 BS3) false)

(define (search-bst n bst)
  (cond [(false? bst) false]
        [else
         (cond [(= n (node-ssn bst)) (node-name bst)]
               [(< n (node-ssn bst)) (search-bst n (node-left bst))]
               [(> n (node-ssn bst)) (search-bst n (node-right bst))])]))

;; Exercise 14.2.5
;; Develop the function create-bst. It consumes a BST B, a number N, and
;; a symbol S. It produces a BST that is just like B and that in place of 
;; one false subtree contains the node structure

;; BST Number Symbol -> BST
;; consumes a BST B, a number N, and a symbol S. It produces a BST that is 
;; just like B and that in place of one false subtree contains the node 
;; structure

(check-expect (create-bst false 1 'a)
              (make-node 1 'a false false))
(check-expect (create-bst (make-node 10 'a false false) 5 'b)
              (make-node 10 'a
                         (make-node 5 'b false false )
                         false))
(check-expect (create-bst (make-node 10 'a false false) 15 'c)
              (make-node 10 'a
                         false
                         (make-node 15 'c false false)))

(define (create-bst bst n s)
  (cond [(false? bst) (make-node n s false false)]
        [else
         (make-node (node-ssn bst)
                    (node-name bst)
                    (if (< n (node-ssn bst))
                        (create-bst (node-left bst) n s)
                        (node-left bst))
                    (if (> n (node-ssn bst))
                        (create-bst (node-right bst) n s)
                        (node-right bst)))]))

(define BST1
  (create-bst
   (create-bst
    (create-bst
     (create-bst
      (create-bst
       (create-bst 
        (create-bst
         (create-bst 
          (create-bst 
           false 
           63 'a)
          29 'b)
         15 'c)
        89 'f)
       77 'g)
      95 'h)
     24 'e)
    10 'd)
   99 'i))

(check-expect BST1 BS3)

;; Exercise 14.2.6
;; Develop the function create-bst-from-list. It consumes a list of 
;; numbers and names; it produces a BST by repeatedly applying create-bst.

;; A (listof Number/Name) is either
;; - empty; or
;; - (cons (list ssn n) lonn); where ssn is a Number, n is a Symbol; and
;;                             lonn is a (listof Number/Name)


(define BST2
  (create-bst
   (create-bst
    (create-bst
     (create-bst
      (create-bst
       (create-bst 
        (create-bst
         (create-bst 
          (create-bst 
           false 
           63 'a)
          29 'b)
         15 'd)
        89 'c)
       77 'l)
      95 'g)
     24 'i)
    10 'h)
   99 'o))

;; (listof Number/Name) -> BST
(define LONN1
  '((99 o)
    (77 l)
    (24 i)
    (10 h)
    (95 g)
    (15 d)
    (89 c)
    (29 b)
    (63 a)))

(check-expect (create-bst-from-list LONN1) BST2)

(define (create-bst-from-list lonn)
  (cond [(empty? lonn) false]
        [else
         (create-bst (create-bst-from-list (rest lonn))
                     (first  (first lonn))
                     (second (first lonn)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 14.3 List in Lists

;; A Web-Page (WP) is either
;; 1. empty
;; 2. (cons s wp)  ; where s is a Symbol and wp is a WP
;; 3. (cons ewp wp); where ewp and wp are WP.

(define WP1 
  '(The TeachScheme! Project aims to improve the 
        problem-solving and organization skills of high 
        school students. It provides software and lecture 
        notes as well as exercises and solutions for teachers.))
(define WP2
  '(The TeachScheme Web Page
        Here you can find: 
        (LectureNotes for Teachers)
        (Guidance for (DrScheme: a Scheme programming environment))
        (Exercise Sets)
        (Solutions for Exercises)
        For further information: write to scheme@cs))

;; template for fn-for-wp
#|
(define (fn-for-wp wp)
  (cond [(empty? wp) ...]
        [(symbol? (first wp)) ... (first wp) ... (fn-for-wp (rest wp)) ...]
        [else
         ... (fn-for-wp (first wp)) ... (fn-for-wp (rest wp)) ... ]))
|#

;; WP -> Natural
;; count the number of symbols that occur in the given WP
(check-expect (size empty)  0)
(check-expect (size '(a))   1)
(check-expect (size '(a b)) 2)
(check-expect (size '(a (a b))) 3)

(define (size wp)
  (cond [(empty? wp) 0]
        [(symbol? (first wp)) (add1 (size (rest wp)))]
        [else
         (+ (size (first wp)) (size (rest wp)))]))

;; Exercise 14.3.2
;; Develop the function occurs1. The function consumes a Web page and a 
;; symbol. It produces the number of times the symbol occurs in the Web 
;; page, ignoring the nested Web pages.

;; WP Symbol -> Natural
;; consumes a Web page and a symbol; produces the number of times the 
;; symbol occurs in the Web page, ignoring the nested Web pages
(check-expect (occurs1 empty            'a) 0)
(check-expect (occurs1 '(a)             'a) 1)
(check-expect (occurs1 '(a)             'b) 0)
(check-expect (occurs1 '((a b))         'a) 0)
(check-expect (occurs1 '(a (a b))       'a) 1)
(check-expect (occurs1 '(a b a)         'a) 2)
(check-expect (occurs1 '(a (a b) a)     'a) 2)
(check-expect (occurs1 '(a b a (a b) a) 'a) 3)

(define (occurs1 wp s)
  (cond [(empty? wp) 0]
        [(symbol? (first wp))
         (if (symbol=? s (first wp))
             (add1 (occurs1 (rest wp) s))
             (occurs1 (rest wp) s))]
        [else
         (occurs1 (rest wp) s)]))

;; Develop the function occurs2. It is like occurs1, but counts all 
;; occurrences of the symbol, including in embedded Web pages. 

;; WP Symbol -> Natural
;; consumes a Web page and a symbol; produces the number of times the 
;; symbol occurs in the Web page.
(check-expect (occurs2 empty            'a) 0)
(check-expect (occurs2 '(a)             'a) 1)
(check-expect (occurs2 '(a)             'b) 0)
(check-expect (occurs2 '((a b))         'a) 1)
(check-expect (occurs2 '(a (a b))       'a) 2)
(check-expect (occurs2 '(a b a)         'a) 2)
(check-expect (occurs2 '(a (a b) a)     'a) 3)
(check-expect (occurs2 '(a b a (a b (b a)) a) 'a) 5)

(define (occurs2 wp s)
  (cond [(empty? wp) 0]
        [(symbol? (first wp))
         (if (symbol=? s (first wp))
             (add1 (occurs2 (rest wp) s))
             (occurs2 (rest wp) s))]
        [else
         (+ (occurs2 (first wp) s) (occurs2 (rest wp) s))]))

;; Exercise 14.3.3
;; Develop the function replace. The function consumes two symbols, new
;; and old, and a Web page, a-wp. It produces a page that is structurally 
;; identical to a-wp but with all occurrences of old replaced by new.

;; WP Symbol Symbol -> WP
;; consumes two symbols and a Web page; produces a page that is 
;; structurally identical to the giveb but with all occurrences of
;; old replaced by new
(check-expect (replace empty            'a 'b) empty)
(check-expect (replace '(a)             'a 'a) '(a))
(check-expect (replace '(a)             'b 'a) '(a))
(check-expect (replace '((a b))         'a 'c) '((c b)))
(check-expect (replace '(a (a b))       'a 'c) '(c (c b)))
(check-expect (replace '(a b a)         'a 'c) '(c b c))
(check-expect (replace '(a (a b) a)     'a 'c) '(c (c b) c))
(check-expect (replace '(a b a (a b (b a)) a) 'a 'c) 
              '(c b c (c b (b c)) c))

(define (replace wp o n)
  (cond [(empty? wp) empty]
        [(symbol? (first wp))
         (if (symbol=? o (first wp))
             (cons n          (replace (rest wp) o n))
             (cons (first wp) (replace (rest wp) o n)))]
        [else
         (cons (replace (first wp) o n) (replace (rest wp) o n))]))

;; Exercise 14.3.4
;; People do not like deep Web trees because they require too many page 
;; switches to reach useful information. For that reason a Web page 
;; designer may also want to measure the depth of a page. A page 
;; containing only symbols has depth 0. A page with an immediately
;; embedded page has the depth of the embedded page plus 1. If a page has 
;; several immediately embedded Web pages, its depth is the maximum of 
;; the depths of embedded Web pages plus 1. Develop depth, which consumes 
;; a Web page and computes its depth.

;; WP -> Number
;; consumes a Web page and computes its depth. 
;; A page containing only symbols has depth 0. 
;; A page with an immediately embedded page has the depth of the embedded
;; page plus 1. 
;; If a page has several immediately embedded Web pages, its depth is the 
;; maximum of the depths of embedded Web pages plus 1.
(check-expect (depth empty) 0)
(check-expect (depth '(a))  0)
(check-expect (depth '((a b))) 1)
(check-expect (depth '(a (a b))) 1)
(check-expect (depth '(a b a)) 0)
(check-expect (depth '(a (a b) a)) 1)
(check-expect (depth '(a b a (a b (b a)) a)) 2)
(check-expect (depth '(a
                       (a (a))
                       (a (a (a)))
                       ((((()))))
                       (((()))))) 5)

(define (depth wp)
  (cond [(empty? wp) 0]
        [(symbol? (first wp)) (depth (rest wp))]
        [else
         (max (add1 (depth (first wp)))
              (depth (rest wp)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 14.4 Extended Exercise: Evaluating Scheme

(define-struct add (left right))
(define-struct mul (left right))

;; Exercise 14.4.1
;; Provide a data definition for the representation of Scheme expressions.
;; Then translate the following expressions into representations:

;; A Scheme-Expression (SE) is either
;;  a Number; representing a Number
;;  a Symbol: representing a Variable
;;  (make-add l r); representing (+ l r); where l,r are SE
;;  (make-mul l r); representing (* l r); where l,r are SE

;;(+ 10 -10)
(make-add 10 -10)

;;(+ (* 20 3) 33)
(make-add (make-mul 20 30) 33)

;;(* 3.14 (* r r))
(make-mul 3.14 (make-mul 'r 'r))

;;(+ (* 9/5 c) 32)
(make-add (make-mul 9/5 'c) 32)

;;(+ (* 3.14 (* o o)) (* 3.14 (* i i)))
(make-add (make-mul 3.14 (make-mul 'o 'o)) 
          (make-mul 3.14 (make-mul 'i 'i)))

;; Exercise 14.4.2
;; Develop the function numeric?, which consumes (the representation of)
;; a Scheme expression and determines whether it is numeric. 

;; SE -> Boolean
;; consumes a SE; produces true if it is numeric.
(check-expect (numeric? 1) true)
(check-expect (numeric? (make-add 1 0)) true)
(check-expect (numeric? (make-add 1 (make-mul 1 1))) true)
(check-expect (numeric? (make-add (make-add 1 1) 0)) true)
(check-expect (numeric? 'i) false)
(check-expect (numeric? (make-add 1 'i)) false)
(check-expect (numeric? (make-add 'i 1)) false)
(check-expect (numeric? (make-add 1 (make-mul 1 'i))) false)
(check-expect (numeric? (make-add (make-add 'i 1) 0)) false)

(define (numeric? se)
  (cond [(number? se) true]
        [(symbol? se) false]
        [(add?    se) (and (numeric? (add-left se)) 
                           (numeric? (add-right se)))]
        [(mul?    se) (and (numeric? (mul-left se)) 
                           (numeric? (mul-right se)))]))

;; Exercise 14.4.3
;; Provide a data definition for numeric expressions. 
;; Develop the function evaluate-expression. The function consumes 
;; (the representation of) a numeric Scheme expression and computes its 
;; value. When the function is tested, modify it so it consumes all kinds
;; of Scheme expressions; the revised version raises an error when it
;; encounters a variable

;; A Numeric-Expression (NE) is either
;; 1. Number, or
;; 2. (make-add l r); where l,r are NE, or
;; 3. (make-mul l r); where l,r are NE

;; NE -> Number
;; consumes a NE and computes its value
(check-expect (evaluate-expression 0) 0)
(check-expect (evaluate-expression (make-add 1 2)) 3)
(check-expect (evaluate-expression (make-mul 2 3)) 6)
(check-expect (evaluate-expression 
               (make-add (make-mul 2 3)
                         (make-add 3 1)))
              (+ (* 2 3)
                 (+ 3 1)))
;(check-expect (evaluate-expression (make-add 1 'i)) 
;              "evaluate-expression: variables not supported yet.")

(define (evaluate-expression ne)
  (cond [(number? ne) ne]
        [(symbol? ne) 
         (error 'evaluate-expression "variables not supported yet.")]
        [(add?    ne) (+ (evaluate-expression (add-left ne))
                         (evaluate-expression (add-right ne)))]
        [(mul?    ne) (* (evaluate-expression (mul-left ne))
                         (evaluate-expression (mul-right ne)))]))

;; Exercise 14.4.4
;; When people evaluate an application (f a) they substitute a for f's
;; parameter in f's body. More generally, when people evaluate expressions
;; with variables, they substitute the variables with values.
;; Develop the function subst. The function consumes (the representation 
;; of) a variable (V), a number (N), and (the representation of) a Scheme
;; expression. It produces a structurally equivalent expression in which 
;; all occurrences of V are substituted by N.

;; Symbol Number SE -> SE
;; consumes (the representation of) a variable (V), a number (N), and 
;; (the representation of) a Scheme expression. 
;; produces a structurally equivalent expression in which all occurrences 
;; of V are substituted by N.
(check-expect (subst 'i 5 (make-add (make-add 'i (make-mul 'i 'i)) 3))
              (make-add (make-add 5 (make-mul 5 5)) 3))
(check-expect (subst 'i 5 (make-add (make-add 'i (make-mul 'x 'i)) 3))
              (make-add (make-add 5 (make-mul 'x 5)) 3))
(check-expect (evaluate-expression 
               (subst 'i 5 
                      (make-add (make-add 'i (make-mul 'i 'i)) 3)))
              (evaluate-expression
               (make-add (make-add 5 (make-mul 5 5)) 3)))

(define (subst v n se)
  (cond [(number? se) se]
        [(symbol? se) (if (symbol=? v se)
                          n
                          se)]
        [(add?    se) (make-add (subst v n (add-left  se))
                                (subst v n (add-right se)))]
        [(mul?    se) (make-mul (subst v n (mul-left  se))
                                (subst v n (mul-right se)))]))
