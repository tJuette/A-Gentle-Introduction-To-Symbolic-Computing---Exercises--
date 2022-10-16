;; 8.1
(defun any-oddp (lst)
  (cond ((null lst) nil)
	((oddp (car lst)) t)
	(t (any-oddp (cdr lst)))))
;; (any-oddp '(3142 5798 6550 8914)) => ((oddp (car lst)) t) is never true here

;; 8.2
(defun any-oddp-2 (lst)
  (if lst
      (if (oddp (first lst))
	  t
	  (any-oddp-2 (rest lst)))))

;; 8.3
(defun fact (n)
  (cond ((zerop n) 1)
	(t (* n (fact (- n 1))))))

;; 8.4
(defun laugh (n)
  (cond ((< n 1) nil)
	(t (cons 'ha (laugh (- n 1))))))

;; 8.5
(defun add-up (lst)
  (cond ((null lst) 0)
	(t (+ (first lst)
	      (add-up (rest lst))))))

;; 8.6
(defun all-oddp (lst)
  (cond ((null lst) t)
	((evenp (first lst)) nil)
	(t (all-oddp (rest lst)))))

;; 8.7
(defun rec-member (x lst)
  (cond ((null lst) nil)
	((equal x (first lst)) lst)
	(t (rec-member x (rest lst)))))

;; 8.8
(defun rec-assoc (x lst)
  (cond ((null lst) nil)
	((equal x (caar lst)) (car lst))
	(t (rec-assoc x (cdr lst)))))

;; 8.9
(defun rec-nth (n lst)
  (cond ((null lst) nil)
	((zerop n) (first lst))
	(t (rec-nth (- n 1) (rest lst)))))

;; 8.10
(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

(defun rec-plus (a b)
  (cond ((zerop b) a)
	(t (rec-plus (add1 a) (sub1 b)))))

(defun rec-minus (a b)
  (cond ((zerop b) a)
	(t (rec-minus (sub1 a) (sub1 b)))))

;; 8.11
(defun fib (n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(t (+ (fib (- n 2))
	      (fib (- n 1))))))

;; 8.12
;; (any-7-p '(3 4 5 6 7)) => t
;; (any-7-p '(3 4 5 6)) => infinite recursion

;; 8.13
;; A negative integer.

;; 8.14
(defun infinite nil
  (infinite))

;; 8.15
;; car = x, cdr = (x . #x .), (count-slices) would recur infinitively

;; 8.16
;; If the list does not contain any odd numbers, switching the first two
;; conditions will cause an error when oddp is applied to nil.

;; 8.17
(defun first-odd (lst)
  (cond ((null lst) nil)
	((oddp (first lst)) (first lst))
	(t (first-odd (rest lst)))))

;; 8.18
(defun last-element (lst)
  (cond ((atom (rest lst)) (first lst))
	(t (last-element (rest lst)))))

;; 8.19
;; It would work correctly for lists that contained an odd number and recurse
;; infinitely for lists without even numbers.

;; 8.20
;; Single-Test Augmenting Recursion
;; Func      : FACT
;; End-test  : (ZEROP X)
;; End-value : 1
;; Aug-fun   : *
;; Aug-val   : X
;; Reduced X : (- X 1)

;; 8.21
(defun add-nums (n)
  (cond ((zerop n) 0)
	(t (+ n (add-nums (- n 1))))))

;; 8.22
;; Double-Test Tail Recursion
(defun all-equal (lst)
  (cond ((atom (rest lst)) t)
	((not (equal (first lst) (second lst))) nil)
	(t (all-equal (rest lst)))))

;; 8.23
;;   N   | 1 arg  | 2 arg         | return
;;   5      'ha    '(HA HA HA HA)   '(HA HA HA HA HA)
;;   4      'ha    '(HA HA HA)      '(HA HA HA HA)
;;   3      'ha    '(HA HA)         '(HA HA HA)
;;   2      'ha    '(HA)            '(HA HA)
;;   1      'ha    '()              '(HA)
;;   0       --     --              '()

;; 8.24
(defun count-down (n)
  (cond ((zerop n) '())
	(t (cons n (count-down (- n 1))))))

;; 8.25
(defun app-fact (n)
  (reduce #'* (count-down n)))

;; 8.26
(defun cnt-dwn-to-zero-1 (n)
  (cond ((zerop n) '(0))
	(t (cons n (cnt-dwn-to-zero-1 (- n 1))))))

(defun cnt-dwn-to-zero-2 (n)
  (cond ((< n 0) '())
	(t (cons n (cnt-dwn-to-zero-2 (- n 1))))))

;; 8.27
(defun square-list (lst)
  (cond ((null lst) '())
	(t (cons (* (first lst)
		    (first lst))
		 (square-list (rest lst))))))

;; 8.28
(defun my-nth (n lst)
  (cond ((null lst) nil)
	((zerop n) (first lst))
	(t (my-nth (- n 1) (rest lst)))))

;; 8.29
(defun my-member (x lst)
  (cond ((null lst) nil)
	((equal x (first lst)) lst)
	(t (my-member x (rest lst)))))

;; 8.30
(defun my-assoc (x lst)
  (cond ((null lst) nil)
	((equal x (caar lst)) (car lst))
	(t (my-assoc x (cdr lst)))))

;; 8.31
(defun compare-lengths (l-1 l-2)
  (cond ((and (null l-1) (null l-2)) '(same length))
	((null l-2) '(first is longer))
	((null l-1) '(second is longer))
	(t (compare-lengths (rest l-1) (rest l-2)))))

;; 8.32
(defun sum-nummeric-elements (lst)
  (cond ((null lst) 0)
	((numberp (first lst)) (+ (first lst)
				  (sum-nummeric-elements (rest lst))))
	(t (sum-nummeric-elements (rest lst)))))

;; 8.33
(defun my-remove (el lst)
  (cond ((null lst) '())
	((equal el (first lst)) (my-remove el (rest lst)))
	(t (cons (first lst) (my-remove el (rest lst))))))

;; 8.34
(defun my-intersection (l-1 l-2)
  (cond ((null l-1) '())
	((member (first l-1) l-2) (cons (first l-1)
					(my-intersection (rest l-1) l-2)))
	(t (my-intersection (rest l-1) l-2))))

;; 8.35
(defun my-set-difference (l-1 l-2)
  (cond ((null l-1) '())
	((member (first l-1) l-2) (my-set-difference (rest l-1) l-2))
	(t (cons (first l-1)
		 (my-set-difference (rest l-1) l-2)))))

;; 8.36
(defun count-odd-1 (lst)
  (cond ((null lst) 0)
	((oddp (first lst)) (+ 1 (count-odd-1 (rest lst))))
	(t (count-odd-1 (rest lst)))))

(defun count-odd-2 (lst)
  (cond ((null lst) 0)
	(t (+ (if (oddp (first lst))
		  1
		  0)
	      (count-odd-2 (rest lst))))))

;; 8.37
(defun combine (a b)
  (+ a b))

(defun fib-2 (n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(t (combine (fib-2 (- n 2))
	            (fib-2 (- n 1))))))

;; The result of terminal calls is passed to COMBINE, the result of
;; non-terminal calls to FIB-2.

(defun find-number (tree)
  (cond ((numberp tree) tree)
	((atom tree) nil)
	(t (or (find-number (car tree))
	       (find-number (cdr tree))))))

(defun atoms-to-q (tree)
  (cond ((null tree) nil)
	((atom tree) 'q)
	(t (cons (atoms-to-q (car tree))
		 (atoms-to-q (cdr tree))))))

;; 8.38
;; Additional q's for each nil in the original tree

;; 8.39
(defun count-atoms (tree)
  (cond ((atom tree) 1)
	(t (combine (count-atoms (car tree))
		    (count-atoms (cdr tree))))))

;; 8.40
(defun count-cons (tree)
  (cond ((atom tree) 0)
	(t (+ 1
	      (count-cons (car tree))
	      (count-cons (cdr tree))))))

;; 8.41
(defun sum-tree (tree)
  (cond ((numberp tree) tree)
	((atom tree) 0)
	(t (+ (sum-tree (car tree))
	      (sum-tree (cdr tree))))))

;; 8.42
(defun my-subst (new old tree)
  (cond ((null tree) nil)
	((equal old tree) new)
	((atom tree) tree)
	(t (cons (my-subst new old (car tree))
		 (my-subst new old (cdr tree))))))

;; 8.43
(defun flatten (tree)
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(t (append (flatten (car tree))
		   (flatten (cdr tree))))))

;; 8.44
(defun tree-depth (tree)
  (cond ((atom tree) 0)
	(t (+ 1 (max (tree-depth (car tree))
		     (tree-depth (cdr tree)))))))

;; 8.45
(defun paren-depth (tree)
  (cond ((atom tree) 0)
	(t (max (+ 1 (paren-depth (car tree)))
	        (paren-depth (cdr tree))))))

;; 8.46
(defun count-up (n)
  (cond ((zerop n) nil)
	(t (append (count-up (- n 1)) (list n)))))

;; 8.47
(defun make-loaf (n)
  (cond ((zerop n) nil)
	(t (cons 'x (make-loaf (- n 1))))))

;; 8.48
(defun bury (x n)
  (cond ((zerop n) x)
	(t (bury (cons x nil) (- n 1)))))

;; 8.49
(defun pairings (l-1 l-2)
  (cond ((null l-1) nil)
	(t (cons (list (first l-1)
		       (first l-2))
		 (pairings (rest l-1)
			   (rest l-2))))))

;; 8.50
(defun sublists (lst)
  (cond ((null lst) nil)
	(t (cons lst
		 (sublists (rest lst))))))

;; 8.51
(defun my-reverse-recursively (lst n-lst)
  (cond ((null lst) n-lst)
	(t (my-reverse-recursively (rest lst)
				   (cons (first lst)
					 n-lst)))))

(defun my-reverse (lst)
  (my-reverse-recursively lst nil))

;; 8.52
(defun my-union (l-1 l-2)
  (cond ((null l-2) l-1)
	((member (first l-2) l-1) (my-union l-1 (rest l-2)))
	(t (my-union (append l-1 (list (first l-2))) (rest l-2)))))
  
;; 8.53
(defun largest-even (lst)
  (cond ((null lst) 0)
	((oddp (first lst)) (largest-even (rest lst)))
	(t (max (first lst) (largest-even (rest lst))))))

;; 8.54
(defun huge-helper (base exponent)
  (cond ((= 1 exponent) base)
	(t (* base
	      (huge-helper base
			   (- exponent 1))))))

(defun huge (n)
  (huge-helper n n))

;; 8.55
;; A recursive function might call itself, a non-recursive function will not.

;; 8.56
(defun every-other (lst)
  (cond ((null lst) nil)
	(t (cons (first lst)
		 (every-other (nthcdr 2 lst))))))

;; 8.57
(defun left-half-helper (lst n)
  (cond ((>= n (length lst)) nil)
	(t (append (list (first lst))
		   (left-half-helper (rest lst) n)))))

(defun left-half (lst)
  (left-half-helper lst (/ (length lst) 2)))

;; 8.58
(defun merge-lists-helper (lst)
  (let ((smallest (apply #'min lst)))
    (cond ((= 1 (length lst)) (list (first lst)))
	  (t (cons smallest
		   (merge-lists-helper (remove smallest lst :count 1)))))))
  
(defun merge-lists (l-1 l-2)
  (merge-lists-helper (append l-1 l-2)))

;; Book-solution:
(defun merg-lists (l-1 l-2)
  (cond ;((null l-1) l-1)
	;((null l-2) l-2)
	((null l-1) (list (first l-2))) ; My alteration, since it does
	((null l-2) (list (first l-1))) ; not work otherwise
	((< (first l-1) (first l-2)) (cons (first l-1) (merg-lists (rest l-1) l-2)))
	(t (cons (first l-2) (merg-lists l-1 (rest l-2))))))

;; 8.59
;; It works only for factorial(0), all other inputs do not move the input
;; closer to the terminal call, but further away.

(defun de-fact (n)
  (cond ((zerop n) (break "N is 0"))
	(t (* n (de-fact (- n 1))))))

;; 8.60
(defvar family '((colin nil nil)
		 (deirdre nil nil)
		 (arthur nil nil)
		 (kate nil nil)
		 (frank nil nil)
		 (linda nil nil)
		 (suzanne colin deirdre)
		 (bruce arthur kate)
		 (charles arthur kate)
		 (david arthur kate)
		 (ellen arthur kate)
		 (george frank linda)
		 (hillary frank linda)
		 (andre nil nil)
		 (tamara bruce suzanne)
		 (vincent bruce suzanne)
		 (wanda nil nil)
		 (ivan george ellen)
		 (julie george ellen)
		 (marie george ellen)
		 (nigel andre hillary)
		 (frederick nil tamara)
		 (zelda vincent wanda)
		 (joshua ivan wanda)
		 (quentin nil nil)
		 (robert quentin julie)
		 (olivia nigel marie)
		 (peter nigel marie)
		 (erica nil nil) 
		 (yvette robert zelda)
		 (diane peter erica)))

;; 8.60a
(defun father (name)
  (if name
      (second (assoc name family))))

(defun mother (name)
  (if name
      (third (assoc name family))))

(defun parents (name)
  (if name
      (remove nil (rest (assoc name family)))))

(defun children (name)
  (if name
      (mapcar #'first
              (remove-if-not #'(lambda (set)
				 (member name (rest set)))
			     family))))

;; 8.60b
(defun siblings (name)
  (if name
      (remove name
	      (union (children (father name))
		     (children (mother name))))))

;; 8.60c
(defun mapunion (func lst)
  (reduce #'union
	  (mapcar func lst)))

;; 8.60d
(defun grandparents (name)
  (if name
      (mapunion #'parents (parents name))))

;; 8.60e
(defun cousins (name)
  (if name
      (mapunion #'children
		(mapunion #'siblings (parents name)))))

;; 8.60f
(defun descended-from (name ancestor)
  (cond ((not (parents name)) nil)
	((member ancestor (parents name)) t)
	(t (or (descended-from (first (parents name)) ancestor)
	       (descended-from (second (parents name)) ancestor)))))

;; 8.60g
(defun ancestors (name)
  (cond ((not (parents name)) nil)
	(t (union (parents name)
		  (union (ancestors (father name))
		         (ancestors (mother name)))))))

;; 8.60h
(defun generations (name ancestor gap)
  (cond ((not (descended-from name ancestor)) nil)
        ((member ancestor (parents name)) (+ gap 1))
        ((descended-from (father name) ancestor) (generations (father name)
							      ancestor
							      (+ gap 1)))
	(t (generations (mother name) ancestor (+ gap 1)))))

(defun generation-gap (name ancestor)
  (generations name ancestor 0))

;; 8.60i
;; 1) (descended-from 'robert 'deirdre) -> NIL
;; 2) (ancestors 'yvette)               -> (WANDA VINCENT SUZANNE BRUCE ARTHUR KATE
;;                                          DEIRDRE COLIN LINDA FRANK GEORGE ELLEN
;;                                          QUENTIN JULIE ROBERT ZELDA)
;; 3) (generation-gap 'olivia 'frank)   -> 3
;; 4) (cousins 'peter)                  -> (ROBERT JOSHUA)
;; 5) (grandparents 'olivia)            -> (HILLARY ANDRE GEORGE ELLEN)

;; 8.61
(defun cu-tr (n res)
  (cond ((zerop n) res)
	(t (cu-tr (- n 1) (cons n res)))))

(defun count-up-tr (n)
  (cu-tr n nil))

;; 8.62
(defun f-tr (n res)
  (cond ((zerop n) res)
	(t (f-tr (- n 1) (* res n)))))

(defun fact-tr (n)
  (f-tr n 1))

;; 8.63
(defun union-tr (l-1 l-2)
  (cond ((null l-1) l-2)
	((member (first l-1) l-2) (union-tr (rest l-1) l-2))
	(t (union-tr (rest l-1) (cons (first l-1) l-2)))))

(defun i-tr (l-1 l-2 res)
  (cond ((null l-1) res)
	((not (member (first l-1) l-2)) (i-tr (rest l-1) l-2 res))
	(t (i-tr (rest l-1) l-2 (cons (first l-1) res)))))

(defun intersection-tr (l-1 l-2)
  (i-tr l-1 l-2 nil))

(defun sd-tr (l-1 l-2 res)
  (cond ((null l-1) res)
	((member (first l-1) l-2) (sd-tr (rest l-1) l-2 res))
	(t (sd-tr (rest l-1) l-2 (cons (first l-1) res)))))

(defun set-difference-tr (l-1 l-2)
  (sd-tr l-1 l-2 nil))

;; 8.64
(defun tree-find-if (func tree)
  (cond ;((and (numberp tree) (funcall func tree)) tree)
        ((and tree (atom tree) (funcall func tree)) tree) ; book solution
        ((atom tree) nil)
	(t (or (tree-find-if func (car tree))
	       (tree-find-if func (cdr tree))))))

;; The function is more variable if in the first condition (numberp tree)
;; is replaced with tree and (atom tree).

;; 8.65
(defun tr-count-slices (lst)
  (labels ((tr-cs (lst result)
	     (cond ((null lst) result)
		   (t (tr-cs (rest lst) (+ result 1))))))
    (tr-cs lst 0)))

(defun tr-reverse (lst)
  (labels ((tr-r (old new)
	     (cond ((null old) new)
		   (t (tr-r (rest old) (cons (first old) new))))))
    (tr-r lst '())))

;; 8.66
(defun arith-eval (expr)
  (cond ((numberp expr) expr)
	(t (funcall (second expr)
		    (arith-eval (first expr))
		    (arith-eval (third expr))))))

;; 8.67
(defun legalp (expr)
  (cond ((numberp expr) t)
	((atom expr) nil)
	(t (and (= 3 (length expr))  ; Had forgotten about this condition
	        (legalp (first expr))
	        (member (second expr) '(+ - * /))
	        (legalp (third expr))))))

;; 8.68
;; ...CDR points to a proper list.

;; 8.69
;; Me   : Integers larger then 1 are either primes or composites
;;        that can be divided into primes.
;; Book : A positive integer greater than one is either a prime, or the
;;        product of a prime and a positive integer greater than one.

;; 8.70
(defun factors (n)
  (labels ((facs (n p)
	     (cond ((= 1 n) nil)
		   ((zerop (rem n p)) (cons p (facs (/ n p)  p)))
		   (t (facs n (+ p 1))))))
    (facs n 2)))

(defun factor-tree (n)
  (labels ((facs (n p)
	     (cond ((= n p) n)
		   ((zerop (rem n p)) (list n p (facs (/ n p) p)))
		   (t (facs n (+ p 1))))))
    (facs n 2)))

;; 8.71
;;    [*|*]
;;    /   \
;;   A     [*|*]
;;         /   \
;;        B     [*|*]
;;              /   \
;;         [*|*]     [*|*]
;;         /   \     /   \
;;        C  [*|*]  E    NIL
;;           /   \
;;          D    NIL
;;
;; Terminal nodes   : D, E
;; Nonterimal nodes : A, B, C
;; Right Answer     : A, B, C, D, E and NIL are terminal nodes,
;;                    the nonterminal ones are the CONS cells.

;; 8.72
;; Right Answer: A book can be described as a tree structure whose nodes have a
;;               varying number of branches. The nonterminal nodes are chapters,
;;               sections, subsections, paragraphs, sentences and words. The terminal
;;               nodes are the characters.

(defun import-all nil
  (load "GISC_Ch_8.lisp")
  (load "dtrace.lisp")
  (load "sdraw.lisp"))
