;; 7.1
(defun add1 (n)
  (+ n 1))

(mapcar #'add1 '(13 5 7 9))

;; 7.2
(defvar daily-planet 
  '((olsen jimmy 123-76-4535 cub-reporter)
    (kent clark 089-52-6787 reporter)
    (lane lois 951-26-1438 reporter)
    (white perry 355-16-7439 editor)))
(mapcar #'third daily-planet)
;;7.3
(mapcar #'zerop '(2 0 3 4 0 -5 -6))
;;7.4
(defun greater-than-fivep (n)
  (> n 5))
;;7.5
(lambda (n) (- n 7))
;;7.6
(lambda (x) 
  (or (eq x t)
      (eq x nil)))
;;7.7
(defun flip (el)
  (if (eq el 'up)
      'down
      'up))
(mapcar #'flip '(up down up up))
;;7.8
(defun my-find (k x)
  (find-if #'(lambda (el) (and (> el (- k 10))
                               (< el (+ k 10)))) x))
;;7.9
(defun find-nested (lst)
  (find-if #'(lambda (el) (and (listp el)
                               (> (length el) 0))) lst)) 
;;7.10
(defvar note-table 
  '((c . 1)
    (c-sharp . 2)
    (d . 3)
    (d-sharp . 4)
    (e . 5)
    (f . 6)
    (f-sharp . 7)
    (g . 8)
    (g-sharp . 9)
    (a . 10)
    (a-sharp . 11)
    (b . 12)))

(defun numbers (notes) 
  (mapcar #'(lambda (el) (cdr (assoc el note-table))) notes))

(defun notes (numbers)
  (mapcar #'(lambda (number)
              (car (rassoc number note-table))) numbers)) 

(defun raise (n numbers)
  (mapcar #'(lambda (el) (+ el n)) numbers))

(defun normalize (numbers)
  (mapcar #'(lambda (n) (if (> n 12) (- n 12) n))  numbers))

(defun transpose (n song)
  (let* ((numbers (numbers song))
         (raised (raise n numbers))
         (normal (normalize raised)))
    (notes normal)))

;;7.11
(defun between-one-and-five (lst)
  (remove-if-not #'(lambda (n) (and (> n 1) (< n 5))) lst))

;;7.12
(defun count-the (lst)
  (length (remove-if-not #'(lambda (word) (eq word 'the)) lst)))

;;7.13
(defun two-elems (lst)
  (remove-if-not #'(lambda (list) (= 2 (length list))) lst))

;;7.14
(defun my-intersection (list-a list-b)
  (remove-if-not #'(lambda (el) (member el list-b)) list-a))

(defun my-union (list-a list-b)
  (append (remove-if #'(lambda (el) (member el list-b)) list-a) list-b))

;;7.15
(defun rank (card)
  (car card))

(defun suit (card)
  (cadr card))

(defvar my-hand 
  '((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))

(defun count-suit (suit hand)
  (length (remove-if-not #'(lambda (card) (equal (suit card) suit)) hand)))
(defvar colors 
  '((clubs black)
    (diamonds red)
    (hearts red)
    (spades black)))
(defun color-of (card)
  (cadr (assoc (suit card) colors)))
(defun first-red (hand)
  (find-if #'(lambda (card) (equal (color-of card) 'red)) hand))
(defun black-cards (hand)
  (remove-if-not #'(lambda (card) (equal (color-of card) 'black)) hand))
(defun what-ranks (suit hand)
  (mapcar #'rank (remove-if-not #'(lambda (card) (equal (suit card) suit)) hand)))
(defvar all-ranks 
  '(2 3 4 5 6 7 8 9 10 jack queen king ace))
(defun higher-rankp (card-1 card-2)
  (if (member (rank card-1) (member (rank card-2) all-ranks))
      t
      nil))
(defun highest-card (hand)
  (find-if #'(lambda (rank) (assoc rank hand)) (reverse all-ranks)))
;; 7.16
(reduce #'union '((a b c) (c d a) (f b d) (g)))
;; 7.17
(defun total-length-1 (lists)
  (length (reduce #'append lists)))
(defun total-length-2 (lists)
  (reduce #'+ (mapcar #'length lists)))
;; 7.19
(defun all-odd (list) 
  (every #'oddp list))
;; 7.20
(defun none-odd (list) 
  (every #'evenp list))
;; 7.21
(defun not-all-odd (list) 
  (if (find-if #'evenp list)
      t
      nil))
;; 7.22
(defun not-none-odd (list) 
  (if (find-if #'oddp list)
      t
      nil))
;; 7.26
(defun my-find-if (predicate list)
  (car (remove-if-not predicate list)))
;; 7.27
(defun my-every (predicate list)
  (null (remove-if predicate list)))

;; 7.29
(defvar database 
  '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

;; a
(defun match-element (symbol-1 symbol-2)
  (or (equal symbol-1 symbol-2)
      (equal symbol-2 '?)))

;; b
(defun match-triple (list-1 list-2)
  (every #'match-element list-1 list-2))

;; c
(defun fetch (query)
  (remove-if-not #'(lambda (entry) (match-triple entry query)) database))

;; d
;; (fetch '(b4 shape ?))
;; (fetch '(? shape brick))
;; (fetch '(b2 ? b3))
;; (fetch '(? color ?))
;; (fetch '(b4 ? ?))

;; e
(defun color-of-block (block)
  (list block 'color '?))

;; f
(defun supporters (block)
  (mapcar #'third (fetch (list block 'supported-by '?))))

;; g
(defun supp-cube (block)
  (member 'cube 
    (mapcar #'(lambda (bl) (third (first (fetch (list bl 'shape '?)))))
      (supporters block))))

;; h
(defun desc1 (block)
  (fetch (list block '? '?)))

;; i
(defun desc2 (block)
  (mapcar #'cdr (desc1 block)))

;; j
(defun description (block) 
  (reduce #'append (desc2 block)))

;; k
;; (description 'b1) 
;; => (shape brick color green size small supported-by b2 supported-by b3)
;; (description 'b4) 
;; => (shape pyramid  color blue size large supported-by b5)

;; l
;; (setf database 
;;   (append database '((b1 composition wood) (b2 composion plastic)))

;; 7.30
(defvar words 
  '((one un) 
    (two deux) 
    (three trois) 
    (four quatre) 
    (five cinq)))
(mapcar #'(lambda (lst word) 
            (append lst (list word))) words '(uno dos tres quatro cinco))

(setq path "D:/Programmieren/Common LISP/GISC_Ch_7.lisp")
