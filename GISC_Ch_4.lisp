;4.1
(defun make-even (n)
  (if (oddp n) 
      (+ n 1) 
      n))
;4.2
(defun further (n)
  (if (< n 0)
      (- n 1)
      (+ n 1)))
;4.3
(defun my-not (i)
  (if (equal i nil)
      t
      nil))
;4.4   
(defun ordered (a b)
  (if (< a b)
      (list a b)
      (list b a)))
;4.6
(defun my-abs (n)
  (cond ((< n 0) (- n))
        (t n)))
;4.8
(defun emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t (cons 'very (rest x)))))
;4.9
(defun make-odd (n)
  (cond ((oddp n) n)
        (t (+ n 1))))
;4.10
(defun constrain1 (x min max)
  (cond ((> x max) max)
        ((< x min) min)
        (t x)))
(defun constrain2 (x min max)
  (if (> x max)
      max
      (if (< x min)
          min
          x)))
;4.11
(defun firstzero (lst)
  (cond ((equal 0 (first lst)) 'first)
        ((equal 0 (second lst)) 'second)
        ((equal 0 (third lst)) 'third)
        (t 'none)))
;4.12
(defun cycle (n)
  (if (or (< n 1)
          (> n 98))
      1
      (+ n 1)))
;4.13
(defun howcompute (a b x)
  (cond ((equal b (- x a)) '(sum of))
        ((equal b (/ x a)) '(product of))
        (t '(beats me))))
;4.15
(defun geq (a b)
  (if (or (> a b)
          (equal a b))
      t
      nil))
;4.16
(defun sqr-dbl-hlf (n)
  (cond ((and (> n 0) 
              (oddp n)) (* n n))
        ((and (< n 0) 
              (oddp n)) (* n 2))
        (t (/ n 2))))
;4.17
(defun age-gender (sex age)
  (cond ((and (or (equal sex 'boy) 
                  (equal sex 'girl))
              (equal age 'child)))
        ((and (or (equal sex 'man) 
                  (equal sex 'woman))
              (equal age 'adult)))))
;4.18
(defun rock-paper-scissors (player1 player2)
  (cond ((equal player1 player2) 'tie)
        ((or (and (equal player1 'rock)
                  (equal player2 'scissors))
             (and (equal player1 'scissors)
                  (equal player2 'paper))
             (and (equal player1 'paper)
                  (equal player2 'rock))) 'first-wins)
        (t 'second-wins)))
;4.19
;(cond ((not x) nil)
;      ((not y) nil)
;      ((not z) nil)
;      (w t)))
;(if (X)
;    (if (y)
;        (if (z)
;            (if (w) t))))
;4.20
(defun compare-2 (a b)
  (if (> a b) (list 'first 'is 'bigger!)
      (if (< a b) 
          (list 'first 'is 'smaller!)
          (list 'both 'are 'equal!))))
(defun compare-3 (a b)
  (or (and (> a b) (list 'first 'is 'bigger!))
      (and (< a b) (list 'first 'is 'smaller!))
      (list 'both 'are 'equal!)))
;4.21
(defun gtest-2 (x y)
  (if (> x y) t
      (if (zerop x) t
          (if (zerop y) t))))
(defun gtest-3 (x y)
  (cond ((> x y) t)
        ((zerop x) t)
        ((zerop y) t)))
;4.22
(defun boilingp (temp scale)
  (cond ((equal scale 'fahrenheit) (> temp 212))
        ((equal scale 'celsius) (> temp 100))))
(defun boilingp-2 (temp scale)
  (if (equal scale 'fahrenheit) (> temp 212)
      (if (equal scale 'celsius) (> temp 100))))
(defun boilingp-3 (temp scale)
  (or (and (equal scale 'fahrenheit) 
           (> temp 212))
      (and (equal scale 'celsius)
           (> temp 100))))
;4.23
;If COND has eight, it needs seven IF-clauses
;WHERE-IS-3 would need just one OR-Clause and 
;four AND-Clauses.

;4.28
;(defun soph ())
;  (or (and (oddp 5)))) 
;           (not (evenp 7)))))
;      'foo))
;4.29
(defun logical-and (a b)
  (if a      
      (if b t)))
(defun logical-and-2 (a b)
  (cond ((not a) nil)
        ((not b) nil)
        (t t)))
;4.30
(defun logical-or (a b)
  (if a
      t
      (if b
          t)))
(defun logical-or-2 (a b)
  (cond (a t)
        (b t)))
;4.36
(defun nand (a b)
  (not (and a b)))
; (nand t t)     => NIL
; (nand t nil)   => T
; (nand nil t)   => T
; (nand nil nil) => T
;4.37
(defun not-2 (a)
  (nand a a))
(defun logical-and-3 (a b)
  (nand (nand a b)
        (nand a b)))
(defun logical-or-3 (a b)
  (nand (nand a a)
        (nand b b)))
;4.38
(defun nor (a b)
  (not (or a b)))

(defun not-3 (a)
  (nor a a))

(defun logical-and-4 (a b)
  (nor (nor a a)
       (nor b b)))

(defun logical-or-4 (a b)
  (nor (nor a b)
       (nor a b)))

(defun nand-2 (a b)
  (nor (nor (nor a b)
            (nor a b))
       (nor (nor a b)
            (nor a b))))
