(defun average (a b)
  (let ((sum (+ a b)))
    (list a b 'average 'is (/ sum 2.0))))

;5.1
(defun good-style (q)
  (let ((p (+ q 5)))
    p))

(defun price-change (old new)
  (let* ((difference (- new old))
         (proportion (/ difference old))
         (percentage (* proportion 100.0)))
    (list 'widgets 'changed 'by percentage 'percent!)))

(defun fair-coin ()
  (let ((toss (random 101)))
    (cond ((< toss 50) 'tails)
          ((> toss 50) 'heads)
          (t 'edge))))
;;; 5.6
;;; a
(defun throw-die ()
  "Generates a random number from 1 to 6 inclusive"
  (+ (random 6) 1))
;;; b
(defun throw-dice ()
  "Generates a list with two random numbers from 1 to 6 inclusive"
  (let ((throw (list (throw-die) (throw-die))))
    throw))
;;; c
(defun box-cars-p (throw)
  "Checks if both random numbers equal 6"
  (and (equal (car throw) 6)
       (equal (cadr throw) 6))
  "Checks if both random numbers equal 1")
(defun snake-eyes-p (throw)
  (and (equal (car throw) 1)
       (equal (cadr throw) 1)))
;;; d
(defun instant-win (throw)
  "Returns T when the sum of throw-dice equals 7 or 11"
  (let ((sum (+ (car throw) 
                (cadr throw))))
    (cond ((equal sum 7) t)
          ((equal sum 11) t))))
(defun instant-loss (throw)
  "Returns T when the sum of throw-dice equals 2, 3 or 12"
  (let ((sum (+ (car throw)
                (cadr throw))))
    (cond ((equal sum 2) t)
          ((equal sum 3) t)
          ((equal sum 12) t))))
;;; e
(defun say-throw (throw)
  "Returns the sum of tow random numbers from 1 to 6 and snake-eyes 
  if the sum is 2 or boxcars if the sum is 12"
  (let ((sum (+ (car throw) 
                (cadr throw))))
    (cond ((equal sum 2) 'snake-eyes)
          ((equal sum 12) 'boxcars)
          (t sum))))
;;; f
(defun craps ()
  "Generates two random numbers between 1 and 6, calculates the sum and
  the result according to craps rules"
  (let* ((throw (throw-dice))
         (die-1 (car throw))
         (die-2 (cadr throw))
         (sum (+ die-1 die-2)))
    (cond ((instant-win throw) (list 'throw die-1 'and die-2 '-- (say-throw throw) '-- 'you 'win))
          ((instant-loss throw) (list 'throw die-1 'and die-2 '-- (say-throw throw) '-- 'you 'lose))
          (t (list 'throw die-1 'and die-2 '-- (say-throw throw) '-- 'your 'point 'is sum)))))
;;; g
(defun try-for-point (point)
  "Generates a random number and compares it to the given point to determine
  the outcome: 7 loss, point win, everything else: try again."
  (let* ((throw (throw-dice))
         (sum (+ (car throw)
                 (cadr throw))))
    (cond ((equal sum point) '(you win))
          ((equal sum 7) '(you lose))
          (t '(try again)))))
