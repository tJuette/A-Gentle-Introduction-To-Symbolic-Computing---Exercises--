;;11.1
(defun it-member (el list)
  (dolist (elem list)
    (when (equal el elem)
      (return t))))

;;11.2
(defun it-assoc (el a-list)
  (dolist (sublist a-list)
    (when (equal el (first sublist))
      (return sublist))))

;;11.3
(defun check-all-odd (list)
  (cond ((null list) t)
	((evenp (first list)) (format t "Checking ~A...~%" (first list))
	                      nil)
	(t (format t "Checking ~A...~%" (first list))
	   (check-all-odd (rest list)))))

;;11.4
(defun it-length (list)
  (let ((counter 0))
    (dolist (element list counter)
      (incf counter))))

;;11.5
(defun it-nth (n list)
  (dolist (element list)
    (when (zerop n)
      (return element))
    (decf n)))

;;11.6
(defun it-union (list-1 list-2)
  (dolist (element list-1 list-2)
    (unless (member element list-2)
      (push element list-2))))

;;11.7
;;Because the elements are pushed in the front of the list. It can either be returend
;;reversed, or the elements can be put in a list and appended to the result-list 

;;11.8
(defun it-reverse (list)
  (let ((reversed-list '()))
    (dolist (element list reversed-list)
      (push element reversed-list))))

;;11.9
(defun check-all-odd (list)
  (do ((lst list (rest lst)))
      ((null lst) t)
    (format t "Checking ~A...~%" (first lst))
    (when (evenp (first lst)) (return nil))))

;;11.10
(defun launch (n)
  (let ((counter n))
    (dotimes (i n)
      (format t "~A..." counter)
      (when (equal counter 1)
	(format t "Blast off!"))
      (decf counter))))

;;11.11
(defun find-largest (list)
  (let ((largest (first list)))
    (do* ((lst list (rest lst))
	  (next (first lst) (first lst)))
	 ((null lst) largest)
      (when (< largest next)
	(setf largest next)))))

;;11.12
(defun power-of-2 (n)
  (do ((result 1 (* result 2))
       (counter n (- counter 1)))
      ((zerop counter) result)))

;;11.13
(defun first-non-integer (list)
  (dolist (elem list 'none)
    (unless (integerp elem)
      (return elem))))

;;11.14
;;An error like "unbound variable x", since it hasn't been initialized before e

;;11.15
;;In this example the iteration variable for the first element will be the first element of;;the passed list two times in a row, and the loop will end before the last element of the ;;list has been checked.

;;11.16
;;DO offers the possibility of implementing an update-function for each variable.

;;11.17
;;(dotimes (i 5 i) (format t "~A~%" i)) returns  4, false, 5

;;11.18
(defun do-dotimes nil
  (do ((i 0 (+ i 1)))
      ((= 5 i) i)
    (format t "~A~%" i)))
;; In 11.17 5 is the last value of i and is returned as such, just the instructions in the ;; body of the loop aren't executed any more.

;;11.19
;; No, since they are all initialized (and possibly updated) at the same time

;;11.20
;;Yes

;;11.21
(defun it-fib (n)
  (do ((next 1 (+ current next))
       (current 0 next)
       (counter n (- counter 1)))
      ((zerop counter) current)))

;;11.22

;;a
(defun complement-base (b)
  (cond ((eql b 'a) 't)
	((eql b 't) 'a)
	((eql b 'g) 'c)
	((eql b 'c) 'g)))

;;b
(defun complement-strand (s)
  (let ((complement '()))
    (dolist (elem s (reverse complement))
      (push (complement-base elem) complement))))

;;c
(defun make-double (s)
  (let ((double '()))
    (dolist (elem s (reverse double))
      (push (list elem (complement-base elem)) double))))

;;d
(defun count-bases (lst)
  (let ((ad 0)
	(th 0)
	(gu 0)
	(cy 0))
  (dolist (elem lst (list (list 'a ad) (list 't th) (list 'g gu) (list 'c cy)))
    (if (listp elem)
	(cond ((eql (first elem) 'a) (incf ad) (incf th))
	      ((eql (first elem) 't) (incf th) (incf ad))
	      ((eql (first elem) 'g) (incf gu) (incf cy))
	      ((eql (first elem) 'c) (incf cy) (incf gu)))
	(cond ((eql elem 'a) (incf ad))
	      ((eql elem 't) (incf th))
	      ((eql elem 'g) (incf gu))
	      ((eql elem 'c) (incf cy)))))))

;;e
(defun prefixp (pre list)
  (dotimes (i (length pre) t)
    (when (not (eql (nth i pre) (nth i list)))
      (return nil))))

;;f
(defun appearsp (sub list)
  (do ((lst list (rest lst)))
      ((< (length lst) (length sub)) nil)
    (when (prefixp sub lst)
      (return t))))

;;g
(defun coverp (sub list)
  (do ((lst list (nthcdr (length sub) lst)))
      ((null lst) t)
    (unless (prefixp sub lst)
      (return nil))))

;;h
(defun prefix (n list)
  (do ((result '())
       (counter (- n 1) (- counter 1)))
      ((zerop counter) (push (nth 0 list) result) result)
    (push (nth counter list) result)))
	 
;;i
(defun kernel (strand)
  (do* ((list strand (rest list))
	(prefix (list (first list)) (append prefix (list (first list)))))
       ((null list) prefix)
    (when (coverp prefix strand)
      (return prefix))))

;;j
(defun draw-string (cnt string)
  (dotimes (i cnt)
    (format t string))
  (format t "~%"))

(defun draw-bases (strand)
  (dolist (base strand)
    (format t "  ~A  " base))
  (format t  "~%"))
    

(defun draw-dna (strand)
  (draw-string (length strand) "-----")
  (draw-string (length strand) "  !  ")
  (draw-bases strand)
  (draw-string (length strand) "  .  ")
  (draw-string (length strand) "  .  ")
  (draw-bases (complement-strand strand))  
  (draw-string (length strand) "  !  ")
  (draw-string (length strand) "-----"))

(defun import-all nil
  (load "GISC_Ch_11.lisp")
  (load "dtrace.lisp")
  (load "sdraw.lisp"))
		 
