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

(defun do-fact (n)
  (do ((counter n (- counter 1))
       (fact 1 (* counter fact)))
      ((zerop counter) fact)))


(defun do-length (list)
  (do ((lst list (rest lst))
       (counter 0 (+ counter 1)))
      ((null lst) counter)))

(defun do-find (elem list)
  (do ((lst list (rest lst)))
      ((null lst) nil)
    (when (equal elem (first lst)) (return (first lst)))))

(defun rec-reverse (list)
  (labels ((rev (l container)
	     (cond ((null l) container)
		   (t (rev (rest l) (cons (first l) container))))))
    (rev list '())))

(defun it-fact (n)
  (let ((prod 1))
    (dotimes (i n prod)
      (setf prod (* prod (+ i 1))))))

(defun import-all nil
  (load "GISC_Ch_11.lisp")
  (load "dtrace.lisp")
  (load "sdraw.lisp"))

