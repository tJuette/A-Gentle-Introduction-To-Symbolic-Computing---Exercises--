(defun add-prop (symb elem prop)
  (pushnew elem (get symb prop)))

(defun record-meeting (x y)
  (add-prop x y 'has-met)
  (add-prop y x 'has-met)
  t)

;;13.1
(defun subprop (symb sub prop)
  (cond ((member sub (get symb prop))
	 (setf (get symb prop) (remove sub (get symb prop))) t)
	(t nil)))
  
;;13.2
(defun forget-meeting (x y)
  (subprop x y 'has-met)
  (subprop y x 'has-met))

;;13.3
(Defun my-get (symb prob)
  (second (member prob
		  (symbol-plist symb))))

;;13.4
(defun hasprop (symb prop)
  (if (not (eql (get symb prop 'no-prop) 'no-prop))
      t))

;;13.5
;;Arrays are more efficient

;;13.6
;;Lists are easier to build

;;13.7
;;A property list, because it needs to CONS-cells per indicator. A dotted list needs only
;;one CONS-cell per key-value pair.

;;13.8

;;a
(defvar *hist-array* nil)
(defvar *total-points* nil)

;;b
(defun new-histogram (n)
  (setf *hist-array* (make-array n :initial-element 0))
  (setf *total-points* 0))

;;c
(defun record-value (n)
  (cond ((<= 0 n 10) (incf (aref *hist-array* n))
	             (incf *total-points* n)
	             t)
	(t (format t "Value ~A out of range (0-10)!" n))))

;;d
(defun print-hist-line (n)
  (format t "~2@S [~3@S] " n (aref *hist-array* n))
  (dotimes (i (aref *hist-array* n)) (format t "*"))
  (format t "~%"))

;;e
(defun print-histogram nil
  (dotimes (i 11)
    (print-hist-line i))
  (format t "    ~A total.~%~%" *total-points*))

;;self
(defun make-hist (n)
  (dotimes (i n)
    (new-histogram 11)
    (dotimes (i 200)
      (record-value (random 11)))
    (print-histogram)))


(defun import-all nil
  (load "./GISC_Ch_13.lisp")
  (load "./SDRAW.lisp")
  (load "./DTRACE.lisp"))
