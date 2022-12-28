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

;;13.9
(defvar crypto-text '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf enlpo pib slafml pvv bfwkj"))

;;a
(defvar *encipher-table* (make-hash-table))
(defvar *decipher-table* (make-hash-table))

;;b
(defun make-substitution (deci enci)
  (setf (gethash deci *decipher-table*) enci)
  (setf (gethash enci *encipher-table*) deci))

;;c
(defun undo-substitution (l)
  (let ((enci (gethash l *decipher-table*)))
    (setf (gethash l *decipher-table*) nil)
    (setf (gethash enci *encipher-table*)  nil)))

;;d
(defun clear nil
  (clrhash *decipher-table*)
  (clrhash *encipher-table*))

;;e
(defun decipher-string (str)
  (let* ((str-len (length str))
	 (dec-str (make-string str-len :initial-element #\Space)))
    (dotimes (i str-len)
      (setf (aref dec-str i) (or (gethash (aref str i) *decipher-table*)
				 #\Space)))
    dec-str))

;;f
(defun show-line nil
  (format t "~A~%" (car crypto-text))
  (format t "~A~%" (decipher-string (car crypto-text))))

;;g
(defun show-text (text)
  (let ((len (length (car crypto-text))))
    (dotimes (i len) (format t "-"))
    (format t "~%~%")
    (show-line)
    (format t "~%")
    (dotimes (i len) (format t "-"))
    (format t "~%")))

;;h
(defun get-first-char (l)
  (char-downcase
   (char (format nil "~A" l) 0)))

;;i
(defun read-letter nil
  (let ((letter (read)))
    (cond ((member letter '(undo end)) letter)
	  (t (get-first-char letter)))))

;;j
(defun sub-letter (ch)
  (let ((deci (gethash ch *decipher-table*)))
    (cond (deci (format t "But ~A has been deciphered to ~A already!~%" ch deci))
	  (t (format t "What does ~A decipher to: " ch)
	     (let* ((letter (read-letter))
		    (enci (gethash letter *encipher-table*)))
	       (cond ((not (characterp letter)) (format t "~A is not a letter!~%" letter))
		     (enci (format t "But ~A already deciphers to ~A!~%" letter enci))
		     (t (make-substitution ch letter))))))))
;;k
(defun undo-letter nil
  (format t "Undo which letter: ")
  (let ((letter (read-letter)))
    (if (gethash letter *decipher-table*)
	(undo-substitution letter))))

;;l
(defun solve nil
  (do ((letter #\Space))
      ((equal letter 'end) (format t "Until next time!"))
    (show-text)
    (format t "Substitute which letter: ")
    (setf letter (read-letter))
    (unless (eql letter 'end)
      (if (eql letter 'undo)
	  (undo-letter)
	  (sub-letter letter)))))
     

(defun import-all nil
  (load "./GISC_Ch_13.lisp")
  (load "./SDRAW.lisp")
  (load "./DTRACE.lisp"))
