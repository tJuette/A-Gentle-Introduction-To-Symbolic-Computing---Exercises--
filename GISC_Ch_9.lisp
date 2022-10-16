;; 9.1
(defun three-lines nil
  (format t "~&There are old pilots,")
  (format t "~&and there are bold pilots,")
  (format t "~&but there are no old bold pilots."))

;; 9.2
(defun draw-line (n)
  (cond ((< n 1) (format t "~%"))
	(t (format t "*")
	   (draw-line (- n 1)))))

;; 9.3
(defun draw-box (n l)
  (cond ((= l 1) (draw-line n))
	(t (draw-line n)
	   (draw-box n (- l 1)))))

;; 9.4
(defun ninety-nine-bottles (n)
  (cond ((= n 1) (format t "~&One last bottle of beer on the wall!")
	         (format t "~&One last bottle of beer!")
	         (format t "~&Take it down,")
	         (format t "~&pass it around,")
	         (format t "~&No more bottles of beer on the wall.~%~%"))
	((= n 2) (format t "~&~A bottles of beer on the wall," n)
	         (format t "~&~A bottles of beer!" n)
	         (format t "~&Take one down,")
	         (format t "~&pass it around,")
	         (format t "~&One more bottle of beer on the wall.~%~%")
	         (ninety-nine-bottles (- n 1)))
	(t (format t "~&~A bottles of beer on the wall," n)
	   (format t "~&~A bottles of beer!" n)
	   (format t "~&Take one down,")
	   (format t "~&pass it around,")
	   (format t "~&~A bottles of beer on the wall.~%~%" (- n 1))
	   (ninety-nine-bottles (- n 1)))))

;; 9.5
(defun format-nil (x)
  (if x x " "))

(defun print-board-row (lst)
  (format t "~& ~A | ~A | ~A " (first lst) (second lst) (third lst)))

(defun print-board (lst)
  (cond ((null lst) nil)
        (t (print-board-row (mapcar #'format-nil (subseq lst 0 3)))
	   (if (> (length lst) 3) (format t "~&-----------"))
	   (print-board (nthcdr 3 lst)))))

;; 9.6
(defun ask-for-number (msg)
  (format t "~&~A" msg)
  (let ((var (read)))
    (cond ((numberp var) var)
	  (t (format t "You have to enter a number!")
	     (ask-for-number msg)))))

(defun gross-wage nil
  (let ((wage (ask-for-number "Please type in the wage you earn per hour: "))
	(hours (ask-for-number "Please type in the hours you worked today: ")))
    (format t "~&You have earned ~A $!" (* hours wage))))

;; 9.7
(defun cookie-monster nil
  (format t "~&Give me cookie!!! ~&Cookie? ")
  (let ((input (read)))
    (cond ((equal input 'cookie) (format t "Thank you!...Munch munch munch...BURP"))
	  (t (format t "No want ~A~%~%" input)
	     (cookie-monster)))))

(defun get-tree-data nil
  (with-open-file (stream "./practice-data/testwrite.txt")
    (let* ((tree-loc (read stream))
	   (tree-table (read stream))
	   (num-trees (read stream)))
      (format t "~&There are ~S trees on ~S." num-trees tree-loc)
      (format t "~&They are ~S~%" tree-table))))

(defun save-tree-data (tree-loc tree-table num-trees)
  (with-open-file (stream "./practice-data/testwrite.txt" :direction :output)
    (format stream "~S~%" tree-loc)
    (format stream "~S~%" tree-table)
    (format stream "~S~%" num-trees)))
    
    

;; 9.8
;; Strings are self-evaluating, unlike every symbol except T and NIL.

;; 9.9
;; a'b      false: aB
;; NIL
;; always
;; broke
;; NIL
;; 'alpha'bet false: ALPHABET
;; NIL

;; 9.10a
(defun space-over (n)
  (cond ((< n 0) (format t "Error!"))
	((= n 0) "")
	(t (format t " ")
	   (space-over (- n 1)))))

(defun test-sp (n)
  (format t "~&>>>")
  (space-over n)
  (format t "<<<~%"))

;; 9.10b
(defun plot-one-point (plotting-string y-val)
  (format t "~A~A~%" (space-over y-val) plotting-string))

;; 9.10c
(defun plot-points (str lst)
  (mapcar #'(lambda (n) (plot-one-point str n)) lst))

(defun plot-points-rec (str lst)
  (cond ((null lst) nil)
	(t (plot-one-point str (first lst))
	   (plot-points-rec str (rest lst)))))

;; 9.10d
(defun generate (m n)
  (labels ((gen (start end result)
	     (cond ((> start end) result)
		   (t (gen start (- end 1) (cons end result))))))
    (gen m n '())))

;; 9.10e
(defun make-graph (func start end plotting-string)
  (plot-points-rec plotting-string
                   (mapcar func
			   (generate start end))))

;; 9.10f
(defun square (n)
  (* n n))

;; 9.11
(defun dot-prin1 (tree)
  (cond ((atom tree) (format t "~S" tree))
	(t (format t "(")
	   (dot-prin1 (car tree))
	   (format t " . ")
	   (dot-prin1 (cdr tree))
	   (format t ")"))))

;; 9.12
;; CL-USER> (dot-prin1 '(A . (B . C)))
;; (A . (B . C))
;; NIL

;; 9.13
;; CL-USER> '(a . b) => (A . B)

;; 9.14
;; Both cause infinite loops

;; 9.15
(defun hybrid-print-car (x)
  (format t "(")
  (hybrid-prin1 x))

(defun hybrid-print-cdr (x)
  (cond ((null x) (format t ")"))
        ((atom x) (format t " . ~S) " x))
	(t (format t " ")
	   (hybrid-prin1 (car x))
	   (hybrid-print-cdr (cdr x)))))

(defun hybrid-prin1 (x)
  (cond ((atom x) (format t "~S" x))
	(t (hybrid-print-car (car x))
	   (hybrid-print-cdr (cdr x)))))

;; End

(defun print-full-name (lst)
  (format t "~&~10S~S" (first lst) (second lst)))

(defparameter club '((steve smith) (peter griffin) (weyland smithers)))

(defun print-table (table)
  (mapcar #'print-full-name table) 'done)


(defun print-document (path)
  (with-open-file (stream path)
    (let ((eof (list '$eof$)))
      (labels ((read-doc nil
                 (let ((line (read stream nil eof)))
		   (cond ((eq line eof) nil)
	                 (t (format t "~&~S" line)
	                    (read-doc))))))
	(read-doc)))))
      

;; Functions for pyramid-printing

(defun print-whitespace (n)
  (cond ((zerop n) nil)
	(t (format t " ")
	   (print-whitespace (- n 1)))))

(defun print-star (n)
  (cond ((zerop n) nil)
	(t (format t "*")
	   (print-star (- n 1)))))

(defun print-pyramid-row (stars whitespace)
  (print-whitespace whitespace)
  (print-star stars)
  (print-whitespace whitespace)
  (format t "~%"))

(defun print-pyramid (n)
  (labels ((pyr (start end)
	     (let ((whitespace (/ (- end start) 2)))
	       (cond ((> start end) nil)
		     (t (print-pyramid-row start whitespace)
			(pyr (+ start 2) end))))))
    (pyr 1 (if (oddp n) n (+ n 1)))))

(defun print-trunk (n)
  (let* ((raw (round (* 0.75 n)))
	 (better (if (evenp raw) raw (+ raw 1)))
	 (width (- n better))
	 (whitespace (/ (- n width) 2))
	 (height (floor (/ n 2))))
    (labels ((pt (w ws h)
	       (cond ((zerop h) nil)
		     (t (print-pyramid-row w ws)
			(pt w ws (- h 1))))))
      (pt width whitespace height))))
    
(defun print-tree (n)
  (print-pyramid n)
  (print-trunk (if (oddp n) n (+ n 1))))


(defun import-all nil
  (load "GISC_Ch_9.lisp")
  (load "dtrace.lisp")
  (load "sdraw.lisp"))
