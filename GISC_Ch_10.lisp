(defparameter *total-glasses* 0)
(defparameter *friends* nil)
(defparameter *reencounters* 0)

(defun sell (n)
  (setf *total-glasses* (+ *total-glasses* n))
  (format t "That makes ~D total glasses!~%" *total-glasses*))

(defun meet (person)
  (cond ((eql person (first *friends*)) "We just met.")
	((member person *friends*) "We know each other.")
	(t (push person *friends*) "Pleased to meet you.")))

;;10.1
;; When sell tries to add a number to a symbol an error message will occur.

;;10.2
(defun sell-2 (n)
  (incf *total-glasses* n)
  (format t "That makes ~D total glasses!~%" *total-glasses*))

;;10.3
(defun meet-2 (person)
  (cond ((eql person (first *friends*))
           (incf *reencounters*)
	   "We just met.")
	((member person *friends*)
           (incf *reencounters*)
	   "We know each other.")
	(t (push person *friends*) "Pleased to meet you.")))

;;10.4
(defun forget (person)
  (if (member person *friends*)
      (setf *friends* (remove person *friends*))
      (format nil "~S isn't on the friends-list!" person)))

;;10.5
(defun pretty (x y)
  (let* ((whole (if (< x y) y x))
	 (avg (/ (+ x y) 2.0))
	 (pct (* 100 (/ avg whole))))
    (format t "Average ~A is ~A percent of ~A." avg pct whole))) 

;;10.6
;;(x) -> ((x) x) -> (((x) x) (x) x)

;;10.7
;; (length x) is neither an ordinary nor a generalized variable

;;10.8
(defvar *computer* 10)
(defvar *opponent* 1)
(defvar *triplets* '((1 2 3) (4 5 6) (7 8 9)
	             (1 4 7) (2 5 8) (3 6 9)
		     (1 5 9) (3 5 7)))
(defvar *corners* '(1 3 7 9))
(defvar *sides* '(2 4 6 8))

(defun find-all (elem list)
  (cond ((null list) '())
	((member elem list) (cons elem (find-all elem (rest (member elem list)))))
        (t nil)))

(defun convert-field (n)
  (cond ((zerop n) nil)
	((equal 1 n) "O")
	((equal 10 n) "X")))

(defun print-board (board)
  (format t "~% ~A | ~A | ~A ~%" (or (convert-field (second board)) "1")
	                         (or (convert-field (third board))  "2")
	                         (or (convert-field (fourth board)) "3"))
  (format t "-----------~%")
  (format t " ~A | ~A | ~A ~%" (or (convert-field (fifth board)) "4")
	                       (or (convert-field (sixth board)) "5")
	                       (or (convert-field (seventh board)) "6"))
  (format t "-----------~%")
  (format t " ~A | ~A | ~A ~%~%" (or (convert-field (eighth board)) "7")
	                         (or (convert-field (ninth board)) "8")
	                         (or (convert-field (tenth board)) "9")))

(defun make-board nil
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defun sum-triplets (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet) (sum-triplets board triplet)) *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun read-a-legal-move (board)
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9))) (format t "Invalid input.~%")
	                            (read-a-legal-move board))
	  ((not (zerop (nth pos board))) (format t "That position is already taken!~%")
	                                 (read-a-legal-move board))
	  (t pos))))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "Random move"))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos) (zerop (nth pos board))) squares))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if #'(lambda (trip)
			      (equal (sum-triplets board trip)
				     target-sum))
			  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board (* 2 *opponent*))))
    (and pos (list pos "Block opponent"))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "Make three in a row"))))

(defun block-squeeze-play (board)
  (let ((pos (find-empty-position board *sides*)))
    (if (or (= (* 2 *opponent*) (+ (nth 1 board) (nth 9 board)))
            (= (* 2 *opponent*) (+ (nth 3 board) (nth 7 board))))
        (and pos (list pos "Block squeeze play")))))
  
(defun block-two-on-one (board)
  (let ((pos (find-empty-position board *corners*)))
    (if (or (= (* 2 *opponent*) (+ (nth 1 board) (nth 5 board)))
            (= (* 2 *opponent*) (+ (nth 3 board) (nth 5 board)))
	    (= (* 2 *opponent*) (+ (nth 7 board) (nth 5 board)))
            (= (* 2 *opponent*) (+ (nth 9 board) (nth 5 board))))
        (and pos (list pos "Block two on one")))))

(defun take-center (board)
  (let ((pos (zerop (nth 5 board))))
    (and pos (list 5 "Occupy center"))))

(defun attempt-squeeze-play (board)
  (let* ((corner-fields (mapcar #'(lambda (c) (nth c board)) *corners*))
	 (corners-sum (reduce #'+ corner-fields))
	 (opposite-corners '((1 9) (9 1) (3 7) (7 3)))
	 (computer-corners (remove-if-not #'(lambda (c)
					      (= *computer* (nth c board)))
					  *corners*))
	 (free-corners (remove-if-not #'(lambda (c) (zerop (nth c board))) *corners*))
	 (random-corner (nth (+ 1 (random 4)) *corners*))
	 (strategy "Attempt squeeze play"))
    (cond ((zerop corners-sum) (list random-corner strategy))
	  ((= *computer* corners-sum)
	   (list (second (assoc (first computer-corners) opposite-corners)) strategy))
	  ((= (* 2 *computer*) corners-sum) (list (nth (+ 1 (random 2)) free-corners)
						  strategy))
	  (t nil))))
	   
(defun attempt-two-on-one (board)
  (let* ((corner-fields (mapcar (lambda (c) (nth c board)) *corners*))
	 (corner-sum (reduce #'+ corner-fields))
	 (free-corners (remove-if-not #'(lambda (c) (zerop (nth c board))) *corners*))
	 (random-free-corner (nth (random (length free-corners)) free-corners))
	 (center (= *computer* (nth 5 board)))
	 (strategy "Attempt two on one"))
    (cond ((and center (= (+ *computer* *opponent*) corner-sum))
	   (list random-free-corner strategy))
	  (t nil))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (attempt-squeeze-play board)
      (take-center board)
      (random-move-strategy board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move *computer* pos board)))
    (format t "My move: ~A~%" pos)
    (format t "My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "I win!~%"))
	  ((board-full-p new-board) (format t "It's a tie game.~%"))
	  (t (opponent-move new-board)))))

(defun opponent-move (board)
  (print-board board)
  (format t "Your move: ")
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move 1 pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "You win!~%"))
	  ((board-full-p new-board) (format t "It's a tie game.~%"))
	  (t (computer-move new-board)))))
    
(defun play-one-game nil
  (if (y-or-n-p "Would you like to go first?  ")
      (opponent-move (make-board))
      (computer-move (make-board))))


;;10.9
(defun chop (list)
  (cond ((not (null list)) (setf (cdr list) nil) list)
	(t nil)))

;;10.10
(defun ntack (list el)
  (setf (cdr list) (list el)))

;;10.11
;;#x(a b c . #x)

;;10.12
;;The first variant returns a new list (HI HO HI HO) but leaves H untouched, while the
;;second changes H in-place to (HI HO HI HO)

(defun import-all nil
  (load "GISC_Ch_10.lisp")
  (load "dtrace.lisp")
  (load "sdraw.lisp"))
