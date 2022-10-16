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

;; stuff
(defun bad-style (n)
  (format t "~S is now " n)
  (setf n (- n 2))
  (format t "~S" n))

(defun good-style (n)
  (let ((new (- n 2)))
    (format t "~S is now ~S" n new)))

(defun get-name nil
  (let ((last-name nil)
	(first-name nil)
	(middle-name nil)
	(title nil))
    (format t "~&Last name? ")
    (setf last-name (read))
    (format t "~&First name? ")
    (setf first-name (read))
    (format t "~&Middle name or initial? ")
    (setf middle-name (read))
    (format t "~&Prefered title? ")
    (setf title (read))
    (list title first-name middle-name last-name)))

(defun picky-multiply (x y)
  (when (evenp x)
    (incf x)
    (format t "~&Changing X to ~A to make it odd." x))
  (unless (evenp y)
    (decf y)
    (format t "~&Changing Y to ~A to make it even." y))
  (format t "~&The product of ~A and ~A is ~A." x y (* x y)))

;; TicTacToe
(defparameter *computer* 10)
(defparameter *opponent* 1)
(defparameter *triplets*
  '((1 2 3) (4 5 6) (7 8 9)
    (1 4 7) (2 5 8) (3 6 9)
    (1 5 9) (3 5 7)))

(defun make-board nil
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (n)
  (cond ((equal n 10) "X")
	((equal n 1) "O")
	(t " ")))

(defun print-row (a b c)
  (format t "~& ~A | ~A | ~A "
	  (convert-to-letter a)
	  (convert-to-letter b)
	  (convert-to-letter c)))

(defun print-board (board)
  (format t "~%")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&-----------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&-----------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

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
    (or (member (* 3 *opponent*) sums)
	(member (* 3 *computer*) sums))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun play-one-game nil
  (if (yes-or-no-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))
    
(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move *opponent* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
	  ((board-full-p new-board) (format t "~&Tie game!"))
	  (t (computer-move new-board)))))

(defun computer-move (board)
  (let* ((best-move (chose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~S" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
	  ((board-full-p new-board) (format t "~&Tie game!"))
	  (t (opponent-move new-board)))))

(defun chose-best-move (board)
  (let* ((win (winning-move-strategy board 20))
	 (counter (winning-move-strategy board 2))
	 (center (zerop (sixth board)))
	 (got-center (equal 10 (nth 5 board)))
	 (attack-from-center (check-surroundings board 2))
	 (corner (find-corner board '(1 3 7 9))))
    (cond (win (list win "winning-move"))
	  (counter (list counter "counter-move"))
	  ((and got-center attack-from-center)
	    (list attack-from-center "counter-squeeze")) 
	  (center (list 5 "center-move"))
	  (corner (list corner "corner-move"))
        (t (random-move-strategy board)))))

(defun random-move-strategy (board)
  (let ((move (+ 1 (random 9))))
    (cond ((equal 0 (nth move board)) (list move "random-move"))
	  (t (random-move-strategy board)))))

(defun find-winning-move (triplet board)
  (cond ((zerop (nth (first triplet) board)) (first triplet))
	(t (find-winning-move (rest triplet) board))))

(defun check-surroundings (board n)
  (cond ((equal 10 n) nil)
	((zerop (nth n board)) n)
	(t (check-surroundings board (+ n 2)))))
	   

(defun find-corner (board corners)
  (cond ((null corners) nil)
	((zerop (nth (first corners) board)) (first corners))
	(t (find-corner board (rest corners)))))

(defun winning-move-strategy (board sum)
  (let* ((sums (compute-sums board))
	 (win (member sum sums))
	 (w-pos (- (length *triplets*) (length win)))
	 (w-triplet (nth w-pos *triplets*)))
    (cond ((not win) nil)
	  (t (find-winning-move w-triplet board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((input (read)))
    (cond ((not (and (integerp input)
		     (<= 1 input 9)))
	   (format t "~&Invalid input!")
	   (read-a-legal-move board))
          ((< 0 (nth input board)) (format t "~&That field is already occupied!")
	                           (read-a-legal-move board))
	  (t input))))

(defun replace-nth (subst n lst)
  "Returns the list with the nth element substituted"
  (cond ((zerop n) (cons subst (rest lst)))
	(t (cons (first lst) (replace-nth subst (- n 1) (rest lst))))))

;; 10.5
(defun elegant (x y)
  (let* ((maxi (max x y))
	 (avg (/ (+ x y) 2.0))
	 (pct (* 100 (/ avg maxi))))
    (list 'average avg 'is pct 'percent 'of 'max maxi)))

;; 10.6
;; (defparameter x nil) => (push x x) => (push x x) => (push x x) = (((nil))) f
;; = (((nil) nil) (nil) nil)


;; 10.7
;; It is attempting to assign a value to another value instead of a variable
;; Book: setf needs a place that has a pointer.

(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
	 (result
	   (cond ((> commission 100) 'rich)
		 ((< commission 100) 'poor))))
    (format t "~&I predict you will be ~S" result)))

(defun average (x y)
  (unless (and (numberp x) (numberp y))
    (error  "~&Arguments ~S and ~S must both be numbers" x y))
  (/ (+ x y) 2))

(defun import-all nil
  (load "GISC_Ch_10.lisp")
  (load "dtrace.lisp")
  (load "sdraw.lisp"))
