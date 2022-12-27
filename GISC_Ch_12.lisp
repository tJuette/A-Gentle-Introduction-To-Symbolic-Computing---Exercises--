;;12.1
;; CAPTAIN          = Component of the structure-type STARSHIP.
;; :CAPTAIN         = Keyword for the component CAPTAIN in the constuctor function.
;; STARSHIP-CAPTAIN = Accessor-function for the component CAPTAIN.

;;12.2
;;No

;;12.3
;;Symbol, compiled function and starship

;;12.4

;;a
(defstruct node (name nil) (question nil) (yes-action nil) (no-action nil))

;;b
(defvar *node-list* nil)

(defun init nil
  (setf *node-list* nil))

;;c
(defun add-node (nam ques yes no)
  (let ((node (make-node :name nam :question ques :yes-action yes :no-action no)))
    (setf *node-list* (append *node-list* (list node)))
    (node-name node)))

;;d
(defun find-node (name)
  (car (remove-if-not #'(lambda (n) (equal (node-name n) name)) *node-list*)))

;;e
(defun process-node (name &aux (node (find-node name)))
  (cond ((node-p node) (if (y-or-n-p (node-question node))
	                   (node-yes-action node)
		           (node-no-action node)))
	(t (format t "The node ~A hasn't been defined yet." name) nil)))

;;f
(defun run nil
  (do ((current-node (process-node 'start) (process-node current-node)))
      ((stringp current-node) (format t current-node))
    (when (not current-node) (return nil))))

;;g
(defun ask-question (question)
  (format t "~A?~%Your answer: " question)
  (read))

(defun write-new-node nil
  (let* ((name (ask-question "Name as Symbol"))
	 (ques (ask-question "Question as String"))
	 (yes (ask-question "Yes-Action as Symbol"))
	 (no (ask-question "No-Action as Symbol")))
    (add-node name ques yes no)))

;;h
(defun example-nodes nil
  (init)
  (add-node 'start
	    "Does the engine turn over"
	    'engine-turns-over
	    'engine-wont-turn-over)
  (add-node 'engine-turns-over
	    "Will the engine run for any period of time"
	    'engine-will-run-briefly
	    'engine-wont-run)
  (add-node 'engine-wont-run
	    "Is there gas in the tank"
	    'gas-in-tank
	    "Fill the tank and try starting the engine again.")
  (add-node 'engine-wont-turn-over
	    "Do you hear any sound when you turn the key"
	    'sound-when-turn-key
	    'no-sound-when-turn-key)
  (add-node 'no-sound-when-turn-key
	    "Is the battery voltage low"
	    "Replace the battery"
	    'battery-voltage-ok)
  (add-node 'battery-voltage-ok
	    "Are the battery cables loose or dirty"
	    "Clean the cables and thighten the connections"
	    'battery-cables-good)
  (add-node 'engine-will-run-briefly
	    "Does the engine stall in cold, but not warm temperatures"
	    'stalls-in-cold-weather
	    "Adjust the idle speed"))

;;12.5
(defun print-starship (ship stream depth)
  (format stream "#<STARSHIP ~A>" (starship-name ship)))

(defstruct (starship (:print-function print-starship))
  (name nil) (captain nil) (speed 0) (condition 'green) (shields 'down))

(defun print-captain (captain stream depth)
  (format stream "#<CAPTAIN ~A>" (captain-name captain)))
  
(defstruct (captain (:print-function print-captain)) (name nil) (age nil) (ship nil))

(defun import-all nil
  (load "./GISC_Ch_12.lisp")
  (load "./dtrace.lisp")
  (load "./sdraw.lisp"))
  
    
      
