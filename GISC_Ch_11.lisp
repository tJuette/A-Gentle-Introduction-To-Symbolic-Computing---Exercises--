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

(defun rec-reverse (list)
  (labels ((rev (l container)
	     (cond ((null l) container)
		   (t (rev (rest l) (cons (first l) container))))))
    (rev list '())))
