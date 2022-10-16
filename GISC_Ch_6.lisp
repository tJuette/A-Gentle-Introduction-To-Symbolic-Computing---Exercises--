;; 6.5
(defvar line '(roses are red))
;; (reverse line)                       => (RED ARE ROSES)
;; (first (last line))                  => RED
;; (nth 1 line)                         => ARE
;; (reverse (reverse line))             => (ROSES ARE RED)
;; (append line (list (first line)))    => (ROSES ARE RED ROSES)
;; (append (last line) line)            => (RED ROSES ARE RED)
;; (list (first line) (last line))      => (ROSES (RED))
;; (cons (last line) line)              => Error! F ((RED) ROSES ARE RED)
;; (remove 'are line)                   => (ROSES RED)
;; (append line '(violets are blue))    => (ROSES ARE RED VIOLETS ARE BLUE)
;; 6.6
(defun last-element-1 (lst)
  (first (last lst)))
(defun last-element-2 (lst)
  (first (reverse lst)))
(defun last-element-3 (lst)
  (nth (- (length lst) 1) lst))
;; 6.7
(defun next-to-last-1 (lst)
  (cadr (reverse lst)))
(defun next-to-last-2 (lst)
  (nth (- (length lst) 2) lst))
;; 6.8
(defun my-butlast (lst)
  (reverse (cdr (reverse lst))))
;; 6.9
;; CAR
;; 6.10
(defun palindromep (lst)
  (if (equal lst (reverse lst))
      t))
;; 6.11
(defun make-palindrome (lst)
  (append lst (reverse lst)))
;; 6.18
(defun add-vowels (lst)
  (union '(a i u e o) lst))
;; 6.21
(defun my-subsetp (lst1 lst2)
  (null (set-difference lst1 lst2)))
;; 6.22
;; (defvar a '(soap water))
;; (union a '(no soap radio))           => (WATER NO RADIO SOAP)
;; (intersection a (reverse a))         => (SOAP SOAP WATER WATER) F (WATER SOAP)
;; (set-difference a '(stop for water)) => (SOAP)
;; (set-difference a a)                 => NIL
;; (member 'soap a)                     => (SOAP WATER)
;; (member 'water a)                    => (WATER)
;; (member 'dasda a)                    => NIL
;; 6.24
(defun set-equal (s1 s2)
  (and (subsetp s1 s2)
       (subsetp s2 s1)))
;; 6.25
(defun proper-subset (s1 s2)
  (and (subsetp s1 s2)
       (not (subsetp s2 s1))))
;; 6.26
(defvar my-list '(large red shiny cube -vs- small shiny red four-sided pyramid))
(defun right-side (set)
  (cdr (member '-vs- set)))
(defun left-side (set)
  (reverse (cdr (member '-vs- (reverse set)))))
(defun count-common (lset rset)
  (length (intersection lset rset)))
(defun compare (set)
  (list (count-common (left-side set)
                      (right-side set)) 'common 'features))
;; 6.30
(defvar books '((war-and-peace leo-tolstoy)
                (slaughterhouse-5 kurt-vonnegut)
                (the-neverending-story michael-ende)
                (1984 george-orwell)
                (the-spy-who-came-in-from-the-cold john-le-carre)))
;; 6.31
(defun who-wrote (book)
  (cadr (assoc book books)))
;; 6.33
(defvar books-2 '((war-and-peace . leo-tolstoy)
                  (slaughterhouse-5 . kurt-vonnegut)
                  (the-neverending-story . michael-ende)
                  (1984 . george-orwell)
                  (the-spy-who-came-in-from-the-cold . john-le-carre)))
(defun what-wrote (author)
  (first (rassoc author books-2)))
;; 6.34
(defvar atlas '((pennsylvania (pittsburgh johnstown))
                (ohio (columbus))
                (new-jersey (newark princeton trenton))))          
;; 6.35
(defvar nerd-states '((sleeping . eating)
                      (eating . waiting-for-a-computer)
                      (waiting-for-a-computer . programming)
                      (programming . debugging)
                      (debugging . sleeping)))
(defun nerdus (state)
  (cdr (assoc state nerd-states)))
(defun sleepless-nerd (state)
  (if (equal state 'debbuging) 
      'eating
      (nerdus state)))
(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))
;; 6.36 
(defun swap-first-last (list)
  (let* ((fst (list (car list)))
         (lst (last list))
         (middle (reverse (cdr (reverse (cdr list))))))
    (append lst middle fst)))
;; 6.37
(defun rotate-left (list)
  (append (cdr list) (list (car list))))
(defun rotate-right (list)
  (append (last list) (reverse (cdr (reverse list)))))
(defvar my-list '(1 2 3 3 4 4 5 6))
;; 6.40
(defvar my-set '((a b c d)
                 (b c d)
                 (c d)
                 (d)))
;; 6.41
(defvar rooms '((libary 
                  (east upstairs-bedroom)
                  (south back-stairs))
                (upstairs-bedroom
                  (west libary)
                  (south front-stairs))
                (back-stairs 
                  (north libary)
                  (south downstairs-bedroom))
                (front-stairs 
                  (north upstairs-bedroom)
                  (south living-room))
                (downstairs-bedroom 
                  (north back-stairs)
                  (east dining-room))
                (living-room 
                  (north front-stairs)
                  (east kitchen)
                  (south dining-room))
                (kitchen 
                  (west living-room)
                  (south pantry))
                (dining-room
                  (north living-room) 
                  (west downstairs-bedroom)
                  (east pantry))
                (pantry 
                  (north kitchen)
                  (west dining-room))))
(defun choices (room)
  (rest (assoc room rooms)))
(defun look (direction room)
  (cadr (assoc direction (choices room))))
(defvar loc 'pantry)
(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the variable LOC"
  (setf loc place))
(defun how-many-choices ()
  "Shows the number of directions Robbie can move to"
  (length (choices loc)))
(defun upstairsp ()
  (or (equal loc 'libary)
      (equal loc 'upstairs-bedroom)))
(defun onstairsp ()
  (or (equal loc 'back-stairs)
      (equal loc 'front-stairs)))
(defun where ()
  (cond ((upstairsp) (list 'robbie 'is 'upstairs 'in 'the loc))
        ((onstairsp) (list 'robbie 'is 'on 'the loc))
        (t (list 'robbie 'is 'downstairs 'in 'the loc))))
(defun move (direction)
  (let ((new-loc (look direction loc)))
    (cond ((not new-loc) '(ouch! robbie hit a wall))
          (t (set-robbie-location new-loc)
             (where)))))
;; 6.42
(defun royal-we (list)
  (subst 'we 'i list))
