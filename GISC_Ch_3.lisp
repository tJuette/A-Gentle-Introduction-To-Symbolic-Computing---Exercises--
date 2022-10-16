; 3.1
; (not (equal 3 (abs -3))) evals to nil

;3-2
(/ (+ 12 8) 2)

;3.3
(+ (* 3 3) (* 4 4))

;3.5
(defun half (n)
  "Returns half of n"
  (/ n 2))

(defun cube (n)
  "Returns the cubic value of n"
  (* n n n))

(defun onemorep (a b)
  (equal (- a 1) b))

;3.6
(defun pythag (x y)
  "Returns the root of the sum of the squares of x and y"
  (sqrt (+ (* x x)
           (* y y))))

; 3.7
(defun mile-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ (- final-odometer-reading
        initial-odometer-reading)
     gallons-consumed))

;3.9
(cons 5 (list 6 7))                     ; => (5 6 7)
(cons 5 '(list 6 7))                    ; => (5 list 6 7)
(list 3 'from 9 'gives (- 9 3))         ; => (3 from 9 gives 6)
(+ (length '(1 foo 2 moo))
   (third '(1 foo 2 moo)))              ; => 6
(rest '(cons is short for construct))   ; => (is short for construct)

;3.10
(third (the quick brown fox))           ; => Error! THE undefined function.
(third '(the quick brown fox))          ; => brown
(list 2 and 2 is 4)                     ; => Error! AND unsassigned variable.
(list 2 'and 2 'is 4)                   ; => (2 and 2 is 4)
(+ 1 '(length (list t t t t)))          ; => Error! Wrong type input to +.
(+ 1 (length (list t t t t)))           ; => 5
(cons 'patrick (seymour marvin))        ; => Error SEYMOUR undefined function.
(cons 'patrick '(seymour marvin))       ; => (PATRICK SEYMOUR MARVIN)
(cons 'patrick (list seymour marvin))   ; => Error! SEYMOUR unassigned variable.
(cons 'patrick (list 'seymour 'marvin)) ; => (PATRICK SEYMOUR MARVIN)

;3.11
(defun longer-than (l1 l2)
  (> (length l1)
     (length l2)))

;3.12
(defun add-length (lst)
  (cons (length lst)
        lst))

; (add-length (add-length '(a b c))) => (4 3 A B C)

;3.13
(defun call-up (caller callee)
  (list 'Hello callee 'this 'is caller 'calling))

; (call-up 'fred 'wanda)    => (HELLO WANDA THIS IS FRED CALLING)

;3.14
(defun crank-call (caller callee)
  '(Hello callee this is caller calling))

; (call-up 'fred 'wanda)    => (HELLO CALLEE THIS IS CALLER CALLING)

;3.15
(defun scrabble (word)
  (list word 'is 'a 'word))
; (scrabble 'aardbark) => (AARDVARK IS A WORD)
; (scrabble 'word) => (WORD IS A WORD)

;3.16
(defun stooge (larry moe curly)
  (list larry (list 'moe curly) curly 'larry))
;(stooge 'moe 'curly 'larry) => (MOE (MOE LARRY) LARRY LARRY)

;3.19
(cons 'grapes '(of wrath))      ; => (GRAPES OF WRATH)
(list t 'is 'not nil)           ; => (T IS NOT NIL)
(first '(list moose goose))     ; => (LIST)
(first (list 'moose 'goose))    ; => (MOOSE)
(cons 'home ('sweet 'home))     ; => Error! SWEET undefined function

;3:20
(defun mystery (x)
  (list (second x) 
        (first x)))
;(mystery '(dancing bear))        => (BEAR DANCING)
;(mystery 'dancing 'bear)         => Error! Too many inputs!
;(mystery '(zowie))               => (NIL ZOWIE)
;(mystery (list 'first 'second))  => (SECOND FIRST)

;3.22
;b
(+ 3 5)                         ; => 8
(3 + 5)                         ; => Error! 3 undefined function.
(+ 3 (5 6))                     ; => Error! 5 undefined function.
(+ 3 (* 5 6))                   ; => 33
'(morning noon night)           ; => (MORNING NOON NIGHT)
('morning 'noon 'night)         ; => Error! MORNING undefined function.
(list 'morning 'noon 'night)    ; => (MORNING NOON NIGHT)
(car nil)                       ; => NIL
(+ 3 foo)                       ; => Error! FOO unassigned variable.
(+ 3 'foo)                      ; => Error! Wrong type input to +.    

;c
(defun myfun (a b)
  (list (list a) b))

;d
(defun firstp (s lst)
  (equal s (first lst)))

;e
(defun mid-add1 (lst)
  (cons (car lst)
        (list (+ (cadr lst) 1)
              (caddr lst))))

(defun f-to-c (f)
  (truncate (/ (* 5 (- f 32)) 9)))

;3.25
(list 'cons t nil)                    ; => (CONS T NIL)
(eval (list 'cons t nil))             ; => (T)
(eval (eval (list 'cons t nil)))      ; => Error! T undefined function.
(apply #'cons '(t nil))               ; => (T)
(eval nil)                            ; => NIL
(list 'eval nil)                      ; => (EVAL NIL)
(eval (list 'eval nil))               ; => NIL
