(defun set-member (set item)
  (cond
    ((equal set nil) nil)
    ((equal (car set) item) t)
    (t (set-member (cdr set) item))))

(defun set-union (set-1 set-2)
  (cond
    ((equal set-2 nil) set-1) 
    ((set-member set-1 (car set-2)) (set-union set-1 (cdr set-2))) 
    (t (set-union (cons (car set-2) set-1) (cdr set-2)))))

(defun set-intersection (set-1 set-2)
  (cond 
    ((equal set-1 nil) nil) 
    ((set-member set-2 (car set-1)) 
    (cons (car set-1) (set-intersection (cdr set-1) set-2))) 
    (t (set-intersection (cdr set-1) set-2))))

(defun set-diff (set-1 set-2)
  (cond
    ((equal set-1 nil) nil)
    ((set-member set-2 (car set-1)) (set-diff (cdr set-1) set-2))
    (t (cons (car set-1) (set-diff (cdr set-1) set-2)))))

(defun boolean-xor (a b)
  (cond
    ((and a b) nil)
    ((or a b) t)
    (t nil)))

(defun boolean-implies (a b)
  (or (not a) b))

(defun boolean-iff (a b)
  (and (boolean-implies a b) (boolean-implies b a)))

; Boolean Expression

;; Evaluate a boolean expression.

;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.

;;

;; Examples:

;;  (boolean-eval '(and t nil)) => nil

;;  (boolean-eval '(and t (or nil t)) => t
```
(defun boolean-eval (exp)

  (cond
    ((OR (EQUAL exp T) (EQUAL exp NIL)) exp)

    ((EQUAL (CAR exp) '(NOT)) NOT(boolean-eval(SECOND exp)))

    ((EQUAL (CAR exp) '(AND)) (AND(boolean-eval(SECOND exp)) (boolean-eval(THIRD exp))))

    ((EQUAL (CAR exp) '(OR)) (OR(boolean-eval(SECOND exp)) (boolean-eval(THIRD exp))))

    ((EQUAL (CAR exp) '(XOR)) (boolean-xor(boolean-eval(SECOND exp)) (boolean-exp(THIRD exp))))

    ((EQUAL (CAR exp) '(IMPLIES)) (boolean-implies(boolean-eval(SECOND exp)) (boolean-eval(THIRD exp))))

    ((EQUAL (CAR exp) '(IIF)) (boolean-iff(boolean-eval(SECOND exp)) (boolean-eval(THIRD exp))))
  )
)

; Merge Sort
