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
  (NOT (boolean-xor a b)))

(defun boolean-eval (exp)
  (cond
    ((OR (EQUAL exp t) (EQUAL exp nil)) exp)

    ((EQUAL (CAR exp) 'not) 
      (NOT(boolean-eval(SECOND exp))))

    ((EQUAL (CAR exp) 'and) 
      (AND(boolean-eval(SECOND exp)) 
          (boolean-eval(THIRD exp))))

    ((EQUAL (CAR exp) 'or) 
      (OR(boolean-eval(SECOND exp)) 
         (boolean-eval(THIRD exp))))

    ((EQUAL (CAR exp) 'xor) 
      (boolean-xor(boolean-eval(SECOND exp)) 
                  (boolean-eval(THIRD exp))))

    ((EQUAL (CAR exp) 'implies) 
      (boolean-implies(boolean-eval(SECOND exp)) 
                      (boolean-eval(THIRD exp))))

     ((equal (first exp) 'iff)
     (boolean-iff (boolean-eval (second exp))
                       (boolean-eval (third exp))))
  )
)


;; Basic constants
(format t "T: ~a~%" (boolean-eval t))                            ; Should be T
(format t "NIL: ~a~%" (boolean-eval nil))                        ; Should be NIL

;; NOT operator tests
(format t "(not t): ~a~%" (boolean-eval '(not t)))              ; Should be NIL
(format t "(not nil): ~a~%" (boolean-eval '(not nil)))          ; Should be T
(format t "(not (not t)): ~a~%" (boolean-eval '(not (not t))))  ; Should be T

;; AND operator tests
(format t "(and t t): ~a~%" (boolean-eval '(and t t)))          ; Should be T
(format t "(and t nil): ~a~%" (boolean-eval '(and t nil)))      ; Should be NIL
(format t "(and nil t): ~a~%" (boolean-eval '(and nil t)))      ; Should be NIL
(format t "(and nil nil): ~a~%" (boolean-eval '(and nil nil)))  ; Should be NIL

;; OR operator tests
(format t "(or t t): ~a~%" (boolean-eval '(or t t)))            ; Should be T
(format t "(or t nil): ~a~%" (boolean-eval '(or t nil)))        ; Should be T
(format t "(or nil t): ~a~%" (boolean-eval '(or nil t)))        ; Should be T
(format t "(or nil nil): ~a~%" (boolean-eval '(or nil nil)))    ; Should be NIL

;; XOR operator tests
(format t "(xor t t): ~a~%" (boolean-eval '(xor t t)))          ; Should be NIL
(format t "(xor t nil): ~a~%" (boolean-eval '(xor t nil)))      ; Should be T
(format t "(xor nil t): ~a~%" (boolean-eval '(xor nil t)))      ; Should be T
(format t "(xor nil nil): ~a~%" (boolean-eval '(xor nil nil)))  ; Should be NIL

;; IMPLIES operator tests
(format t "(implies t t): ~a~%" (boolean-eval '(implies t t)))          ; Should be T
(format t "(implies t nil): ~a~%" (boolean-eval '(implies t nil)))      ; Should be NIL
(format t "(implies nil t): ~a~%" (boolean-eval '(implies nil t)))      ; Should be T
(format t "(implies nil nil): ~a~%" (boolean-eval '(implies nil nil)))  ; Should be T

;; IFF operator tests
(format t "(iff t t): ~a~%" (boolean-eval '(iff t t)))          ; Should be T
(format t "(iff t nil): ~a~%" (boolean-eval '(iff t nil)))      ; Should be NIL
(format t "(iff nil t): ~a~%" (boolean-eval '(iff nil t)))      ; Should be NIL
(format t "(iff nil nil): ~a~%" (boolean-eval '(iff nil nil)))  ; Should be T

;; Nested expression tests
(format t "(and t (or nil t)): ~a~%" (boolean-eval '(and t (or nil t))))          ; Should be T
(format t "(or (and t nil) (and t t)): ~a~%" (boolean-eval '(or (and t nil) (and t t))))  ; Should be T
(format t "(not (and t (not t))): ~a~%" (boolean-eval '(not (and t (not t)))))    ; Should be T
(format t "(implies (or t nil) (and t t)): ~a~%" (boolean-eval '(implies (or t nil) (and t t))))  ; Should be T
(format t "(iff (or t nil) (and t (not nil))): ~a~%" (boolean-eval '(iff (or t nil) (and t (not nil)))))  ; Should be T
(format t "(xor (and t t) (or nil nil)): ~a~%" (boolean-eval '(xor (and t t) (or nil nil))))  ; Should be T

;; Complex nested expressions
(format t "(not (implies (and t nil) (or t (not nil)))): ~a~%" 
        (boolean-eval '(not (implies (and t nil) (or t (not nil))))))  ; Should be NIL

(format t "(iff (xor (and t t) (or nil t)) (not (implies nil t))): ~a~%" 
        (boolean-eval '(iff (xor (and t t) (or nil t)) (not (implies nil t))))) ; Should be NIL
