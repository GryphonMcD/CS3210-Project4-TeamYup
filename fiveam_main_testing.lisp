#| Enter these statements in the REPL. Compile first

1. (ql:quickload :fiveam)
2. (load "<test file absolute path HERE>") --> Output: T
3. (in-package program-functions) --> Output: #<PACKAGE "PROGRAM-FUNCTIONS">
4. (run! 'program-tests) --> Output: Running test suite PROGRAM-TESTS...|#

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

(defun merge-sort (list predicate)
  ;; Base case: empty list or single element is already sorted
  (if (or (null list) (null (cdr list)))
      list
      ;; Recursive case: split, sort halves, and merge
      (let* ((length (length list))
             (middle (floor length 2))
             (left-half (subseq list 0 middle))
             (right-half (subseq list middle)))
        ;; Recursively sort each half and merge them
        (merge-lists (merge-sort left-half predicate)
                     (merge-sort right-half predicate)
                     predicate))))

(defun merge-lists (list1 list2 predicate)
  (cond ((null list1) list2)
        ((null list2) list1)
        ;; Compare the heads of both lists
        ((funcall predicate (car list1) (car list2))
         (cons (car list1) (merge-lists (cdr list1) list2 predicate)))
        (t
         (cons (car list2) (merge-lists list1 (cdr list2) predicate)))))

(defpackage :program-functions
  (:use :cl :fiveam))
(in-package :program-functions)
         
(def-suite program-tests)
(in-suite program-tests)

;; Member Tests
(test set-member-tests
  (is (equal (set-member '(1 2 3 4) 1) t))
  (is (equal (set-member '(1 2 3 4) 4) t))
  (is (equal (set-member '(1 2 3 4) 5) nil))
  (is (equal (set-member '() 1) nil))
  (is (equal (set-member '((a) (b) (c)) '(a)) t))
)

;; Union Tests
(test set-union-tests
  (is (equal (set-union '(1 2 3) '(3 4 5)) '(5 4 1 2 3))) 
  (is (equal (set-union '(1 2) '()) '(1 2)))              
  (is (equal (set-union '() '(3 4)) '(4 3)))              
  (is (equal (set-union '(a b) '(c d)) '(d c a b)))
)

;; Intersection Tests
(test set-intersection-tests
  (is (equal (set-intersection '(1 2 3) '(2 3 4)) '(2 3)))
  (is (equal (set-intersection '(1 2) '(3 4)) '()))
  (is (equal (set-intersection '() '(1 2)) '()))
  (is (equal (set-intersection '(a b) '(a b c)) '(a b)))
)

;; Difference Tests
(test set-diff-tests
  (is (equal (set-diff '(1 2 3) '(2 3)) '(1)))
  (is (equal (set-diff '(1 2 3) '()) '(1 2 3)))
  (is (equal (set-diff '(a b c) '(a b c)) '()))
  (is (equal (set-diff '() '(1 2)) '()))
)

;; Boolean XOR Tests
(test boolean-xor-tests
  (is (equal (boolean-xor t t) nil))
  (is (equal (boolean-xor nil nil) nil))
  (is (equal (boolean-xor t nil) t))
  (is (equal (boolean-xor nil t) t))
)

;; Boolean Implication Tests
(test boolean-implies-tests
  (is (equal (boolean-implies t t) t))
  (is (equal (boolean-implies t nil) nil))
  (is (equal (boolean-implies nil t) t))
  (is (equal (boolean-implies nil nil) t))
)

;; Boolean bi-implication Tests
(test boolean-iff-tests
  (is (equal (boolean-iff t t) t))
  (is (equal (boolean-iff nil nil) t))
  (is (equal (boolean-iff t nil) nil))
  (is (equal (boolean-iff nil t) nil))
)

;; Merge Sort Tests
(test merge-sort-tests
  (is (equal (merge-sort '(2 1 5 0) #'<) '(0 1 2 5)))
  (is (equal (merge-sort '(2 1 5 0) #'>) '(5 2 1 0)))
  (is (equal (merge-sort '(3 1 4 2) #'<) '(1 2 3 4)))
  (is (equal (merge-sort '(3 1 4 2) #'>) '(4 3 2 1)))
  (is (equal (merge-sort '() #'<) '()))
  (is (equal (merge-sort '(1) #'<) '(1))) 
)

;; Boolean-eval Tests
;; Basic constants
(test boolean-eval-tests
  (is (equal (boolean-eval t) t))                                       ; Should be T
  (is (equal (boolean-eval nil) nil))                                   ; Should be NIL
)

;; NOT operator tests
(test not-operator-tests
  (is (equal (boolean-eval '(not t)) nil))                              ; Should be NIL
  (is (equal (boolean-eval '(not nil)) t))                              ; Should be T
  (is (equal (boolean-eval '(not (not t))) t))                          ; Should be T
)

;; AND operator tests
(test and-operator-tests
  (is (equal (boolean-eval '(and t t)) t))                              ; Should be T
  (is (equal (boolean-eval '(and t nil)) nil))                          ; Should be NIL
  (is (equal (boolean-eval '(and nil t)) nil))                          ; Should be NIL
  (is (equal (boolean-eval '(and nil nil)) nil))                        ; Should be NIL
)

;; OR operator tests
(test or-operator-tests
  (is (equal (boolean-eval '(or t t)) t))                               ; Should be T
  (is (equal (boolean-eval '(or t nil)) t))                             ; Should be T
  (is (equal (boolean-eval '(or nil t)) t))                             ; Should be T
  (is (equal (boolean-eval '(or nil nil)) nil))                         ; Should be NIL
)

;; XOR operator tests
(test xor-operator-tests
  (is (equal (boolean-eval '(xor t t)) nil))                            ; Should be NIL
  (is (equal (boolean-eval '(xor t nil)) t))                            ; Should be T
  (is (equal (boolean-eval '(xor nil t)) t))                            ; Should be T
  (is (equal (boolean-eval '(xor nil nil)) nil))                        ; Should be NIL
)

;; IMPLIES operator tests
(test implies-operator-tests
  (is (equal (boolean-eval '(implies t t)) t))                          ; Should be T
  (is (equal (boolean-eval '(implies t nil)) nil))                      ; Should be NIL
  (is (equal (boolean-eval '(implies nil t)) t))                        ; Should be T
  (is (equal (boolean-eval '(implies nil nil)) t))                      ; Should be T
)

;; IFF operator tests
(test iff-operator-tests
  (is (equal (boolean-eval '(iff t t)) t))                              ; Should be T
  (is (equal (boolean-eval '(iff t nil)) nil))                          ; Should be NIL
  (is (equal (boolean-eval '(iff nil t)) nil))                          ; Should be NIL
  (is (equal (boolean-eval '(iff nil nil)) t))                          ; Should be T
)

;; Nested expression tests
(test nested-expression-tests
  (is (equal (boolean-eval '(and t (or nil t))) t))                     ; Should be T
  (is (equal (boolean-eval '(or (and t nil) (and t t))) t))             ; Should be T
  (is (equal (boolean-eval '(not (and t (not t)))) t))                  ; Should be T
  (is (equal (boolean-eval '(implies (or t nil) (and t t))) t))         ; Should be T
  (is (equal (boolean-eval '(iff (or t nil) (and t (not nil)))) t))     ; Should be T
  (is (equal (boolean-eval '(xor (and t t) (or nil nil))) t))           ; Should be T
)

;; Complex nested expressions
(test complex-nested-expression-tests
  (is (equal (boolean-eval '(not (implies (and t nil) (or t (not nil))))) nil))           ; Should be NIL
  (is (equal (boolean-eval '(iff (xor (and t t) (or nil t)) (not (implies nil t)))) t))   ; Should be T
)