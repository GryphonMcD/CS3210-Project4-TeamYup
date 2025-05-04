(defun set-diff (set-1 set-2)
  (COND
    ((EQUAL set-1 NIL) NIL)                ; If set-1 is empty, return empty set
    ((set-member set-2 (CAR set-1))      ; If first element of set-1 is in set-2
     (set-diff (CDR set-1) set-2))         ; Skip it and check rest of set-1
    (T (CONS (CAR set-1)                 ; Otherwise, include it in the result
             (set-diff (CDR set-1) set-2))))) ; Otherwise, check rest of set-1


(defun boolean-xor (a b)
  (or (and a (not b))
      (and (not a) b)))


(defun set-intersection (set-1 set-2)
  (cond
    ((EQUAL set-1 NIL) NIL)
    ((set-member set-2 (CAR set-1)) (CONS (CAR set-1) (set-intersection (CDR set-1) set-2)))
    (T (set-intersection (CDR set-1) set-2))
  
  )
)


(defun set-union (set-1 set-2)

  (COND
    ((EQUAL set-1 NIL) set-2)
    ((EQUAL set-2 NIL) set-1)
    ((set-member set-1 (CAR set-2)) (set-union (CDR set-1) set-2) )
    (T (cons (CAR set-1) (set-union (CDR set-1) set-2)))
  )
)


(defun set-member (set item)
  (COND
    ((EQUAL set NIL) NIL)
    ((EQUAL item (CAR set)) T)  
    (T (set-member (CDR item) item)))
)