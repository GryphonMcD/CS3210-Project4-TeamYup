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
