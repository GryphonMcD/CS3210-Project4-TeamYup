;;;;;;;;;;;;;;; ticket 3 / member set ;;;;;;;;;;;;;;;;
(defun set-member (set item)
  (cond
    ((equal set nil) nil)
    ((equal (car set) item) t)
    (t (set-member (cdr set) item))))

;;;;;;;;;;;;;; ticket 7 / xor ;;;;;;;;;;;;;;;;;;
(defun boolean-xor (a b)
  (cond
    ((and a b) nil)
    ((or a b) t)
    (t nil)))
