;; Return the intersection of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;; Examples:

;;   (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)
    ((NULL set-1) NULL set-2)
    (set-member set-1 (CAR set-2) append(CAR set-2))
    (set-intersection (CDR set-1 CDR set-2))

)