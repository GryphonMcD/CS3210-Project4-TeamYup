;;;;;;;;;;;;;;; ticket 6 / difference set ;;;;;;;;;;;;;;;;
(defun set-diff (set-1 set-2)

    (let ((item (car set-1)))
    (if (set-member set-2 item)
        (set-diff (cdr set-1) set-2)
        (cons item (set-diff (cdr set-1) set-2))))
        
)