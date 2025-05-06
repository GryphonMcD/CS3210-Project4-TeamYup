;; Set Operations
(set-member '(1 2 3) 2)                 ; Should be t
(set-union '(1 2) '(2 3))               ; Should be (3 1 2)
(set-intersection '(1 2) '(2 3))        ; Should be (2)
(set-diff '(1 2 3) '(2))                ; Should be (1 3)

;; Boolean Operations
(boolean-xor t nil)                     ; Should be t
(boolean-implies t t)                   ; Should be t
(boolean-iff t nil)                     ; Should be nil
(boolean-eval '(and t nil))             ; Should be nil

;; Sorting
(merge-sort '(3 1 2) '<)                ; Should be (1 2 3)
(merge-sort '("b" "a" "c") 'string<)    ; Should be ("a" "b" "c")