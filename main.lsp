(defun set-member (set item)

  ((NULL? set) #F)
  ((EQ? item (CAR set)) #T)
  ((ELSE (set item (CDR item))))

)
