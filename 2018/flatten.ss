(define (flatten a_list)
  (cond
    ((null? a_list) '()); if null
    (else 