(define (dot_product vector1 vector2)
  (cond
    ((null? vector1) 0); item 1 is null
    ((null? vector2) 0); item 2 is null
    (else 
      (+ 
       (* (car vector1) (car vector2)) 
       (dot_product (cdr vector1) (cdr vector2))
       ));else
    );cond
  );define