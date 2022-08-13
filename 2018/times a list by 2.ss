(define (double list_of_numbers)
  (cond
    ((null? list_of_numbers) '());if null
    (else 
      (cons 
        (* 2 (car list_of_numbers)) ;multiply by two
        (double (cdr list_of_numbers));recursive call
        );cons
      );else
    );cond
  );define double