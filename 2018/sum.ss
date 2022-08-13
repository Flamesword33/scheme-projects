(define (sum list_of_numbers)
  (cond 
    ((null? list_of_numbers) 0);if
    (else (+ (car list_of_numbers) (sum (cdr list_of_numbers))));else
    );cond
  );define