(define (factorial x)
  (cond
    ((<= x 1) 1)
    (else (* x (factorial (- x 1))));else
    );cond
  );factorial(x)

(define (factorial2 x)
  (factorial2_helper x 1)
  );factorial2

(define (factorial2_helper x result_so_far)
  (cond
    ((<= x 1) result_so_far)
    (else (factorial2_helper (- x 1) (* x result_so_far)));else recursive call minus 1
    );cond
  );factorial2_helper