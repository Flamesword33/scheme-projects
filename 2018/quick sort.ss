(define (partition compFn aList)
  (cond
    ((null? aList) '())
    ((compFn (car aList)) (cons (car aList) (partition compFn (cdr aList))))
    (else (partition compFn (cdr aList)))))

(define (qSort aList)
  (cond
    ((null? aList) '())
    (else                     ;;(car aList) is pivot
      (append (append
                (qSort (partition (lambda (x) (< x (car aList))) aList))
                (partition (lambda (x) (= x (car aList))) aList)
                (qSort (partition (lambda (x) (> x (car aList))) aList))
                ));append
      );else
    );cond
  );qSort