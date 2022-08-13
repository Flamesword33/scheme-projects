(set! students '((Mickey Mouse 25 30 65 80)
                (Goofy The Dog 80 69 20 35 76)
                (Tooth Fairy 25 25 25 25 25 25 25)
                (Casper Friendly Ghost)
                (Santa Clause 100 95 100)
                (Rudolf Rednose Reindeer 80 70 90))) 


;; This function takes a database, where each entry is
;; just a list, with names at the begining and 
;; numbers at the names
;; 
;; returns a formmatted list, where the names are 
;; collected into the first item of each entry
;; and the numbers are collected into the 2nd item of 
;; each entry
(define (convert_to_list_of_numbers_with_titles database)
  (cond
    ((null? database) '())
    (else (cons (collect_the_names (car database) '())
      (convert_to_list_of_numbers_with_titles (cdr database))))))


(define (collect_the_names one_entry name_list)
  (cond
    ((null? one_entry) (list name_list '()));; if we are done
    ((integer? (car one_entry)) (list name_list (find_the_average one_entry)));; if int is found
    (else (collect_the_names (cdr one_entry) (append name_list (list (car one_entry)))))))

(define (find_the_average list_of_numbers)
  (cond
    ((null? list_of_numbers) 0)
    (else (/ (apply + list_of_numbers) (length list_of_numbers)))))