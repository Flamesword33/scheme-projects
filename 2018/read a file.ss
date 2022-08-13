;; readLine() --> line (as String)
;; readLoop(currentCharacter line) --> line (as String)
;; by Rosanna Heise 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLine() --> line (as String)
;;
;; Read one line from standard input, not including the newline
;; but eliminating it. This is wrapper for the recursive method
;; that does the work (readLoop).
(define (readLine)
  (readLoop (read-char (current-input-port)) '())) ;do wait for one char

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLoop(currentCharacter line) --> line (as String)
;; 
;; This recursive method reads a character at a time form the
;; current input port (assuming Scheme's "Interaction Window")
;; until it finds the newline (i.e. enter). It builds the characters
;; into a string which is returned at the end. Newline is not part
;; of the string, but is eliminated from the input
;;
(define (readLoop curChar line)
  (cond
    ((char=? #\newline curChar) (list->string line))
    (else (readLoop (read-char (current-input-port))
            (append line (list curChar))))))