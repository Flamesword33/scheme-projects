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

(define (readLoop curChar line)
  (cond
    ((char=? #\newline curChar) line)
    (else (readLoop (read-char (current-input-port))
            (append line (list curChar))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rock paper scissors in scheme.ss 
;;
;; by Nathan Pelletier
;;
;; Starts a game of rock paper scissors 
;; with the user that runs for ten games.
;; 
;;
;;;;;;;;;;;;;
;; METHODS ;;
;;;;;;;;;;;;;
;;
;; main()
;; start(int, int, int, int)
;; update_end_scores(symbol, int, int, int, int)
;; score_display(int, int, int)
;; start_game() --> symbol
;; get_user_choice(char*) --> symbol
;; computer_choice() --> symbol
;; get_computer_choice(int) --> symbol
;; compair_choices(symbol, symbol) --> symbol
;; is_winner(symbol, symbol) --> boolean
;; 
;; To do list:
;;   get input from everyone DONE
;;   interpret inputs        DONE
;;   give user feed back of who won and who lost DONE
;;   print the users win tally      THANK YOU ROSANNA!!
;;   print the pc's win tally       THANK YOU ROSANNA!!
;;   print the number of tied games THANK YOU ROSANNA!!
;;   loop then entire program 10 times DONE
;;   give the user feedback after 10 games DONE
;;   repeat on bad input DONE
;;
;; NOTE: not allowed to use let, let* or if

;; main
;;
;; Welcomes the user, starts the global variables and starts the game
(define (main)
  (display "Welcome to rock paper scissors THE GAME\n") 
  (start 0 0 0 0))


;; start(int counter, int user_score, int computer_score, int tied_games)
;;
;; runs start game ten times
;; ALWAYS START COUNTER AT 0
;; return a value of u c or t to this function
;; compair to send a new start value

(define (start counter user_score computer_score tied_games)
  
  (cond
    ((>= counter 10) (display "\nGame Over") 
    (score_display user_score computer_score tied_games))
    (else (display "Pick your poison. \n (rock, paper or scissors):") 
      (update_end_scores (start_game) 
      counter user_score computer_score tied_games) )))


;; update_end_scores(symbol x, int counter, 
;;   int user_score, int computer_score, int tied_games)
;;
;; increments counter and the appropriate value 
;; based on return value from function compair_choice()

(define (update_end_scores x counter user_score computer_score tied_games)
  (cond
    ((equal? x 'u) 
    (start (+ counter 1) (+ user_score 1) computer_score tied_games ))
    ((equal? x 'c) 
    (start (+ counter 1) user_score (+ computer_score 1) tied_games ))
    ((equal? x 't) 
    (start (+ counter 1) user_score computer_score (+ tied_games 1)))))


;; score_display(int user_score, int computer_score, int tied_games)
;;
;; Tells the user how many games they: won, lost and tied.

(define (score_display user_score computer_score tied_games)
  (display "\nYou won: ") (display user_score) 
  (display " times. \n You lost: ") 
  (display computer_score) 
  (display " times. \n The game was tied: ") (display tied_games))  
    
    
;; start_game()--> symbol
;;
;; starts the game and uses unconnected functions:
;;   compair_choices
;;   get_computer_choice
;;   get_user_choice

(define (start_game)
 (cond (#t 
   (compair_choices (get_user_choice(readLine)) (computer_choice)) )))


;; get_user_choice(char* user_input) --> symbol
;;
;; compairs first letter user entered to r, p and s
;; returns r, p or s respectively

(define (get_user_choice user_input) 
  (cond
    ((equal? (car user_input) '#\r) 'r)
    ((equal? (car user_input) '#\R) 'r)
    ((equal? (car user_input) '#\p) 'p)
    ((equal? (car user_input) '#\P) 'p)
    ((equal? (car user_input) '#\s) 's)
    ((equal? (car user_input) '#\S) 's)
    (else 
      (display "Incorrect input. Try again. \n (rock, paper, scissors): ") 
      (get_user_choice(readLine)))
    ));get_user_choice


;; computer_choice() --> symbol
;;
;; generates a random number of mod 3 and interprets it into a char

(define (computer_choice)
  (get_computer_choice (random 3) ));computer_choice


;; get_computer_choice(int random_number) --> symbol
;;
;; takes a number from (random 3) and changes it to a letter

(define (get_computer_choice random_number)  
  (cond
    ((equal? 0 random_number) (display "Computer chose rock\n") 'r)  
    ((equal? 1 random_number) (display "Computer chose paper\n") 'p) 
    ((equal? 2 random_number) (display "Computer chose scissors\n")'s) 
    (else (display "UH OH rng isn't correct."))));get_computer_choice
    

;; compair_choices(symbol user, symbol computer) --> symbol
;;
;; takes two inputs, one from the user and one from the computer 
;; prints out who won who lost or an incorrect input
;; returns an incremented user_score or computer_score or tied_game

(define (compair_choices user computer)
  (cond
    ((equal? user computer) (display "Tie \n") 't);
    ((is_winner computer user) (display "You lose \n") 'c);if comp wins
    ((is_winner user computer) (display "You win \n") 'u);if user wins
    (else (display "Something went wrong with compair_choices" ))))
;compair_choices


;; is_winner(symbol a, symbol b) --> boolean
;;
;; takes two characters and determines if variable a won else returns false

(define (is_winner a b)
  (cond
    ((and (equal? a 'r) (equal? b 's)) #t)
    ((and (equal? a 'p) (equal? b 'r)) #t)
    ((and (equal? a 's) (equal? b 'p)) #t)
    (else #f)));is_winner  
