; Resources
;   https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-2.html#%_toc_start
;   https://docs.scheme.org/tyscheme/index-Z-H-1.html#TAG:__tex2page_toc

; <-- is a comment

; Syntax rule:
;   elements are on their own
;   strings are surrounded with ""
;   functions are surrounded with ()

;data types:
; Booleans:  #t #f
; number:
;   integers:  42
;   rationals: 22/7
;   reals:     3.1415
;   complex:   2+3i
; Number base prefixes:
;   binary:    #b
;   octal:     #o
;   decimal:   #d
;   hex:       #x


;Keywords:
( 
    
)

; =
;   checks if arguments mathwise come out to the same value
;   does not work with non number data types

; <
;   checks if first argument is less than second

; >=
;   checks if first argument is greater than or equal to second

; +,-,*,/
;    arithmatic opperations
;    - with one argument is negation
;    / with one argument returns 1/x

; ' notes litteral data
; ` almost constant data
; \ character constants and escape characters

; abs
;   returns absolute value 

; begin
;   schemes version of main, it tells the program to start here

; boolean?
;   checks if argument is boolean

; ceiling
;   rounds a number up to the next nearest int

; complex?
;   checks if argument is a complex number

; display
;   outputs a line of text in ""

; eqv?
;   checks if arguments are the same value and datatype

; expt
;   exponent: first argument is base, second is power 

; floor
;   rounds a number down to the closest int

; integer?
;   checks if arguement is an integer

; load
;   loads a file into the enviroment

; max/min
;   returns highest/lowest number in argument list

; newline
;   same as /n, just function instead of character

; not
;   negates its argumetents

; number?
;   checks if argument is a number

; rational?
;   checks if argument is a rational number

; real?
;   checks if argument is a real number

; round
;   rounds a number to an integer
