#lang racket
(include "utils.rkt")

; Primitive parse
; Decide if str begins with special lexem (e.g. let = + -)
; Return list with 2 elements if success ( first element is special lexem (string) and all other symbols (list of char))
; Return 'nothing if fail
(define (lexical-special-lexem str)
  (cond ((equal? (car str) #\=) (list '("=") (list-tail str 1)))
        ((char-whitespace? (car str)) (list (list (string (car str))) (cdr str)))
        ((list-begins-with? str '(#\l #\e #\t)) (list '("let") (list-tail str 3)))
        (else 'nothing)))

; Check is next symbol is special lexem
; Return boolean
(define (lexical-special-lexem? str)
  (not (equal? (lexical-special-lexem str) 'nothing)))

; Lexical parser
; Return list of string
(define (lexical-parse str)
  (define (process-result buffer result spec-lexems-data)
    (append result (if (not (null? buffer)) (list (list->string buffer)) '()) (car spec-lexems-data)))
  (define (lexical-parse-rec str buffer result)
    (if (not (null? str))
        (if (lexical-special-lexem? str)
            (let ((spec-lexems-data (lexical-special-lexem str)))
              (lexical-parse-rec (cadr spec-lexems-data) '() (process-result buffer result spec-lexems-data)))
            (lexical-parse-rec (cdr str) (append buffer (list (car str))) result))
        (process-result buffer result (list '() '())))) 
  (lexical-parse-rec str '() '()))

(define (syntax-let? data)
  (and 
   (equal? (get-lexem data 0) "let")
   (type-name? (get-lexem data 1))
   (equal? (get-lexem data 2) "=")
   (type-not-language? (get-lexem data 3))
   (>= (syntax-length data) 4)))

(define (syntax-let data)
  (list (syntax-tail data 4) (list "(" "define" " " (get-lexem data 1) " " (get-lexem data 3) ")" "\n")))

(define (syntax-set? data)
  (and 
   (>= (syntax-length data) 3)
   (type-name? (get-lexem data 0))
   (equal? (get-lexem data 1) "=")
   (type-not-language? (get-lexem data 2))))

(define (syntax-set data)
  (list (syntax-tail data 3) (list "(" "set!" " " (get-lexem data 0) " " (get-lexem data 2) ")" "\n")))

(define (syntax-equal? data)
  (and
   (>= (syntax-length data) 4)
   (type-name? (get-lexem data 0))
   (equal? (get-lexem data 1) "=")
   (equal? (get-lexem data 2) "=")
   (type-not-language? (get-lexem data 3))))

(define (syntax-equal data)
  (list (syntax-tail data 4) (list "(" "equal?" " " (get-lexem data 0) " " (get-lexem data 3) ")" "\n")))

(define (syntax-if? data)
  (and
   (>= (syntax-length data) 4)
   (equal? (get-lexem data 0) "if")
   (not (equal? (member "then" data) #f))
   (not (equal? (member "end" data) #f))
   (< (list-find data "then") (list-find data "end"))))

(define (syntax-if-then-end data)
  (list (list-tail data (+ (list-find data "end") 1)) (append '( "(" "if" " ")
                                                              (remove-last-newline (syntax-parse (list-mid data (+ (list-find data "if") 1) (list-find data "then"))))
                                                              (remove-last-newline (syntax-parse (list-mid data (+ (list-find data "then") 1) (list-find data "end"))))
                                                              '(")" "\n"))))
(define (syntax-if-then-else-end data)
  (list (list-tail data (+ (list-find data "end") 1)) (append '( "(" "if" " ")
                                                              (remove-last-newline (syntax-parse (list-mid data (+ (list-find data "if") 1) (list-find data "then"))))
                                                              (remove-last-newline (syntax-parse (list-mid data (+ (list-find data "then") 1) (list-find data "else"))))
                                                              (remove-last-newline (syntax-parse (list-mid data (+ (list-find data "else") 1) (list-find data "end"))))
                                                              '(")" "\n"))))

(define (syntax-if data)
  (if (and (not (equal? (member "else" data) #f)) (< (list-find data "then") (list-find data "else")) (< (list-find data "else") (list-find data "end")))
      (syntax-if-then-else-end data)
      (syntax-if-then-end data)))
      

(define (syntax-scheme? data)
  (and
   (>= (syntax-length data) 2)
   (equal? (get-lexem data 0) "scheme")
   (not (equal? (member "end" data) #f))))

(define (syntax-scheme data)
  (list (list-tail data (+ (list-find data "end") 1)) (list-mid data (+ (list-find data "scheme") 1) (list-find data "end"))))
        
      

(define (syntax-parse data)
  (define (syntax-parse-rec data result)
    (if (not (null? data))
        (cond
          ((string-whitespace? (car data)) (syntax-parse-rec (cdr data) result))
          ((syntax-let? data)
           (let ((let-result (syntax-let data)))
             (syntax-parse-rec (car let-result) (append result (cadr let-result)))))
          ((syntax-set? data)
           (let ((set-result (syntax-set data)))
             (syntax-parse-rec (car set-result) (append result (cadr set-result)))))
          ((syntax-equal? data)
           (let ((equal-result (syntax-equal data)))
             (syntax-parse-rec (car equal-result) (append result (cadr equal-result)))))
          ((syntax-if? data)
           (let ((if-result (syntax-if data)))
             (syntax-parse-rec (car if-result) (append result (cadr if-result)))))
          ((syntax-scheme? data)
           (let ((scheme-result (syntax-scheme data)))
             (syntax-parse-rec (car scheme-result) (append result (cadr scheme-result)))))
          (else 'error))
        result))
  (syntax-parse-rec data '()))


(display (apply string-append (syntax-parse (lexical-parse (file->list-of-chars "/Users/lobster/documents/bmstu/scheme/work/sample.rkt")))))

