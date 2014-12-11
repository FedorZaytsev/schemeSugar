(define (file->list-of-chars file)
  (with-input-from-file file
    (lambda ()
      (let reading ((chars '()))
        (let ((char (read-char)))
          (if (eof-object? char)
              (reverse chars)
              (reading (cons char chars))))))))

(define (list-find lst el)
  (define (list-find-rec lst el idx)
    (if (null? lst)
        #f
        (if (equal? (car lst) el)
            idx
            (list-find-rec (cdr lst) el (+ idx 1)))))
  (list-find-rec lst el 0))

(define (list-mid lst from to)
  (take (list-tail lst from) (- to from)))

(define (syntax-tail data idx)
  (if (string-whitespace? (car data))
      (syntax-tail (cdr data) idx)
      (if (equal? idx 1)
          (cdr data)
          (syntax-tail (cdr data) (- idx 1)))))

(define (syntax-length data)
  (define (syntax-length-rec data count)
    (if (null? data)
        count
        (if (string-whitespace? (car data))
            (syntax-length-rec (cdr data) count)
            (syntax-length-rec (cdr data) (+ count 1)))))
  (syntax-length-rec data 0))

(define (string-whitespace? data)
  (define (syntax-whitespace-helper data)
    (and (= (length data) 1) (char-whitespace? (car data))))
  (syntax-whitespace-helper (string->list data)))

; Check if list begins with given list
; Return boolean
(define (list-begins-with? lst lst2)
  (if (null? lst2)
      #t
      (if (equal? (car lst) (car lst2))
          (list-begins-with? (cdr lst) (cdr lst2))
          #f)))

(define (type-name? el)
  (and (equal? (string->number el) #f) (not (lexical-special-lexem? (string->list el)))))

(define (type-not-language? el)
  (not (lexical-special-lexem? (string->list el))))

(define (get-lexem data idx)
  (if (string-whitespace? (car data))
      (get-lexem (cdr data) idx)
      (if (zero? idx)
          (car data)
          (get-lexem (cdr data) (- idx 1)))))

(define (remove-last-newline data)
  (if (equal? (last data) "\n")
      (take data (- (length data) 1))
      data))
      

