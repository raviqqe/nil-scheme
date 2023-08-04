(define pair-type 0)
(define procedure-type 1)
(define symbol-type 2)
(define string-type 3)
(define char-type 4)
(define vector-type 5)
(define bytevector-type 6)
(define eof-object-type 7)
(define port-type 8)

; Primitives

(define (primitive id) (rib id '() procedure-type))

(define cons (primitive 1))
(define pop (primitive 2))
(define skip (primitive 3))
(define close (primitive 4))
(define rib? (primitive 5))
(define rib-car (primitive 6))
(define rib-cdr (primitive 7))
(define rib-tag (primitive 8))
(define rib-set-car! (primitive 9))
(define rib-set-cdr! (primitive 10))
(define rib-set-tag! (primitive 11))
(define eq? (primitive 12))
(define < (primitive 13))
(define + (primitive 14))
(define - (primitive 15))
(define * (primitive 16))
(define / (primitive 17))
(define read-u8 (primitive 18))
(define write-u8 (primitive 19))

; Continuation

(define (call/cc receiver)
  (let ((continuation (rib-car (rib-cdr (rib-cdr (lambda () #f))))))
    (receiver (lambda (argument)
        (let ((frame (rib-cdr (rib-cdr (lambda () #f)))))
          (rib-set-car! frame continuation)
          argument)))))

(define unwind #f)

((call/cc
    (lambda (k)
      (set! unwind k)
      (lambda () #f))))

; Error

(define (error message)
  (unwind
    (lambda ()
      (let ((frame (rib-cdr (lambda () #f))))
        (rib-set-car! frame (cons '() '()))
        ; TODO Print an error message.
        #f))))

(define (todo)
  ; TODO Set an error message.
  (error #f))

; Types

(define (instance? type)
  (lambda (x)
    (and
      (rib? x)
      (eqv? (rib-tag x) type))))

(define eqv? eq?)

;; Boolean

(define (not x)
  (eq? x #f))

;; Character

(define char? (instance? char-type))

(define (integer->char x)
  (rib x '() char-type))

(define (char->integer x)
  (rib-car x))

;; List

(define pair? (instance? pair-type))

(define (null? x)
  (eq? x '()))

(define car rib-car)
(define cdr rib-cdr)

(define (length* xs y)
  (if (null? xs)
    y
    (length* (cdr xs) (+ y 1))))

(define (length xs)
  (length* xs 0))

;; Number

(define (integer? x)
  (not (rib? x)))

(define rational? integer?)
(define real? rational?)
(define complex? real?)
(define number? complex?)

(define (exact? x) #t)
(define (inexact? x) #f)

;; Procedure

(define procedure? (instance? procedure-type))

;; String

(define string? (instance? string-type))

(define (list->string x)
  (rib (length x) x string-type))

(define (string->list x)
  (rib-cdr x))

; Write

(define (write-char x)
  (write-u8 (char->integer x)))

(define (newline)
  ; TODO Use a character.
  (write-u8 10))
