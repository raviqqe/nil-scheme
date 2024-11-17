(import (scheme base) (scheme write))

(define constant-instruction 0)
(define get-instruction 1)
(define set-instruction 2)
(define if-instruction 3)
(define nop-instruction 4)
(define call-instruction 5)

(define integer-base 128)
(define number-base 64)
(define tag-base 32)
(define share-base 31)

(define-record-type *rib*
  (make-rib car cdr tag)
  *rib*?
  (car rib-car)
  (cdr rib-cdr)
  (tag rib-tag))

(define-record-type *stack*
  (make-stack values)
  stack?
  (values stack-values stack-set-values!))

(define (stack-push stack value)
  (set! stack (cons value stack)))

(define (stack-pop stack)
  (let ((value (car stack)))
    (set! stack (cdr stack))
    value))

(define dictioanry (make-stack '()))

(define stack (make-stack '()))

(define (decode-integer-tail x base)
  (let loop ((x x)
             (y (quotient x 2))
             (base base))
    (if (even? x)
      y
      (let ((x (read-u8)))
        (loop x (+ y (* base (quotient x 2))) (* base integer-base))))))

(define (decode-number integer)
  (cond
    ((even? integer)
      (quotient integer 2))

    ((even? (quotient integer 2))
      (- (quotient integer 4)))

    (else
      (let* ((x (quotient integer 4))
             (m (* (if (even? x) 1 -1) (quotient x 4096)))
             (e (- (modulo (quotient integer 2) 2048) 1023)))
        (* m (expt 2 e))))))

(define (decode-ribs data)
  (do ((byte (read-u8) (read-u8)))
    ((eof-object? byte))
    (cond
      ((even? byte)
        (stack-push (decode-number (decode-integer-tail byte number-base)) stack))

      ((even? (quotient byte 2))
        (let* ((d (stack-pop stack))
               (a (stack-pop stack))
               (tag (decode-integer-tail (quotient byte 4) tag-base)))
          (stack-push (make-rib a d tag) stack)))

      (else
        (let* ((head (quotient byte 4))
               (integer (decode-integer-tail (- head 1) share-base))
               (index (quotient integer 2)))
          (when (> index 0)
            (let ((foo (tail (stack-values dictionary))))
              (write-string "- ")))
          (write byte)
          (newline))))))

(decode-ribs #f)
