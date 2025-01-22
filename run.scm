; We import all libraries here because the compiler strips any unimported libraries.
(import
  (scheme base)
  (scheme char)
  (only (scheme cxr))
  (only (scheme eval) eval)
  (scheme file)
  (only (scheme inexact))
  (only (scheme lazy))
  (scheme process-context)
  (only (scheme r5rs))
  (scheme read)
  (scheme repl)
  (only (scheme time))
  (only (scheme write)))

(define (main)
  (define program (open-input-file (list-ref (command-line) 1)))

  (do ()
    ((eof-object? (peek-char program))
      #f)
    (if (char-whitespace? (peek-char program))
      (read-char program)
      (eval (read program) (interaction-environment)))))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme interpreter.\n\n")
    (write-string "Usage: stak SOURCE_FILE [ARGUMENTS]...\n")
    (exit)))

(main)
