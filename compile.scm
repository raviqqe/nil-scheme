; Stak Scheme compiler.
;
; All compiler-internal variables contain at least one `$` or `%` character in their names.

(import
  (scheme base)
  (scheme cxr)
  (scheme eval)
  (scheme inexact)
  (scheme lazy)
  (scheme process-context)
  (scheme read)
  (scheme write))

(cond-expand
  (stak
    (define cons-rib cons)
    (define target-procedure? procedure?))

  (else
    (define-record-type *rib*
      (rib car cdr tag)
      rib?
      (car rib-car)
      (cdr rib-cdr)
      (tag rib-tag))

    (define (cons-rib car cdr)
      (rib car cdr pair-type))

    (define (target-procedure? value)
      (and (rib? value) (eq? (rib-tag value) procedure-type)))

    (define string->uninterned-symbol string->symbol)))

; Instructions

(define constant-instruction 0)
(define get-instruction 1)
(define set-instruction 2)
(define if-instruction 3)
(define nop-instruction 4)
(define call-instruction 5)

; Primitives

(define primitives
  '(($$rib 0)
    ($$close 1)
    ($$< 9)
    ($$+ 10)
    ($$- 11)
    ($$* 12)
    ($$/ 13)))

; Types

(define pair-type 0)
(define null-type 1)
(define boolean-type 2)
(define procedure-type 3)
(define symbol-type 4)
(define string-type 5)
(define char-type 6)
(define vector-type 7)
(define bytevector-type 8)

; Utility

(define (debug . xs)
  (write xs (current-error-port))
  (newline (current-error-port)))

(define (code-rib tag car cdr)
  (rib car cdr tag))

(define (call-rib arity procedure continuation)
  (code-rib (+ call-instruction arity) procedure continuation))

(define (constant-rib constant continuation)
  (code-rib constant-instruction constant continuation))

(define (data-rib type car cdr)
  (rib car cdr type))

(define (make-procedure arity code environment)
  (data-rib procedure-type (cons-rib arity code) environment))

(define (procedure-code procedure)
  (rib-cdr (rib-car procedure)))

(define (bytevector->list xs)
  (let loop ((index 0) (result '()))
    (if (< index (bytevector-length xs))
      (cons
        (bytevector-u8-ref xs index)
        (loop (+ 1 index) result))
      result)))

(define (last-cdr xs)
  (if (pair? xs)
    (last-cdr (cdr xs))
    xs))

(define (set-last-cdr! xs x)
  (if (pair? (cdr xs))
    (set-last-cdr! (cdr xs) x)
    (set-cdr! xs x)))

(define (filter f xs)
  (if (null? xs)
    '()
    (let ((x (car xs))
          (xs (filter f (cdr xs))))
      (if (f x)
        (cons x xs)
        xs))))

(define (fold-left f y xs)
  (if (null? xs)
    y
    (fold-left
      f
      (f y (car xs))
      (cdr xs))))

(define (list-head xs n)
  (if (zero? n)
    '()
    (cons
      (car xs)
      (list-head (cdr xs) (- n 1)))))

(define (list-position f xs)
  (let loop ((xs xs) (index 0))
    (cond
      ((null? xs)
        #f)

      ((f (car xs))
        index)

      (else
        (loop (cdr xs) (+ index 1))))))

(define (member-position x xs . rest)
  (define eq? (if (null? rest) equal? (car rest)))

  (list-position (lambda (y) (eq? x y)) xs))

(define (memv-position x xs)
  (member-position x xs eqv?))

(define (memq-position x xs)
  (member-position x xs eq?))

(define (flat-map f xs)
  (apply append (map f xs)))

(define (relaxed-length xs)
  (do ((xs xs (cdr xs)) (y 0 (+ y 1)))
    ((not (pair? xs))
      y)))

(define (relaxed-map f xs)
  (if (pair? xs)
    (cons
      (f (car xs))
      (relaxed-map f (cdr xs)))
    (f xs)))

(define (relaxed-deep-map f xs)
  (if (pair? xs)
    (cons
      (relaxed-deep-map f (car xs))
      (relaxed-deep-map f (cdr xs)))
    (f xs)))

(define (maybe-append xs ys)
  (and xs ys (append xs ys)))

(define (unique xs)
  (if (null? xs)
    '()
    (let ((ys (unique (cdr xs))))
      (if (memq (car xs) ys)
        ys
        (cons (car xs) ys)))))

(define (deep-unique x)
  (cond
    ((and (pair? x) (symbol? (car x)))
      (unique (cons (car x) (deep-unique (cdr x)))))

    ((pair? x)
      (deep-unique
        (append
          (deep-unique (car x))
          (deep-unique (cdr x)))))

    ((symbol? x)
      (list x))

    (else
      '())))

(define (map-values f xs)
  (map (lambda (pair) (cons (car pair) (f (cdr pair)))) xs))

(define (filter-values f xs)
  (filter (lambda (pair) (f (cdr pair))) xs))

; TODO Set a true machine epsilon.
(define epsilon
  (let ((x (/ 1 10000000 100000000)))
    (if (zero? x) 1 x)))

(define (predicate expression)
  (and (pair? expression) (car expression)))

(define (count-parameters parameters)
  (if (pair? parameters)
    (+ 1 (count-parameters (cdr parameters)))
    0))

(define (parameter-names parameters)
  (cond
    ((pair? parameters)
      (cons (car parameters) (parameter-names (cdr parameters))))

    ((symbol? parameters)
      (list parameters))

    ((null? parameters)
      '())

    (else
      (error "invalid variadic parameter" parameters))))

(define (symbol-append . xs)
  (string->symbol (apply string-append (map symbol->string xs))))

(define (id->string id)
  (number->string id 32))

; Source code reading

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (read-source)
  (cons
    '$$begin
    ; Keep an invariant that a `begin` body must not be empty.
    (cons #f (read-all))))

; Inception

(define (incept-expression inceptions expression)
  (if (pair? expression)
    (let ((expression
            (relaxed-map
              (lambda (expression)
                (incept-expression inceptions expression))
              expression)))
      (cond
        ((and (null? (cdr expression)) (assq (car expression) inceptions)) =>
          cdr)

        (else
          expression)))
    expression))

(define (incept expression)
  (incept-expression
    (list
      (cons
        '$$relaxed-map
        '(define (relaxed-map f xs)
          (if (pair? xs)
           (cons
            (f (car xs))
            (relaxed-map f (cdr xs)))
           (f xs)))))
    expression))

; Library system

;; Types

(define-record-type library
  (make-library id name exports imports body symbols)
  library?
  (id library-id)
  (name library-name)
  (exports library-exports)
  (imports library-imports)
  (body library-body)
  (symbols library-symbols))

(define-record-type library-state
  (make-library-state library imported)
  library-state?
  (library library-state-library)
  (imported library-state-imported library-state-set-imported!))

(define-record-type library-context
  (make-library-context libraries name-maps)
  library-context?
  (libraries library-context-libraries library-context-set-libraries!)
  (name-maps library-context-name-maps library-context-set-name-maps!))

(define (library-context-assoc context name)
  (cond
    ((assoc name (library-context-libraries context)) =>
      cdr)

    (else
      (error "unknown library" name))))

(define (library-context-id context)
  (length (library-context-libraries context)))

(define (library-context-find context name)
  (library-state-library (library-context-assoc context name)))

(define (library-context-add! context library)
  (library-context-set-libraries!
    context
    (cons
      (cons
        (library-name library)
        (make-library-state library #f))
      (library-context-libraries context))))

(define (library-context-import! context name)
  (let* ((state (library-context-assoc context name))
         (imported (library-state-imported state)))
    (library-state-set-imported! state #t)
    imported))

;; Procedures

(define library-symbol-separator #\%)

(define (library-symbol? name)
  (memv library-symbol-separator (string->list (symbol->string name))))

(define (build-library-name id name)
  (string-append
    (id->string id)
    (list->string (list library-symbol-separator))
    (symbol->string name)))

; TODO Remove library symbol prefixes.
(define (resolve-library-symbol name)
  (let* ((string (symbol->string name))
         (position (memv-position library-symbol-separator (string->list string))))
    (if position
      (string->symbol (string-copy string (+ position 1)))
      name)))

(define (rename-library-symbol context id name)
  (if (or
       (not id)
       (let ((name (symbol->string name)))
         (equal? (substring name 0 (min 2 (string-length name))) "$$")))
    name
    (let* ((maps (library-context-name-maps context))
           (pair (or (assq id maps) (cons id '())))
           (names (cdr pair)))
      (when (null? names)
        (library-context-set-name-maps! context (cons pair maps)))
      (let ((names (cdr pair)))
        (cond
          ((assq name names) =>
            cdr)

          (else
            (let ((renamed (string->uninterned-symbol (build-library-name id name))))
              (set-cdr! pair (cons (cons name renamed) names))
              renamed)))))))

(define (expand-import-set context importer-id qualify set)
  (define (expand qualify)
    (expand-import-set context importer-id qualify (cadr set)))

  (case (predicate set)
    ((except)
      (let ((names (cddr set)))
        (expand
          (lambda (name)
            (if (memq name names)
              #f
              (qualify name))))))

    ((only)
      (let ((names (cddr set)))
        (expand
          (lambda (name)
            (if (memq name names)
              (qualify name)
              #f)))))

    ((prefix)
      (expand
        (lambda (name)
          (qualify (symbol-append (caddr set) name)))))

    ((rename)
      (expand
        (lambda (name)
          (qualify
            (cond
              ((assq name (cddr set)) =>
                cadr)

              (else
                name))))))

    (else
      (let ((library (library-context-find context set)))
        (append
          (if (library-context-import! context set)
            '()
            (append
              (expand-import-sets
                context
                (library-id library)
                (library-symbols library)
                (library-imports library))
              (library-body library)))
          (flat-map
            (lambda (names)
              (let ((name (qualify (car names))))
                (if name
                  (list
                    (list
                      '$$alias
                      (rename-library-symbol context importer-id name)
                      (cdr names)))
                  '())))
            (library-exports library)))))))

(define (expand-import-sets context importer-id importer-symbols sets)
  (flat-map
    (lambda (set)
      (expand-import-set
        context
        importer-id
        (lambda (name)
          (and
            (or
              (not importer-symbols)
              (memq name (force importer-symbols)))
            name))
        set))
    sets))

(define (expand-library-expression context body-symbols expression)
  (case (and (pair? expression) (car expression))
    ((define-library)
      (let* ((collect-bodies
               (lambda (predicate)
                 (flat-map
                   cdr
                   (filter
                     (lambda (body) (eq? (car body) predicate))
                     (cddr expression)))))
             (id (library-context-id context))
             (exports (collect-bodies 'export))
             (bodies (collect-bodies 'begin)))
        (library-context-add!
          context
          (make-library
            id
            (cadr expression)
            (map
              (lambda (name)
                (if (eq? (predicate name) 'rename)
                  (cons (caddr name) (rename-library-symbol context id (cadr name)))
                  (cons name (rename-library-symbol context id name))))
              exports)
            (collect-bodies 'import)
            (relaxed-deep-map
              (lambda (value)
                (if (symbol? value)
                  (rename-library-symbol context id value)
                  value))
              bodies)
            (delay (deep-unique (cons exports bodies)))))
        '()))

    ((import)
      (expand-import-sets context #f body-symbols (cdr expression)))

    (else
      (list expression))))

(define library-predicates '(define-library import))

(define (expand-libraries expression)
  (let* ((context (make-library-context '() '()))
         (body-symbols
           (delay
             (deep-unique
               (filter
                 (lambda (expression)
                   (not (and (pair? expression) (memq (car expression) library-predicates))))
                 (cdr expression)))))
         (expression
           (cons
             (car expression)
             (flat-map
               (lambda (expression)
                 (expand-library-expression context body-symbols expression))
               (cdr expression)))))
    (values
      expression
      (map-values
        library-exports
        (map-values library-state-library (library-context-libraries context))))))

; Macro system

;; Types

(define-record-type macro-state
  (make-macro-state id literals)
  macro-state?
  (id macro-state-id macro-state-set-id!)
  (literals macro-state-literals macro-state-set-literals!))

(define-record-type macro-context
  (make-macro-context state environment)
  macro-context?
  (state macro-context-state)
  (environment macro-context-environment macro-context-set-environment!))

(define (macro-context-append context pairs)
  (make-macro-context
    (macro-context-state context)
    (append pairs (macro-context-environment context))))

(define (macro-context-set! context name denotation)
  (let* ((environment (macro-context-environment context))
         (pair (assq name environment)))
    (when pair (set-cdr! pair denotation))
    pair))

(define (macro-context-set-last! context name denotation)
  (unless (macro-context-set! context name denotation)
    (let ((environment (macro-context-environment context))
          (tail (list (cons name denotation))))
      (if (null? environment)
        (macro-context-set-environment! context tail)
        (set-last-cdr! environment tail)))))

(define (macro-context-generate-id! context)
  (let* ((state (macro-context-state context))
         (id (macro-state-id state)))
    (macro-state-set-id! state (+ id 1))
    id))

(define (macro-context-append-literal! context name syntax)
  (define state (macro-context-state context))

  (macro-state-set-literals!
    state
    (cons
      (cons name syntax)
      (macro-state-literals state))))

(define-record-type rule-context
  (make-rule-context definition-context use-context ellipsis literals)
  rule-context?
  (definition-context rule-context-definition-context)
  (use-context rule-context-use-context)
  (ellipsis rule-context-ellipsis)
  (literals rule-context-literals))

;; Procedures

(define (resolve-denotation context expression)
  (cond
    ((assq expression (macro-context-environment context)) =>
      cdr)

    (else
      expression)))

(define (rename-variable context name)
  ; Share tails when appending strings.
  (string->uninterned-symbol
    (string-append
      (id->string (macro-context-generate-id! context))
      "$"
      (symbol->string name))))

(define (find-pattern-variables ellipsis bound-variables pattern)
  (define excluded-variables (cons ellipsis bound-variables))

  (let loop ((pattern pattern) (variables '()))
    (cond
      ((pair? pattern)
        (loop
          (car pattern)
          (loop
            (cdr pattern)
            variables)))

      ((and (symbol? pattern) (not (memq pattern excluded-variables)))
        (cons pattern variables))

      (else
        variables))))

(define-record-type ellipsis-match
  (make-ellipsis-match value)
  ellipsis-match?
  (value ellipsis-match-value))

(define-record-type ellipsis-pattern
  (make-ellipsis-pattern element variables)
  ellipsis-pattern?
  (element ellipsis-pattern-element)
  (variables ellipsis-pattern-variables))

(define (compile-pattern context ellipsis literals pattern)
  (define (compile pattern)
    (compile-pattern context ellipsis literals pattern))

  (cond
    ((not (pair? pattern))
      pattern)

    ((and
        (pair? (cdr pattern))
        (eq? ellipsis (resolve-denotation context (cadr pattern))))
      (cons
        (make-ellipsis-pattern
          (compile (car pattern))
          (find-pattern-variables ellipsis literals (car pattern)))
        (compile (cddr pattern))))

    (else
      (cons
        (compile (car pattern))
        (compile (cdr pattern))))))

(define (match-ellipsis-pattern context pattern expression)
  (map-values
    make-ellipsis-match
    (apply
      map
      list
      (ellipsis-pattern-variables pattern)
      (map
        (lambda (expression)
          (match-pattern context (ellipsis-pattern-element pattern) expression))
        expression))))

(define (match-pattern context pattern expression)
  (define (match pattern expression)
    (match-pattern context pattern expression))

  (cond
    ((and
        (symbol? pattern)
        (memq pattern (rule-context-literals context)))
      (unless (eq?
               (resolve-denotation (rule-context-use-context context) expression)
               (resolve-denotation (rule-context-definition-context context) pattern))
        (raise #f))
      '())

    ((symbol? pattern)
      (list (cons pattern expression)))

    ((pair? pattern)
      (cond
        ((ellipsis-pattern? (car pattern))
          (let ((length (- (relaxed-length expression) (relaxed-length (cdr pattern)))))
            (when (negative? length)
              (raise #f))
            (append
              (match-ellipsis-pattern context (car pattern) (list-head expression length))
              (match (cdr pattern) (list-tail expression length)))))

        ((pair? expression)
          (append
            (match (car pattern) (car expression))
            (match (cdr pattern) (cdr expression))))

        (else
          (raise #f))))

    ((equal? pattern expression)
      '())

    (else
      (raise #f))))

(define (fill-ellipsis-template context matches template)
  (let* ((variables (ellipsis-pattern-variables template))
         (template (ellipsis-pattern-element template))
         (matches (filter (lambda (pair) (memq (car pair) variables)) matches))
         (singleton-matches (filter-values (lambda (match) (not (ellipsis-match? match))) matches))
         (ellipsis-matches (filter-values ellipsis-match? matches)))
    (when (null? ellipsis-matches)
      (error "no ellipsis pattern variables" template))
    (apply
      map
      (lambda matches (fill-template context (append matches singleton-matches) template))
      (map (lambda (pair) (ellipsis-match-value (cdr pair))) ellipsis-matches))))

(define (fill-template context matches template)
  (define (fill template)
    (fill-template context matches template))

  (cond
    ((and (symbol? template) (assq template matches)) =>
      cdr)

    ((pair? template)
      (append
        (let ((first (car template)))
          (if (ellipsis-pattern? first)
            (fill-ellipsis-template context matches first)
            (list (fill first))))
        (fill (cdr template))))

    (else
      template)))

(define (make-transformer definition-context transformer)
  (let-values (((transformer definition-context) (expand-outer-macro definition-context transformer)))
    (case (resolve-denotation definition-context (predicate transformer))
      (($$syntax-rules)
        (let* ((ellipsis (resolve-denotation definition-context (cadr transformer)))
               (literals (caddr transformer))
               (rules
                 (map
                   (lambda (rule)
                     (map
                       (lambda (pattern)
                         (compile-pattern definition-context ellipsis literals pattern))
                       rule))
                   (cdddr transformer))))
          (lambda (use-context expression)
            (let loop ((rules rules))
              (unless (pair? rules)
                (error "invalid syntax" expression))
              (let ((rule (car rules))
                    (rule-context (make-rule-context definition-context use-context ellipsis literals)))
                (guard (value
                        ((not value)
                          (loop (cdr rules))))
                  (let* ((matches (match-pattern rule-context (car rule) expression))
                         (template (cadr rule))
                         (names
                           (map
                             (lambda (name) (cons name (rename-variable use-context name)))
                             (find-pattern-variables ellipsis (append literals (map car matches)) template))))
                    (values
                      (fill-template rule-context (append names matches) template)
                      (macro-context-append
                        use-context
                        (map
                          (lambda (pair)
                            (cons
                              (cdr pair)
                              (resolve-denotation definition-context (car pair))))
                          names))))))))))

      (else
        (error "unsupported macro transformer" transformer)))))

(define (expand-outer-macro context expression)
  (if (pair? expression)
    (let ((value (resolve-denotation context (car expression))))
      (if (procedure? value)
        (let-values (((expression context) (value context expression)))
          (expand-outer-macro context expression))
        (values expression context)))
    (values expression context)))

; https://www.researchgate.net/publication/220997237_Macros_That_Work
(define (expand-macro context expression)
  (define (expand expression)
    (expand-macro context expression))

  (define (resolve name)
    (resolve-denotation context name))

  (cond
    ((symbol? expression)
      (let ((value (resolve expression)))
        (when (procedure? value)
          (error "invalid syntax" expression))
        value))

    ((pair? expression)
      (case (resolve (car expression))
        (($$alias)
          (macro-context-set-last!
            context
            (cadr expression)
            (resolve (caddr expression)))
          (macro-context-append-literal!
            context
            (cadr expression)
            (caddr expression))
          #f)

        (($$define)
          (let ((name (cadr expression)))
            (macro-context-set! context name name)
            (expand (cons '$$set! (cdr expression)))))

        (($$define-syntax)
          (macro-context-set-last!
            context
            (cadr expression)
            (make-transformer context (caddr expression)))
          (macro-context-append-literal!
            context
            (cadr expression)
            (caddr expression))
          #f)

        (($$lambda)
          (let* ((parameters (cadr expression))
                 (context
                   (macro-context-append
                     context
                     (map
                       (lambda (name) (cons name (rename-variable context name)))
                       (parameter-names parameters))))
                 ; We need to resolve parameter denotations before expanding a body.
                 (parameters
                   (relaxed-deep-map
                     (lambda (name) (resolve-denotation context name))
                     parameters)))
            (list
              '$$lambda
              parameters
              (expand-macro context (caddr expression)))))

        (($$let-syntax)
          (expand-macro
            (macro-context-append
              context
              (map-values
                (lambda (transformer)
                  (make-transformer context (car transformer)))
                (cadr expression)))
            (caddr expression)))

        (($$letrec-syntax)
          (let* ((bindings (cadr expression))
                 (context
                   (macro-context-append
                     context
                     (map-values
                       (lambda (value) #f)
                       bindings))))
            (for-each
              (lambda (pair)
                (macro-context-set!
                  context
                  (car pair)
                  (make-transformer context (cadr pair))))
              bindings)
            (expand-macro context (caddr expression))))

        (($$quote)
          (cons
            '$$quote
            (relaxed-deep-map
              (lambda (value)
                (if (symbol? value)
                  (resolve-library-symbol value)
                  value))
              (cdr expression))))

        (else =>
          (lambda (value)
            (if (procedure? value)
              (let-values (((expression context) (value context expression)))
                (expand-macro context expression))
              (map expand expression))))))

    (else
      expression)))

(define (expand-macros expression)
  (let* ((context (make-macro-context (make-macro-state 0 '()) '()))
         (expression (expand-macro context expression)))
    (values
      expression
      (reverse
        (filter
          (lambda (pair) (library-symbol? (car pair)))
          (macro-state-literals (macro-context-state context)))))))

; Optimization

(define optimization-codes
  '((define-record-type optimization-context
     (make-optimization-context optimizers literals)
     optimization-context?
     (optimizers optimization-context-optimizers optimization-context-set-optimizers!)
     (literals optimization-context-literals optimization-context-set-literals!))

    (define (optimization-context-append! context name optimizer)
     (optimization-context-set-optimizers!
      context
      (cons
       (cons name optimizer)
       (optimization-context-optimizers context))))

    (define (optimization-context-append-literal! context name literal)
     (optimization-context-set-literals!
      context
      (cons
       (cons name literal)
       (optimization-context-literals context))))

    (define (make-optimizer name optimizer)
     (define (match-pattern pattern expression)
      (cond
       ((and (pair? pattern) (pair? expression))
        (maybe-append
         (match-pattern (car pattern) (car expression))
         (match-pattern (cdr pattern) (cdr expression))))

       ((symbol? pattern)
        (list (cons pattern expression)))

       ((equal? pattern expression)
        '())

       (else
        #f)))

     (define (fill-template matches template)
      (cond
       ((pair? template)
        (cons
         (fill-template matches (car template))
         (fill-template matches (cdr template))))

       ((and (symbol? template) (assq template matches)) =>
        cdr)

       (else
        template)))

     (case (car optimizer)
      (($$syntax-rules)
       (let ((rules (cdddr optimizer)))
        (lambda (expression)
         (let loop ((rules rules))
          (if (null? rules)
           expression
           (let ((rule (car rules)))
            (cond
             ((match-pattern (car rule) expression) =>
              (lambda (matches)
               (fill-template matches (cadr rule))))

             (else
              (loop (cdr rules))))))))))

      (else
       (error "unsupported optimizer" optimizer))))

    (define (optimize-expression context expression)
     (define (optimize expression)
      (optimize-expression context expression))

     (if (pair? expression)
      (let* ((expression (relaxed-map optimize expression))
             (predicate (car expression)))
       (cond
        ((eq? predicate '$$define-optimizer)
         (let ((name (cadr expression)))
          (optimization-context-append! context name (make-optimizer name (caddr expression)))
          (optimization-context-append-literal! context name (caddr expression)))
         #f)

        ((eq? predicate '$$begin)
         ; Omit top-level constants.
         ; TODO Define this pass by `define-optimizer`.
         (cons '$$begin
          (let loop ((expressions (cdr expression)))
           (let ((expression (car expressions))
                 (expressions (cdr expressions)))
            (cond
             ((null? expressions)
              (list expression))

             ((pair? expression)
              (cons expression (loop expressions)))

             (else
              (loop expressions)))))))

        ((assq predicate (optimization-context-optimizers context)) =>
         (lambda (pair)
          ((cdr pair) expression)))

        (else
         expression)))
      expression))

    (define (optimize optimizers expression)
     (let* ((context (make-optimization-context optimizers '()))
            (expression (optimize-expression context expression)))
      (values expression (optimization-context-literals context))))))

(define (optimize2 optimizers expression)
  (eval
    `(begin
      ,@optimization-codes
      (optimize ',optimizers ',expression))
    '((scheme base))))

; Compilation

;; Context

(define-record-type compilation-context
  (make-compilation-context environment symbols libraries macros optimizers)
  compilation-context?
  (environment compilation-context-environment)
  (symbols compilation-context-symbols)
  (libraries compilation-context-libraries)
  (macros compilation-context-macros)
  (optimizers compilation-context-optimizers))

(define (compilation-context-append-locals context variables)
  (make-compilation-context
    (append variables (compilation-context-environment context))
    (compilation-context-symbols context)
    (compilation-context-libraries context)
    (compilation-context-macros context)
    (compilation-context-optimizers context)))

(define (compilation-context-push-local context variable)
  (compilation-context-append-locals context (list variable)))

; If a variable is not in environment, it is considered to be global.
(define (compilation-context-resolve context variable)
  (or (memq-position variable (compilation-context-environment context)) variable))

;; Procedures

(define (find-quoted-symbols expression)
  (cond
    ((symbol? expression)
      (list expression))

    ((vector? expression)
      (find-quoted-symbols (vector->list expression)))

    ((pair? expression)
      (append (find-quoted-symbols (car expression)) (find-quoted-symbols (cdr expression))))

    (else
      '())))

(define (find-symbols expression)
  (define (find expression)
    (cond
      ((not (pair? expression))
        '())

      ((eq? (car expression) '$$quote)
        (find-quoted-symbols (cadr expression)))

      (else
        (append (find (car expression)) (find (cdr expression))))))

  (unique (find expression)))

(define (compile-arity argument-count variadic)
  (+
    (* 2 argument-count)
    (if variadic 1 0)))

(define (drop? codes)
  (and
    (rib? codes)
    (not (null? codes))
    (eq? (rib-tag codes) set-instruction)
    (eq? (rib-car codes) 0)))

(define (compile-unspecified continuation)
  (if (drop? continuation)
    ; Skip a "drop" instruction.
    (rib-cdr continuation)
    (constant-rib #f continuation)))

(define (compile-drop continuation)
  (if (null? continuation)
    continuation
    (code-rib set-instruction 0 continuation)))

(define (compile-sequence context expressions continuation)
  (compile-expression
    context
    (car expressions)
    (if (null? (cdr expressions))
      continuation
      (compile-drop (compile-sequence context (cdr expressions) continuation)))))

(define (compile-raw-call context procedure arguments arity continuation)
  (if (null? arguments)
    (call-rib
      arity
      (compilation-context-resolve context procedure)
      continuation)
    (compile-expression
      context
      (car arguments)
      (compile-raw-call
        (compilation-context-push-local context #f)
        procedure
        (cdr arguments)
        arity
        continuation))))

(define (compile-call context expression variadic continuation)
  (let* ((procedure (car expression))
         (arguments (cdr expression))
         (continue
           (lambda (context procedure continuation)
             (compile-raw-call
               context
               procedure
               arguments
               (compile-arity
                 (- (length arguments) (if variadic 1 0))
                 variadic)
               continuation))))
    (if (symbol? procedure)
      (continue context procedure continuation)
      (compile-expression
        context
        procedure
        (continue
          (compilation-context-push-local context '$procedure)
          '$procedure
          (compile-unbind continuation))))))

(define (compile-unbind continuation)
  (if (null? continuation)
    continuation
    (code-rib set-instruction 1 continuation)))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (code-rib
        get-instruction
        (compilation-context-resolve context expression)
        continuation))

    ((pair? expression)
      (case (car expression)
        (($$apply)
          (compile-call context (cdr expression) #t continuation))

        (($$begin)
          (compile-sequence context (cdr expression) continuation))

        (($$if)
          (compile-expression
            context
            (cadr expression)
            (let ((continuation
                    (if (null? continuation)
                      '()
                      (code-rib nop-instruction 0 continuation))))
              (code-rib
                if-instruction
                (compile-expression
                  context
                  (caddr expression)
                  continuation)
                (compile-expression context (cadddr expression) continuation)))))

        (($$lambda)
          (let ((parameters (cadr expression)))
            (constant-rib
              (make-procedure
                (compile-arity
                  (count-parameters parameters)
                  (symbol? (last-cdr parameters)))
                (compile-sequence
                  (compilation-context-append-locals
                    context
                    ; #f is for a frame.
                    (reverse (cons #f (parameter-names parameters))))
                  (cddr expression)
                  '())
                '())
              (call-rib (compile-arity 1 #f) '$$close continuation))))

        (($$libraries)
          (constant-rib (compilation-context-libraries context) continuation))

        (($$macros)
          (constant-rib (compilation-context-macros context) continuation))

        (($$optimizers)
          (constant-rib (compilation-context-optimizers context) continuation))

        (($$quote)
          (constant-rib (cadr expression) continuation))

        (($$set!)
          (compile-expression
            context
            (caddr expression)
            (code-rib
              set-instruction
              (compilation-context-resolve
                (compilation-context-push-local context #f)
                (cadr expression))
              (compile-unspecified continuation))))

        (($$symbols)
          (constant-rib (compilation-context-symbols context) continuation))

        (else
          (compile-call context expression #f continuation))))

    (else
      (constant-rib expression continuation))))

(define (compile libraries macros optimizers expression)
  (compile-expression
    (make-compilation-context
      '()
      (filter
        (lambda (symbol)
          (not (library-symbol? symbol)))
        (unique
          (append
            (map car primitives)
            (find-symbols expression)
            (find-quoted-symbols libraries)
            (find-quoted-symbols macros)
            (find-quoted-symbols optimizers))))
      libraries
      macros
      optimizers)
    expression
    '()))

; Marshalling

(define-record-type marshal-context
  (make-marshal-context constants continuations)
  marshal-context?
  (constants marshal-context-constants marshal-context-set-constants!)
  (continuations marshal-context-continuations marshal-context-set-continuations!))

(define (nop-code? codes)
  (and
    (rib? codes)
    (eq? (rib-tag codes) nop-instruction)))

(define (marshal-constant context value)
  (define (marshal value)
    (marshal-rib context value #t))

  (cond
    ((null? value)
      (data-rib null-type 0 (cons-rib 0 0)))

    ((boolean? value)
      (if value
        (data-rib boolean-type 0 (marshal '()))
        (data-rib boolean-type (marshal '()) (marshal #t))))

    ((symbol? value)
      (data-rib
        symbol-type
        (marshal #f)
        (marshal (symbol->string (resolve-library-symbol value)))))

    ((char? value)
      (data-rib char-type (char->integer value) (marshal '())))

    ((string? value)
      (data-rib
        string-type
        (string-length value)
        (marshal (map char->integer (string->list value)))))

    ((pair? value)
      (cons-rib (marshal (car value)) (marshal (cdr value))))

    ((vector? value)
      (data-rib vector-type (vector-length value) (marshal (vector->list value))))

    ((bytevector? value)
      (data-rib bytevector-type (bytevector-length value) (marshal (bytevector->list value))))

    (else
      (error "invalid type"))))

(define (marshal-unique-constant context value)
  (cond
    ((assoc value (marshal-context-constants context)) =>
      cdr)

    (else
      (let ((marshalled (marshal-constant context value)))
        (marshal-context-set-constants!
          context
          (cons
            (cons value marshalled)
            (marshal-context-constants context)))
        marshalled))))

(define (marshal-rib context value data)
  (define (marshal value data)
    (marshal-rib context value data))

  (cond
    ((number? value)
      value)

    ((or data (null? value))
      (cond
        ((target-procedure? value)
          (unless (null? (rib-cdr value))
            (error "invalid environment"))
          (data-rib procedure-type (marshal (rib-car value) #f) (marshal '() #t)))

        ((or
            (null? value)
            (boolean? value)
            (char? value)
            (string? value)
            (symbol? value))
          (marshal-unique-constant context value))

        ((or (bytevector? value) (pair? value) (vector? value))
          (marshal-constant context value))

        (else
          (error "invalid type"))))

    ((nop-code? value)
      (cond
        ((assq value (marshal-context-continuations context)) =>
          cdr)

        (else
          (let ((continuation (code-rib nop-instruction 0 (marshal (rib-cdr value) #f))))
            (marshal-context-set-continuations!
              context
              (cons (cons value continuation) (marshal-context-continuations context)))
            continuation))))

    (else
      (rib
        (marshal (rib-car value) (not (= (rib-tag value) if-instruction)))
        (marshal (rib-cdr value) data)
        (rib-tag value)))))

(define (marshal codes)
  (marshal-rib (make-marshal-context '() '()) codes #f))

; Encoding

;; Context

(define-record-type encode-context
  (make-encode-context dictionary counts null)
  encode-context?
  (dictionary encode-context-dictionary encode-context-set-dictionary!)
  (counts encode-context-counts encode-context-set-counts!)
  (null encode-context-null))

(define (encode-context-push! context value)
  (encode-context-set-dictionary!
    context
    (cons value (encode-context-dictionary context))))

(define (encode-context-remove! context index)
  (let* ((dictionary (cons #f (encode-context-dictionary context)))
         (pair (list-tail dictionary index))
         (value (cadr pair)))
    (set-cdr! pair (cddr pair))
    (encode-context-set-dictionary! context (cdr dictionary))
    value))

(define (encode-context-position context value)
  (memq-position value (encode-context-dictionary context)))

(define (encode-context-find-count context value)
  (assq value (encode-context-counts context)))

;; Codes

(define integer-base 128)
(define number-base 16)
(define tag-base 16)
(define share-base 31)

(define (shared-value? value)
  (and
    (rib? value)
    (memq
      (rib-tag value)
      (list
        boolean-type
        char-type
        null-type
        string-type
        symbol-type
        ; This is technically equivalent to `symbol-type`. But we include this check for sanity.
        nop-instruction))))

(define (strip-nop-instructions codes)
  ; `symbol-type` is equal to `nop-instruction` although `car`s of symbols are
  ; all `#f` and nop instructions' are `0`.
  (if (and (nop-code? codes) (eq? (rib-car codes) 0))
    (strip-nop-instructions (rib-cdr codes))
    codes))

(define (increment-count! context value)
  (cond
    ((encode-context-find-count context value) =>
      (lambda (pair)
        (set-cdr! pair (+ 1 (cdr pair)))))

    (else
      (encode-context-set-counts!
        context
        (cons (cons value 1) (encode-context-counts context))))))

(define (decrement-count! pair)
  (set-cdr! pair (- (cdr pair) 1)))

(define (count-ribs! context codes)
  (define (count-data! value)
    (when (rib? value)
      (unless (and (shared-value? value) (encode-context-find-count context value))
        ((if (target-procedure? value) count-code! count-data!) (rib-car value))
        (count-data! (rib-cdr value)))
      (when (shared-value? value)
        (increment-count! context value))))

  (define (count-code! codes)
    (cond
      ((number? codes)
        #f)

      ((eq? codes (encode-context-null context))
        (count-data! codes))

      ((nop-code? codes)
        (let* ((codes (strip-nop-instructions codes))
               (counted (encode-context-find-count context codes)))
          (increment-count! context codes)
          (unless counted
            (count-code! codes))))

      (else
        ((if (= (rib-tag codes) if-instruction) count-code! count-data!)
          (rib-car codes))
        (count-code! (rib-cdr codes)))))

  (count-code! codes))

(define (fraction x)
  (- x (floor x)))

; We need to fit 64-bit floating-point numbers' sign, mantissa, and exponent into its mantissa...
; TODO Shouldn't this exponent be a higher number like 37 = 52 - 12 - 3?
(define maximum-float-integer (expt 2 30))

; Lossy decomposition of floating-point numbers into a signed mantissa and an exponent.
(define (decompose-float x)
  (define (mantissa y)
    (/ x (expt 2 y)))

  (do ((y (log x 2) (- y 1)))
    ((or
        (< (fraction (mantissa (floor y))) epsilon)
        (> (mantissa (+ y 1)) maximum-float-integer))
      (let ((y (floor y)))
        (values (exact (round (mantissa y))) (exact y))))))

(define (encode-integer-part integer base bit)
  (+ bit (* 2 (modulo integer base))))

(define (encode-integer-parts integer base)
  (let ((rest (quotient integer base)))
    (values
      (encode-integer-part integer base (if (zero? rest) 0 1))
      rest)))

; Unlike Ribbit Scheme, we use the forward encoding algorithm. So this integer encoding also proceeds forward.
; Therefore, we need to adopt little endianness like the `varint` in Protocol Buffer.
(define (encode-integer-tail x)
  (do ((x x (quotient x integer-base)))
    ((zero? x))
    (write-u8
      (encode-integer-part
        x
        integer-base
        (if (zero? (quotient x integer-base)) 0 1)))))

(define (encode-number x)
  (cond
    ((and (integer? x) (negative? x))
      (+ 1 (* 4 (abs x))))

    ((integer? x)
      (* 2 x))

    (else
      (let-values (((m e) (decompose-float (abs x))))
        (+
          3
          (*
            4
            (+
              (if (negative? x) 1 0)
              (* 2 (+ e 1023))
              (* 4096 m))))))))

(define (encode-rib context value)
  (cond
    ((rib? value)
      (let* ((value (strip-nop-instructions value))
             (entry (encode-context-find-count context value)))
        (cond
          ((and
              (not entry)
              (zero? (rib-tag value))
              (number? (rib-car value))
              (integer? (rib-car value))
              (<= 0 (rib-car value) 127))
            (encode-rib context (rib-cdr value))
            (write-u8 (* 2 (rib-car value))))

          ((and entry (encode-context-position context value)) =>
            (lambda (index)
              (decrement-count! entry)
              (let ((removed (zero? (cdr entry)))
                    (value (encode-context-remove! context index)))
                (unless removed
                  (encode-context-push! context value))
                (let-values (((head tail)
                               (encode-integer-parts
                                 (+ (* 2 index) (if removed 0 1))
                                 share-base)))
                  (write-u8 (+ 1 (* 4 (+ 1 head))))
                  (encode-integer-tail tail)))))

          (else
            (let ((tag (rib-tag value)))
              (encode-rib context (rib-car value))
              (encode-rib context (rib-cdr value))

              (let-values (((head tail) (encode-integer-parts tag tag-base)))
                (write-u8 (+ 3 (* 8 head)))
                (encode-integer-tail tail))

              (when entry
                (encode-context-push! context value)
                (decrement-count! entry)
                (write-u8 1)))))))

    (else
      (let-values (((head tail) (encode-integer-parts (encode-number value) number-base)))
        (write-u8 (+ 7 (* 8 head)))
        (encode-integer-tail tail)))))

;; Primitives

(define (build-primitive primitive continuation)
  (code-rib
    constant-instruction
    (data-rib procedure-type (cadr primitive) '())
    (code-rib
      set-instruction
      (car primitive)
      continuation)))

; TODO Consider moving this logic to marshalling.
(define (build-primitives primitives continuation)
  (if (null? primitives)
    continuation
    (build-primitive
      (car primitives)
      (build-primitives (cdr primitives) continuation))))

;; Main

(define (encode codes)
  (let ((context (make-encode-context '() '() (rib-car (rib-car codes)))))
    (count-ribs! context codes)
    (encode-context-set-counts!
      context
      (filter
        (lambda (pair) (> (cdr pair) 1))
        (encode-context-counts context)))
    (encode-rib context codes)

    (let ((size (length (encode-context-dictionary context))))
      (unless (zero? size)
        (error "dictionary not empty" size)))

    (do ((counts (encode-context-counts context) (cdr counts)))
      ((null? counts))
      (unless (zero? (cdar counts))
        (error "invalid constant count" (map cdr counts))))))

; Main

(define (main)
  (define expression0 (incept (read-source)))
  (define-values (expression1 libraries) (expand-libraries expression0))
  (define-values (expression2 macros) (expand-macros expression1))
  (define-values (expression3 optimizers) (optimize2 '() expression2))

  (encode
    (marshal
      (cons-rib
        #f
        (build-primitives
          primitives
          (compile libraries macros optimizers expression3))))))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme bytecode compiler.\n\n")
    (write-string "Usage: stak-compile < SOURCE_FILE > BYTECODE_FILE\n")
    (exit)))

(main)
