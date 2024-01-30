Feature: Library system
  @stak @gauche
  Scenario: Define a library
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (export foo)
      (import (scheme base))

      (begin
        (define (foo x)
          (write-u8 x))))
    """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  @stak @gauche
  Scenario: Import a library twice
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (import (scheme base) (scheme write))

      (begin
        (display "foo")))

    (import (foo))
    (import (foo))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"

  @stak @gauche
  Scenario: Import a function
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (export foo)
      (import (scheme base))

      (begin
        (define (foo x)
          (write-u8 x))))

    (import (foo))

    (foo 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak @gauche
  Scenario: Import functions
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (export foo bar)
      (import (scheme base))

      (begin
        (define (foo x)
          (write-u8 x))

        (define (bar x)
          (write-u8 (+ x 1)))))

    (import (foo))

    (foo 65)
    (bar 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  @todo @stak @gauche
  Scenario: Import a function with a prefix
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (export foo)
      (import (scheme base))

      (begin
        (define (foo x)
          (write-u8 x))))

    (import (prefix (foo) bar-))

    (bar-foo 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @todo @stak @gauche
  Scenario: Import a renamed function
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (export foo)
      (import (scheme base))

      (begin
        (define (foo x)
          (write-u8 x))))

    (import (rename (foo) (foo bar)))

    (bar 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @todo @stak @gauche
  Scenario: Export a renamed function
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (export (rename foo bar))
      (import (scheme base))

      (begin
        (define (foo x)
          (write-u8 x))))

    (import (foo))

    (bar 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
