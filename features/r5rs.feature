Feature: The R5RS library
  Scenario: Add numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write (+ 40 2))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "42"

  Scenario: Quote a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (for-each write-char '(#\A #\B #\C))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Read an S-expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write (read))
      """
    And a file named "input.txt" with:
      """text
      (1 2 3)
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "(1 2 3)"

  Scenario: Write an S-expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write '(1 2 3))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "(1 2 3)"

  Scenario: Force a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write-char (force (delay (integer->char 65))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Evaluate an S-expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write
        (eval
          '(+ 40 2)
          (scheme-report-environment 5)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "42"
