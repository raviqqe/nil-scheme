Feature: Parameter
  Scenario: Make a parameter
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 ((make-parameter 65)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Parameterize a function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define x (make-parameter 65))

    (write-u8 (x))
    (write-u8 (parameterize ((x 66)) (x)))
    (write-u8 (x))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABA"

  Scenario: Use multiple parameters
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define x (make-parameter 65))
    (define y (make-parameter 66))

    (write-u8 (x))
    (write-u8 (y))
    (parameterize ((x 67) (y 68))
      (write-u8 (x))
      (write-u8 (y)))
    (write-u8 (x))
    (write-u8 (y))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABCDAB"
