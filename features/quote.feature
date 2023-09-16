Feature: Quote
  Scenario: Quote a number
    Given a file named "main.scm" with:
    """scheme
    (write-u8 '65)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Quote a list
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 '(65 66 67))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"

  Scenario: Quasi-quote a number
    Given a file named "main.scm" with:
    """scheme
    (write-u8 `65)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Quasi-quote a list
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 `(65 66 67))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"

  Scenario: Expand a variable in a quasi-quotation
    Given a file named "main.scm" with:
    """scheme
		(define x 65)
		(define y 66)
		(define z 67)

    (map write-u8 `(,x ,y ,z))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"
