Feature: Bytevector
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Write a bytevector
    Given a file named "main.scm" with:
    """scheme
    (write-bytevector #u8(65 66 67))
    """
    When I run the following script:
    """sh
    cat prelude.scm main.scm | ./compile.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"
