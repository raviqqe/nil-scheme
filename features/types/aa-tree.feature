Feature: AA tree
  @stak
  Scenario: Create an empty tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (aa-tree-empty <)
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  @stak
  Scenario: Check if a value is a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (write-u8 (if (aa-tree? (aa-tree-empty <)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak
  Scenario: Find no value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (write-u8 (if (aa-tree-find tree 1) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  @stak
  Scenario: Insert a value into a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)

      (write-u8 (if (= (aa-tree-find tree 1) 1) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak
  Scenario: Insert a value into a left of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 2)
      (aa-tree-insert! tree 1)

      (for-each
        (lambda (x)
          (write-u8 (if (= (aa-tree-find tree x) x) 65 66)))
        '(1 2 3))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAB"

  @stak
  Scenario: Insert a value into a right of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)
      (aa-tree-insert! tree 2)

      (for-each
        (lambda (x)
          (write-u8 (if (= (aa-tree-find tree x) x) 65 66)))
        '(1 2 3))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAB"

  @stak
  Scenario: Insert a value into the same node of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)
      (aa-tree-insert! tree 1)

      (write-u8 (if (= (aa-tree-find tree 1) 1) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak
  Scenario Outline: Insert values into a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (define (check x)
        (write-u8 (if (= (aa-tree-find tree x) x) 65 66)))

      (for-each
        (lambda (x)
          (check x)
          (aa-tree-insert! tree x)
          (check x))
        '(<values>))

      (for-each check '(<values>))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values  | output       |
      | 1 2 3   | BABABAAAA    |
      | 1 3 2   | BABABAAAA    |
      | 2 1 3   | BABABAAAA    |
      | 2 3 1   | BABABAAAA    |
      | 3 1 2   | BABABAAAA    |
      | 3 2 1   | BABABAAAA    |
      | 1 2 3 4 | BABABBAAAAAA |
      | 1 2 4 3 | BABABBAAAAAA |
      | 1 3 2 4 | BABABBAAAAAA |
      | 1 3 4 2 | BABABBAAAAAA |
      | 1 4 2 3 | BABABBAAAAAA |
      | 1 4 3 2 | BABABBAAAAAA |
      | 2 1 3 4 | BABABBAAAAAA |
      | 2 1 4 3 | BABABBAAAAAA |
      | 2 3 1 4 | BABABBAAAAAA |
      | 2 3 4 1 | BABABBAAAAAA |
      | 2 4 1 3 | BABABBAAAAAA |
      | 2 4 3 1 | BABABBAAAAAA |
