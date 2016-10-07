Feature: Movement
  Background:
    And I turn on lb-datalog-mode
    And I insert:
    """
    foo(x) -> bar(x).
    foo(x) <- bar(x).

    foobar(x,y) <-
       foo(x), bar(y).
    """

  Scenario: Move by clause
    And I move "2" clauses backwards
    Then the cursor should be before "foo(x) <- bar(x)."
    And I move "1" clauses forward
    Then the cursor should be after "foo(x) <- bar(x)."
    And I move "1" clauses forward
    Then the cursor should be after "foo(x), bar(y)."
    And I move "1" clauses backwards
    Then the cursor should be before "foobar(x,y) <-"
    And I move "2" clauses backwards
    Then the cursor should be before "foo(x) -> bar(x)."

  Scenario: Move by atom
    And I move "2" atoms backwards
    Then the cursor should be before "foo(x),"
    And I move "1" atoms forward
    Then the cursor should be after "foo(x)"
    And I move "1" atoms forward
    Then the cursor should be after "bar(y)"
    And I move "3" atoms backwards
    Then the cursor should be before "foobar(x,y)"
