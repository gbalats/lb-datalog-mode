Feature: Movement
  Background:
    And I turn on lb-datalog-mode
    And I insert:
    """
    foo(x) -> bar(x).
    foo(x) <- bar(x).

    foobar(x,y) <-
       foo(x), bar(y).

    foobar:bar[x, y] = z, foobar:foo[y, z] = x
     <-
       foo(x), foobar:foo[x] = z, foo(z),
       bar(y), foobar:foo[y] = z.
    """

  Scenario: Move by clause
    When I go to end of buffer
    And I move "3" clauses backwards
    Then the cursor should be before "foo(x) <- bar(x)."
    And I move "1" clauses forward
    Then the cursor should be after "foo(x) <- bar(x)."
    And I move "1" clauses forward
    Then the cursor should be after "foo(x), bar(y)."
    And I move "1" clauses backwards
    Then the cursor should be before "foobar(x,y) <-"
    And I move "2" clauses backwards
    Then the cursor should be before "foo(x) -> bar(x)."
    When I go to beginning of buffer
    And I move "4" clauses forward
    Then the cursor should be after "foobar:foo[y] = z."

  Scenario: Move by atom
    When I go to line "6"
    And I move "2" atoms backwards
    Then the cursor should be before "foo(x),"
    And I move "1" atoms forward
    Then the cursor should be after "foo(x)"
    And I move "1" atoms forward
    Then the cursor should be after "bar(y)"
    And I move "3" atoms backwards
    Then the cursor should be before "foobar(x,y)"

  Scenario: Move through functional atoms
    When I go to end of buffer
    And I move "1" atoms backwards
    Then the cursor should be before "foobar:foo[y] = z"
    And I move "5" atoms backwards
    Then the cursor should be before "foobar:foo[y, z] = x"
    And I move "1" atoms backwards
    Then the cursor should be before "foobar:bar[x, y] = z"
    And I move "2" atoms forward
    Then the cursor should be after "foobar:foo[y, z] = x"
    And I move "1" atoms forward
    Then the cursor should be after "foo(x)"
    And I move "4" atoms forward
    Then the cursor should be after "foobar:foo[y] = z"

  Scenario: Interclausal movement by atom
    When I go to end of buffer
    And I move "1" clauses backwards
    And I move "2" atoms forward
    And I move "3" atoms backwards
    Then the cursor should be before "bar(y)"
    And I move "2" atoms forward
    Then the cursor should be after "foobar:bar[x, y] = z"
