Feature: Marking
  Background:
    Given there is no region selected
    And I turn on lb-datalog-mode
    And I insert:
    """
    foo(x) -> bar(x).
    foo(x) <- bar(x).

    foobar(x,y) <-
       foo(x), bar(y).
    """

  Scenario: Mark clause at beginning
    When I place the cursor before "foobar(x,y) <-"
    And I mark the surrounding clause
    Then the region should be:
    """
    foobar(x,y) <-
       foo(x), bar(y).
    """
    And I press "C-g"
    Then the region should not be active

  Scenario: Mark clause at end
    When I place the cursor after "foo(x), bar(y)."
    And I mark the surrounding clause
    Then the region should be:
    """
    foobar(x,y) <-
       foo(x), bar(y).
    """
  
  Scenario: Mark clause at middle
    When I place the cursor between "foobar(x,y) " and "<-"
    And I mark the surrounding clause
    Then the region should be:
    """
    foobar(x,y) <-
       foo(x), bar(y).
    """

  Scenario: Mark definition clause
    When I place the cursor after "->"
    And I mark the surrounding clause
    Then the region should be "foo(x) -> bar(x)."

  Scenario: Mark nearby clause
    When I place the cursor before "<- bar(x)"
    And I mark the surrounding clause
    Then the region should be "foo(x) <- bar(x)."
