@refactor
Feature: Rename variable inside clause
  Background:
    And I turn on lb-datalog-mode
    And I insert:
    """
    f(Var2) <- g(Var2).
    
    pred(Var1, Var2) <-
       foo(Var1), bar(Var2),
       foobar[Var2] = Var1.

    """
  
  Scenario: Rename variable used as key
    When I place the cursor between "bar(V" and "ar2)"
    Then the region should not be active
    And I rename symbol to "Var3"
    Then I should see:
    """
    f(Var2) <- g(Var2).
    
    pred(Var1, Var3) <-
       foo(Var1), bar(Var3),
       foobar[Var3] = Var1.
    """
