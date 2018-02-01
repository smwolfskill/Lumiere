# Code formatting

* All code should adhere to a strict 80 column limit and 2 **SPACE** tabs. No
  hard tabs, period.
* All operators should be spaced: `c = a + b * c`.
* Parentheses should be used only when absolutely necessary for clarity. Often
  times, it is much more clear to assign things inside a parenthetical to a
  temporary variable, which makes understanding the computation much easier. The
  optimizer can figure out how to make these computations efficient.
* Always add a space between keywords. No space is required inside parentheses.
  e.g. `while (condition != true) { ... }`.
* Braces are to be put on a separate line. e.g.

```c
int main(int argc, char **argv)
{
  while (global_condition != true)
  {
    // ...
  }
  return 0;
}
```

* Variable names should reflect their scope. If at any point a variable name
  (minus prefixes) is over 8 characters long, ask yourself if it is really
  necessary. Nobody wants to type a 5 character loop variable several times
  inside the loop body.
* camelCase is the standard naming convention.
* Global variables should be prefixed with `g_`, and class member variables
  should be prefixed with `m_`. e.g. `g_globVar` and `m_membVar`.
* Functions with a long list of arguments should be formatted as follows:

```c
functionWithAReallyLongNameForSomeReason(arg1, arg2, arg3,
                                         arg4, arg5, arg6,
                                         // ...
                                        );
functionWithAReallyLongNameForSomeReason(
  arg1, arg2, arg3,
  arg4, arg5, arg6,
  // ...
);
```

  If arguments occur on the same line as the function, indent to the first
  argument. Otherwise, add one level of indentation.
* Predetermined constants should be capitalized and underscored (not camelCase).
  e.g. `BinarySpacePartition.MAX_DEPTH`. Constant variables follow the previous
  convention. e.g. `ImmutableArray.m_arrSize`.
