 Some example materials for an upcoming Haskell DC talk on GADTs.
 There are three examples here, listed (roughly) in order of
 increasing sophistication:
 
 - *tuple*: A simple, and simply typed, embedded language allowing
   manipulation of constants. Allowed types are integers, booleans,
   and pairs of types.
 - *calc*: A reverse Polish notation (RPN) stack-based calculator
   language allowing arithmetic operations on integers.
 - *stlc*: An implementation of a simply-typed lambda calculus (STLC)
   with integer and boolean primitive types.
   
Each example is implemented in three different ways, and so within
each example directory, there are three independent Haskell files
which may be loaded with GHCi:

- *Step1.hs*: Implementation using Haskell98 types, without checking
  for well-formedness of the embedded language. Evaluation is left
  undefined for expressions which are not well-formed, so attempting
  to evaluate non-well-formed expressions results in an incomplete 
  pattern match failure.
- *Step2.hs*: Implementation using GADTs (and for *calc* and *stlc*,
  DataKinds as well). The Haskell compiler's type system verifies
  that expressions within the embedded language are well-formed.
  The code to evaluate expressions need not handle data corresponding
  to non-well-formed expressions; in fact, doing so causes a Haskell
  compiler error. The pattern matching failures from *Step1.hs* are
  now verified to be impossible. (NOTE: GHC currently still warns of
  incomplete patterns due to a bug. If you try to handle the so-called
  missing patterns, a compiler error will result.)
- *Step3.hs*: An implementation with GADTs (similar to *Step2.hs*)
  together with a Parsec parser that parses in expressions of the
  embedded language from strings into the GADT expression language,
  verifying the well-formedness of strings which are parsed successfully.
  
The main files for *stlc* accomplish variable referencing via de
Bruijn indices. There is a file [*Named.hs*]
(https://github.com/bmsherman/gadts-talk/blob/master/stlc/Named.hs)
where variables are given names (Strings, or, at the type level,
"Symbols") instead. My implementation requires closed type families
with pattern matching to do the variable name lookup at compile-time.
Unfortunately, it is impossible to recapitulate the lookup at runtime
because failure to match types in a type-family definition cannot be
saved in a constraint or data structure. So we use `unsafeCoerce`
to make GHC happy.

