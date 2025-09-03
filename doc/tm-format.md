# Turing machine file format

A Turing machine file is processed line-by-line. The ordering of the lines does not matter.
A line can be (in any order):

- A comment (starting with `;`).
- The word `start` followed by a whitespace character, followed by a  state identifier. Note that the last `start` statement "wins"
  Example: `start z1`
- The word `final` followed by a whitespace character, followed by at least one state identifier.
  Example: `final z1 z2`.
- A transition starting with an opening parenthesis `(`, followed by a state identifier, 
  a comma, a character from the alphabet, a closing parenthesis `)`,
  an arrow `->`, an opening parenthesis `(`, followed by state identifier, a comma, 
  a symbol to be written, a comma, a direction instruction `R`, `L` or `N` and finally
  a closing parenthesis `)`.
  Whitespaces can be interleaved arbitrarily.
  Example: `(z0, a -> (z0, a, N)`.

Exactly line containing the start state as well as one line specifying accepting states is expected.
