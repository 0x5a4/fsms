# NFA file format

A NFA file is processed line-by-line. The ordering of the lines does not matter. Anything after a `;` is regarded as a comment until the end of the line.
A line can be (in any order):

- The word `start` followed by a whitespace character, followed by at least one state identifier.
  Example: `start z1`, `start z1 z2`.
- The word `final` followed by a whitespace character, followed by at least one state identifier.
  Example: `final z1 z2`.
- A transition starting with an opening parenthesis `(`, followed by a state identifier, 
  a comma, a character from the alphabet, a closing parenthesis `)`,
  an arrow `->`,
  a state identifier,
  Whitespaces can be interleaved arbitrarily.
  Example: `(z0, a -> z1`.

Exactly line containing the start state as well as one line specifying accepting states is expected.
