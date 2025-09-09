# DPDA file format

A DPDA file is processed line-by-line. The ordering of the lines does not matter. Anything after a `;` is regarded as a comment until the end of the line.
A line can be (in any order):

- A comment (starting with `;`).
- The word `start` followed by a whitespace character, followed by a valid state identifier.
  Example: `start z0`.
- The word `final` followed by a whitespace character, followed by at least one state identifier.
  Example: `final z1 z2`.
- A transition starting with an opening parenthesis `(`, followed by a state identifier, a comma,
  a character from the alphabet, a comma
  a character from the working alphabet or lambda, a closing parenthesis `)`,
  an arrow `->`,
  an opening parenthesis `(`,
  a state identifier,
  at least one character from the working alphabet,
  and a closing parenthsis `).
  Whitespaces can be interleaved arbitrarily.
  Example: `(z0, a, #) -> (z1, _)`.

Exactly line containing the start state as well as one line specifying accepting states is expected.
