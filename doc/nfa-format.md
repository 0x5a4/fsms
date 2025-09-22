# NFA/DFA Dateiformat

Da ein DFA einfach nur ein deterministischer NFA ist, deklarieren wir sie im selben Format.
Anschließend wird überprüft ob unser NFA auch wirklich ein DFA ist.

TL;DR: Beispiel:

```
start z_0 z_1
final z_42 z_e

; Kommentar, diese Zeile wird ignoriert
(z_0, a) -> z_e
(z_0, b) -> z_0 ;Auch ein Kommentar
```

Ihre Datei wird zeilenweise verarbeitet. Die Reihenfolge ist egal.
Valide Zeilen beginnen mit:

- `start` gefolgt von mindestens einem Leerzeichen und einem Zustands-Identifier (z.B. `start z0`, `start z0 z1`).
   Es muss mindestens eine solche Definition geben, weitere Definitionen werden kombiniert.
- `final` gefolgt von mindestens einem Leerzeichen und mindestens einem Zustands-Identifier (z.B. `final z1 z2 z3`).
   Es muss mindestens eine solche Definition geben, weitere Definitionen werden kombiniert.
- Zeilen, die mit `;` beginnen, werden ignoriert.
- `(` für Definitionen der Überführungsfunktion in der Form von
  `(Zustand, Symbol) -> Zustand`,
  z.B. `(z0, a) -> z1`.
  Klammern und Kommata sind wichtig, Whitespaces dürfen dazwischen nach Belieben eingefügt werden.

Weitere Anmerkungen:
- Alles nach einem `;` wird bis zum Zeilenende ignoriert
- Da ein DFA deterministisch ist, ist nur ein Startzustand erlaubt.
- Zustands-Identifier bestehen aus mindestens einem Word-Character (die Zeichen in der Range `[a-zA-Z_0-9]`, also a, ..., z, A, .., Z, \_, 0, ..., 9).
  Beispiele für gültige Zustände wären also: `A`, `z0`, `z_42` aber nicht `z-0`
- Für das Arbeitsalphabet darf jedes (einzelne) Zeichen verwendet werden, außer Whitespace, ',', '(' und `)`.
