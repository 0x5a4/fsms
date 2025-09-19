# DPDA Dateiformat

TL;DR: Beispiel:

```
start z_0 z_1
final z_42 z_e

; Kommentar, diese Zeile wird ignoriert
(z_0, a, A) -> (z_0, A)
(z_0, b, B) -> (z_0, AAAA)
(z_1, _, B) -> (z_e, _)
```

Ihre Datei wird zeilenweise verarbeitet. Die Reihenfolge ist egal.
Valide Zeilen beginnen mit:

- `start` gefolgt von mindestens einem Leerzeichen und einem Zustands-Identifier (z.B. `start z0`).
   Es muss genau eine solche Definition geben. (Falls es mehrere gibt, "gewinnt" die letzte)
- `final` gefolgt von mindestens einem Leerzeichen und mindestens einem Zustands-Identifier (z.B. `final z1 z2 z3`).
   Es muss mindestens eine solche Definition geben, weitere Definitionen werden kombiniert.
- Zeilen, die mit `;` beginnen, werden ignoriert.
- `(` für Definitionen der Überführungsfunktion in der Form von
  `(Zustand, Symbol, Stack-Symbol) -> (Zustand, Stack-Symbol+)`,
  z.B. `(z0, a, A) -> (z1, B)`.
  Klammern und Kommata sind wichtig, Whitespaces dürfen dazwischen nach Belieben eingefügt werden.

Weitere Anmerkungen:
- Das Symbol für das Blank-Symbol lautet `_`. Dieses Symbol wird auch verwendet um "Nichts" in den Keller zu legen
- Zustands-Identifier bestehen aus mindestens einem Word-Character (die Zeichen in der Range `[a-zA-Z_0-9]`, also a, ..., z, A, .., Z, \_, 0, ..., 9).
  Beispiele für gültige Zustände wären also: `A`, `z0`, `z_42` aber nicht `z-0`
- Für das Arbeitsalphabet darf jedes (einzelne) Zeichen verwendet werden, außer Whitespace, ',', '(' und `)`.
