# Turing machine Dateiformat

TL;DR: Beispiel:

```
start z_0
final z_42 z_e

; Kommentar, diese Zeile wird ignoriert
(z_0, a) -> (z_0, _, R)
(z_0, _) -> (z_0, a, N)
(z_1, b) -> (z_3, $, L) ;Auch ein Kommentar
```

Ihre Datei wird zeilenweise verarbeitet. Die Reihenfolge ist egal.
Valide Zeilen beginnen mit:

- `start` gefolgt von mindestens einem Leerzeichen und einem Zustands-Identifier (z.B. `start z0`).
   Es muss genau eine solche Definition geben. (Falls es mehrere gibt, "gewinnt" die letzte)
- `final` gefolgt von mindestens einem Leerzeichen und mindestens einem Zustands-Identifier (z.B. `final z1 z2 z3`).
   Es muss mindestens eine solche Definition geben, weitere Definitionen werden kombiniert.
- `(` für Definitionen der Überführungsfunktion in der Form von
  `(Zustand, Symbol) -> (Zustand, neues-Symbol, Richtung)`,
  z.B. `(z0, a) -> (z1, b, L)`.
  Erlaubte Richtungen sind `L` (links), `R` (rechts) und `N` (neutral).
  Klammern und Kommata sind wichtig, Whitespaces dürfen dazwischen nach Belieben eingefügt werden.

Weitere Anmerkungen:
- Alles nach einem `;` wird bis zum Zeilenende ignoriert
- Das Symbol für das Blank-Symbol lautet `_`.
- Zustands-Identifier bestehen aus mindestens einem Word-Character (die Zeichen in der Range `[a-zA-Z_0-9]`, also a, ..., z, A, .., Z, \_, 0, ..., 9).
  Beispiele für gültige Zustände wären also: `A`, `z0`, `z_42` aber nicht `z-0`
- Für das Arbeitsalphabet darf jedes (einzelne) Zeichen verwendet werden, außer Whitespace, ',', '(' und `)`.

## LBA Dateiformat

TL;DR: Beispiel:

```
start z_0
final z_42 z_e
symbols (0, a) (1, b)

; Kommentar, diese Zeile wird ignoriert
(z_0, a) -> (z_0, _, R)
(z_0, _) -> (z_0, a, N) ;Auch ein Kommentar
(z_1, b) -> (z_3, $, L)
```

Ein LBA wird fast wie eine Turing-Maschine angegeben.
Die initiale Konfiguration ersetzt nach Vorlesung das letzte Zeichen mit einer Version mit einem "Hut" darauf.
Diese sind in der Regel schwierig zu tippen.
Stattdessen müssen Sie für alle Zeichen des Eingabealphabetes einen Ersatz für das Symbol mit "Hut" definieren.

Der einzige Unterschied im Format ist, dass in einer Zeile, welche mit `symbols` startet, eine Reihe an Tupel definiert werden muss.
Ein Tupel besteht aus einer öffnenden Klammer `(`, einem Word-Character (die Zeichen in der Range `[a-zA-Z_0-9]`, also a, ..., z, A, .., Z, \_, 0, ..., 9),
einem Komma `,` einem nicht-Whitespace Zeichen (Sie dürfen also auch Unicode-Symbole, die bspw. "Hut"-Versionen des Symbols darstellen, verwenden)
sowie einer schließenden Klammer `)`.

Das Beispiel oben ersetzt also das letzte Symbol, falls es eine `0` ist, mit einem `a`, oder falls es eine `1` ist durch ein `b`.
Die initiale Eingabe `0011` auf dem Band wird dabei also als `001b` dargestellt,
die initiale Eingabe `0000` als `000a` und das leere Wort als `_`.
