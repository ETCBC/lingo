# lingo
Various (linguistic) applications of the BHSA data:

## Minimal pairs
Compute pairs of words that have a limited edit distance.
If the distance is 1, both words differ in exactly one character.

Various decisions have to be taken:

* do we feed lexemes (lemmas) or word occurrences
* do we retain accents?
* do use unicode normalization or denormalization?

We do not provide all options here.
But the code is reasonably clear, so, clone it and go your own way!
