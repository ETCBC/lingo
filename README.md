[![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/ETCBC/lingo/)](https://archive.softwareheritage.org/browse/origin/https://github.com/ETCBC/lingo/)

# lingo
Various (linguistic) applications of the BHSA data:

## Minimal pairs
Compute pairs of words that have a limited edit distance.
If the distance is 1, both words differ in exactly one character.

Various decisions have to be taken:

* do we feed lexemes (lemmas) or word occurrences (both of course)
* do we retain accents?
* do use unicode normalization or denormalization?

We do not provide all options here.
But the code is reasonably clear, so, clone it and go your own way!

Done for Hebrew and Greek.

## Trees
Generate syntax trees from the Hebrew data.
The notebook does all BHSA versions.
De trees are exported in plain text in bracket notation.
The output is in the repo.

## Vocabulary
A simple notebook to produce list of Hebrew and Aramaic lexemes
in order of decreasing frequency.

## Voyant
If you want to put the data in a corpus form, you can use the notebook
[prepare.ipynb](https://github.com/ETCBC/lingo/blob/master/voyant/prepare.ipynb).

You can choose a text format and a granularity, and then the notebook will
create a corpus for that in a local `_temp` directory.
Then you can zip it and upload to text processing services such as
[Voyant](http://voyant-tools.org).

## Heads
Export edge relationships between phrases and their head words. A head word is defined as "the word that determines the syntactic category of that phrase" [(wikipedia)](https://en.wikipedia.org/wiki/Head_(linguistics)).
