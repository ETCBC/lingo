# Heads

Export edge relationships between phrases and their head words.

We define a "head word" in accordance with a basic linguistic definition:

> In linguistics, the head of a phrase is the word that determines the syntactic category of that phrase. For example, the head of the noun phrase boiling hot water is the noun water. ([wikipedia](https://en.wikipedia.org/wiki/Head_(linguistics))).

## Contents
There are two datasets produced in this directory:
1) heads.tf - an edge feature from a phrase node to its head and the head's coordinate words
2) prep_obj.tf - an edge feature from a preposition to its object. This feature is only for prepositions in a head position in the phrase

## Methodology
The methodology used to produce this dataset is explained in [getting_heads.ipynb](getting_heads.ipynb).

## Disclaimer
This is an experimental dataset. Some complex phrases cannot yet be processed properly due to shortcomings in the data model. See [getting_heads.ipynb](getting_heads.ipynb) for a full explanation. There remains room for improvement. We would be happy to receive suggestions and feedback if you have any.
