names
=====

A tool for processing lists of words into letter transition matrices, and generating dendrograms from a list of such transition matrices for different word lists. Random words can also be generated from the transition matrices.

It can be used for example to construct trees of relations between written languages.

## Example Usage

Process a word list to a transition matrix:

    names analyse -w words.txt -t words.trans

Generate a dendrogram from a directory of transition matrices

    names dendrogram -T trans/ -o dendrogram.svg

Generate 20 random word from a word list

    names analyse -w words.txt | names words -c 20
