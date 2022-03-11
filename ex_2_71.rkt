#lang sicp

;Exercise 2.71: Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of the symbols are 1 , 2 , 4 , ... , 2**(n - 1) .
;Sketch the tree for n = 5 ;
;for n = 10 . In such a tree (for general n ) how many bits are required to encode the most frequent symbol? The least frequent symbol?

;The tree looks like a chain, with a leaf on one breach and the rest of the tree on the other.
;So the most frequent leaf directly hangs off the top of the tree. The least frequent leaf hangs off the bottom of the tree.
;The most frequent symbol just requires 1 bit to encode, the least frequent symbol requires n-1 bits.
