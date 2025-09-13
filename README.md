# CDPlayer
Playing with Conceptual Dependencies ideas

# Components:

- tokenizer
- POS tagger: HMM, trained on CoNLLU files -- C++
- dependency tree builder: statistics (3D matrix *from* -> *to* -> *dependency relation*), building graph of all possible relation for input sentence, then find minimal spanning tree from root (chu liu edmonds algorithm) -- C++
- coreference resolution
- tree -> semantic primitives rules -- Haskell
