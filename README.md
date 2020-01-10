# zeta
functional neural networks in ocaml

## Why?
functions are values :)

But, with all seriousness, I personally think that the combination between functional programming and deep learning can create interesting results as I've realized several dualities between the two.

## Features of zeta

1. Pedagogical
The source code of zeta is designed to be easy-to-read and succinct so that the user can get more beyond merely using this library for their daily research. I will later add more documentations and possibly tutorials for this library.

2. Functional
One of the most annoying error messages I've encountered in PyTorch looks something like this:

```
RuntimeError: expected Double tensor (got Float tensor)
```

zeta aims to "moves" errors like this from runtime to compile-time by adopting a functional programming paradigm in OCaml.

3. Dynamic Computation Graphs
zeta provides interfaces similar to that of the PyTorch, where users can create a computational graph on-the-fly.

4. Imperative
The implementation of zeta's core module, Tensor, is inherently imperative. This is to help create a more efficient representation of a computation graph, and therefore a neural network.

5. ADTs/GADTs (Algebraic Datatypes / Generalized Algebraic Datatypes)
One of the main contributions of zeta is to abstract neural network and tensor operations into numerous ADTs/GADTs, and in the process summarizing some of the basic behaviors deep learning algorithms exhibit. For example, a tensor can be recursively defined as a GADT:

```
type 'a tensordata = 
      | IntScalar : int ref -> int tensordata
      | FloatScalar : float ref -> float tensordata
      | BoolScalar : bool ref -> bool tensordata
      | IntTensor : int tensordata array -> int tensordata
      | FloatTensor : float tensordata array -> float tensordata
      | BoolTensor : bool tensordata array  -> bool tensordata
```

Which inherently restricts creations of ill-typed tensors, e.g., implicit casting is performed in this PyTorch example:

```
>>> b = torch.FloatTensor([False])
>>> b
tensor([0.])
```

But the following would not type check in zeta:

```
let a = FloatTensor [| BoolScalar (ref false) |];;
Error: This expression has type bool tensordata
       but an expression was expected of type float tensordata
       Type bool is not compatible with type float 
```

## To-Do List

- Tensor viewing, slicing, reshaping, concatenating
- Tensor dot product and tensor product
- Matrix multiplication
- Convolutions (1d, 2d, 3d)
- Autograd mechanisms: hooks, backward, etc.
- Neural Network (G)ADT abstractions
- Optimizer (G)ADT abstractions
- Data loader and training abstractions

## References/Inspirations

Also, this project is inspired/helped by the following works:

http://www.cs.cmu.edu/afs/cs/academic/class/15210-f14/www/docs/sig/SEQUENCE.html

https://arxiv.org/pdf/1411.0583.pdf

https://en.wikipedia.org/wiki/Automatic_differentiation#Automatic_differentiation_using_dual_numbers

https://github.com/ThoughtWorksInc/DeepDarkFantasy

https://en.wikibooks.org/wiki/Haskell/GADT
