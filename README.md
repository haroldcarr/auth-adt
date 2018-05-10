<p align="center">
  <a href="http://www.adjoint.io"><img src="https://www.adjoint.io/images/logo-small.png" width="250"/></a>
</p>

[![CircleCI](https://circleci.com/gh/adjoint-io/auth-adt.svg?style=svg&circle-token=ab946f8d110bbc5e7fe0550d05d414a1fcbcedb0)](https://circleci.com/gh/adjoint-io/auth-adt)

Derive inclusion and membership proofs for arbitrary sum-of-product datatypes
in Haskell using GHC.Generics.

Authenticated Data Structures, Generically
=========================================

Authenticated data structures (ADS) allow untrusted parties (provers) answer queries on a data
structures on behalf of a trusted source (verifier) and provide a compact proof of the computation.
A verifier can then efficiently check the authenticity of the answer.

In their paper "Authenticated Data Structures, Generically"[1], A. Miller et al. present a generic method to program authenticated operations over any data structure.
They define a well-typed functional programming language (lambda-auth) whose programs result in code that is secure under the standard cryptographic assumption of collision-resistant hash functions.

We present an implementation in Haskell of the lambda-auth PL. In this model of computation,
the prover holds the full ADS of type `Auth T`, which consist of pairs <h<sub>i</sub>, v<sub>i</sub>>
where v<sub>i</sub> is any value of type `T` and h<sub>i</sub> is its digest, i.e. the hash of the
shallow projection of `v`. The verifier only keeps the digest `h` of the ADS.







Membership proofs with GHC.Generics
===================================

We also present a method to construct a membership proof for any data structure using GHC.Generics.


An `Authable` typeclass provides two methods, `prove` and `authenticate`.
An untrusted party can invoke the `prove` method to construct a proof of inclusion of an element in any data structure.
A trusted source can ...

The `authenticate` method generates an authenticated data structure from a non-authenticated data structure.













**References**:
1. A. Miller, M. Hicks, J. Katz, and E. Shi "Authenticated Data Structures, Generically" (https://www.cs.umd.edu/~mwh/papers/gpads.pdf)


License
-------

```
Copyright 2018 Adjoint Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
