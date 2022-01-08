---
title: Projects
---

## Haskell CTF Crypto
This is an attempt to create an environment for doing CTF challenges in Haskell. Currently, it contains a set of abstract algebraic types and number-theoretic algorithms which are a first attempt to emulate the functionality of [SageMath](https://www.sagemath.org), and a small set of cryptographic algorithms and types which are a first attempt to emulate [PyCryptoDome](https://www.sagemath.org).

It is definitely not worthy of proper use just yet, with the following things needing to be done in order to truly get some use from it:

- Separate the desired functionality into three libraries:
  - One for handling abstract/linear algebra and common number-theoretic algorithms. Similar to `SageMath`.
  - One for integrating the types from the previous library into existing cryptography libraries such as `Cryptonite`.
  - One for abstracting away most of the IO for running a program or connecting to a socket, supplying input and reading output. An example of something like this is `pwntools`.
