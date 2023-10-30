# Programming Bitcoin in Haskell

Haskell implementation of [jimmysong/programmingbitcoin](https://github.com/jimmysong/programmingbitcoin)

## Run tests

```
$ stack test
```

## Contents

### Chapter 1. Finite Fields

Source code

- [src/FiniteField.hs](./src/FiniteField.hs)

Test code

- [test/TestFiniteField.hs](./test/TestFiniteField.hs)

### Chapter 2. Elliptic Curves

Source code

- [src/EllipticCurve.hs](./src/EllipticCurve.hs)

Test code

- [test/TestEllipticCurve.hs](./test/TestEllipticCurve.hs)

### Chapter 3. Elliptic Curve Cryptography

Source code

- [src/ECDSA.hs](./src/ECDSA.hs)
- [src/Hash.hs](./src/Hash.hs)
- [src/Secp256k1.hs](./src/Secp256k1.hs)

Modified code

- [src/FiniteField.hs](./src/FiniteField.hs)
- [src/EllipticCurve.hs](./src/EllipticCurve.hs)

Test code

- [test/TestHash.hs](./test/TestHash.hs)
- [test/TestSecp256k1.hs](./test/TestSecp256k1.hs)
