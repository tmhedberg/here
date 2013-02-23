# `here`

`here` is a package that adds support for multi-line string literals (a.k.a. "here docs") to Haskell via GHC's `QuasiQuotes` extension.

It includes two quasiquoters:

  - `here`: Strips leading and trailing whitespace. This allows you to add a line break after the opening quote bracket, which looks nicer.
  - `hereLit`: Quotes the here doc literally, with no whitespace stripping

## Example

````haskell
{-# LANGUAGE QuasiQuotes #-}

import Data.String.Here

main = putStrLn [here|
Hello world,

I am a here doc!
|]
````

### Output

    Hello world,

    I am a here doc!
