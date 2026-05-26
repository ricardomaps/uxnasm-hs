# uxnasm-hs

An assembler written in Haskell for [Uxntal](https://wiki.xxiivv.com/site/uxntal.html), the assembly language of the [Uxn](https://wiki.xxiivv.com/site/uxn.html) virtual machine.

## Differences
This assembler does not output the symbols in the symbol file in the same order as drifblim. It's also not as strict with comments and whitespace.

## Building
With a recent enough version of ghc, just `ghc Main.hs`

## Usage
Just like uxnasm or drifblim, "uxnasm-hs [-v] input.tal output.rom"
