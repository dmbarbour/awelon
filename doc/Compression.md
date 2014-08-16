This is a design document regarding compression, primarily for Awelon Bytecode (ABC) resources, and potentially for ABC network streams. ABC resources are compressed then encrypted. The intention is to save a significant factor for bandwidth and storage by compressing bytecode for storage and transport.

ABC has a highly repetitive nature and should typically compress very well. 

However, ABC may also contain embedded binaries, i.e. via a specialized first-pass compression targeting base16. In many cases such as ciphertext or compressed images or audio, embedded binaries will often not be further compressible. And in these cases we don't want to expand them very much, e.g. total expansion of less than 3% (including the 0.8% for embedding them) is about the highest I'd find acceptable.

## Initial Compression Pass for Embedded Binaries

An simple but powerful idea for working with embedded binaries: 

* encode binaries in a base16 alphabet (this is what ABC sees)
* have a specialized compression pass that handles base16

We can take a large sequence of base16 symbols and encode them using a run length encoding of large bytes. We'll later decode them back to the original base16 alphabet. So this technique requires a standard base16 alpahabet.

In this case, our encoding will be simple:

* header byte 0xF8
* length byte L indicating 3..256 full bytes (0..253 + 3)
* encoding 6..512 base16 symbols
* specialized base16 alphabet: `a-z` minus `aeiou` and `vrwlc`

In case 0xF8 is already in the stream, we'll escape it by following with 0xFE. However, since this pass occurs just after UTF-8 encoding the Awelon bytecode, we should never encounter this situation (UTF-8 doesn't use 0xC0, 0xC1, or 0xF5..0xFF). The escape is only defined for completeness.

The result is 1:128 expansion for large binaries, or about 0.8%. For 48-byte resource identifiers, the expansion is 4.2%.

For determinism, we shall always compress the largest sequence possible.

## Primary Compression of Bytecode

As noted earlier, ABC is highly repetitive - not only in-the-small, but also in-the-large. Even ABC resource IDs may see considerable reuse. We can expect ABC resources to vary in size about five orders of magnitude (100B-10MB), but most of the very large resources will be embedded binaries that are compressed anyway.

Desiderata:

* simple to describe and implement
* deterministic compression (for convergence)
* greedy compression is optimal for data structure
* small expansion for non-compressible data (2% at most)
* effective; 75%+ compression for most pure ABC programs; okay for text
* fast, streaming decompression in bounded space
* stream compression also in bounded space
* byte aligned manipulations (avoid bit manipulation)
* able to easily compress repeating resource IDs

Observations:

* dictionary compression (LZ78/LZW) too slow to reuse resource ids
* sliding window algorithms seem relatively simple and promising
* sliding window: represent end-of-input by encoding zero offset?
* LZSS has too much overhead (12.5% expansion, worst case)
* must encode literals blocks at a time to reduce worst-case
* dynamic literal run-length: very difficult to determine optimality
  * decision to make: larger literal run for *better* match later? 
  * decision to make: shorter literal run for *more* matches?
  * too many strategies, results in 'optimization levels' (LZ4/LZO/etc.)
* flag-based techniques: byte align by combining 8 flags into one byte

Ideas with potential:

* As an LZSS variation, I could potentially apply an LZSS variant that emits 8 bytes per literal flag, and at least three bytes per match. This would mitigate the worst case - no matches - down to a fixed 1:64 overhead. The cost is that I may lose many opportunities for shorter matches. (Maybe call this LZSS8).

This is my best idea for a plain old sliding window compression. I've filtered a few others, e.g. involving flag bits representing increasing subsequent sizes, for being too complicated and again too difficult to reason about.

## Other Possibilities?

I should also look into PPM compression or Burrows Wheeler transformation. I don't understand these well enough yet to make a judgement.
