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

As noted earlier, ABC is highly repetitive - not only in-the-small, but also in-the-large. Even ABC resource IDs may see considerable reuse. We can expect ABC resources to vary in size about five orders of magnitude (100B-10MB). I imagine the larger resources likely contain binaries, though large embedded literal objects (blocks of ABC) or are also likely.

I don't have any strong requirements here, but many desiderata:

* simple, deterministic, greedy algorithm
* small expansion for non-compressible data (target 1:64 or lower)
* effective; 75%+ compression for most pure ABC, 50%+ for text
* fast, streaming decompression in bounded space
* stream compression also in bounded space
* able to easily compress repeating resource IDs

If expansion is 1:64, then the total expansion for large embedded binaries 3:128 or 2.34%. I think that will be acceptable for most applications.

Observations:

* dictionary compression (LZ78/LZW) too slow to reuse resource ids
* sliding window algorithms seem relatively simple and promising
* sliding window: represent end-of-input by encoding zero offset?
* LZSS has too much overhead (12.5% expansion, worst case)
* must encode literals blocks at a time to reduce worst-case
* literal run-lengths: very difficult to determine optimality
  * decision to make: larger literal run for *better* match later? 
  * decision to make: shorter literal run for *more* matches?
  * too many strategies, results in 'optimization levels' (LZ4/LZO/etc.)
* flag-based techniques: byte align by combining 8 flags into one byte

The dynamic match length may cause similar issues.

## LZSS8

One idea is to tweak LZSS such that each literal flag is followed by eight bytes, instead of by a single byte. This gives us our target 1:64 worst case expansion, at the cost of some compression opportunities (for short compression). To mitigate lost compression opportunities, we should perhaps seek to compress even two-byte sequences, e.g. keeping flag+offset+length under two bytes. 

It isn't clear to me this actually meets the 'greedy algorithm' goals. Might it be better to sometimes shift a byte to a subsequent match, to avoid using a literal? The compression ratio of that particular byte may be diminished in the transfer, but it could allow better compression within the 8-byte sequence. OTOH, perhaps the 'greedy algorithm' is too strong a condition, so long as there is one and only one obvious 'greedy' encoding? In this case, we can simply find the largest match possible in each step, and emit a literal otherwise.

I'd be interested in a sliding window of about 2k-8k elements, and match lengths of over 60. Encoding larger match lengths using a variable encoding is not a problem. 

Alternatively, we could seek a byte-aligned LZSS8, combining flags into a flag-byte indicating up to eight matches and literals groups. In this case, we'd match at least three bytes, and lose a little on the pairs.

## Other Possibilities?

PPM compression should work well enough, but may have a larger overhead. I wonder if it's feasible to build PPM above a bloom filter? Hmm, might not be very useful for decoding, though.
