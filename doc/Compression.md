This is a design document regarding compression, primarily for Awelon Bytecode (ABC) resources, and potentially for ABC network streams. ABC resources are compressed then encrypted. The intention is to save a significant factor for bandwidth and storage by compressing bytecode for storage and transport.

ABC has a highly repetitive nature and should typically compress very well. 

However, ABC may also contain embedded binaries, i.e. via a specialized first-pass compression targeting base16. In many cases such as ciphertext or compressed images or audio, embedded binaries will often not be further compressible. And in these cases we don't want to expand them very much, e.g. total expansion of 2.5% is about the highest I'd find acceptable.

An advantage of ABC is that I don't actually need to encode lengths or terminators. I could simply zero-fill the end of input, then trim off any remaining zeroes in the input (valid ABC doesn't use any control characters). 

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

The result is 1:128 expansion for large binaries, or about 0.8%. For 48-byte resource identifiers, the expansion is 4.2%. It seems tempting to create specialized headers for larger sequences, but I'm avoiding that for simplicity reasons.

For determinism, we shall always compress the largest sequence possible.

## Primary Compression of Bytecode

As noted earlier, ABC is highly repetitive - not only in-the-small, but also in-the-large. Even ABC resource IDs may see considerable reuse. We can expect ABC resources to vary in size about five orders of magnitude (100B-10MB). I imagine the larger resources likely contain binaries, though large embedded literal objects (blocks of ABC) or are also likely.

I don't have strong requirements here, but many desiderata:

* simple, deterministic algorithm
* optimal parsing, preferably a greedy algorithm
* worst-case expansion less than 1.6% 
* effective; 75%+ compression for typical ABC, 50%+ for text
* fast, streaming decompression in bounded space
* stream compression also in bounded space
* able to easily compress repeating resource IDs

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
* smaller match sizes, more matches, may be better for stream encoding

I have a strong preference at this point for sliding window approaches. I feel those are a very good fit for ABC in particular.

### Optimal Parsing vs. Streaming Compression

Idea: use deterministic optimal parsing for ABC resources (even if it takes many seconds to compress), then use faster, greedy encodings for streaming compression. This isn't ideal, IMO, but it may work pretty well.

A streaming encode is much less valuable than streaming decode for ABC resources. Apparently, there are simple approaches to perform optimal parsing [1](http://www.cbloom.com/algs/dictionary.html) if we abandon a streaming encode. If we pursue this route, we could potentially use a standard sliding window algorithm, though sticking with a custom algorithm would still be okay.

From Matt Mahoney's 'data compression explained' [2](http://mattmahoney.net/dc/dce.html):

> Matias and Sahinalp (1999) proved that for any dictionary encoding method with the prefix property, that there is a fast (O(n)) algorithm that optimally parses the input into the minimum number of phrases by looking only at the lengths of the next two phrases considered. A dictionary has the prefix property if for any phrase in it, all of its prefixes are also in the dictionary. LZ77 and LZW both have this property. However, optimal parsing is more difficult to achieve in practice because phrases may have different code lengths, so that the minimum number of phrases does not necessarily give the best compression.

Interestingly, we might be able to get a fast optimal streaming encoding if we simply ensure that "minimal number of phrases" corresponds to "minimal encoding". This would be easy to do if we have fixed-width phrases, e.g. requiring matches of three or more.

### Sliding Window and Match Sizes

ABCD and ABC resources offer a means to compress common patterns independently of reuse within a sliding window. So ABC compression will be focusing mostly on the smaller patterns, e.g. across objects or within larger functions. A window size of between 2k and 8k should work very nicely. Match size of 32 or 64 should also work pretty well. 

### LZSS8

One idea is to tweak LZSS such that each literal flag is followed by eight bytes, instead of by a single byte. This gives us our target 1:64 worst case expansion, at the cost of some compression opportunities (for short compression). To mitigate lost compression opportunities, we should perhaps seek to compress even two-byte sequences, e.g. keeping flag+offset+length under two bytes. 

It isn't clear to me this actually meets the 'greedy algorithm' goals. But there might still be a relatively simple 'optimal parsing' algorithm for it? Might it be better to sometimes shift a byte to a subsequent match, to avoid using a literal? The compression ratio of that particular byte may be diminished in the transfer, but it could allow better compression within the 8-byte sequence. OTOH, perhaps the 'greedy algorithm' is too strong a condition, so long as there is one and only one obvious 'greedy' encoding? In this case, we can simply find the largest match possible in each step, and emit a literal otherwise.

I'd be interested in a sliding window of about 2k-8k elements, and match lengths of over 60. Encoding larger match lengths using a variable encoding is not a problem. If I don't seek byte alignment, it will be a lot easier to develop a simple LZSS scheme.

### Segmented LZSS

A potential approach is to take a straightforward, optimal LZSS encoding, then classify each sequence of eight phrases as either being purely literals or mixed. A sequence of purely literal phrases is then encoded with one bit overhead for eight bytes, while a mixed sequence is encoded in normal LZSS fashion.

Worst case for non-compressible segment is thus 1:64 overhead. Nice! 

Worst-case for a compressible segment will be based on the smallest encoding and match-offset size. We must also keep this at 1:64 or lower. Worst case is seven literal bytes (56 bits + 7 flags), one match (flag+offset+length), plus the segment flag. I.e. 65 bits + offset+length, encoding either 72 or 80 bits (respectively for minimum length 2 or 3). If we encoded 72 bits in 73, or 80 in 81, we'll (very slightly!) surpass our 1:64 threshold.

Realistically, we can't do an 8-bit offset-length pair for ABC. So we'll want to use minimum match size 3.

This approach can easily be byte-aligned:

* byte for eight segment flags
* literal segment: just eight bytes
* compressed segment: 
  * byte for eight phrase flags
  * 16-bit length-offset matches

With a fixed-size offset-length pairs, we can be reasonably certain that 'minimal number of phrases' corresponds to minimal number of bits in the final output (we'll always save slightly with a compressed segment, which uses at least two fewer phrases). With variable sized phrases, a similar guarantee is more difficult, unless we ensure a monotonic compression factor.

...

I'm not sure what offset+length I want. Pros and cons:

* 12+4: window 227x full match size and 77x link size; many match opportunities; 8-9x max compression; need resources larger than 2k to see any benefit; leans towards larger resources
* 11+5: window 60x full match size and 38x link size; fewer 'small' matches at the tail end; fewer match opportunities; 16-17x max compression; compression mostly within functions, in-the-small; leans towards smaller resources

I wouldn't be surprised to get near maximum compression when working with 12+4, due to larger scale repetitions of whole functions. I would be surprised to max out the 11+5 compression. OTOH, I think the 11+5 compression might encourage better practices for resource size.

### Other Possibilities?

* PPM compression should work well enough, but may have a larger overhead. 
