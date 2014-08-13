This is a design document regarding compression, primarily for Awelon Bytecode (ABC) resources. ABC resources are compressed then encrypted. The intention is to save a significant factor for bandwidth and storage by compressing them prior to encryption. 

To preserve the benefits of convergent encryption, the compression algorithm must be standard, simple, unambiguous, and deterministic (no heuristic decisions). ABC should be usable on embedded systems, so decompression should also be streamable and use finite space. Ideally, encoding should also be fast and bounded space, and 'secure' in a sense that decompression either cannot fail or it's easy to determine why it should fail.

Experiments with LZW variants were disappointing. The dictionary's warmup time is too long, suitable only for large resources. 

Sliding window compression algorithms (LZSS, LZ4, LZ77) are promising candidates. I believe we can resolve ambiguity by greedily favoring a longest match with a shortest distance. But I'll still need to study these algorithms for ambiguity and security. LZ4 has the advantages of doing at worst -0.4% compression asymptotically (whereas LZSS does -12.5% at worst, yikes!), and of being octet-oriented in its encoding and decoding. Thus, LZSS is a better fit for most OS and filesystem integration. 

## Special Case Compression for Base16

An simple but powerful idea for working with embedded binaries: 

* encode binaries in a base16 alphabet (this is what ABC sees)
* have a specialized compression pass that handles base16

The current plan is to pursue this as a separate pass from the more general purpose compression algorithm. We can take a large sequence of base16 symbols and encode them using a header followed by raw binary data. We'll decode them back to the original base16 alphabet.

We'll leverage the fact that ABC is encoded in UTF-8, and UTF-8 does not use several symbols in the octet range (0xC0, 0xC1, 0xF5..0xFF). We can use one or more of these to indicate an upcoming volume of binary data. We have a few options: 

1. use a header and follow with a length byte (always two bytes overhead)
2. use a range of headers, encoding common lengths directly in the header

After having experimented a little, I favor the first option. So the current proposal is:

* header byte 0xF8
* length byte: 0..253 (+3)
* only encodes a whole number of bytes
* encodes 6..512 base16 symbols

Thus, the final storage and transmission overhead is about 0.8% for large binaries, or 4% for ABC resource identifiers (48 bytes). This is quite acceptable. The maximum lookahead requirement is also only 512 characters when encoding, and no overhead when decoding.

## Simplifying LZ4?

LZ4 is a nice data structure - very simple, octet aligned. But it has problems:

1. maximum offset (64k) is too large for embedded systems decompression
2. unlimited literal length per segment is a problem for security reasons
3. difficult to select an optimal encoding for convergent encryption

I think the first two points could be addressed. Proposed simplifications:

1. Say we want an 8k history at most, so we only need 13 bits to encode the offset. In this case, we could use the additional 3 bits to encode match length, together with 4 bits in the initial token, so we can match up to ~128 tokens at a time. This is pretty good... using ~3 octets (token+offset) to encode ~128 octets would give us a maximum 97.6% compression ratio, which is better than we could reasonably expect from most ABC code.

2. Conversely, let's say we allow at most one extra byte to encode literal-count. Thus, the maximum number of literals at a time is 15+255 = 270. This would essentially be a 2-byte header followed by 270 octets. This gives us a worst-case compression ratio of about -0.75% for large incompressible data. That is acceptable.

But the 'optimal encoding' issue is more challenging. I have difficulty even thinking about it. When is it worthwhile to 'change' the current match for an LZ4 segment vs. switching to a new segment? How do we prove this? Encodings like LZ4 end up using 'compression levels' which are somewhat heuristic and ambiguous in nature to address these issues.

We could perhaps separate LZ4 so our initial token is either-or, i.e. such that we either encode a run of literals OR encode a match. In this case, it's almost always better for us to encode a match of size 4 or more than to encode an extra token... unless encoding the extra token allows us to encode a much longer match. Hmm.


## Octet-Aligned, Variable Width LZSS

Would it be better to stick with LZSS and accept the -12.5% worst case in order to guarantee a simple, deterministic compression result? For most of ABC, the worst case won't happen. Consider:

1. repeatedly encode an octet of flag bits followed by eight body-elements
2. upon reaching end of the data, just stop, even if flag-bits remain

This is a very simple, easy to describe and implement, and should work efficiently in practice.

The octet of flag bits will be encoded thusly:

* little-endian orientation (extract bits by 2 divmod)
* value `0` to indicate a (distance,match-length) pair
* value `1` to indicate a literal (an octet in this case)
* leftover flags use the `0` encoding

This leaves encoding of the (distance,match-length) pair. Consider: 12-bits distance, 4-bits match length (at least 2). 

        DDDDDDDD DDDDLLLL       D: 1..4095, L: 2..17 (0..15 + 2)

Allowing a match of size two is a bit trivial, but it does save us one bit (the flag bit). On the other end, we could encode 17 octets in 17 bits (sixteen bits for the distance-length, plus the flag bit), for an optimal compression of 87.5% (one bit per byte). 

We can potentially use a variable-width encoding of length, such as:

        DDDDDDDD DDDDLLLL           D: 1..4095, L: 2..16 (0..14 + 2)
        DDDDDDDD DDDD1111 LLLLLLLL  D: 1..4095, L: 17..272 (0..255 + 17)

It's always better to add a length-byte than a literal-byte because we save the flag bit. The cost is that encoding 17 octets is now less efficient, and that encoding in general is a bit more sophisticated. Allowing a single extra octet for match length would raise the optimal compression factor to 90x, but might not raise the average match length by much. 

We can expect only a small fraction of matches are very large... OTOH, it might be a case where rare but large matches dominate the compression ratio. Consider a made up profile:
     
                                ct  data  enc   ratio
        no match:               20  160   180   -12.5%
        matches of size 2:      40  640   680   -6.25%
        matches of size 6:      19  912   323    64.6%
        matches of size 170:     1  1360   17    98.8%
                            total:  3072 1200    60.9%

        vs.

                                ct  data  enc   ratio
        no match:               20  160   180   -12.5%
        matches of size 2:      40  640   680   -6.25%
        matches of size 6:      19  912   323    64.6%
        matches of size 17:     10  1360  170    87.5%
                            total:  3072 1353    56.0%

A single large match, in this artificial profile, makes a difference of 4.9% final compression ratio. That's significant, but not really dominant... and comes at the cost of a much larger match buffer. Is this sort of profile likely in ABC? Yet, I can imagine occasionally matching larger subprograms, and this would certainly be worthwhile if we tried for larger match windows and thus lost a few bits for encoding match lengths: 

        8k window:
        DDDDDDDD DDDDDLLL           D: 1..8191, L: 2..8 (0..6 + 2)
        DDDDDDDD DDDDD111 LLLLLLL   D: 1..8191, L: 9..263 (0..255 + 9)

        16k window:
        DDDDDDDD DDDDDDLL           D: 1..16383, L: 2..4 (0..2 + 2)
        DDDDDDDD DDDDDD11 LLLLLLL   D: 1..16383, L: 5..261 (0..255 + 6)

I think I'll make this 'extra match byte' part of the definition. 

Anyhow, there is a major issue here: while the worst case won't happen for *most* of ABC, it can quite potentially occur for embedded binaries.



