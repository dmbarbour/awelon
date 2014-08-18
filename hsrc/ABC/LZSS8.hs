
-- | Awelon Bytecode resources are to be compressed then encrypted.
-- Encryption is two passes: First, a base16 compression will capture
-- embedded binaries. Second, a more conventional compression will
-- eliminate repeating subsequences of code. 
--
-- ABC compression should be a 'greedy algorithm', i.e. such that
-- greedy local decisions lead to a globally optimal encoding for
-- the specified representation. LZO and LZ4 fail here, but LZSS
-- is a reasonable candidate.
--
-- But LZSS has a horrible worst-case expansion of 12.5%. And while
-- this would not apply to most ABC code, it could hurt for embedded
-- binaries.
--
-- LZSS8 is a variation that outputs 8 bytes per literal flag, for
-- an overhead of 1.6%. Thus, the worst case for binary encoding is
-- expansion of 2.4%, which seems acceptable for most applications.
--
-- The cost of this design is losing some compression opportunities.
--
-- LZSS8 is a candidate for phase 2 compression. Other approaches,
-- such as PPM, are also under consideration.
--
--
module ABC.LZSS8
    ( 
    ) where
