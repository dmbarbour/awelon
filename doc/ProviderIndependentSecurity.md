
To support provider-independent security (i.e. such that we can upload sensitive code to an untrusted cloud) the bytecode resources are encrypted by use of the hash. Pseudocode:

        makeResource(bytecode)
            hashBC = drop(24,SHA3-384(bytecode))
            cipherText = AES_encrypt(compress(bytes),hashBC)
            hashCT = take(24,SHA3-384(cipherText))
            store(hashCT,cipherText)
            resourceId = encode_base64url(hashCT) + ":" + 
                         encode_base64url(hashBC)
            return resourceId

The resulting ciphertext may securely be stored anywhere, e.g. in a peer to peer network, a data distribution system, or an untrusted cloud server. However, this is subject to a class of confirmation attacks, i.e. where servers can discover information by encrypting billions of different bytecode sequences and testing whether it matches one on the server. ABC leaves this problem to higher level languages, which may distinguish some resources as sensitive and mix in some secret text to render them unguessable. 

ABC may then load and link the resource via invocation: `{#resourceId}`. This process is essentially the reverse of the above, plus a few validations:

        loadResource(resourceId) 
            hashBC = decode_base64url(drop(33,resourceId))
            hashCT = decode_base64url(take(32,resourceId))
            cipherText = fetch(hashCT)
            bytecode = decompress(AES_decrypt(cipherText,hashBC))
            validateHash(bytecode,hashBC)
            validateABC(bytecode)
            return bytecode

If successfully accessed, a resource invocation should be logically equivalent to inlining the identified bytecode. It is possible that `fetch` may fail, e.g. because the network is down. We can mitigate this by looking up resources ahead of time, e.g. during `validateABC`. After downloading a resource, we might locally cache the bytecode, and possibly compile it for performance.

I've selected hash algorithm SHA3-384 and encryption algorithm AES. There are still minor details to work out, such as modes and initialization vectors for encryption. 

A compression algorithm is not yet decided. Considerations:

* resources will range 4+ orders magnitude in size (e.g. 200B - 2MB)
* many resources have large amounts of embedded text (EDSLs, content)
* some resources deeply structured with `{#resourceId}` dependencies
* need simple specification: no ambiguity, deterministic, just works
* space-time requirements should be very predictable and not slow

It's the large resources and embedded text that really benefit from compression, especially in context of mobile networking. A reasonable candidate algorithm is LZW + Huffman, or a variant on LZW. LZW is not a very good compression algorithm, but does meet the simplicity and determinism requirements. Alternatively, it might be worth trying a Huffman encoding on the unicode characters followed by a simplified bit-level PPM model. 
