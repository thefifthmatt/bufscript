"""Implements murmurhash3, used by the fpt command.

This is also used to make seeds for fpt from strings. It just uses the default
mmh3 output without any seed.
"""

# TODO: This doesn't match fpt? Check this...
def murmurhash3_32(data, seed=0):
    """Implements murmurhash3 returning an int.

    Based on
    https://svn.apache.org/repos/asf/mahout/trunk/math/src/main/java/org/apache/mahout/math/MurmurHash3.java
    which is in the public domain.
    """
    def rsh(num, by):
        # Python does not have logical right shift, so simulate it relatively
        # expensively here.
        return (num % 0x100000000) >> by

    c1 = 0xcc9e2d51
    c2 = 0x1b873593

    h1 = seed
    data = bytearray(data, 'utf_8')
    # Round end down to 4 byte block for initial loop
    rounded_end = len(data) & 0xfffffffc

    for i in range(0, rounded_end, 4):
        k1 = (data[i] & 0xff) | ((data[i + 1] & 0xff) << 8) | ((data[i + 2] & 0xff) << 16) | (data[i + 3] << 24)
        k1 *= c1
        k1 &= 0xffffffff
        # rotl32(k1, 15)
        k1 = (k1 << 15) | rsh(k1, 17)
        k1 *= c2
        k1 &= 0xffffffff

        h1 ^= k1
        # rotl32(k1, 13)
        h1 = (h1 << 13) | rsh(h1, 19)
        h1 = h1 * 5 + 0xe6546b64
        h1 &= 0xffffffff

    k1 = 0

    tail = len(data) & 0x03
    if tail >= 3:
        k1 = (data[rounded_end + 2] & 0xff) << 16
    if tail >= 2:
        k1 |= (data[rounded_end + 1] & 0xff) << 8
    if tail >= 1:
        k1 |= data[rounded_end] & 0xff
        k1 *= c1
        # rotl32(k1, 15)
        k1 = (k1 << 15) | rsh(k1, 17)
        k1 *= c2
        k1 &= 0xffffffff
        h1 ^= k1


    # Finalization
    h1 ^= len(data)

    # fmix(h1)
    h1 ^= rsh(h1, 16)
    h1 *= 0x85ebca6b
    h1 &= 0xffffffff
    h1 ^= rsh(h1, 13)
    h1 *= 0xc2b2ae35
    h1 &= 0xffffffff
    h1 ^= rsh(h1, 16)

    ans = int(h1)
    # Convert unsigned to signed
    if ans >= 2**31:
        ans -= 2**32
    return ans

