#include <algorithm>
#include <cstdint>
#include <vector>
#include <limits>

using std::size_t;
using std::function;

class BlockPool {
  public:
    BlockPool() = delete;

    /** @brief Must be called before any allocations can be made. */
    static void Initialize();

    static void* AllocBlock(size_t bytes);
    static void FreeBlock(void* p);

    /** @brief Apply function to all allocated blocks (small + large). */
    static void ApplyToAllBlocks(function<void(void*)> f);

    /** @brief Find heap allocation start pointer. */
    static void* Lookup(void* candidate);

    /** @brief Print allocation overview for debugging. */
    static void DebugPrint();

    static void DebugRebalance(int low_bits);

  private:
    /** Allocate a small block. The pool index is the allocation size divided by 8. */
    static void* AllocSmall(size_t bytes);

    /** Free a block in this block pool. */
    static void FreeSmall(void* p, unsigned superblock_id);

    /** Allocate a large block in the sparse block table. */
    static void* AllocLarge(unsigned bytes);
};

