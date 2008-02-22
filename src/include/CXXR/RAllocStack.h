/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  Andrew Runnalls
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file RAllocStack.h
 * @brief Function R_alloc() and kin.
 *
 * Defines functions R_alloc() and related functions, and the class
 * CXXR::RAllocStack which is used to implement them.
 */

#ifndef RALLOCSTACK_H
#define RALLOCSTACK_H 1

#ifdef __cplusplus

#include <stack>
#include <vector>

namespace CXXR {
    /** @brief Class for implementing R_alloc() and kindred functions.
     *
     * This class has only static members.  It implements a stack of
     * pointers to blocks of memory.
     */
    class RAllocStack {
    public:
	/** @brief Allocate a new block of memory.
	 *
	 * The block will be aligned on a multiple of
	 * <tt>sizeof(double)</tt> .
	 * @param sz The required size in bytes (strictly, as a
	 *           multiple of <tt>sizeof(char)</tt>), of the memory block.
	 * @return Pointer to the start of the memory block.
	 */
	static void* allocate(size_t sz);

	/** @brief Restore stack to a previous size.
	 *
	 * Restore the stack to a previous size by popping elements
	 * off the top.
	 * @param new_size The size to which the stack is to be
	 *          restored.  Must not be greater than the current
	 *          size.
	 * @note In future this method will probably cease to be
	 * public, and be accessible only by a class encapsulating R
	 * contexts.
	 */
	static void restoreSize(size_t new_size);

	/** @brief Current size of stack.
	 *
	 * @return the current size of the stack.
	 *
	 * @note This method is intended for use in conjunction with
	 * restoreSize(), and like it may cease to be public in
	 * future.
	 */
	static size_t size()
	{
	    return s_stack.size();
	}
    private:
	typedef std::pair<size_t, void*> Pair;
	typedef std::stack<Pair, std::vector<Pair> > Stack;
	static Stack s_stack;

	// Not implemented.  Declared to stop the compiler generating
	// a constructor.
	RAllocStack();
    };
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /* ***** C interface ***** */

    /** @brief Allocate a block of memory.
     *
     * This function is provided for the use of code called the R
     * <tt>.C</tt> function.  It will allocate a block of memory that
     * will automatically be reclaimed by R at the end of the
     * <tt>.C</tt> call.
     * @param num_elts Number of data items to be accommodated in the
     *          block.
     * @param elt_size Size in bytes (strictly, as a multiple of
     *          <tt>sizeof(char)</tt>) of each data item.  Must be
     *          non-negative.
     * @return Pointer to the start of the memory block.
     *
     * @note The signed type of \a elt_size is anomalous, but is part
     * of the R API.
     */
    char* R_alloc(size_t num_elts, int elt_size);

    /** @brief Allocate a block of memory, and initialize it to zero.
     *
     * This is part of the S compatibility interface.  It does the
     * same thing as R_alloc(), except that the memory block is
     * guaranteed to be initialized to zero.
     * @param num_elts Number of data items to be accommodated in the
     *          block.  Must be non-negative.
     * @param elt_size Size in bytes (strictly, as a multiple of
     *          <tt>sizeof(char)</tt>) of each data item.  Must be
     *          non-negative.
     * @return Pointer to the start of the memory block.
     */
    char* S_alloc(long num_elts, int elt_size);

    /** @brief Reallocate a block of memory.
     *
     * This is part of the S compatibility interface, and is used when
     * it is decided that a block of memory previously allocated by 
     * S_alloc() or S_realloc() needs to be expanded.  It allocates a
     * new block of memory, copies across the previous contents, and
     * zeroes any additional elements.
     * @param prev_block Pointer to a block of memory previously
     *          allocated by S_alloc() or S_realloc().
     * @param new_sz New number of elements (>= 0) to be
     *          accommodated.
     * @param old_sz Number of elements contained in prev_block.
     * @param elt_size Size in bytes (strictly, as a multiple of
     *          sizeof(char)) of each data item.  Must be
     *          non-negative.
     * @return Pointer to the start of the newly allocated memory
     *         block.  If \a new_sz \c <= \a old_sz, the function does
     *         not allocate a new block, and simply returns \a
     *         prev_block.
     */
    char* S_realloc(char *prev_block, long new_sz, long old_sz, int elt_size);

    /** @brief Number of memory blocks allocated.
     *
     * @return The current number of blocks allocated via R_alloc(),
     *         S_alloc() and S_realloc().
     *
     * @note C++ code should preferably use CXXR::RAllocStack::size() directly.
     */
#ifndef __cplusplus
    unsigned int vmaxget(void);
#else
    inline unsigned int vmaxget(void)
    {
	return CXXR::RAllocStack::size();
    }
#endif

    /** @brief Reclaims memory blocks.
     *
     * @param stack_size A value previously returned by a call to
     *          vmaxget().  vmaxset() will reclaim the memory
     *          from all blocks allocated via R_alloc(),
     *          S_alloc() and S_realloc() subsequent to that call of
     *          vmaxget().
     *
     * @deprecated For expert use only.  C++ code should preferably
     * use CXXR::RAllocStack::restoreSize() directly.
     */
#ifndef __cplusplus
    void vmaxset(unsigned int stack_size);
#else
    inline void vmaxset(unsigned int stack_size)
    {
	CXXR::RAllocStack::restoreSize(stack_size);
    }
#endif

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  // RALLOCSTACK_H
