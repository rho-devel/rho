dyn.load('allocator_test.so')
# Do very many pointer lookups.
.C('alloc_lookup_scalar', as.integer(100000), as.integer(1000))
