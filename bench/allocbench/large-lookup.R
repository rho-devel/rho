dyn.load('allocator_test.so')
.C('alloc_lookup_intvec', as.integer(1000), as.integer(50000), as.integer(1000))
