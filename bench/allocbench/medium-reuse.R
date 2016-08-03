dyn.load('allocator_test.so')
.C('alloc_reuse_intvec', as.integer(500000), as.integer(1000), as.integer(50))
