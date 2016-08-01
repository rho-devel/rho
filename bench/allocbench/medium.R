dyn.load('allocator_test.so')
.C('alloc_intvec', as.integer(50000), as.integer(50))
