dyn.load('allocator_test.so')
.C('alloc_intvec', as.integer(100), as.integer(100000))
