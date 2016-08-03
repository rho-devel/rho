dyn.load('allocator_test.so')
.C('alloc_intvec', as.integer(10000), as.integer(1000))
