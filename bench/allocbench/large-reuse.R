dyn.load('allocator_test.so')
.C('alloc_intvec', as.integer(20000), as.integer(200), as.integer(1000))
