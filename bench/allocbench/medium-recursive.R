dyn.load('allocator_test.so')
.C('alloc_recursive_intvec', as.integer(50000), as.integer(100), as.integer(50))
