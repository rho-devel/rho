dyn.load('allocator_test.so')
.C('alloc_scalar', as.integer(100000))
