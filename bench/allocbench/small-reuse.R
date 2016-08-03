dyn.load('allocator_test.so')
.C('alloc_reuse_scalar', as.integer(1000000), as.integer(500))
