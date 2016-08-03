dyn.load('allocator_test.so')
.C('alloc_recursive_scalar', as.integer(50000), as.integer(1000))
