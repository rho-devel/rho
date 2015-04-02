expected <- eval(parse(text="FALSE"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(list(y = c(0.219628047744843, 0.360454661130887, NA, 0.114681204747219, -1.14267533343616, 0.772374419482067, 0.681741904304867, 0.171869265068012, 2.08409180391906, 0.367547276775469), x1 = c(1L, 2L, 3L, NA, 5L, 6L, 7L, 8L, 9L, 10L), x2 = 1:10, x3 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), wt = c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c(\"y\", \"x1\", \"x2\", \"x3\", \"wt\"), row.names = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\"), class = \"data.frame\"))"));    
do.call(`is.array`, argv);    
}, o=expected);    

