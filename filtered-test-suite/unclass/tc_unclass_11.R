expected <- eval(parse(text="structure(list(y = c(0.219628047744843, 0.360454661130887, -1.14267533343616, 0.772374419482067, 0.681741904304867, 0.171869265068012, 2.08409180391906, 0.367547276775469), x1 = c(1L, 2L, 5L, 6L, 7L, 8L, 9L, 10L), x2 = c(1L, 2L, 5L, 6L, 7L, 8L, 9L, 10L), `(weights)` = c(0, 1, 1, 1, 1, 1, 1, 1)), .Names = c(\"y\", \"x1\", \"x2\", \"(weights)\"), terms = quote(y ~ x1 + x2), row.names = c(\"a\", \"b\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\"), na.action = structure(3:4, .Names = c(\"c\", \"d\"), class = \"omit\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(y = c(0.219628047744843, 0.360454661130887, -1.14267533343616, 0.772374419482067, 0.681741904304867, 0.171869265068012, 2.08409180391906, 0.367547276775469), x1 = c(1L, 2L, 5L, 6L, 7L, 8L, 9L, 10L), x2 = c(1L, 2L, 5L, 6L, 7L, 8L, 9L, 10L), `(weights)` = c(0, 1, 1, 1, 1, 1, 1, 1)), .Names = c(\"y\", \"x1\", \"x2\", \"(weights)\"), terms = quote(y ~ x1 + x2), row.names = c(\"a\", \"b\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\"), na.action = structure(3:4, .Names = c(\"c\", \"d\"), class = \"omit\")))"));     
do.call(`unclass`, argv);     
}, o=expected);     

