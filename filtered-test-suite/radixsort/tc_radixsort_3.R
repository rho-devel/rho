expected <- eval(parse(text="c(3L, 7L, 1L, 9L, 4L, 8L, 2L, 6L, 10L, 5L)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(3L, 7L, 1L, 5L, 10L, 8L, 2L, 6L, 4L, 9L), .Label = c(\"Svansota\", \"No. 462\", \"Manchuria\", \"No. 475\", \"Velvet\", \"Peatland\", \"Glabron\", \"No. 457\", \"Wisconsin No. 38\", \"Trebi\"), class = \"factor\"), TRUE, FALSE)"));         
.Internal(radixsort(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

