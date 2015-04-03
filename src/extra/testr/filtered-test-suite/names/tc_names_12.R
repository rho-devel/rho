expected <- eval(parse(text="c(\"V1\", \"V2\", \"V3\", \"V4\", \"V5\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(V1 = c(-1L, -2L, 1L, 2L, 3L, 4L, 5L), V2 = c(-3L, -4L, 6L, 7L, 8L, 9L, 10L), V3 = c(-5L, -6L, 11L, 12L, 13L, 14L, 15L), V4 = c(-7L, -8L, 16L, 17L, 18L, 19L, 20L), V5 = c(-9L, -10L, 21L, 22L, 23L, 24L, 25L)), .Names = c(\"V1\", \"V2\", \"V3\", \"V4\", \"V5\"), row.names = c(NA, 7L), class = \"data.frame\"))"));         
do.call(`names`, argv);         
}, o=expected);         

