expected <- eval(parse(text="structure(list(`0` = 5, `2` = 7, `6` = c(5, 5), `8` = NA_real_, `15` = 3, `22` = 3, `29` = 4, `35` = 1), .Names = c(\"0\", \"2\", \"6\", \"8\", \"15\", \"22\", \"29\", \"35\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(1, 3, 5, 7, 8, 3, 5, NA, 4, 5, 7, 9), structure(c(8L, 6L, 3L, 2L, NA, 5L, 1L, 4L, 7L, 3L, NA, NA), .Label = c(\"0\", \"2\", \"6\", \"8\", \"15\", \"22\", \"29\", \"35\"), class = \"factor\"))"));             
.Internal(split(argv[[1]], argv[[2]]));             
}, o=expected);             

