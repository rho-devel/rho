expected <- eval(parse(text="structure(c(32, 36, 53, 66, 10, 16, 3, 4, 11, 9, 50, 34, 10, 7, 30, 64, 10, 5, 25, 29, 7, 7, 5, 5, 3, 2, 15, 14, 7, 7, 8, 8), .Dim = c(2L, 4L, 4L), .Dimnames = structure(list(Sex = c(\"Male\", \"Female\"), Hair = c(\"Black\", \"Brown\", \"Red\", \"Blond\"), Eye = c(\"Brown\", \"Blue\", \"Hazel\", \"Green\")), .Names = c(\"Sex\", \"Hair\", \"Eye\")))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(32, 53, 10, 3, 11, 50, 10, 30, 10, 25, 7, 5, 3, 15, 7, 8, 36, 66, 16, 4, 9, 34, 7, 64, 5, 29, 7, 5, 2, 14, 7, 8), .Dim = c(4L, 4L, 2L), .Dimnames = structure(list(Hair = c(\"Black\", \"Brown\", \"Red\", \"Blond\"), Eye = c(\"Brown\", \"Blue\", \"Hazel\", \"Green\"), Sex = c(\"Male\", \"Female\")), .Names = c(\"Hair\", \"Eye\", \"Sex\")), class = \"table\"), c(3L, 1L, 2L), TRUE)"));           
.Internal(aperm(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

