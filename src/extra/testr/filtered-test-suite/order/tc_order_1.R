expected <- eval(parse(text="1:8"));     
test(id=0, code={     
argv <- eval(parse(text="list(TRUE, FALSE, structure(c(1, 1, 1, 2, 2, 2, 3, 4), .Names = c(\"CsparseMatrix\", \"nsparseMatrix\", \"generalMatrix\", \"nMatrix\", \"sparseMatrix\", \"compMatrix\", \"Matrix\", \"mMatrix\")))"));     
.Internal(`order`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

