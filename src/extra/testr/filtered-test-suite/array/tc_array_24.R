expected <- eval(parse(text="structure(c(4L, 10L, 16L, 22L, 28L, 34L, 40L, 46L, 52L, 58L, 64L, 70L, 76L, 82L, 88L, 94L, 100L, 106L, 112L, 118L), .Dim = 4:5, .Dimnames = list(NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(4L, 10L, 16L, 22L, 28L, 34L, 40L, 46L, 52L, 58L, 64L, 70L, 76L, 82L, 88L, 94L, 100L, 106L, 112L, 118L), 4:5, list(NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\")))"));     
.Internal(`array`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

