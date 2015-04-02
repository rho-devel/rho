expected <- eval(parse(text="c(\"/home/lzhao/hg/r-instrumented/library\", \"/home/lzhao/R/x86_64-unknown-linux-gnu-library/3.0\", \"/home/lzhao/hg/r-instrumented/library\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(\"/home/lzhao/hg/r-instrumented/library\", \"/home/lzhao/R/x86_64-unknown-linux-gnu-library/3.0\", \"/home/lzhao/hg/r-instrumented/library\"), \"/\", NA)"));         
.Internal(normalizePath(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

