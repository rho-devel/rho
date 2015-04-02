expected <- eval(parse(text="c(\"/home/lzhao/hg/r-instrumented/tests/compiler.Rcheck\", \"/home/lzhao/R/x86_64-unknown-linux-gnu-library/3.0\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(\"/home/lzhao/hg/r-instrumented/tests/compiler.Rcheck\", \"/home/lzhao/R/x86_64-unknown-linux-gnu-library/3.0\"))"));         
.Internal(path.expand(argv[[1]]));         
}, o=expected);         

