expected <- eval(parse(text="character(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(\"/home/lzhao/hg/r-instrumented/src/library/utils/man/unix/*.rd\", FALSE)"));       
.Internal(Sys.glob(argv[[1]], argv[[2]]));       
}, o=expected);       

