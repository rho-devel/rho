expected <- eval(parse(text="\"/vignettes\""));                   
test(id=0, code={                   
argv <- eval(parse(text="list(\"/home/lzhao/hg/r-instrumented/src/library/utils\", \"\", \"/home/lzhao/hg/r-instrumented/src/library/utils/vignettes\", FALSE, FALSE, TRUE, FALSE)"));                   
.Internal(gsub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));                   
}, o=expected);                   

