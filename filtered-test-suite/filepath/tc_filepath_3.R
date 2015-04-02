expected <- eval(parse(text="\"/home/lzhao/hg/r-instrumented/tests/tcltk.Rcheck/tcltk/help\""));            
test(id=0, code={            
argv <- eval(parse(text="list(list(\"/home/lzhao/hg/r-instrumented/tests/tcltk.Rcheck\", structure(\"tcltk\", .Names = \"Package\"), \"help\"), \"/\")"));            
.Internal(file.path(argv[[1]], argv[[2]]));            
}, o=expected);            

