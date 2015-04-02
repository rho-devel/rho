expected <- eval(parse(text="\"tools:::check_compiled_code(\\\"/home/lzhao/hg/r-instrumented/library/foreign\\\")\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"tools:::check_compiled_code(\\\"%s\\\")\", \"/home/lzhao/hg/r-instrumented/library/foreign\")"));           
.Internal(sprintf(argv[[1]], argv[[2]]));           
}, o=expected);           

