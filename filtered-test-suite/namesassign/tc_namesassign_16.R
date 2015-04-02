expected <- eval(parse(text="structure(1:3, .Names = c(\"foo\", \"bar\", \"baz\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1:3, .Names = c(\"foo\", \"bar\", \"baz\")), value = c(\"foo\", \"bar\", \"baz\"))"));       
do.call(`names<-`, argv);       
}, o=expected);       

