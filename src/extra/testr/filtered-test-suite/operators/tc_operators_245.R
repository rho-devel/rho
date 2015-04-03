expected <- eval(parse(text="1:15"));     
test(id=0, code={     
argv <- eval(parse(text="list(1L, structure(15L, .Names = \"nc\"))"));     
do.call(`:`, argv);     
}, o=expected);     

