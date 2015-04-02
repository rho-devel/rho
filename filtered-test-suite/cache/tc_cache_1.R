expected <- eval(parse(text="c(\"ddenseMatrix\", \"dMatrix\", \"denseMatrix\", \"Matrix\", \"mMatrix\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(\"ddenseMatrix\", c(\"ddenseMatrix\", \"dMatrix\", \"denseMatrix\", \"Matrix\", \"mMatrix\"))"));         
do.call(`.cache_class`, argv);         
}, o=expected);         

