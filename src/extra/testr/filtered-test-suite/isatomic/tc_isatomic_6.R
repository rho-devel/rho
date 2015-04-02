expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1, 2, 2, 3, 3, 3, 4, 5), .Names = c(\"dsyMatrix\", \"ddenseMatrix\", \"symmetricMatrix\", \"dMatrix\", \"denseMatrix\", \"compMatrix\", \"Matrix\", \"mMatrix\")))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

