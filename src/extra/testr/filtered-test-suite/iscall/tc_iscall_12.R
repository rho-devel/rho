expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(NA, 9.93, 26.79, 820.91), .Names = c(\"<none>\", \"- x4\", \"- x2\", \"- x1\")))"));       
do.call(`is.call`, argv);       
}, o=expected);       

