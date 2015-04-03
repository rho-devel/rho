expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(8.444, 12.244, 11.967, 32.826), .Names = c(\"+ Temp\", \"<none>\", \"+ Soft\", \"- M.user\")))"));       
do.call(`is.call`, argv);       
}, o=expected);       

