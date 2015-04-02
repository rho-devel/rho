expected <- eval(parse(text="FALSE"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE, FALSE), .Names = c(\"+ Temp\", \"<none>\", \"+ Soft\", \"- M.user\")))"));     
do.call(`all`, argv);     
}, o=expected);     

