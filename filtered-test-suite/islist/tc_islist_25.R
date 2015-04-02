expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(NA, 1, 1, 2), .Names = c(\"<none>\", \"M.user\", \"Temp\", \"Soft\")))"));       
do.call(`is.list`, argv);       
}, o=expected);       

