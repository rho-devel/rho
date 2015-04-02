expected <- eval(parse(text="structure(list(error = function (e) -1), .Names = \"error\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(error = function (e) -1)"));         
do.call(`list`, argv);         
}, o=expected);         

