expected <- eval(parse(text="structure(list(error = function (...) {}), .Names = \"error\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(error = function (...) {})"));         
do.call(`list`, argv);         
}, o=expected);         

