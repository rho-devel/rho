expected <- eval(parse(text="structure(list(`ANY#ANY` = .Primitive(\"==\")), .Names = \"ANY#ANY\")"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(`ANY#ANY` = .Primitive(\"==\")), .Names = \"ANY#ANY\"), list())"));                  
do.call(`c`, argv);                  
}, o=expected);                  

