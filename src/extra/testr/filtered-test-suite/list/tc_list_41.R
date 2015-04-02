expected <- eval(parse(text="structure(list(properties = structure(list(.Data = \"numeric\", comment = \"character\"), .Names = c(\".Data\", \"comment\")), prototype = structure(3.14159265358979, comment = \"Start with pi\")), .Names = c(\"properties\", \"prototype\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(properties = structure(list(.Data = \"numeric\", comment = \"character\"), .Names = c(\".Data\", \"comment\")), prototype = structure(3.14159265358979, comment = \"Start with pi\"))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

