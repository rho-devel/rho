expected <- eval(parse(text="structure(list(`_R_NS_LOAD_` = structure(\"survival\", .Names = \"name\")), .Names = \"_R_NS_LOAD_\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(`_R_NS_LOAD_` = structure(\"survival\", .Names = \"name\"))"));         
do.call(`list`, argv);         
}, o=expected);         

