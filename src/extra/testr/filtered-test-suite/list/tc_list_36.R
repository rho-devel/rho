expected <- eval(parse(text="structure(list(name = \"list\", objs = structure(list(`package:base` = .Primitive(\"list\"), .Primitive(\"list\")), .Names = c(\"package:base\", \"\")), where = c(\"package:base\", \"namespace:base\"), visible = c(TRUE, FALSE), dups = c(FALSE, TRUE)), .Names = c(\"name\", \"objs\", \"where\", \"visible\", \"dups\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(name = \"list\", objs = structure(list(`package:base` = .Primitive(\"list\"), .Primitive(\"list\")), .Names = c(\"package:base\", \"\")), where = c(\"package:base\", \"namespace:base\"), visible = c(TRUE, FALSE), dups = c(FALSE, TRUE))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

