expected <- eval(parse(text="structure(list(class = \"try-error\", condition = structure(list(message = \"undefined columns selected\", call = quote(`[.data.frame`(dd, , \"x\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))), .Names = c(\"class\", \"condition\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(class = \"try-error\", condition = structure(list(message = \"undefined columns selected\", call = quote(`[.data.frame`(dd, , \"x\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\")))"));         
do.call(`list`, argv);         
}, o=expected);         

