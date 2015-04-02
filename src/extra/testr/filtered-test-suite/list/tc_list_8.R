expected <- eval(parse(text="structure(list(upper = quote(~M.user * Temp * Soft)), .Names = \"upper\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(upper = quote(~M.user * Temp * Soft))"));         
do.call(`list`, argv);         
}, o=expected);         

