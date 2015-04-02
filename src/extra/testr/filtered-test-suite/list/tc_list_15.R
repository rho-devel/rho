expected <- eval(parse(text="structure(list(error = function (e) warning(gettextf(\"%s namespace cannot be unloaded:\\n  \", sQuote(pkgname)), conditionMessage(e), call. = FALSE, domain = NA)), .Names = \"error\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(error = function (e) warning(gettextf(\"%s namespace cannot be unloaded:\\n  \", sQuote(pkgname)), conditionMessage(e), call. = FALSE, domain = NA))"));         
do.call(`list`, argv);         
}, o=expected);         

