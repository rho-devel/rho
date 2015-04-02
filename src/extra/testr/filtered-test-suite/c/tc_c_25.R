expected <- eval(parse(text="structure(list(class = \"try-error\", condition = structure(list(message = \"line 1 did not have 4 elements\", call = quote(scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings, flush, fill, strip.white, quiet, blank.lines.skip, multi.line, comment.char, allowEscapes, encoding))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))), .Names = c(\"class\", \"condition\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(NULL, structure(list(class = \"try-error\", condition = structure(list(message = \"line 1 did not have 4 elements\", call = quote(scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings, flush, fill, strip.white, quiet, blank.lines.skip, multi.line, comment.char, allowEscapes, encoding))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))), .Names = c(\"class\", \"condition\")))"));        
do.call(`c`, argv);        
}, o=expected);        

