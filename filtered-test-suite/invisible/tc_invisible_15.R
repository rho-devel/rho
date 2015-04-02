expected <- eval(parse(text="structure(\"Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  : \\n  line 1 did not have 4 elements\\n\", class = \"try-error\", condition = structure(list(message = \"line 1 did not have 4 elements\", call = quote(scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings, flush, fill, strip.white, quiet, blank.lines.skip, multi.line, comment.char, allowEscapes, encoding))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(\"Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  : \\n  line 1 did not have 4 elements\\n\", class = \"try-error\", condition = structure(list(message = \"line 1 did not have 4 elements\", call = quote(scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings, flush, fill, strip.white, quiet, blank.lines.skip, multi.line, comment.char, allowEscapes, encoding))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))))"));      
do.call(`invisible`, argv);      
}, o=expected);      

