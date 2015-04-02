expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(object = c(0.568, 1.432, -1.08, 1.08), max.level = NA, vec.len = 4, digits.d = 3, nchar.max = 128, give.attr = TRUE, give.head = TRUE, width = 80L, envir = NULL, strict.width = \"no\", formatNum = function (x, ...) format(x, trim = TRUE, drop0trailing = TRUE, ...), list.len = 99, give.length = TRUE, nest.lev = 1, indent.str = \"  ..\"), .Names = c(\"object\", \"max.level\", \"vec.len\", \"digits.d\", \"nchar.max\", \"give.attr\", \"give.head\", \"width\", \"envir\", \"strict.width\", \"formatNum\", \"list.len\", \"give.length\", \"nest.lev\", \"indent.str\")))"));       
do.call(`is.list`, argv);       
}, o=expected);       

