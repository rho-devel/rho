expected <- eval(parse(text="structure(numeric(0), class = \"Date\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(sec = numeric(0), min = integer(0), hour = integer(0), mday = integer(0), mon = integer(0), year = integer(0), wday = integer(0), yday = integer(0), isdst = integer(0)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\")))"));     
.Internal(POSIXlt2Date(argv[[1]]));     
}, o=expected);     

