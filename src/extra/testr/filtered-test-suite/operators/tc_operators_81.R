expected <- eval(parse(text="c(FALSE, FALSE)"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(1386500270.17764, class = c(\"POSIXct\", \"POSIXt\")), structure(c(1383854025.35606, 1386388510.66806), class = c(\"POSIXct\", \"POSIXt\")))"));          
do.call(`<`, argv);          
}, o=expected);          

