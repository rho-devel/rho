expected <- eval(parse(text="NA"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(NA_real_, class = c(\"POSIXct\", \"POSIXt\")), structure(1386394754, class = c(\"POSIXct\", \"POSIXt\")))"));           
do.call(`>`, argv);           
}, o=expected);           

