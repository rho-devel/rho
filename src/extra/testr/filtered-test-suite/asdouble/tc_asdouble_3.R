expected <- eval(parse(text="345600"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(4, tzone = \"GMT\", units = \"days\", class = \"difftime\"), units = \"secs\")"));     
do.call(`as.double`, argv);     
}, o=expected);     

