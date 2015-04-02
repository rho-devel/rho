expected <- eval(parse(text="2419200"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(28, units = \"days\", class = \"difftime\"), units = \"secs\")"));               
do.call(`as.double`, argv);               
}, o=expected);               

