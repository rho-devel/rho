expected <- eval(parse(text="15634800"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(180.958333333333, units = \"days\", class = \"difftime\"), units = \"secs\")"));               
do.call(`as.double`, argv);               
}, o=expected);               

