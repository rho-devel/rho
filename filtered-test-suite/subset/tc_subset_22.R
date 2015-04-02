expected <- eval(parse(text="NA_real_"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(3.14159265358979, class = \"testit\"), structure(3.14159265358979, class = \"testit\"))"));                
do.call(`.subset`, argv);                
}, o=expected);                

