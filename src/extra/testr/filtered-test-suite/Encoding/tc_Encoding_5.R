expected <- eval(parse(text="\"unknown\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(\"Type 'demo(PKG::FOO)' to run demonstration 'PKG::FOO'.\", .Names = \"demo\"))"));                
.Internal(Encoding(argv[[1]]));                
}, o=expected);                

