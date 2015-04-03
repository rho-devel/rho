expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Names = c(\"age\", \"eet\", \"g2\", \"grade\", \"gleason\", \"ploidy\")))"));            
do.call(`is.recursive`, argv);            
}, o=expected);            

