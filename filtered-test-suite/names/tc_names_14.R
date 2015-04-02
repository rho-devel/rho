expected <- eval(parse(text="c(\"ii\", \"xx\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(ii = 1:10, xx = c(-9.42477796076938, -6.28318530717959, -3.14159265358979, 0, 3.14159265358979, 6.28318530717959, 9.42477796076938, 12.5663706143592, 15.707963267949, 18.8495559215388)), .Names = c(\"ii\", \"xx\")))"));         
do.call(`names`, argv);         
}, o=expected);         

