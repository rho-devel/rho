expected <- eval(parse(text="\"/tmp/RtmptPgrXI/Pkgs/pkgA\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/tmp/RtmptPgrXI/Pkgs/pkgA\")"));  
.Internal(`path.expand`(argv[[1]]));  
}, o=expected);  

