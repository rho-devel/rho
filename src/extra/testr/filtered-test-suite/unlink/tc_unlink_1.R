expected <- eval(parse(text="0L"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/tmp/RtmptPgrXI/Pkgs\", TRUE, FALSE)"));  
.Internal(`unlink`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

