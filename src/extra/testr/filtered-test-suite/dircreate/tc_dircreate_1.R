expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"/home/lzhao/tmp/RtmptS6o2G/translations\", FALSE, FALSE, structure(511L, class = \"octmode\"))"));      
.Internal(dir.create(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

