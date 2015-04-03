expected <- eval(parse(text="c(953553600L, 953640000L, 953726400L, 953812800L, 953899200L, 953985600L, 954072000L, 954158400L, 954244800L, 954331200L)"));            
test(id=0, code={            
argv <- eval(parse(text="list(953553600, by = 86400, length.out = 10)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

