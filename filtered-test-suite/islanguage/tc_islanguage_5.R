expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(\"«Latin-1 accented chars»: éè øØ å<Å æ<Æ\")"));            
do.call(`is.language`, argv);            
}, o=expected);            

