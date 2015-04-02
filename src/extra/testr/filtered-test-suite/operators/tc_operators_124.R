expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- eval(parse(text="list(\"«Latin-1 accented chars»: ghè øØ å<Å æ<Æ gh ghè\", \"«Latin-1 accented chars»: ghè øØ å<Å æ<Æ gh ghè\")"));            
do.call(`==`, argv);            
}, o=expected);            

