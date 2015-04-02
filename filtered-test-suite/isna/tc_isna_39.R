expected <- eval(parse(text="c(FALSE, FALSE)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(\"«Latin-1 accented chars»: éè øØ å<Å æ<Æ é éè\", \"éè\"))"));                
do.call(`is.na`, argv);                
}, o=expected);                

