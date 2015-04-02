expected <- eval(parse(text="c(\"«L\", \"tin-1 \", \"\", \"\", \"ented \", \"h\", \"rs»: éè øØ å<Å æ<Æ é éè\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(\"«L\", \"tin-1 \", \"\", \"\", \"ented \", \"h\", \"rs»: éè øØ å<Å æ<Æ é éè\"))"));               
do.call(`(`, argv);               
}, o=expected);               

