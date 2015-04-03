expected <- eval(parse(text="FALSE"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(usr = c(-4.82721591443179, -1.44459960821772, -4.82721591443179, -1.44459960821772)), .Names = \"usr\"))"));         
do.call(`is.character`, argv);         
}, o=expected);         

