expected <- eval(parse(text="\"itemBullet\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(itemBullet = \"• \"), .Names = \"itemBullet\"))"));         
do.call(`names`, argv);         
}, o=expected);         

