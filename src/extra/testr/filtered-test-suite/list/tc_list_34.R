expected <- eval(parse(text="structure(list(itemBullet = \"• \"), .Names = \"itemBullet\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(itemBullet = \"• \")"));         
do.call(`list`, argv);         
}, o=expected);         

