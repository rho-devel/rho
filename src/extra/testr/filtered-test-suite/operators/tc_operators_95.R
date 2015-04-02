expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(1386478800, 1386651600, 1386824400, 1386997200, 1387170000, 1387342800, 1387515600, 1387688400, 1387861200, 1388034000, 1388206800, 1388379600, 1388552400, 1388725200), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), structure(1387538790.57927, class = c(\"POSIXct\", \"POSIXt\")))"));             
do.call(`<=`, argv);             
}, o=expected);             

