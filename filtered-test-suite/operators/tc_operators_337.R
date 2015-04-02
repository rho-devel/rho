expected <- eval(parse(text="structure(492L, class = \"octmode\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(508L, class = \"octmode\"), \"755\")"));       
do.call(`&`, argv);       
}, o=expected);       

