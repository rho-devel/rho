expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1.12411954394441, -0.567321126080105, 1.28594901629635, -0.519809468914999, -1.485548782458, -0.435305441405687, -0.281625943801696, -0.527525498975648, 2.60041695299567, NA), .Names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\")))"));       
do.call(`is.infinite`, argv);       
}, o=expected);       

