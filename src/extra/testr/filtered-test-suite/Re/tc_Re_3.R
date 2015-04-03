expected <- eval(parse(text="c(-0.1, -0.2, -0.3, -0.4, -0.0999999999999999, -0.2, -0.3, -0.4, -0.1)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(-0.1-0.9i, -0.2-0.8i, -0.3-0.7i, -0.4-0.6i, -0.1-0.5i, -0.2-0.4i, -0.3-0.3i, -0.4-0.2i, -0.1-0.1i))"));         
do.call(`Re`, argv);         
}, o=expected);         

