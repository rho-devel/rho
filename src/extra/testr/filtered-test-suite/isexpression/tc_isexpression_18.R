expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(-0.0880891704401362, -0.508170309402877, -0.00510235947825228, 0.0737329622006759), .Names = c(\"(Intercept)\", \"x1\", \"z\", \"x1:z\")))"));       
do.call(`is.expression`, argv);       
}, o=expected);       

