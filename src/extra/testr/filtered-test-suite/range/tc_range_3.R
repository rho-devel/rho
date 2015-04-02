expected <- eval(parse(text="c(-2.92498527625946, 2.46253591019012)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(-2.92498527625946, 2.46253591019012), na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

