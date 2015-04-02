expected <- eval(parse(text="structure(0L, .Names = \"expsumNoisy\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(100L, .Names = \"expsumNoisy\"), 100L)"));           
do.call(`%%`, argv);           
}, o=expected);           

