expected <- eval(parse(text="\"Modes\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"Modes\")"));           
do.call(`enc2utf8`, argv);           
}, o=expected);           

