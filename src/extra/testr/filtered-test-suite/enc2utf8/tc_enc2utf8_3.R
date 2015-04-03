expected <- eval(parse(text="c(\"\", \"(De)compress I/O Through Connections\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(\"\", \"(De)compress I/O Through Connections\"))"));           
do.call(`enc2utf8`, argv);           
}, o=expected);           

