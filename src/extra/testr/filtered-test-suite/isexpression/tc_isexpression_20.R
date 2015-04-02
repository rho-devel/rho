expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(print(.leap.seconds, tz = \"PST8PDT\")))"));       
do.call(`is.expression`, argv);       
}, o=expected);       

