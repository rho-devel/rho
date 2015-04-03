expected <- eval(parse(text="12.8025995273675"));     
test(id=0, code={     
argv <- eval(parse(text="list(998.602763134667, 78L)"));     
do.call(`/`, argv);     
}, o=expected);     

