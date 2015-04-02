expected <- eval(parse(text="NULL"));         
test(id=0, code={         
argv <- eval(parse(text="list(list(character(0), numeric(0), numeric(0), complex(0), integer(0), logical(0), character(0)))"));         
do.call(`names`, argv);         
}, o=expected);         

