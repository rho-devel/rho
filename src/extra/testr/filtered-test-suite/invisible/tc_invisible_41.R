expected <- eval(parse(text="structure(\"checkRd: (-3) Surv.Rd:90: Unnecessary braces at ‘{time2}’\", class = \"checkRd\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(\"checkRd: (-3) Surv.Rd:90: Unnecessary braces at ‘{time2}’\", class = \"checkRd\"))"));             
do.call(`invisible`, argv);             
}, o=expected);             

