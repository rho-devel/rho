expected <- eval(parse(text="structure(c(0.100911915809157, 0.103585133630346, 0.24612916273096, 0.00838809509029823), .Names = c(\"ar1\", \"ar2\", \"intercept\", \"trend\"))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(0.0101832147522745, 0.0107298799092166, 0.0605795647466432, 7.03601392438852e-05), .Names = c(\"ar1\", \"ar2\", \"intercept\", \"trend\")))"));            
do.call(`sqrt`, argv);            
}, o=expected);            

