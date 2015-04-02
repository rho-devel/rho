expected <- eval(parse(text="structure(function (...) new(\"test1\", ...), className = structure(\"test1\", package = \".GlobalEnv\"), package = \".GlobalEnv\", class = structure(\"classGeneratorFunction\", package = \"methods\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(function (...) new(\"test1\", ...), className = structure(\"test1\", package = \".GlobalEnv\"), package = \".GlobalEnv\", class = structure(\"classGeneratorFunction\", package = \"methods\")))"));      
do.call(`invisible`, argv);      
}, o=expected);      

