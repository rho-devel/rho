expected <- eval(parse(text="structure(list(linkfun = function (mu) log(mu), linkinv = function (eta) pmax(exp(eta), .Machine$double.eps), mu.eta = function (eta) pmax(exp(eta), .Machine$double.eps), valideta = function (eta) TRUE, name = \"log\"), .Names = c(\"linkfun\", \"linkinv\", \"mu.eta\", \"valideta\", \"name\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(linkfun = function (mu) log(mu), linkinv = function (eta) pmax(exp(eta), .Machine$double.eps), mu.eta = function (eta) pmax(exp(eta), .Machine$double.eps), valideta = function (eta) TRUE, name = \"log\")"));         
do.call(`list`, argv);         
}, o=expected);         

