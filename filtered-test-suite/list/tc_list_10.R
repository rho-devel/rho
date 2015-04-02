expected <- eval(parse(text="structure(list(linkfun = function (mu) .Call(C_logit_link, mu), linkinv = function (eta) .Call(C_logit_linkinv, eta), mu.eta = function (eta) .Call(C_logit_mu_eta, eta), valideta = function (eta) TRUE, name = \"logit\"), .Names = c(\"linkfun\", \"linkinv\", \"mu.eta\", \"valideta\", \"name\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(linkfun = function (mu) .Call(C_logit_link, mu), linkinv = function (eta) .Call(C_logit_linkinv, eta), mu.eta = function (eta) .Call(C_logit_mu_eta, eta), valideta = function (eta) TRUE, name = \"logit\")"));         
do.call(`list`, argv);         
}, o=expected);         

