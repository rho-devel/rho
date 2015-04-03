expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(loglik = c(-577.080015666702, -568.702653976085, -567.639101463216, -565.252511135152), Chisq = c(NA, 16.7547233812336, 2.12710502573896, 4.77318065612872), Df = c(NA, 1, 1, 3), `Pr(>|Chi|)` = c(NA, 4.25362427346476e-05, 0.144713844418628, 0.189179603743297)), .Names = c(\"loglik\", \"Chisq\", \"Df\", \"Pr(>|Chi|)\"), row.names = c(\"NULL\", \"ph.ecog\", \"wt.loss\", \"poly(age, 3)\"), class = c(\"anova\", \"data.frame\"), heading = \"Analysis of Deviance Table\\n Cox model: response is Surv(time, status)\\nTerms added sequentially (first to last)\\n\"))"));          
do.call(`is.environment`, argv);          
}, o=expected);          

