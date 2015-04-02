expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(1.67451869393188, 0.668927329809365, 0.0791361259651342, 0.543924729050942, 0.00967644138302005, 0.464139419264689, 1.12629957234273), 1e-30)"));          
do.call(`>=`, argv);          
}, o=expected);          

