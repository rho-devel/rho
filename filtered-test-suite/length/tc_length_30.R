expected <- eval(parse(text="4L"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(Df = c(NA, 1, 1, 1), `Sum of Sq` = c(NA, 820.907401534698, 26.7893827563485, 9.93175377572661), RSS = c(47.9727294003871, 868.880130935086, 74.7621121567356, 57.9044831761137), AIC = c(24.9738836085411, 60.6293256496563, 28.7417044039189, 25.4199908988691)), .Names = c(\"Df\", \"Sum of Sq\", \"RSS\", \"AIC\"), row.names = c(\"<none>\", \"- x1\", \"- x2\", \"- x4\"), class = c(\"anova\", \"data.frame\")))"));          
do.call(`length`, argv);          
}, o=expected);          

