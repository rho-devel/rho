expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"prediction from a rank-deficient fit may be misleading\", quote(predict.lm(object, newdata, se.fit, scale = residual.scale, type = ifelse(type == \"link\", \"response\", type), terms = terms, na.action = na.action)))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

