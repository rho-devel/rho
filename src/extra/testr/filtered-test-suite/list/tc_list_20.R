expected <- eval(parse(text="list(\"Residuals vs Fitted\", \"Normal Q-Q\", \"Scale-Location\", \"Cook's distance\", \"Residuals vs Leverage\", expression(\"Cook's dist vs Leverage  \" * h[ii]/(1 - h[ii])))"));         
test(id=0, code={         
argv <- eval(parse(text="list(\"Residuals vs Fitted\", \"Normal Q-Q\", \"Scale-Location\", \"Cook's distance\", \"Residuals vs Leverage\", expression(\"Cook's dist vs Leverage  \" * h[ii]/(1 - h[ii])))"));         
do.call(`list`, argv);         
}, o=expected);         

