expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(list(double.eps = 2.22044604925031e-16, double.neg.eps = 1.11022302462516e-16, double.xmin = 2.2250738585072e-308, double.xmax = 1.79769313486232e+308, double.base = 2L, double.digits = 53L, double.rounding = 5L, double.guard = 0L, double.ulp.digits = -52L, double.neg.ulp.digits = -53L, double.exponent = 11L, double.min.exp = -1022L, double.max.exp = 1024L, integer.max = 2147483647L, sizeof.long = 8L, sizeof.longlong = 8L, sizeof.longdouble = 16L, sizeof.pointer = 8L), .Names = c(\"double.eps\", \"double.neg.eps\", \"double.xmin\", \"double.xmax\", \"double.base\", \"double.digits\", \"double.rounding\", \"double.guard\", \"double.ulp.digits\", \"double.neg.ulp.digits\", \"double.exponent\", \"double.min.exp\", \"double.max.exp\", \"integer.max\", \"sizeof.long\", \"sizeof.longlong\", \"sizeof.longdouble\", \"sizeof.pointer\")))"));            
do.call(`is.function`, argv);            
}, o=expected);            

