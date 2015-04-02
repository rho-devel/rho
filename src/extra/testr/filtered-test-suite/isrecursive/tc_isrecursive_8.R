expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(list(var = structure(c(2L, 1L, 1L), .Label = c(\"<leaf>\", \"Start\"), class = \"factor\"), n = c(81L, 62L, 19L), wt = c(81, 62, 19), dev = c(24.3, 8.57647058823529, 14.175), yval = c(1, 1, 2), complexity = c(0.0637254901960785, 0, 1e-15), ncompete = c(2L, 0L, 0L), nsurrogate = c(0L, 0L, 0L), yval2 = structure(c(1, 1, 2, 64, 56, 8, 17, 6, 11, 0.7, 0.852610030706244, 0.310704960835509, 0.3, 0.147389969293756, 0.689295039164491, 0.999999999999999, 0.718382352941176, 0.281617647058824), .Dim = c(3L, 6L), .Dimnames = list(NULL, c(\"\", \"\", \"\", \"\", \"\", \"nodeprob\")))), .Names = c(\"var\", \"n\", \"wt\", \"dev\", \"yval\", \"complexity\", \"ncompete\", \"nsurrogate\", \"yval2\"), row.names = c(NA, 3L), class = \"data.frame\"))"));            
do.call(`is.recursive`, argv);            
}, o=expected);            

