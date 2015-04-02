expected <- eval(parse(text="c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(\"{refObject}\", c(\"\\\\section{Extends}{\", \"Class \\\\code{\\\"\\\\linkS4class{refClassA}\\\"}, directly.\", \"Class \\\\code{\\\"\\\\linkS4class{envRefClass}\\\"}, by class \\\"refClassA\\\", distance 2.\", \"Class \\\\code{\\\"\\\\linkS4class{.environment}\\\"}, by class \\\"refClassA\\\", distance 3.\", \"Class \\\\code{\\\"\\\\linkS4class{refClass}\\\"}, by class \\\"refClassA\\\", distance 3.\", \"Class \\\\code{\\\"\\\\linkS4class{environment}\\\"}, by class \\\"refClassA\\\", distance 4, with explicit coerce.\", \"Class \\\\code{\\\"\\\\linkS4class{refObject}\\\"}, by class \\\"refClassA\\\", distance 4.\", \"}\"), FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)"));                 
.Internal(grepl(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));                 
}, o=expected);                 

