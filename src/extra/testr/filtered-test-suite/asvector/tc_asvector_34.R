expected <- eval(parse(text="c(0, 0.0123079727211562, 0.00970882237374837, 0.62883302403078, 0.689843718945119, 0.689843718944881, 0.672453157851573, 0.534493702379921, 0.171039529097608, 0.17103952909345, 0.50219835346871, 0.530975095958163, 0.0050966004562048, 0.0106639382954144, 0.811192712625201, 0.0957932531337699)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(0, 0.0123079727211562, 0.00970882237374837, 0.62883302403078, 0.689843718945119, 0.689843718944881, 0.672453157851573, 0.534493702379921, 0.171039529097608, 0.17103952909345, 0.50219835346871, 0.530975095958163, 0.0050966004562048, 0.0106639382954144, 0.811192712625201, 0.0957932531337699), .Names = c(\"(Intercept)\", \"M.userY\", \"TempHigh\", \"M.userY:TempHigh\", \"SoftMedium\", \"SoftSoft\", \"M.userY:SoftMedium\", \"M.userY:SoftSoft\", \"TempHigh:SoftMedium\", \"TempHigh:SoftSoft\", \"M.userY:TempHigh:SoftMedium\", \"M.userY:TempHigh:SoftSoft\", \"BrandM\", \"M.userY:BrandM\", \"TempHigh:BrandM\", \"M.userY:TempHigh:BrandM\")), \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

