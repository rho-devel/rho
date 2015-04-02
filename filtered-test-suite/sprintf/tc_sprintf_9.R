expected <- eval(parse(text="\"tools:::.createExdotR(\\\"KernSmooth\\\", \\\"/home/lzhao/hg/r-instrumented/library/KernSmooth\\\", silent = TRUE, use_gct = FALSE, addTiming = FALSE)\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"tools:::.createExdotR(\\\"%s\\\", \\\"%s\\\", silent = TRUE, use_gct = %s, addTiming = %s)\", structure(\"KernSmooth\", .Names = \"Package\"), \"/home/lzhao/hg/r-instrumented/library/KernSmooth\", FALSE, FALSE)"));           
.Internal(sprintf(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));           
}, o=expected);           

