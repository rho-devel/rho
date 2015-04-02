expected <- eval(parse(text="\"67% said yes (out of a sample of size 3)\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"%.0f%% said yes (out of a sample of size %.0f)\", 66.666, 3)"));           
.Internal(sprintf(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

