expected <- eval(parse(text="list(c(\"\", \"\", \"\\036\", \"\", \"Complex\", \"arithmetic\", \"sometimes\", \"warned\", \"incorrectly\", \"about\", \"\", \"\", \"\", \"\", \"\", \"\", \"producing\", \"NAs\", \"when\", \"there\", \"were\", \"NaNs\", \"in\", \"the\", \"input.\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(\"  \\036  Complex arithmetic sometimes warned incorrectly about       producing NAs when there were NaNs in the input.\", \"[ \\t\\n]\", FALSE, TRUE, TRUE)"));              
.Internal(strsplit(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));              
}, o=expected);              

