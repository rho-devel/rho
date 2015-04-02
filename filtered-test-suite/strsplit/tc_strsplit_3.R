expected <- eval(parse(text="list(c(\"\", \"\", \"\\036\", \"\", \"isSeekable()\", \"now\", \"returns\", \"FALSE\", \"on\", \"connections\", \"\", \"\", \"\", \"\", \"\", \"\", \"which\", \"have\", \"non-default\", \"encoding.\", \"\", \"Although\", \"documented\", \"to\", \"\", \"\", \"\", \"\", \"\", \"\", \"record\", \"if\", \"‘in\", \"principle’\", \"the\", \"connection\", \"supports\", \"seeking,\", \"\", \"\", \"\", \"\", \"\", \"\", \"it\", \"seems\", \"safer\", \"to\", \"report\", \"FALSE\", \"when\", \"it\", \"may\", \"not\", \"work.\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(\"  \\036  isSeekable() now returns FALSE on connections       which have non-default encoding.  Although documented to       record if ‘in principle’ the connection supports seeking,       it seems safer to report FALSE when it may not work.\", \"[ \\t\\n]\", FALSE, TRUE, FALSE)"));              
.Internal(strsplit(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));              
}, o=expected);              

