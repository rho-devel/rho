expected <- eval(parse(text="\"The following object is masked from ‘package:base’:\\n\\n    det\\n\""));         
test(id=0, code={         
argv <- eval(parse(text="list(NULL, \"The following object is masked from ‘package:base’:\\n\\n    det\\n\")"));         
.Internal(gettext(argv[[1]], argv[[2]]));         
}, o=expected);         

