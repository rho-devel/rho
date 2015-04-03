expected <- eval(parse(text="structure(\"dataFrameD\", package = \".GlobalEnv\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(time = 1:10, y = c(1, 1.4142135623731, 1.73205080756888, 2, 2.23606797749979, 2.44948974278318, 2.64575131106459, 2.82842712474619, 3, 3.16227766016838)), .Names = c(\"time\", \"y\"), row.names = c(NA, 10L), .S3Class = \"data.frame\", date = structure(16045, class = \"Date\"), class = structure(\"dataFrameD\", package = \".GlobalEnv\")))"));              
do.call(`class`, argv);              
}, o=expected);              

