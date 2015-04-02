expected <- eval(parse(text="structure(c(17.868, 2.469, 20.341, 0, 0), class = \"proc_time\", .Names = c(\"user.self\", \"sys.self\", \"elapsed\", \"user.child\", \"sys.child\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(58.56, 2.776, 61.341, 0, 0), .Names = c(\"user.self\", \"sys.self\", \"elapsed\", \"user.child\", \"sys.child\"), class = \"proc_time\"), structure(c(40.692, 0.307, 41, 0, 0), .Names = c(\"user.self\", \"sys.self\", \"elapsed\", \"user.child\", \"sys.child\"), class = \"proc_time\"))"));               
do.call(`-`, argv);               
}, o=expected);               

