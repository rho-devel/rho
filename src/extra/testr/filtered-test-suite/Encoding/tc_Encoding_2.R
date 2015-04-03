expected <- eval(parse(text="c(\"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\", \"unknown\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(\"\\n\", \"\\n\", \"## These cannot be run by examples() but should be OK when pasted\\n\", \"## into an interactive R session with the tcltk package loaded\\n\", \"\\n\", \"tt <- tktoplevel()\\n\", \"tkpack(txt.w <- tktext(tt))\\n\", \"tkinsert(txt.w, \\\"0.0\\\", \\\"plot(1:10)\\\")\\n\", \"\\n\", \"# callback function\\n\", \"eval.txt <- function()\\n\", \"   eval(parse(text = tclvalue(tkget(txt.w, \\\"0.0\\\", \\\"end\\\"))))\\n\", \"tkpack(but.w <- tkbutton(tt, text = \\\"Submit\\\", command = eval.txt))\\n\", \"\\n\", \"## Try pressing the button, edit the text and when finished:\\n\", \"\\n\", \"tkdestroy(tt)\\n\", \"\\n\", \"\\n\"))"));                
.Internal(Encoding(argv[[1]]));                
}, o=expected);                

