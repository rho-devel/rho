expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(\"mtext(\\\"«Latin-1 accented chars»: éè øØ å<Å æ<Æ\\\", side = 3)\\n\", Rd_tag = \"RCODE\"), TRUE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

