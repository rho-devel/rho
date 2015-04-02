expected <- eval(parse(text="\"RCODE\""));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(\"mtext(\\\"«Latin-1 accented chars»: éè øØ å<Å æ<Æ\\\", side = 3)\\n\", Rd_tag = \"RCODE\"), \"Rd_tag\")"));        
do.call(`attr`, argv);        
}, o=expected);        

