expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(list(c(\"r1\", \"r3\", \"r4\", \"r5\", \"r6\", \"r7\", \"r8\", \"r9\", \"r10\", \"r11\", \"r12\", \"r13\", \"r14\", \"r15\", \"r16\", \"r17\", \"r18\", \"r19\", \"r20\", \"r21\", \"r22\", \"r23\", \"r24\", \"r25\", \"r26\", \"r27\", \"r28\", \"r29\", \"r30\", \"r31\", \"r32\", \"r33\", \"r34\", \"r35\", \"r36\", \"r37\", \"r38\", \"r39\", \"r40\"), c(\"c1\", \"c2\", \"c3\", \"c4\", \"c5\", \"c6\", \"c7\", \"c8\", \"c9\", \"c10\", \"c11\", \"c12\", \"c13\", \"c14\", \"c15\", \"c16\", \"c17\", \"c18\", \"c19\", \"c20\")), list(character(0), character(0)), TRUE, TRUE, TRUE, TRUE, FALSE)"));             
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

