expected <- eval(parse(text="c(2147483647L, 1073741823L, 536870911L, 268435455L, 134217727L, 67108863L, 33554431L, 16777215L, 8388607L, 4194303L, 2097151L, 1048575L, 524287L, 262143L, 131071L, 65535L, 32767L, 16383L, 8191L, 4095L, 2047L, 1023L, 511L, 255L, 127L, 63L, 31L, 15L, 7L, 3L, 1L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(-1, 1:31)"));  
.Internal(bitwiseShiftR(argv[[1]], argv[[2]]));  
}, o=expected);  

