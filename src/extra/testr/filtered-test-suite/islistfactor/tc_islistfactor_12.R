expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(event = c(\"Min.   : 1.00  \", \"1st Qu.: 9.00  \", \"Median :18.00  \", \"Mean   :14.74  \", \"3rd Qu.:20.00  \", \"Max.   :23.00  \", NA), mag = c(\"Min.   :5.000  \", \"1st Qu.:5.300  \", \"Median :6.100  \", \"Mean   :6.084  \", \"3rd Qu.:6.600  \", \"Max.   :7.700  \", NA), station = c(\"117    :  5  \", \"1028   :  4  \", \"113    :  4  \", \"112    :  3  \", \"135    :  3  \", \"(Other):147  \", \"NA's   : 16  \"), dist = c(\"Min.   :  0.50  \", \"1st Qu.: 11.32  \", \"Median : 23.40  \", \"Mean   : 45.60  \", \"3rd Qu.: 47.55  \", \"Max.   :370.00  \", NA), accel = c(\"Min.   :0.00300  \", \"1st Qu.:0.04425  \", \"Median :0.11300  \", \"Mean   :0.15422  \", \"3rd Qu.:0.21925  \", \"Max.   :0.81000  \", NA)), .Names = c(\"event\", \"mag\", \"station\", \"dist\", \"accel\")), TRUE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

