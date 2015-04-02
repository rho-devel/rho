expected <- eval(parse(text="structure(17L, .Names = \"1 DSTday\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(345595, 172795, 69115, 34555, 23035, 11515, 5755, 2875, 1147, 571, 379, 187, 91, 27, 11, 3, 1, 3, 4.42857142857143, 4.73716632443532, 4.86858316221766, 4.95619438740589, 4.97809719370294, 4.98904859685147, 4.99452429842574, 4.99780971937029, 4.99890485968515, 4.99945242984257, 4.99978097193703, 4.99989048596851, 4.99994524298426, 4.9999780971937, 4.99998904859685), .Names = c(\"1 sec\", \"2 secs\", \"5 secs\", \"10 secs\", \"15 secs\", \"30 secs\", \"1 min\", \"2 mins\", \"5 mins\", \"10 mins\", \"15 mins\", \"30 mins\", \"1 hour\", \"3 hours\", \"6 hours\", \"12 hours\", \"1 DSTday\", \"2 DSTdays\", \"1 week\", \"halfmonth\", \"1 month\", \"3 months\", \"6 months\", \"1 year\", \"2 years\", \"5 years\", \"10 years\", \"20 years\", \"50 years\", \"100 years\", \"200 years\", \"500 years\", \"1000 years\")))"));  
.Internal(`which.min`(argv[[1]]));  
}, o=expected);  

