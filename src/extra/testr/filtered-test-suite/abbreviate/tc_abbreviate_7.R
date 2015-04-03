expected <- eval(parse(text="c(\"1_\", \"Wght\", \"Cyl4\", \"Cyl5\", \"Cyl6\", \"Cyl8\", \"Cyln\", \"TypL\", \"TypM\", \"TypS\", \"TypS\", \"TypV\", \"EngS\", \"DrTF\", \"DrTR\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"1_\", \"Weight\", \"Cylinders4\", \"Cylinders5\", \"Cylinders6\", \"Cylinders8\", \"Cylindersrotary\", \"TypeLarge\", \"TypeMidsize\", \"TypeSmall\", \"TypeSporty\", \"TypeVan\", \"EngineSize\", \"DriveTrainFront\", \"DriveTrainRear\"), 4L, TRUE)"));  
.Internal(`abbreviate`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

