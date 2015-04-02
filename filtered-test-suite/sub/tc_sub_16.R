expected <- eval(parse(text="c(\"1_\", \"Weight\", \"Cylinders4\", \"Cylinders5\", \"Cylinders6\", \"Cylinders8\", \"Cylindersrotary\", \"TypeLarge\", \"TypeMidsize\", \"TypeSmall\", \"TypeSporty\", \"TypeVan\", \"EngineSize\", \"DriveTrainFront\", \"DriveTrainRear\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"^ +\", \"\", c(\"1_\", \"Weight\", \"Cylinders4\", \"Cylinders5\", \"Cylinders6\", \"Cylinders8\", \"Cylindersrotary\", \"TypeLarge\", \"TypeMidsize\", \"TypeSmall\", \"TypeSporty\", \"TypeVan\", \"EngineSize\", \"DriveTrainFront\", \"DriveTrainRear\"), FALSE, FALSE, FALSE, FALSE)"));   
.Internal(`sub`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));   
}, o=expected);   

