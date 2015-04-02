expected <- structure(c(-0.0296690260968828, 0.200337918547016, -0.38901358729166, 
0.076054310915896, -0.5953576286578, 1.55058467328697, -0.189955959788191, 
-1.31965097077132, 0.596281133731208, 1.22982396127581, -0.0296690260968828, 
0.200337918547016, -0.38901358729166, 0.076054310915896, -0.5953576286578, 
1.55058467328697, -0.189955959788191, -1.31965097077132, 0.596281133731208, 
1.22982396127581), .Dim = c(10L, 2L), .Dimnames = list(NULL, 
    c("runif.10...pi.2..pi.2.", "runif.10...pi.2..pi.2.")))
test(id=164, code={
argv <- list(structure(c(-0.0296690260968828, 0.200337918547016, -0.38901358729166, 
0.076054310915896, -0.5953576286578, 1.55058467328697, -0.189955959788191, 
-1.31965097077132, 0.596281133731208, 1.22982396127581), .Dim = c(10L, 
1L), .Dimnames = list(NULL, "runif.10...pi.2..pi.2."), circularp = structure(list(
    type = "angles", units = "radians", template = "none", modulo = "asis", 
    zero = 0, rotation = "counter"), .Names = c("type", "units", 
"template", "modulo", "zero", "rotation")), class = c("circular", 
"matrix")), structure(c(-0.0296690260968828, 0.200337918547016, 
-0.38901358729166, 0.076054310915896, -0.5953576286578, 1.55058467328697, 
-0.189955959788191, -1.31965097077132, 0.596281133731208, 1.22982396127581
), .Dim = c(10L, 1L), .Dimnames = list(NULL, "runif.10...pi.2..pi.2."), circularp = structure(list(
    type = "angles", units = "radians", template = "none", modulo = "asis", 
    zero = 0, rotation = "counter"), .Names = c("type", "units", 
"template", "modulo", "zero", "rotation")), class = c("circular", 
"matrix")))
do.call('cbind', argv);
},  o = expected);

