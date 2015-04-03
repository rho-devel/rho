expected <- "./Pkgs"      
test(id=9, code={      
argv <- structure(list(".", "Pkgs"), .Names = c("", ""))      
do.call('file.path', argv);      
},  o = expected);      
      
