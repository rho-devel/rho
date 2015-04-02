expected <- eval(parse(text="list(\"* Edit the help file skeletons in 'man', possibly combining help files for multiple functions.\", \"* Edit the exports in 'NAMESPACE', and add necessary imports.\", \"* Put any C/C++/Fortran code in 'src'.\", \"* If you have compiled code, add a useDynLib() directive to 'NAMESPACE'.\", \"* Run R CMD build to build the package tarball.\", \"* Run R CMD check to check the package tarball.\", character(0), \"Read \\\"Writing R Extensions\\\" for more information.\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(\"* Edit the help file skeletons in 'man', possibly combining help files for multiple functions.\", \"* Edit the exports in 'NAMESPACE', and add necessary imports.\", \"* Put any C/C++/Fortran code in 'src'.\", \"* If you have compiled code, add a useDynLib() directive to 'NAMESPACE'.\", \"* Run R CMD build to build the package tarball.\", \"* Run R CMD check to check the package tarball.\", \"\", \"Read \\\"Writing R Extensions\\\" for more information.\"), \"\\n[ \\t\\n]*\\n\", FALSE, TRUE, TRUE)"));    
.Internal(`strsplit`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
}, o=expected);    

