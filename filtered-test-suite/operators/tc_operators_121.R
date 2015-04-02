expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(1L, 5L))"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(1.22408179743946+0i, 0.359813827057364+0i, 0.400771450594052+0i, 0.11068271594512+0i, -0.555841134754075+0i), structure(c(1.22408179743946+0i, 0.359813827057364+0i, 0.400771450594052+0i, 0.11068271594512+0i, -0.555841134754075+0i), .Dim = c(1L, 5L)))"));            
do.call(`==`, argv);            
}, o=expected);            

