data(iris3)
#data(iris)
#iris3 <- structure(aperm(array(t(as.matrix(iris[1:4])), c(4, 50, 3)),
#	 		c(2, 1, 3)),
#		  dimnames = list(NULL,
#		      c("Sepal L.", "Sepal W.", "Petal L.", "Petal W."),
#		      c("Setosa", "Versicolor", "Virginica")))
train <- rbind(iris3[1:25,,1],iris3[1:25,,2],iris3[1:25,,3])
test <- rbind(iris3[26:50,,1],iris3[26:50,,2],iris3[26:50,,3])
cl <- c(rep(1,25),rep(2,25), rep(3,25))
knn1(train, test, cl)
cl <- factor(cl, labels=c("s", "c", "v"))
knn1(train, test, cl)
knn(train, test, cl)
cl1 <- knn(train, test, cl, 2, 2)
sum(cl1!=cl, na.rm=T)/75 # lower bound on Bayes' error

table(cl, knn1(train, test, cl))

cd <- lvqinit(train, cl, 10)
table(cl, lvqtest(cd, train))
cd1 <- olvq1(train, cl, cd)
table(cl, lvqtest(cd1, train))
cd2 <- lvq1(train, cl, cd1)
table(cl, lvqtest(cd2, train))
cd3 <- lvq2(train, cl, cd1)
table(cl, lvqtest(cd3, train))
cd4 <- lvq3(train, cl, cd1)
table(cl, lvqtest(cd4, train))

tr <- log(train[1:75, 3:4])
te <- log(test[1:75, 3:4])
cl <- cl[1:75]

cd <- lvqinit(tr, cl, 6)
table(cl, lvqtest(cd, tr))
cd1 <- olvq1(tr, cl, cd)
table(cl, lvqtest(cd1, tr))
cd2 <- lvq1(tr, cl, cd1)
table(cl, lvqtest(cd2, tr))
cd3 <- lvq2(tr, cl, cd1,10000)
table(cl, lvqtest(cd3, tr))
cd4 <- lvq3(tr, cl, cd1,10000)
table(cl, lvqtest(cd4, tr))

plot(tr, type="n")
text(tr, text = rep(as.character(cl), 2), cex=0.7)

text(cd$x, text = as.character(cd$cl), col=2, cex=0.7)
text(cd1$x, text = as.character(cd1$cl), col=3, cex=0.7)
text(cd4$x, text = as.character(cd4$cl), col=6, cex=0.7)
text(cd3$x, text = as.character(cd3$cl), col=5, cex=0.7)

points(cd1$x, pch=3, mkh=0.2)
points(cd4$x, pch=3, mkh=0.2, col=6)

plot(tr, type="n")
text(tr, text = as.character(cl))
arrows(cd$x[,1], cd$x[,2], cd1$x[,1], cd1$x[,2])
arrows(cd1$x[,1], cd1$x[,2], cd4$x[,1], cd4$x[,2])
