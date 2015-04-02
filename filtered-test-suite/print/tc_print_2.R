expected <- structure(list(modelID = 0L, terms = Species ~ Sepal.Length + 
    Sepal.Width + Petal.Length + Petal.Width, class.lev = c("setosa", 
"versicolor", "virginica"), model = "rf", formula = Species ~ 
    Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
    noClasses = 3L, priorClassProb = c(0.333333333333333, 0.333333333333333, 
    0.333333333333333), avgTrainPrediction = 0, noNumeric = 4L, 
    noDiscrete = 1L, discAttrNames = "Species", discValNames = list(
        c("setosa", "versicolor", "virginica")), numAttrNames = c("Sepal.Length", 
    "Sepal.Width", "Petal.Length", "Petal.Width"), discmap = 1L, 
    nummap = 2:5, skipmap = integer(0)), .Names = c("modelID", 
"terms", "class.lev", "model", "formula", "noClasses", "priorClassProb", 
"avgTrainPrediction", "noNumeric", "noDiscrete", "discAttrNames", 
"discValNames", "numAttrNames", "discmap", "nummap", "skipmap"
), class = "CoreModel")
test(id=1, code={
argv <- structure(list(x = structure(list(modelID = 0L, terms = Species ~ 
    Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
    class.lev = c("setosa", "versicolor", "virginica"), model = "rf", 
    formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + 
        Petal.Width, noClasses = 3L, priorClassProb = c(0.333333333333333, 
    0.333333333333333, 0.333333333333333), avgTrainPrediction = 0, 
    noNumeric = 4L, noDiscrete = 1L, discAttrNames = "Species", 
    discValNames = list(c("setosa", "versicolor", "virginica"
    )), numAttrNames = c("Sepal.Length", "Sepal.Width", "Petal.Length", 
    "Petal.Width"), discmap = 1L, nummap = 2:5, skipmap = integer(0)), .Names = c("modelID", 
"terms", "class.lev", "model", "formula", "noClasses", "priorClassProb", 
"avgTrainPrediction", "noNumeric", "noDiscrete", "discAttrNames", 
"discValNames", "numAttrNames", "discmap", "nummap", "skipmap"
), class = "CoreModel")), .Names = "x")
do.call('print', argv);
},  o = expected);

