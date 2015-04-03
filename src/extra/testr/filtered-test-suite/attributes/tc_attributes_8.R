expected <- eval(parse(text="NULL"));               
test(id=0, code={               
argv <- eval(parse(text="list(\"Error in setClass(\\\"class3\\\", representation(\\\"class1\\\", \\\"class2\\\")) : \\n  error in contained classes (\\\"class2\\\") for class “class3”; class definition removed from ‘.GlobalEnv’\\n\")"));               
do.call(`attributes`, argv);               
}, o=expected);               

