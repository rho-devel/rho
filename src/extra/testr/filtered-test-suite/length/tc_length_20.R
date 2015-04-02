expected <- eval(parse(text="9L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(c(\"  These operators return vectors containing the result of the element\", \"  by element operations.  The elements of shorter vectors are recycled\", \"  as necessary (with a ‘warning’ when they are recycled only\", \"  _fractionally_).  The operators are ‘+’ for addition,\", \"  ‘-’ for subtraction, ‘*’ for multiplication, ‘/’ for\", \"  division and ‘^’ for exponentiation.\", \"\", \"  ‘%%’ indicates ‘x mod y’ and ‘%/%’ indicates\", \"  integer division.  It is guaranteed that \"))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

