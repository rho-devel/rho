expected <- eval(parse(text="1L"));               
test(id=0, code={               
argv <- eval(parse(text="list(list(list(c(\"\", \"\", \"\\036\", \"\", \"New\", \"print()\", \"(S3)\", \"method\", \"for\", \"class\", \"\\\"function\\\",\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"also\", \"used\", \"for\", \"auto-printing.\", \"\", \"Further,\", \".Primitive\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"functions\", \"now\", \"print\", \"and\", \"auto-print\", \"identically.\", \"\", \"The\", \"new\", \"method\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"is\", \"based\", \"on\", \"code\", \"suggestions\", \"by\", \"Romain\", \"François.\"))))"));               
do.call(`seq_along`, argv);               
}, o=expected);               

