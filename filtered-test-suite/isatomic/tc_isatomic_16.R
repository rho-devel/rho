expected <- eval(parse(text="FALSE"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(base = c(11L, 11L, 6L, 8L, 66L, 27L, 12L, 52L, 23L, 10L, 52L, 33L, 18L, 42L, 87L, 50L, 18L, 111L, 18L, 20L, 12L, 9L, 17L, 28L, 55L, 9L, 10L, 47L, 76L, 38L, 19L, 10L, 19L, 24L, 31L, 14L, 11L, 67L, 41L, 7L, 22L, 13L, 46L, 36L, 38L, 7L, 36L, 11L, 151L, 22L, 41L, 32L, 56L, 24L, 16L, 22L, 25L, 13L, 12L)), .Names = \"base\", class = \"data.frame\", row.names = c(1L, 5L, 9L, 13L, 17L, 21L, 25L, 29L, 33L, 37L, 41L, 45L, 49L, 53L, 57L, 61L, 65L, 69L, 73L, 77L, 81L, 85L, 89L, 93L, 97L, 101L, 105L, 109L, 113L, 117L, 121L, 125L, 129L, 133L, 137L, 141L, 145L, 149L, 153L, 157L, 161L, 165L, 169L, 173L, 177L, 181L, 185L, 189L, 193L, 197L, 201L, 205L, 209L, 213L, 217L, 221L, 225L, 229L, 233L)))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

