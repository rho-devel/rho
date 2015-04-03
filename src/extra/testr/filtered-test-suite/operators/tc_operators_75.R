expected <- eval(parse(text="c(101L, 100L, 99L, 98L, 97L, 96L, 95L, 94L, 93L, 92L, 91L, 90L, 89L, 88L, 87L, 86L, 85L, 84L, 83L, 82L, 81L, 80L, 79L, 78L, 77L, 76L, 75L, 74L, 73L, 72L, 71L, 70L, 69L, 68L, 67L, 66L, 65L, 64L, 63L, 62L, 61L, 60L, 59L, 58L, 57L, 56L, 55L, 54L, 53L, 52L, 51L, 50L, 49L, 48L, 47L, 46L, 45L, 44L, 43L, 42L, 41L, 40L, 39L, 38L, 37L, 36L, 35L, 34L, 33L, 32L, 31L, 30L, 29L, 28L, 27L, 26L, 25L, 24L, 23L, 22L, 21L, 20L, 19L, 18L, 17L, 16L, 15L, 14L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L)"));             
test(id=0, code={             
argv <- eval(parse(text="list(101L, 1L)"));             
do.call(`:`, argv);             
}, o=expected);             

