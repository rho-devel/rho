# Miscellaneous tests

# 'break' in Promise:

quint <- function(arg) {
    sum <- 0
    for (i in 1:5) {
        sum <- sum + 1
	if (i == 3) sum <- sum + arg;
    }
    sum
}

quint(0)
quint(2)
try(quint(break))

# Missing ...

missdots <- function(...) missing(...)
missdots()
missdots(2)
