## From: Prof Brian Ripley
## To: Martin Maechler
## cc: Doug and Martin
## Subject: Re: [Rd] Package Matrix does not compile in R-devel_2009-01-10   (fwd)
## Date: Thu, 15 Jan 2009 14:22:17 +0000 (GMT)


AMD <- c("aat", "1", "2", "postorder", "post_tree", "defaults",
         "order", "control", "info", "valid", "preprocess", "dump", "global")
cat("OBJS = ")
for (i in AMD) cat(sprintf("amd_i_%s.o amd_l_%s.o ", i, i))
cat("\n\n")
        
CC1 <- "\t$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include"
for (i in AMD)
     cat(sprintf("amd_i_%s.o: amd_%s.c $(INC)", i, i),
         sprintf(paste(CC1, "-DDINT -c amd_%s.c -o $@"), i), sep="\n")
cat("\n")
for (i in AMD)
     cat(sprintf("amd_l_%s.o: amd_%s.c $(INC)", i,i),
         sprintf(paste(CC1, "-DDLONG -c amd_%s.c -o $@"), i), sep="\n")
