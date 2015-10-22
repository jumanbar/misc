Repositorio de funciones útiles y/o interesantes

# R

## read.arff

This function is a modified version of the one found in the foreign package. The changes aim to correct a problem I found with the standard read.arff: levels in factors do not match what's explicitly written in the arff file.

For example, if a nominal attribute in your arff datafile has this line in the header:

@attribute X {'A', 'B', 'C'}

But in your data you only have instances of 'A' and 'B', but not 'C', then what you get in R is:
    dat <- read.arff("data.arff")
    levels(dat$X)
    [1] "a"  "b"

Not only the levels are in lowercase, which I think is not convenient, but also there is one level which has disappeared. This is troublesome, specially if I wish to export my data frame to an arff file using write.arff.

With this version of read.arff, when dealing with the aforementioned case, i get:

    levels(dat$X)
    [1] "A"  "B"  "C"

And also I can set a couple of parameters which can help me tune up my work flow to better fit my needs (for example, reading only a limited number of lines, since I just want to make a couple of fast tests and therefore, I don't need the whole dataset).
 
