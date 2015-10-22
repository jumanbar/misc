# Repositorio de funciones útiles y/o interesantes

## R

### read.arff

This function is a modified version of the one found in the [foreign](https://cran.r-project.org/web/packages/foreign/index.html) package. 
This changes aim to correct a problem I found with the standard `read.arff`: levels in factors do not match what's explicitly written in the original arff file.

For example, if a nominal attribute in some arff datafile has this line in the header:

    @attribute X {'A', 'B', 'C'}

But the data only have instances of '`A`' and '`B`', but not '`C`', then what R imports is:

    dat <- read.arff("data.arff")
    levels(dat$X)
    [1] "a"  "b"

Not only the levels are in lowercase, but also there is one level which has disappeared. This is troublesome, specially if I wish to export my data frame to an arff file using `write.arff`.

With this version of `read.arff`, when dealing with the aforementioned case, I get:

    levels(dat$X)
    [1] "A"  "B"  "C"

And also I can set a couple of parameters which can help me tune up my work flow to better fit my needs (for example, reading only a limited number of lines, since I just want to make a couple of fast tests and therefore, I don't need the whole dataset).
 
### write.arff

It's the same on found in the [foreign](https://cran.r-project.org/web/packages/foreign/index.html) package, only with a slightly different handling of the "relation" argument. It assumes that the data.frame x has a "relation" attribute and uses it accordingly (pastes the R object name with the relation attribute).
