cat("Atencion: se cargan las funciones read.arff y write.arff modificadas, basadas en el paquete foreign\n")

#' @title
#'  Read Data from ARFF Files
#'
#' @description
#'  Reads data from Weka Attribute-Relation File Format (ARFF) files.
#'
#' @param file a character string with the name of the ARFF file to read from, 
#'             or a \code{\link{connection}} which will be opened if necessary, and if so 
#'             closed at the end of the function call.
#'
#' @param nrows integer: the maximum number of rows of data to read in. Negative and other 
#'        invalid values are ignored.
#'
#' @param skip integer: the number of rows of data to skip before beginning 
#'        to read data.
#'
#' @value 
#'  A data frame containing the data from the ARFF file.
#'  
#' @references
#'  Attribute-Relation File Format \link{http://www.cs.waikato.ac.nz/~ml/weka/arff.html}
#'  \link{http://weka.sourceforge.net/wekadoc/index.php/en:ARFF_(3.5.1)}
#'
#' @seealso \code{\link{write.arff}}
read.arff <- function (file, nrows = -1, skip = 0) {
    # Authors: R Core Team and Juan M. Barreneche
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("Argument 'file' must be a character string or connection.")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    col_names <- NULL
    col_types <- NULL
    col_dfmts <- character() ## Data formats
    line <- readLines(file, n = 1L)
    nom_levels <- NULL  ## List with al the labels, used to edit factor levels later on
    a <- 0L             ## Iterator to keep account of attribute numbers
    nom_attribs <- NULL ## Vector to store nominal attribute indexes
    while (length(line) && regexpr("^[[:space:]]*@(?i)data", 
        line, perl = TRUE) == -1L) {
        if (regexpr("^[[:space:]]*@(?i)relation", line, perl = TRUE) > 0L) {
            relation <- gsub("^[[:space:]]*@(?i)relation[[:space:]]+", "", line)
        }
        if (regexpr("^[[:space:]]*@(?i)attribute", line, perl = TRUE) > 0L) {
            a <- a + 1L
            con <- textConnection(line)
            line <- scan(con, character(), quiet = TRUE)
            close(con)
            if (length(line) < 3L) 
                stop("Invalid attribute specification.")
            col_names <- c(col_names, line[2L])
            if ((type <- tolower(line[3L])) == "date") {
                col_types <- c(col_types, "character")
                col_dfmts <- c(col_dfmts, if (length(line) > 
                  3L) ISO_8601_to_POSIX_datetime_format(line[4L]) else "%Y-%m-%d %H:%M:%S")
            }
            else if (type == "relational") 
                stop("Type 'relational' currently not implemented.")
            else {
                ## Here i deal with the levels of nominal attributes:
                if (grepl("^\\{.*\\}$", type)) {
                    nom_attribs <- c(nom_attribs, a)
                    lev <- strsplit(gsub("\\{|\\}|\\'", "", line[3L]), ",")[[1]]
                    nom_levels[[a]] <- lev
                }
                type <- sub("\\{.*", "factor", type)
                type <- sub("string", "character", type)
                type <- sub("real", "numeric", type)
                col_types <- c(col_types, type)
                col_dfmts <- c(col_dfmts, NA)
            }
        }
        line <- readLines(file, n = 1L)
    }
    if (length(line) == 0L) 
        stop("Missing data section.")
    if (is.null(col_names)) 
        stop("Missing attribute section.")
    if (length(col_names) != length(grep("factor|numeric|character", 
        col_types))) 
        stop("Invalid type specification.")
    data <- read.table(file, sep = ",", na.strings = "?", colClasses = col_types, 
        comment.char = "%", nrows = nrows, skip = skip)
    if (any(ind <- which(!is.na(col_dfmts))))
        for (i in ind) data[i] <- as.data.frame(strptime(data[[i]], col_dfmts[i]))
    for (i in nom_attribs) {
        data[[i]] <- factor(data[[i]], levels = nom_levels[[i]])
    }
        
    for (i in seq_len(length(data))) {
        if (is.factor(data[[i]])) {
            levels(data[[i]]) <- gsub("\\\\", "", levels(data[[i]]))
        }
    }
    names(data) <- col_names
    attr(data, "relation") <- relation
    class(data) <- c(class(data), "arff.data.frame")
    data
}
