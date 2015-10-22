
write.arff <- function (x, file, eol = "\n", 
                        relation = paste(deparse(substitute(x)), attr(x, "relation"), sep=" - ")) {
    if (file == "") 
        file <- stdout()
    else if (is.character(file)) {
        file <- file(file, "wb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("Argument 'file' must be a character string or connection.")
    if (!is.data.frame(x) && !is.matrix(x)) 
        x <- data.frame(x)
    squote <- function(s) {
        ifelse(is.na(s), s, sprintf("'%s'", gsub("(['\\])", 
            "\\\\\\1", s)))
    }
    spquote <- function(s) {
        if (length(grep("^[[:alpha:]]", s)) == 0L) 
            s <- paste("X", s, sep = "")
        if (length(grep(" ", s))) 
            s <- paste("\"", s, "\"", sep = "")
        s
    }
    text <- paste("@relation", spquote(make.names(relation)))
    if (length(text) > 1L)
        warning("Argument 'relation' has length > 1, only first element was used")
    writeLines(text[1], file, sep = eol)
    for (name in colnames(x)) {
        text <- paste("@attribute", spquote(name))
        if (is.data.frame(x) && is.factor(x[[name]])) {
            lev <- squote(levels(x[[name]]))
            levels(x[[name]]) <- lev
            text <- paste(text, " {", paste(lev, collapse = ","), 
                "}", sep = "")
        }
        else if (is.character(x[, name])) {
            text <- paste(text, "string")
            x[, name] <- squote((x[, name]))
        }
        else if (inherits(x[, name], "Date")) {
            text <- paste(text, "date \"yyyy-MM-dd\"")
            x[, name] <- squote(format(x[, name]))
        }
        else if (inherits(x[, name], "POSIXt")) {
            text <- paste(text, "date \"yyyy-MM-dd HH:mm:ss\"")
            x[, name] <- squote(format(x[, name]))
        }
        else text <- paste(text, "numeric")
        writeLines(text, file, sep = eol)
    }
    writeLines("@data", file)
    write.table(x, file = file, na = "?", sep = ",", eol = eol, 
        quote = FALSE, row.names = FALSE, col.names = FALSE)
}
