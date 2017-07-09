#' @rdname as.xtokens
#' @return external pointer to tokens tokens object
#' @export
as.xtokens <- function(x, ...) {
    docvars(x, '_docname') <- docnames(x)
    names(x) <- NULL
    result <- quanteda:::qatd_cpp_xpointer(x)
    attributes(result) <- attributes(x)
    class(result) <- c('xtokens', 'tokens', 'tokenizedTexts')
    return(result)
}

#' print a stokens objects
#' print method for a tokenizedTextsHashed object
#' @param x a tokens object created by \code{\link{tokens}}
#' @param ... further arguments passed to base print method
#' @export
#' @method print xtokens
#' @noRd
print.xtokens <- function(x, ...) {
    attrs <- attributes(x)
    temp <- qatd_cpp_list(x)
    cat(attrs$class[1], " from ", ndoc(x), " document", 
        if (length(temp) > 1L) "s" else "", ".\n", sep = "")
    types <- c("", attrs$types)
    result <- lapply(temp, function(y) types[y + 1]) # shift index to show padding 
    class(result) <- "listof"
    print(result, ...)
}

#' @method "[" xtokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' xpt <- as.xtokens(toks)
#' str(xpt)
#' xpt[c(1,3)]
"[.xtokens" <- function(x, i, ...) {
    attrs <- attributes(x)
    if (is.logical(i))
        i <- which(i)
    result <- quanteda:::qatd_cpp_xpointer_subset(x, i)
    if (is.data.frame(attrs$docvars)) {
        attrs$docvars <- attrs$docvars[i,,drop = FALSE]
    }
    attributes(result) <- attrs
    return(result)
}

#' @method "[[" xtokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
#' xpt <- as.xtokens(toks)
#' str(xpt)
#' xpt[[1]]
"[[.xtokens" <- function(x, i, ...) {
    attrs <- attributes(x)
    if (is.logical(i))
        i <- which(i)
    result <- quanteda:::qatd_cpp_xpointer_subset(x, i)
    types <- c("", attrs$types)
    types[qatd_cpp_list(result)[[1]] + 1] # shift index to show padding 
}

#' @noRd
#' @export
docnames.xtokens <- function(x) {
    docvars(x, '_docname')
}


#' @method "tokens_select" xtokens
#' @export
#' @noRd
#' @examples 
#' toks <- tokens(data_corpus_inaugural)
#' xtoks <- as.xtokens(toks)
#' head(xtoks)
#' xtoks2 <- tokens_select(xtoks, stopwords(), padding = TRUE)
#' head(xtoks2)
tokens_select.xtokens <- function(x, features, selection = c("keep", "remove"), 
                                 valuetype = c("glob", "regex", "fixed"),
                                 case_insensitive = TRUE, padding = FALSE, ...) {
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    
    types <- types(x)
    features <- features2list(features)
    features_id <- regex2id(features, types, valuetype, case_insensitive)
    
    if ("" %in% features) features_id <- c(features_id, list(0)) # append padding index
    
    if (selection == 'keep') {
        result <- quanteda:::qatd_cpp_xpointer_select(x, types, features_id, 1, padding)
    } else {
        result <- quanteda:::qatd_cpp_xpointer_select(x, types, features_id, 2, padding)
    }
    attributes(result, FALSE) <- attrs
    return(result)
}
