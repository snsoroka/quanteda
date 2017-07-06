require(quanteda)

compile_dfm.tokens2 <- function(x) {
    
    attrs <- attributes(x) 
    attributes(x) <- NULL
    index <- qatd_cpp_xpointer_unlist(x)
    attributes(x) <- attrs
    
    # shift index for padding, if any
    if (attrs$padding) {
        types <- c("", types)
        index <- index + 1
    }
    
    temp <- Matrix::sparseMatrix(j = index, 
                         p = attr(index, 'position'), 
                         x = 1L, 
                         dims = c(length(attrs$docnames), length(attrs$types)),
                         dimnames = list(docs = attrs$docnames,
                                         features = as.character(attrs$ttypes)))
    new("dfmSparse", temp)
}

toks <- tokens(data_corpus_irishbudget2010)

load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
toks <- data_tokens_guardian

xpt <- qatd_cpp_xpointer(toks)

attr(xpt, 'types') <- attr(toks, 'types')
attr(xpt, 'padding') <- FALSE
attr(xpt, 'docnames') <- docnames(toks)

microbenchmark::microbenchmark(
    quanteda:::compile_dfm.tokens(toks),
    compile_dfm.tokens2(xpt),
    times = 5
)
