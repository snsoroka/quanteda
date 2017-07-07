require(quanteda)

compile_dfm.tokens2 <- function(x) {
    
    attrs <- attributes(x) 
    index <- quanteda:::qatd_cpp_xpointer_unlist(x)
    
    # shift index for padding, if any
    if (attrs$padding) {
        types <- c("", types)
        index <- index + 1
    }
    
    temp <- Matrix::sparseMatrix(j = index, 
                         p = attr(index, 'position'), 
                         x = 1L, 
                         dims = c(length(docvars(x, '_docname')), length(attrs$types)),
                         dimnames = list(docs = docvars(x, '_docname'),
                                         features = as.character(attrs$ttypes)))
    new("dfmSparse", temp)
}

toks <- tokens(data_corpus_irishbudget2010)

#load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
#toks <- data_tokens_guardian

xpt <- as.xtokens(toks)
head(xpt)
tail(xpt)
docnames(xpt)

microbenchmark::microbenchmark(
    quanteda:::compile_dfm.tokens(toks, verbose = FALSE),
    compile_dfm.tokens2(xpt),
    times = 100
)

microbenchmark::microbenchmark(
    quanteda:::qatd_cpp_xpointer_unlist(xpt),
    unlist(unclass(toks), use.names = FALSE),
    times = 100
)

out1 <- quanteda:::qatd_cpp_xpointer_unlist(xpt)
out2 <- unlist(unclass(toks), use.names = FALSE)

tail(out1)
tail(out2)

all(out1 == out2)
