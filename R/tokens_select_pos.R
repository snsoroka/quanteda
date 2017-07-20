#' select or remove tokens from a tokens object
#' 
#' This function selects or discards tokens from a \link{tokens} objects, with 
#' the shortcut \code{tokens_remove(x, pattern)} defined as a shortcut for 
#' \code{tokens_select_pos(x, pattern, selection = "remove")}.  The most common 
#' usage for \code{tokens_remove} will be to eliminate stop words from a text or
#' text-based object, while the most common use of \code{tokens_select} will be 
#' to select tokens with only positive pattern matches from a list of regular 
#' expressions, including a dictionary.
#' @param x \link{tokens} object whose token elements will be selected
#' @inheritParams pattern
#' @param selection whether to \code{"keep"} or \code{"remove"} the tokens 
#'   matching \code{pattern}
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param verbose if \code{TRUE} print messages about how many tokens were 
#'   selected or removed
#' @param padding if \code{TRUE}, leave an empty string where the removed tokens
#'   previously existed.  This is useful if a positional match is needed between
#'   the pre- and post-selected tokens, for instance if a window of adjacency 
#'   needs to be computed.
#' @return a \link{tokens} object with tokens selected or removed based on their
#'   match to \code{pattern}
#' @export
#' @examples 
#' ## tokens_select with simple examples
#' toks <- tokens(c("This is a sentence.", "This is a second sentence."), 
#'                  remove_punct = TRUE)
#' tokens_select_pos(toks, c("is", "a", "this"), selection = "keep", padding = FALSE)
#' tokens_select_pos(toks, c("is", "a", "this"), selection = "keep", padding = TRUE)
#' tokens_select_pos(toks, c("is", "a", "this"), selection = "remove", padding = FALSE)
#' tokens_select_pos(toks, c("is", "a", "this"), selection = "remove", padding = TRUE)
#' 
#' # how case_insensitive works
#' tokens_select_pos(toks, c("is", "a", "this"), selection = "remove", case_insensitive = TRUE)
#' tokens_select_pos(toks, c("is", "a", "this"), selection = "remove", case_insensitive = FALSE)
#' 
tokens_select_pos <- function(x, pattern, selection = c("keep", "remove"), 
                          valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE, padding = FALSE, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_select_pos")
}

#' @rdname tokens_select
#' @noRd
#' @importFrom RcppParallel RcppParallelLibs
#' @export
#' @examples
#' txts <- c("This is a sentence.", "This is a second sentence.")
#' pos <- spacyr::spacy_parse(txts)
#' toks <- as.tokens(pos)
#' quanteda:::has_postags(toks)
#' print(toks)
#' tokens_select_pos(toks, 'DET', selection = 'remove', padding = FALSE)
#' tokens_select_pos(toks, 'DET', selection = 'keep', padding = TRUE)
#' 
#' identical(as.list(tokens_select_pos(toks, 'DET', padding = FALSE)),
#'           as.list(tokens_select(toks, c('a', 'this'), padding = FALSE)))
#'           
#' identical(as.list(tokens_remove_pos(toks, 'DET', padding = FALSE)),
#'           as.list(tokens_remove(toks, c('a', 'this'), padding = FALSE)))
#' 
#' pos2 <- spacyr::spacy_parse(texts(data_corpus_inaugural))
#' toks2 <- as.tokens(pos2)
#' tokens_select_pos(toks2, 'PROPN', selection = 'keep', padding = FALSE)
#' tokens_select_pos(toks2, 'PROPN', selection = 'keep', padding = TRUE)
#' tokens_select_pos(toks2, phrase('PROPN PROPN'), selection = 'keep', padding = TRUE)
#' tokens_select_pos(toks2, phrase('VERB PART'), selection = 'keep', padding = TRUE)
#' 
tokens_select_pos.tokens <- function(x, pattern, selection = c("keep", "remove"), 
                                 valuetype = c("glob", "regex", "fixed"),
                                 case_insensitive = TRUE, padding = FALSE, 
                                 verbose = quanteda_options("verbose"), ...) {
    if (!has_annotation(x))
        stop('tokens object must have part-of-speech tags')
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    
    attrs <- attributes(x)
    types <- types(x)
    y <- annotation(x)
    tags <- tags(y)

    tags_id <- features2id(pattern, tags, valuetype, case_insensitive)
    if ("" %in% pattern) tags_id <- c(tags_id, list(0)) # append padding index

    if (verbose) 
        message_select(selection, length(tags_id), 0)
    
    # get binary flags for matched tags
    flag <- qatd_cpp_tokens_detect(y, tags_id)
    
    # combine tokens and tags
    temp <- data.frame(tok = unlist(unclass(x), use.names = FALSE),
                       tag = unlist(unclass(y), use.names = FALSE),
                       doc = factor(rep(seq_along(x), lengths(x)), levels = seq_along(x)))
    
    # vectorized selection
    if (selection == 'keep') {
        flag <- unlist(flag, use.names = FALSE) == 1L
    } else {
        flag <- unlist(flag, use.names = FALSE) == 0L
    }
    if (padding) {
        temp[!flag, 1:2] <- 0L
    } else {
        temp <- temp[flag, ]
    }

    x <- qatd_cpp_tokens_recompile(base::split(temp$tok, temp$doc), types)
    y <- qatd_cpp_tokens_recompile(base::split(temp$tag, temp$doc), tags)
    attributes(y) <- list('tags' = attr(y, 'types'))
    attributes(x, FALSE) <- attrs
    attr(x, 'annotation') <- y
    return(x)
}

#' @rdname tokens_select
#' @export
#' @examples
#' ## tokens_remove example
#' txt <- c(wash1 <- "Fellow citizens, I am again called upon by the voice of my country to 
#'                    execute the functions of its Chief Magistrate.",
#'          wash2 <- "When the occasion proper for it shall arrive, I shall endeavor to express
#'                    the high sense I entertain of this distinguished honor.")
#' tokens_remove_pos(tokens(txt, remove_punct = TRUE), stopwords("english"))
#'
tokens_remove_pos <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE, padding = FALSE, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_remove_pos")
}

#' @noRd
#' @export
tokens_remove_pos.tokens <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                                 case_insensitive = TRUE, padding = FALSE, verbose = quanteda_options("verbose")) {
    tokens_select_pos(x, pattern, selection = "remove", valuetype = valuetype, 
                      case_insensitive = case_insensitive, padding = padding, verbose = verbose)
}

#' @rdname as.tokens
#' @export
as.tokens.spacyr_parsed <- function(x, concatenator = '_') {
    result <- as.tokens(base::split(x$token, factor(x$doc_id, levels = unique(x$doc_id))))
    temp <- as.tokens(base::split(x$pos, factor(x$doc_id, levels = unique(x$doc_id))))
    attributes(temp) <- list('tags' = attr(temp, 'types'))
    attr(result, 'annotation') <- temp
    return(result)
}

#' @keywords internal
has_annotation <- function(x) {
    !is.null(attr(x, 'annotation'))
}

#' @keywords internal
annotation <- function(x) {
    attr(x, 'annotation')
}

#' @keywords internal
tags <- function(x) {
    attr(x, 'tags')
}


