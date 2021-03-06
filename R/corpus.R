#' construct a corpus object
#' 
#' Creates a corpus object from available sources.  The currently available  
#' sources are: 
#' \itemize{ 
#' \item a \link{character} vector, consisting of one document per element; if 
#'   the elements are named, these names will be used as document names.
#' \item a \link{data.frame}, whose default document id is a variable identified
#' by \code{docid_field}; the text of the document is a variable identified by
#' \code{textid_field}; and other variables are imported as document-level
#' meta-data.  This matches the format of data.frames constructed by the the
#' \pkg{readtext} package.
#' \item a \link{kwic} object constructed by \code{\link{kwic}}.
#' \item a \pkg{tm} \link[tm]{VCorpus} or \link[tm]{SimpleCorpus} class  object, 
#'   with the fixed metadata 
#'   fields imported as \link{docvars} and corpus-level metadata imported
#'   as \link{metacorpus} information.
#' \item a \link{corpus} object.
#' } 
#' @param x a valid corpus source object
#' @param docnames Names to be assigned to the texts.  Defaults to the names of 
#'   the character vector (if any); \code{doc_id} for a data.frame; the document
#'   names in a \pkg{tm} corpus; or a vector of user-supplied labels equal in 
#'   length to the number of documents.  If none of these are round, then 
#'   "text1", "text2", etc. are assigned automatically.
#' @param docvars a data.frame of document-level variables associated with each text
#' @param text_field the character name or numeric index of the source
#'   \code{data.frame} indicating the variable to be read in as text, which must
#'   be a character vector. All other variables in the data.frame will be
#'   imported as docvars.  This argument is only used for \code{data.frame}
#'   objects (including those created by \pkg{readtext}).
#' @param metacorpus a named list containing additional (character) information
#'   to be added to the corpus as corpus-level metadata.  Special fields
#'   recognized in the \code{\link{summary.corpus}} are:
#' \itemize{
#' \item{\code{source }}{a description of the source of the texts, used for 
#'   referencing;}
#' \item{\code{citation }}{information on how to cite the corpus; and}
#' \item{\code{notes }}{any additional information about who created the text, warnings, 
#'   to do lists, etc.}
#' }
#' @param compress logical; if \code{TRUE}, compress the texts in memory using
#'   gzip compression. This significantly reduces the size of the corpus in
#'   memory, but will slow down operations that require the texts to be
#'   extracted.
#' @param ... not used directly
#' @return A \link{corpus-class} class object containing the original texts,
#'   document-level variables, document-level metadata, corpus-level metadata,
#'   and default settings for subsequent processing of the corpus.
#' @section A warning on accessing corpus elements: A corpus currently consists
#'   of an S3 specially classed list of elements, but \strong{you should not 
#'   access these elements directly}. Use the extractor and replacement 
#'   functions instead, or else your code is not only going to be uglier, but 
#'   also likely to break should the internal structure of a corpus object 
#'   change (as it inevitably will as we continue to develop the package, 
#'   including moving corpus objects to the S4 class system).
#' @seealso \link{corpus-class}, \code{\link{docvars}}, \code{\link{metadoc}}, 
#'   \code{\link{metacorpus}}, 
#'   \code{\link{settings}}, \code{\link{texts}}, \code{\link{ndoc}}, 
#'   \code{\link{docnames}}
#' @details The texts and document variables of corpus objects can also be 
#'   accessed using index notation. Indexing a corpus object as a vector will 
#'   return its text, equivalent to \code{texts(x)}.  Note that this is not the 
#'   same as subsetting the entire corpus -- this should be done using the 
#'   \code{\link{subset}} method for a corpus.
#'   
#'   Indexing a corpus using two indexes (integers or column names) will return 
#'   the document variables, equivalent to \code{docvars(x)}.  It is also
#'   possible to access, create, or replace docvars using list notation, e.g.
#'   
#'   \code{myCorpus[["newSerialDocvar"]] <- 
#'   paste0("tag", 1:ndoc(myCorpus))}.
#'   
#'   For details, see \link{corpus-class}.
#' @author Kenneth Benoit and Paul Nulty
#' @export
#' @keywords corpus
#' @examples
#' # create a corpus from texts
#' corpus(data_char_ukimmig2010)
#' 
#' # create a corpus from texts and assign meta-data and document variables
#' summary(corpus(data_char_ukimmig2010, 
#'                docvars = data.frame(party = names(data_char_ukimmig2010))), 5) 
#'
#' corpus(texts(data_corpus_irishbudget2010))
#' 
#' # import a tm VCorpus
#' if (requireNamespace("tm", quietly = TRUE)) {
#'     data(crude, package = "tm")    # load in a tm example VCorpus
#'     mytmCorpus <- corpus(crude)
#'     summary(mytmCorpus, showmeta=TRUE)
#'     
#'     data(acq, package = "tm")
#'     summary(corpus(acq), 5, showmeta=TRUE)
#'     
#'     tmCorp <- tm::VCorpus(tm::VectorSource(data_char_ukimmig2010))
#'     quantCorp <- corpus(tmCorp)
#'     summary(quantCorp)
#' }
#' 
#' # construct a corpus from a data.frame
#' mydf <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
#'                   some_ints = 1L:6L,
#'                   some_text = paste0("This is text number ", 1:6, "."),
#'                   stringsAsFactors = FALSE,
#'                   row.names = paste0("fromDf_", 1:6))
#' mydf
#' summary(corpus(mydf, text_field = "some_text", 
#'                metacorpus = list(source = "From a data.frame called mydf.")))
#' 
#' # construct a corpus from a kwic object
#' mykwic <- kwic(data_corpus_inaugural, "southern")
#' summary(corpus(mykwic))
corpus <- function(x, ...) {
    UseMethod("corpus")
}

#' @rdname corpus
#' @export
corpus.corpus <- function(x, docnames = quanteda::docnames(x), docvars = quanteda::docvars(x), metacorpus = quanteda::metacorpus(x), compress = FALSE, ...) {
    if (!compress) {
        if (!missing(docnames)) docnames(x) <- docnames
        if (!missing(docvars)) docnames(x) <- docvars
        if (!missing(metacorpus)) metacorpus(x) <- metacorpus
        x
    } else {
        corpus(texts(x), docnames, docvars, metacorpus, compress = compress)
    }
}

#' @rdname corpus
#' @export
corpus.character <- function(x, docnames = NULL, docvars = NULL, metacorpus = NULL, compress = FALSE, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    x_names <- names(x)
    
    # convert the dreaded "curly quotes" to ASCII equivalents
    x <- stringi::stri_replace_all_fixed(x, 
                                         c("\u201C", "\u201D", "\u201F",
                                           "\u2018", "\u201B", "\u2019"),                                     
                                         c("\"", "\"", "\"", 
                                           "\'", "\'", "\'"), vectorize_all = FALSE)
    
    # replace all hyphens with simple hyphen
    x <- stringi::stri_replace_all_regex(x, "\\p{Pd}", "-")
    
    # normalize EOL
    x <- stringi::stri_replace_all_fixed(x, "\r\n", "\n") # Windows
    x <- stringi::stri_replace_all_fixed(x, "\r", "\n") # Old Macintosh
    
    # name the texts vector
    if (!is.null(docnames)) {
        stopifnot(length(docnames) == length(x))
        names(x) <- docnames
    } else if (is.null(x_names)) {
        names(x) <- paste(quanteda_options("base_docname"), seq_along(x), sep="")
    } else if (is.null(names(x))) {
        # if they previously existed, but got obliterated by a stringi function
        names(x) <- x_names
    }

    # ensure that docnames are unique
    if (any(duplicated(names(x))))
        names(x) <- make.unique(names(x))

    # create document-meta-data
    if (is.null(metacorpus$source)) {
        metacorpus$source <- paste(getwd(), "/* ", "on ",  Sys.info()["machine"], " by ", Sys.info()["user"], sep="")
    }
    if (is.null(metacorpus$created)) {
        metacorpus$created <- date()
    }

    # create the documents data frame starting with the texts, using an empty field
    # this saves space if it needs to be separated later
    documents <- data.frame(texts = rep(NA, length(x)), 
                            row.names = names(x),
                            check.rows = TRUE, stringsAsFactors = FALSE)

    # user-supplied document-level variables (one kind of meta-data)
    if (!is.null(docvars)) {
        if (nrow(docvars) > 0) {
            stopifnot(nrow(docvars)==length(x))
            documents <- cbind(documents, docvars)
        } 
    }
    
    # initialize results corpus
    tempCorpus <- list()
    
    ## compress and separate texts if compress == TRUE
    # paste delimiters into object to be compressed
    if (compress) {
        x[1 : (length(x)-1)] <- paste0(x[1 : (length(x)-1)], quanteda_document_delimiter)
        # compress texts
        texts <- memCompress(x, 'gzip')
        # remove texts from documents
        documents$texts <- NULL
        tempCorpus <- c(tempCorpus, list(texts = memCompress(x, "gzip")))
    } else {
        # otherwise replace NA placeholder with the actual text
        documents$texts <- x
    }

    # build and return the corpus object
    tempCorpus <- c(tempCorpus, list(documents = documents, 
                                     metadata = metacorpus, 
                                     settings = settings(),
                                     tokens = NULL))
                    
    ## add some elements if compress
    if (compress) {
        tempCorpus$docnames <- names(x)
        # compute the compression %
        tempCorpus$compression_rate <- utils::object.size(tempCorpus$texts) / utils::object.size(unname(x)) * 100
    }
    
    class(tempCorpus) <- c("corpus", class(tempCorpus))
    if (compress) {
        class(tempCorpus) <- c("corpuszip", class(tempCorpus))
    }
    return(tempCorpus)
}

#' @rdname corpus
#' @param docid_field column index of a document
#'   identifier; defaults to \code{doc_id} but if this is not found, will use
#'   the row.names of the data.frame if these are assigned
#' @keywords corpus
#' @method corpus data.frame
#' @export
corpus.data.frame <- function(x, docid_field = "doc_id", text_field = "text", metacorpus = NULL, compress = FALSE, ...) {
    
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    docnames <- docid_field
    
    x <- as.data.frame(x)
        
    args <- list(...)
    if ("docvars" %in% names(args))
        stop("docvars are assigned automatically for data.frames")
    
    if (is.character(text_field)) {
        text_fieldi <- which(names(x) %in% text_field)
        if (length(text_fieldi)==0)
            stop("column name ", text_field, " not found")
        text_field <- text_fieldi
    } else if (is.numeric(text_field)) {
        text_fieldi <- text_field
    } else {
        stop("text_field must be a character (variable name) or numeric index")
    }

    docnamesi <- integer()
    if (is.numeric(docnames)) {
        docnames <- names(docnames)[docnames]
    }
    
    if (length(docnames) > 1) 
        stop("docid_field must refer to a single column")
    
    if (docnames %in% names(x)) {
        docnamesi <- which(names(x) == docnames)
        docnames <- x[[docnames]]
    } else if (!identical(row.names(x), as.character(seq_len(nrow(x))))) {
        docnames <- row.names(x)
    } else {
        docnames <- NULL
    }
    
    if (length(text_fieldi) != 1)
        stop("only one text_field may be specified")

    if (text_fieldi > ncol(x) | text_fieldi <= 0 | (text_fieldi - as.integer(text_fieldi)))
        stop("text_field index refers to an invalid column")
    
    if (!is.character(x[, text_fieldi]))
        stop("text_field must refer to a character mode column")
    
    corpus(x[, text_fieldi], 
           docvars = x[, -c(text_fieldi, docnamesi), drop = FALSE],
           docnames = docnames, 
           metacorpus = metacorpus, compress = compress)
}


#' @rdname corpus
#' @keywords corpus
#' @export
corpus.kwic <- function(x, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    class(x) <- "data.frame"
    
    # convert docnames to a factor, as in original kwic
    x$docname <- factor(x$docname)
    
    result <- corpus(x, text_field = "pre")
    result[["post"]] <- NULL
    result[["context"]] <- "pre"
    docnames(result) <- paste0(docnames(result), ".pre")

    tempCorp <- corpus(x, text_field = "post")
    tempCorp[["pre"]] <- NULL
    tempCorp[["context"]] <- "post"
    docnames(tempCorp) <- paste0(docnames(tempCorp), ".post")
    
    result <- result + tempCorp
    metacorpus(result, "source") <- paste0("Corpus created from kwic(x, keywords = \"", 
                                           paste(attr(x, "keywords"), collapse = ", "),
                                           "\")")
    result
}

#' @rdname corpus
#' @keywords corpus
#' @importFrom data.table rbindlist data.table
#' @importFrom lubridate is.POSIXlt
#' @export
corpus.Corpus <- function(x, metacorpus = NULL, compress = FALSE, ...) {
    
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    # special handling for VCorpus meta-data
    if (inherits(x, what = "VCorpus")) {
        # remove the classes that mess parsing this list
        x <- unclass(x)
        x <- lapply(x, unclass)
        x$content <- lapply(x$content, unclass)
        
        # texts and associated docvars
        if (is.data.frame(x$content[[1]][["content"]])) {
            df <- as.data.frame(data.table::rbindlist(lapply(x$content, "[[", "content"), fill = TRUE))
            doc_lengths <- sapply(lapply(x$content, "[[", "content"), nrow)
            rownames(df) <- make_unique_tm_names(names_tmCorpus(x), doc_lengths)
        } else {
            texts <- sapply(x$content, "[[", "content")
            # paste together texts if they appear to be vectors
            if (any(lengths(texts) > 1))
                texts <- vapply(texts, paste, character(1), collapse = " ")
            doc_lengths <- 1
            df <- data.frame(text = texts, stringsAsFactors = FALSE, row.names = names_tmCorpus(x))
        }
        
        # document-level metadata
        metad <- unclass(lapply(x$content, "[[", "meta"))
        # flatten any elements that are themselves lists, into pasted vectors
        metad <- flatten_lists(metad)
        # get rid of any empty fields
        metad <- lapply(metad, function(y) y[lengths(y) > 0])
        metad <- data.table::rbindlist(metad, fill = TRUE)
        # add metad to df, where meta is repeated as appropriate for content
        df <- cbind(df, metad[rep(seq_len(nrow(metad)), times = doc_lengths), ])
        
    } else if (inherits(x, what = "SimpleCorpus")) {
        df <- data.frame(text = as.character(x$content), stringsAsFactors = FALSE,
                         row.names = names(x$content))
        if (length(x$dmeta)) df <- cbind(df, x$dmeta)
    } else {
        stop("Cannot construct a corpus from this tm ", class(x)[1], " object")
    }
    
    # corpus-level meta-data
    if (is.null(metacorpus)) metacorpus <- x$meta
    metacorpus <- c(metacorpus, 
                    list(source = paste("Converted from tm Corpus \'", as.character(match.call())[2], "\'", sep="")))
    
    corpus(df, metacorpus = metacorpus, compress = compress)
}

setOldClass("corpus")
