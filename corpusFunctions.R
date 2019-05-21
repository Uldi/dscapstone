loadCorpusInMem <- function(dir) {
    textCorpus <- VCorpus(DirSource(dir),
                    readerControl = list(reader = readPlain,
                                         language = "en_US",
                                         load = TRUE))
    textCorpus
}

loadTestCorpus <- function() {
    loadCorpusInMem("data/test")
}

loadSampleCorpus <- function() {
    loadCorpusInMem("data/samples")
}

loadFullCorpus <- function() {
    loadCorpusInMem("data/final/en_US")
}

filterProfanityWords <- function(corpus) {
    tm_map(corpus, FUN = removeWords, getProfanityWords())
}

maxChars <- function(textDocument) {
    l <- lapply(textDocument$content, nchar)
    i <- which.max(l)
    l[i]
}

allStepsWithSampleData <- function() {
    c <- loadSampleCorpus()
#    c <- tm_map(c, FUN = stemDocument)
    c <- filterProfanityWords(c)
    c <- tm_map(c, FUN = stripWhitespace)
    c <- tm_map(c, FUN = content_transformer(tolower))
    c <- tm_map(c, FUN = removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE, ucp=TRUE)
    c <- tm_map(c, FUN = removeNumbers)
    cm <- VCorpus(VectorSource(c(c[[1]]$content, c[[2]]$content, c[[3]]$content)))
    cm
    
}

allStepsWithFullData <- function() {
    c <- loadFullCorpus()
    #    c <- tm_map(c, FUN = stemDocument)
    c <- filterProfanityWords(c)
    c <- tm_map(c, FUN = stripWhitespace)
    c <- tm_map(c, FUN = content_transformer(tolower))
    c <- tm_map(c, FUN = removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE, ucp=TRUE)
    c <- tm_map(c, FUN = removeNumbers)
    cm <- VCorpus(VectorSource(c(c[[1]]$content, c[[2]]$content, c[[3]]$content)))
    cm
    
}

allStepsWithTestData <- function() {
    c <- loadTestCorpus()
#    c <- tm_map(c, FUN = stemDocument)
    c <- filterProfanityWords(c)
    c <- tm_map(c, FUN = stripWhitespace)
    c <- tm_map(c, FUN = content_transformer(tolower))
    c <- tm_map(c, FUN = removePunctuation, preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE, ucp=TRUE)
    c <- tm_map(c, FUN = removeNumbers)
    cm <- VCorpus(VectorSource(c(c[[1]]$content)))
    cm
    
}

getTermCountDF <- function(tdm, minFreq=10) {
    df <- (tdm[findFreqTerms(tdm,minFreq),] %>%
                   as.matrix() %>%
                   rowSums() %>% sort(decreasing = TRUE)) %>% as.data.frame()
    df <- tibble::rownames_to_column(df, "ngram")
    colnames(df) <- c("ngram", "count")
    df
}

listMostFrequentTerms <- function(tdm, numTerms=10, minFreq=100, ngram="n") {
    ft <- (tdm[findFreqTerms(tdm,minFreq),] %>%
               as.matrix() %>%
               rowSums() %>% sort(decreasing = TRUE)) [1:numTerms]
    as.matrix(ft)
} 

analyseFrequentTerms <- function(tdm, numTerms=10, minFreq=100, ngram="n") {
    ft <- (tdm[findFreqTerms(tdm,minFreq),] %>%
                as.matrix() %>%
                rowSums() %>% sort(decreasing = TRUE)) [1:numTerms]
    barplot(ft, cex.names = 0.4, main= paste("top", numTerms, ngram, "-grams", sep = " "))
}

plotTopFrequentTerms <- function(tdm, frequency=100, ngram="n") {
    ft <- (tdm[findFreqTerms(tdm,10),] %>%
               as.matrix() %>%
               rowSums() %>% sort(decreasing = TRUE)) [1:100]
    plot(ft, main=paste("top", frequency, "frequent", ngram, "-grams"), ylab="frequency", xlab="terms with decreasing frequency")
}

getTermDocMatrix <- function(corpus, ngram) {
    TermDocumentMatrix(corpus,list(wordLengths=c(1, Inf), tokenize = 
                                   function(x) NGramTokenizer(x, Weka_control(min=ngram, max=ngram, delimiters=" \r\n\t.,;:\"()?!"))))
}

