initLibraries <- function() {
    library(dplyr)
    library(tokenizers)
    library(quanteda)
    library(readtext)
    library(futile.logger)
    flog.threshold(TRACE)
}

#predict using SBO approach
#return orginial text appended with predicted word
predictSBONLP <- function(ngramSBOTables, text) {
    nextWord <- primPredictSBONLP(ngramSBOTables, text)
    
    #return orginial text appended with next word
    trimws(paste(trimws(text), nextWord,sep = " "))
}

#predict using SBO approach
#return predicted word
primPredictSBONLP <- function(ngramSBOTables, text) {
    
    #ToDo
    #idee wäre hier mit dem tokenizer package den Input zu zerstückeln...
    flog.trace("primPredictSBONLP - text to predict %s", text)
    words <- c()
    if (nchar(trimws(text)) > 0) {
        
        words <- removeStopwords(text)
        flog.trace("primPredictSBONLP - text after removing stopwords: %s", paste(words, collapse = " "))
        # inputSubSentences <- unlist(text %>% tokenize_sentences(simplify = TRUE) %>% tokenize_regex(", "))
        # subSentence <- last(inputSubSentences)
        # #ToDo: Achtung ein neuer Satz muss mit Grossbuchstaben oder Zahl beginnen!
        # print(subSentence)
        # words <- subSentence %>% tokenize_words(simplify = TRUE)
    }
    
    #Algorithmus:
    #0-gram --> 1-grams mit höchster P (mle)
    #1-gram --> 2-grams mit hächster P (mle) für 1-gram
    #2-gram --> 3-grams mit hächster P (mle) für 2-gram
    #3-gram --> 4-grams mit hächster P (mle) für 3-gram
    
    #words <- unlist(strsplit(subSentence, " "))
    
    ngramWord <- words[length(words)]
    ngram_1 <- paste(words[-length(words)], collapse = " ")
    nextWord <- NA
    numWords <- length(words)
    
    # a = 0.4 (heuristik), k = 0
    # P(C|AB) = count(ABC)/count(AB) (wenn count(ABC)  > k), a * P(B|A) sonst
    # P(B|A) = count(AB)/count(A) wenn count(AB) > 0 und sonst a * P(A)
    # P(A) = count(A)/V (Anzahl unigrams)
    #Algorithmus bei 4-grams beginnen und dann runter iterieren
    #4-gram bedingt ein geschriebenes 3-gram
    #Assumption: k = 0!
    if (numWords >= 3) {
        last3Words <- paste(words[(numWords-2): numWords], collapse = " ")
        flog.trace("4-gram: %s", last3Words)
        
        rows <- lookupNGram(ngramSBOTables[[4]], last3Words)
        if (nrow(rows) > 0) {
            nextWord <- rows[1,2]
            flog.trace("4-gram found: %s", nextWord)
        }
    }
    if (is.na(nextWord) && (numWords >= 2)) {
        last2Words <- paste(words[(numWords-1): numWords], collapse = " ")
        flog.trace("3-gram: %s", last2Words)
        
        rows <- lookupNGram(ngramSBOTables[[3]], last2Words)
        if(nrow(rows) > 0) {
            nextWord <- rows[1,2]
            flog.trace("3-gram found: %s", nextWord)
        }
    }
    if (is.na(nextWord) && (numWords >= 1)) {
        lastWord <- words[numWords]
        flog.trace("2-gram: %s", lastWord)
        
        rows <- lookupNGram(ngramSBOTables[[2]], lastWord)
        if(nrow(rows) > 0) {
            nextWord <- rows[1,2]
            flog.trace("2-gram found: %s", nextWord)
        } 
    } 
    if (is.na(nextWord)) {
        print("1-gram")
        flog.trace("1-gram")
        rows <- ngramSBOTables[[1]]
        nextWord <- rows[1,1]
        flog.trace("1-gram found: %s", nextWord)
    }
    
    nextWord
}

lookupNGram <- function(sboTable, ngram) {
    rows <- sboTable %>% filter(ngram_1 == ngram)
    #TODO: hier könnte ich evtl. nur die erste row zurückgeben...
    rows
}

lookupDfmForMatch <- function(dfms, root, words, n) {
    topfeatures(dfm_match(dfms[[c]], c(paste(root, words[1]), paste(root, words[2]), paste(root, words[3]), paste(root, words[4]))))
}

