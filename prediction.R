
#predict using ngram tables approach
#return orginial text appended with predicted word
predictNextWord <- function(ngramKBOTables, text) {
    nextWord <- primPredictNextWord(ngramKBOTables, text)
    
    #return orginial text appended with next word
    trimws(paste(trimws(text), nextWord,sep = " "))
}

#predict using ngram tables approach
#return predicted word
primPredictNextWord <- function(ngramKBOTables, text) {
    
    #ToDo
    #idee wäre hier mit dem tokenizer package den Input zu zerstückeln...
    flog.trace("primPredictSBONLP - text to predict %s", text)
    words <- c()
    if (!is.na(text) && (nchar(trimws(text)) > 0)) {
        
        words <- removeStopwords(text)
        flog.trace("primPredictSBONLP - text after removing stopwords: %s", paste(words, collapse = " "))
    }
    
    #Algorithmus:
    #0-gram --> 1-grams mit höchster P (mle)
    #1-gram --> 2-grams mit hächster P (mle) für 1-gram
    #2-gram --> 3-grams mit hächster P (mle) für 2-gram
    #3-gram --> 4-grams mit hächster P (mle) für 3-gram
    
    nextWord <- NA
    numWords <- length(words)

    if (numWords >= 3) {
        last3Words <- paste(words[(numWords-2): numWords], collapse = " ")
        flog.trace("4-gram: %s", last3Words)
        
        rows <- lookupNGram(ngramKBOTables[[4]], last3Words)
        if (nrow(rows) > 0) {
            nextWord <- rows[1,2]
            flog.trace("4-gram found: %s", nextWord)
        }
    }
    if (is.na(nextWord) && (numWords >= 2)) {
        last2Words <- paste(words[(numWords-1): numWords], collapse = " ")
        flog.trace("3-gram: %s", last2Words)
        
        rows <- lookupNGram(ngramKBOTables[[3]], last2Words)
        if(nrow(rows) > 0) {
            nextWord <- rows[1,2]
            flog.trace("3-gram found: %s", nextWord)
        }
    }
    if (is.na(nextWord) && (numWords >= 1)) {
        lastWord <- words[numWords]
        flog.trace("2-gram: %s", lastWord)
        
        rows <- lookupNGram(ngramKBOTables[[2]], lastWord)
        if(nrow(rows) > 0) {
            nextWord <- rows[1,2]
            flog.trace("2-gram found: %s", nextWord)
        } 
    } 
    if (is.na(nextWord)) {
        print("1-gram")
        flog.trace("1-gram")
        rows <- ngramKBOTables[[1]]
        nextWord <- rows[1,1]
        flog.trace("1-gram found: %s", nextWord)
    }
    
    nextWord
}

#predict using ngram tables
#return a data frame with predictions (ngram_1, nextword, p)
# a = 0.4 (heuristik), k = 0
# P(C|AB) = count(ABC)/count(AB) (wenn count(ABC)  > k), a * P(B|A) sonst
# P(B|A) = count(AB)/count(A) wenn count(AB) > 0 und sonst a * P(A)
# P(A) = count(A)/V (Anzahl unigrams)
#Algorithmus bei 4-grams beginnen und dann runter iterieren
#4-gram bedingt ein geschriebenes 3-gram
#Assumption: k = 0!
getNextWordPredictionTable <- function(ngramKBOTables, text, k=0, alpha=0.4, useKBO = TRUE) {

    #ToDo
    #idee wäre hier mit dem tokenizer package den Input zu zerstückeln...
    boMethod <- if (useKBO) "KBO" else "SBO"
    flog.trace("getNextWordPredictionTable with method=%s, k=%i and alpha=%f - text to predict %s", boMethod, k, alpha,text)
    words <- c()
    if (!is.na(text) && (nchar(trimws(text)) > 0)) {
        
        words <- removeStopwords(text)
        flog.trace("getNextWordPredictionTable - text after removing stopwords: %s", paste(words, collapse = " "))
    }
    
    #Algorithmus:
    #0-gram --> 1-grams mit höchster P (mle)
    #1-gram --> 2-grams mit hächster P (mle) für 1-gram
    #2-gram --> 3-grams mit hächster P (mle) für 2-gram
    #3-gram --> 4-grams mit hächster P (mle) für 3-gram
    ngramWord <- words[length(words)]
    numWords <- length(words)
    
    predDF <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("ngram_1", "nextWord", "mle", "count"))
    
    #4grams
    if (numWords >= 3) {
        last3Words <- paste(words[(numWords-2): numWords], collapse = " ")
        flog.trace("4-gram: %s", last3Words)
        rows <- lookupNGram(ngramKBOTables[[4]], last3Words)
        if (!useKBO) {
            # use SBO
            rows <- rows %>% filter(count > k)
            predDF <- rbind(predDF, rows4)
        } else {
            #apply GoodTouring Estimator
            gttable <- getGoodTouringTable(k, ngramKBOTables[[4]])
            rowsGreaterK <- rows %>% filter(count>k)
            rowsNotGreaterK <- rows %>% filter(count<=k) %>% mutate(mle = mle * gttable[count])
            predDF <- rbind(predDF, rowsGreaterK, rowsNotGreaterK)
        }
    }
    
    #3grams
    if (numWords >= 2) {
        last2Words <- paste(words[(numWords-1): numWords], collapse = " ")
        flog.trace("3-gram: %s", last2Words)
        #apply KB alpha value
        rows <- lookupNGram(ngramKBOTables[[3]], last2Words) %>% mutate(mle=mle*alpha)
        if (!useKBO) {
            # use SBO
            rows <- rows %>% filter(count > k)
            predDF <- rbind(predDF, rows3)
        } else {
            #apply GoodTouring Estimator
            gttable <- getGoodTouringTable(k, ngramKBOTables[[3]])
            rowsGreaterK <- rows %>% filter(count>k)
            rowsNotGreaterK <- rows %>% filter(count<=k) %>% mutate(mle = mle * gttable[count])
            predDF <- rbind(predDF, rowsGreaterK, rowsNotGreaterK)
        }
    }
    
    #2grams
    if (numWords >= 1) {
        lastWord <- words[numWords]
        flog.trace("2-gram: %s", lastWord)
        #apply KB alpha value
        rows <- lookupNGram(ngramKBOTables[[2]], lastWord) %>% mutate(mle=mle*alpha^2)
        if (!useKBO) {
            # use SBO
            rows <- rows %>% filter(count > k)
            predDF <- rbind(predDF, rows2)
        } else {
            #apply GoodTouring Estimator
            gttable <- getGoodTouringTable(k, ngramKBOTables[[2]])
            rowsGreaterK <- rows %>% filter(count>k)
            rowsNotGreaterK <- rows %>% filter(count<=k) %>% mutate(mle = mle * gttable[count])
            predDF <- rbind(predDF, rowsGreaterK, rowsNotGreaterK)
        }
    } 
    predDF <- predDF %>% arrange(desc(mle)) #%>% top_n(10, mle)
    flog.trace("getNextWordPredictionTable - done")
    predDF
}


#####
# helper functions
#####

lookupNGram <- function(kboTable, ngram) {
    rows <- kboTable %>% filter(ngram_1 == ngram) %>% arrange(desc(mle))
    rows
}

lookupDfmForMatch <- function(dfms, root, words, n) {
    topfeatures(dfm_match(dfms[[c]], c(paste(root, words[1]), paste(root, words[2]), paste(root, words[3]), paste(root, words[4]))))
}

