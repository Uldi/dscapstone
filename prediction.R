
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
getNextWordPredictionTable <- function(ngramKBOTables, text, k=0, alpha=0.4) {

    #ToDo
    #idee wäre hier mit dem tokenizer package den Input zu zerstückeln...
    flog.trace("getNextWordPredictionTable - text to predict %s", text)
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
    
    predDF <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("ngram_1", "nextWord", "mle"))
    
    #4grams
    if (numWords >= 3) {
        last3Words <- paste(words[(numWords-2): numWords], collapse = " ")
        flog.trace("4-gram: %s", last3Words)
        rows4 <- lookupNGram(ngramKBOTables[[4]], last3Words) %>% filer(count > k)
        predDF <- rbind(predDF, rows4)
    }
    #3grams
    if (numWords >= 2) {
        last2Words <- paste(words[(numWords-1): numWords], collapse = " ")
        flog.trace("3-gram: %s", last2Words)
        rows3 <- lookupNGram(ngramKBOTables[[3]], last2Words) %>% filer(count > k) %>% mutate(mle=mle*alpha)
        predDF <- rbind(predDF, rows3)
    }
    #2grams
    if (numWords >= 1) {
        lastWord <- words[numWords]
        flog.trace("2-gram: %s", lastWord)
        rows2 <- lookupNGram(ngramKBOTables[[2]], lastWord) %>% filer(count > k) %>% mutate(mle=mle*alpha^2)
        predDF <- rbind(predDF, rows2)
    } 
    predDF <- predDF %>% arrange(desc(mle)) #%>% top_n(10, mle)
    predDF
}

lookupNGram <- function(kboTable, ngram) {
    rows <- kboTable %>% filter(ngram_1 == ngram) %>% arrange(desc(mle))
    rows
}

lookupDfmForMatch <- function(dfms, root, words, n) {
    topfeatures(dfm_match(dfms[[c]], c(paste(root, words[1]), paste(root, words[2]), paste(root, words[3]), paste(root, words[4]))))
}

