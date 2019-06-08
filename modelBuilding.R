initLibraries <- function() {
    library(tm)
    library(RWeka)
    library(dplyr)
    library(tokenizers)
    library(quanteda)
    library(readtext)
    library(futile.logger)
    library(stringr)
    flog.threshold(TRACE)
}

setupTestNLP <- function() {
    initLibraries()
    tokens <- prepareTestCorpus()
    persistCorpus(tokens, "test1")
#    Rprof("rprof.out", append=FALSE)
    primSetupNLP(tokens)
#    Rprof(NULL)
}

setupSampleNLP <- function(corpName="sample") {
    initLibraries()
    flog.info("setupSampleNLP: start")
    tokens <- prepareSampleCorpus()
    persistCorpus(tokens, corpName)
    flog.trace("setupSampleNLP: corpus prepared")
    Rprof("rprof.out", append=FALSE)
    sbo <- primSetupNLP(tokens, 5)
    Rprof(NULL)
    flog.info("setupSampleNLP: end")
    sbo
}

setupFullNLP <- function(minFreq=10, corpName="full") {
    initLibraries()
    flog.info("setupFullNLP: start - minFreq = %i", minFreq)
    tokens <- prepareFullCorpus()
    persistCorpus(tokens, corpName)
    flog.trace("setupFullNLP: corpus prepared")
    sbo <- primSetupNLP(tokens, minFreq)
    flog.info("setupFullNLP: end")
    sbo
}

primSetupNLP <- function(tokens, minFreq=1) {
    #bei getTermDocMatrix lohnt sich die Parallelisierung nicht, da immer auf cm zugegriffen wird!
    flog.trace("primSetupNLP: getDocFeatureMatrix 1")
    dfm1 <- getDocFeatureMatrix(tokens, 1)
    flog.trace("primSetupNLP: getDocFeatureMatrix 2")
    dfm2 <- getDocFeatureMatrix(tokens, 2)
    flog.trace("primSetupNLP: getDocFeatureMatrix 3")
    dfm3 <- getDocFeatureMatrix(tokens, 3)
    flog.trace("primSetupNLP: getDocFeatureMatrix 4")
    dfm4 <- getDocFeatureMatrix(tokens, 4)
    # flog.trace("4grams: before sw removal: num features %i", length(featnames(dfm4)))
    # dfm4 <- removeNGramEndingWithStopwords(dfm4)
    # flog.trace("4grams: after sw removal: num features %i", length(featnames(dfm4)))
    
    saveRDS(list(dfm1, dfm2, dfm3, dfm4),  file="data/temp/dfms.rds")
    
    flog.trace("primSetupNLP: getSBOTables")
    getSBOTables(dfm1, dfm2, dfm3, dfm4, minFreq)
    
    # flog.trace("primSetupNLP: getSBOTablesParallel")
    # getSBOTablesParallel(dfm1, dfm2, dfm3, dfm4, minFreq)
}

saveKBOModel <- function(kboTables, modelName="model") {
    saveRDS(kboTables, file=paste0("data/model/", modelName, "KBO.rds"))
}

readKBOModel <- function(modelName="model") {
    readRDS(file=paste0("data/model/", modelName, "KBO.rds"))
}

persistSBOModel <- function(sboTables, modelName="model") {
    write.csv(sboTables[[4]], file=paste0("data/model/", modelName, "SBO4.csv"), row.names = FALSE)
    write.csv(sboTables[[3]], file=paste0("data/model/", modelName, "SBO3.csv"), row.names = FALSE)
    write.csv(sboTables[[2]], file=paste0("data/model/", modelName, "SBO2.csv"), row.names = FALSE)
    write.csv(sboTables[[1]], file=paste0("data/model/", modelName, "SBO1.csv"), row.names = FALSE)
}

loadSBOModel <- function(modelName="model") {
    library(tokenizers)
    sbo4 <- read.csv(file=paste0("data/model/", modelName, "SBO4.csv"), as.is = TRUE)
    sbo3 <- read.csv(file=paste0("data/model/", modelName, "SBO3.csv"), as.is = TRUE)
    sbo2 <- read.csv(file=paste0("data/model/", modelName, "SBO2.csv"), as.is = TRUE)
    sbo1 <- read.csv(file=paste0("data/model/", modelName, "SBO1.csv"), as.is = TRUE)
    list(sbo1, sbo2, sbo3, sbo4)
}


# source: https://gigadom.in/tag/katz-backoff/
# source: https://en.wikipedia.org/wiki/Katz%27s_back-off_model
# https://rpubs.com/mszczepaniak/predictkbo3model


# offene Frage: berechne ich die p für alle möglichen Kombinationen im Voraus?
# sprich ich wende für p(c|ab) alle terms aus 1-ngrams an? ansonsten macht das
# smoothing ja nicht soviel sinn...
# Katz-backoff probabilities P(ngram) = 
#    d * mle für non zero count ngrams 
#    a * p (ngram_1) für zero count ngrams
#
#  d via Good-Turing  d = C* / C, wobei C* estimate für C durch Good-Turing
#
#  a 
#
# ToDo's
# for a ngram eine lookup table mit ngram_1 und smoothed mle für ngram

######
# SBO: Stupid Backoff Modell: https://www.aclweb.org/anthology/D07-1090.pdf
#
# a = 0.4 (heuristik)
# k = 0
# P(C|AB) = count(ABC)/count(AB) (wenn count(ABC)  > k), a * P(B|A) sonst
# P(B|A) = count(AB)/count(A) wenn count(AB) > 0 und sonst a * P(A)
# P(A) = count(A)/V (Anzahl unigrams)
#
# algorithmus bei einem alle ngrams, die mit ngram_1 beginnen P berechnen und 
# dasjenige Wort vorschlagen mit grösstem P
#
# dazu benötige ich für ein ngram eine tabelle mit ngram_1, ngram Wort (ohne ngram_1), berechnetes P
######

getSBOTables <-  function(dfm1, dfm2, dfm3, dfm4, minFreq=5) {
    tcdfList <- buildTermCountDF(dfm1, dfm2, dfm3, dfm4, minFreq)
    # tsboList <- buildSBOTables(tcdfList[[1]], tcdfList[[2]], tcdfList[[3]], tcdfList[[4]])
    tsboList <- buildSmoothedBOTables(tcdfList[[1]], tcdfList[[2]], tcdfList[[3]], tcdfList[[4]])
    tsboList
}
    
buildTermCountDF <- function(dfm1, dfm2, dfm3, dfm4, minFreq=5) {
    flog.trace("getSBOTables: getTermCountDF 4")
    df_4 <- getTermCountDF(dfm4, minFreq)
    flog.trace("getSBOTables: getTermCountDF 3")
    df_3 <- getTermCountDF(dfm3, minFreq)
    flog.trace("getSBOTables: getTermCountDF 2")
    df_2 <- getTermCountDF(dfm2, minFreq)
    flog.trace("getSBOTables: getTermCountDF 1")
    df_1 <- getTermCountDF(dfm1, minFreq)
    
    tcdfList <- list(df_1, df_2, df_3, df_4)
    saveRDS(tcdfList,  file="data/temp/tcdf.rds")
    tcdfList
}

#helper function
restartGetSBOTables <- function(file="dfms.rds", minFreq=5) {
    dfmsList <- readRDS(file=paste0("data/temp/", file))
    getSBOTables(dfmsList[[1]], dfmsList[[2]], dfmsList[[3]], dfmsList[[4]], minFreq)
}

#helper function
restartBuildSBOTables <- function(file="tcdf.rds") {
    tcdfList <- readRDS(file=paste0("data/temp/", file))
    buildSBOTables(tcdfList[[1]], tcdfList[[2]], tcdfList[[3]], tcdfList[[4]])
}

buildSBOTables <- function(df_1, df_2, df_3, df_4) {
    flog.trace("getSBOTables: primCalculateSBOTables 4")
    sdf_4 <- primCalculateSBOTables(df_4, df_3)
    flog.trace("getSBOTables: primCalculateSBOTables 3")
    sdf_3 <- primCalculateSBOTables(df_3, df_2)
    flog.trace("getSBOTables: primCalculateSBOTables 2")
    sdf_2 <- primCalculateSBOTables(df_2, df_1)
    flog.trace("getSBOTables: primCalculateSBO_1Table 1")
    sdf_1 <- primCalculateSBO_1Table(df_1)
    
    tsbo <- list(sdf_1, sdf_2, sdf_3, sdf_4)
    saveRDS(tsbo,  file="data/temp/tsbo.rds")
    tsbo
}



primCalculateSBOTables <- function(df_n, df_n_1) {
    #evaluate the n of the ngram to setup regex
    n <- length(strsplit(df_n[1,1], " ")[[1]])
    if (n==4) ngram_1Regex <- "^[^ ]+ [^ ]+ [^ ]+"
    else if(n==3) ngram_1Regex <- "^[^ ]+ [^ ]+"
    else if(n==2) ngram_1Regex <- "^[^ ]+"
    df <- df_n %>% mutate(ngram_1 = str_extract(ngram, ngram_1Regex), nextWord = str_extract(ngram, "[^ ]+$")) %>% select(count, ngram_1, nextWord)
    
    distinceNGram_1 <- df %>% distinct(ngram_1)
    df_n_1_f <- df_n_1 %>% filter(ngram %in% distinceNGram_1$ngram_1) %>% rename(count_1=count)
    if(nrow(df_n_1_f) != nrow(distinceNGram_1)) {
        stop("primCalculateSBOTables - issue mit ngrams...")
    }
    df <- df %>% left_join(df_n_1_f, by=c("ngram_1" = "ngram"))
    df <- df %>% mutate(mle=count/count_1) %>% select(ngram_1, nextWord, mle)

    #filter ngrams die auf ein Stopword enden...
    flog.trace("primCalculateSBOTables - nrows before filtering: %i", nrow(df))
    sw <- stopwords("en")
    df <- df %>% filter(!(nextWord %in% sw))
    flog.trace("primCalculateSBOTables - nrows after filtering stopwords: %i", nrow(df))
    
    #keep for sbo only the ngrams with the highest probability per ngram_1
    #don't filter for the quiz 3 as i need the other probabilities
    # df <- df %>% group_by(ngram_1) %>% filter(mle==max(mle))
    # flog.trace("primCalculateSBOTables - nrows after filtering non max mle: %i", nrow(df))
    
    df
}

#
primCalculateSBO_1Table <- function(df1) {
    V <- as.integer(count(df1))
    df <- df1 %>% mutate(mle=count/V) %>% select(nextWord=ngram, mle) 
    # mle <- numeric(V)
    # nextWord <- character(V)
    # for (i in 1:V) {
    #     if ((i %% 100000) == 0)  {flog.trace("primCalculateSBO_1Table - step %i", i)}
    #     mle[i] <- df1$count[i] / V
    #     nextWord[i] <- df1$ngram[i]
    # }
    # df <- data.frame(nextWord, mle, stringsAsFactors=FALSE)
    
    #filter ngrams die auf ein Stopword enden...
    flog.trace("primCalculateSBO_1Table - nrows before filtering: %i", nrow(df))
    sw <- stopwords("en")
    df <- df %>% filter(!(nextWord %in% sw))
    flog.trace("primCalculateSBO_1Table - nrows after filtering stopwords: %i", nrow(df))
    
    #keep for sbo only tghe ngrams with the highest probability
    #todo: man könnte noch optimieren und nur im das ngram mit dem höchsten MLE sammeln...
    #don't filter for the quiz 3 as i need the other probabilities
    df <- df %>% filter(mle==max(mle))
    # flog.trace("primCalculateSBO_1Table - nrows after filtering non max mle: %i", nrow(df))
    df
}

#helper function
filterStopwordNGrams <- function(sboTable) {
    sw <- stopwords("en")
    df <- sboTable %>% mutate(ngram_1 = as.character(ngram_1), nextWord = as.character(nextWord))
    df <- df %>% filter(!(nextWord %in% sw))
    df
}
filterStopword1Grams <- function(sboTable) {
    sw <- stopwords("en")
    df <- sboTable %>% mutate(nextWord = as.character(nextWord))
    df <- df %>% filter(!(nextWord %in% sw))
    df
}


# just calc the mle for known ngrams, no smoothing...
# momentan nicht genutzt
# calcMLE <- function(ngram, df_x, df_x_1) {
#     #print(ngram)
#     # helper calculations..
#     words <- unlist(strsplit(ngram, " "))
#     nextWord <- words[length(words)]
#     ngram_1 <- paste(words[-length(words)], collapse = " ")
#     c_x <- df_x$count[df_x$ngram==ngram]
#     c_x_1 <- df_x_1$count[df_x_1$ngram==ngram_1]
#     mle <- c_x / c_x_1
#     data.frame(ngram_1, nextWord, mle)
# }

# just calc the mle for known 1-grams, no smoothing...
# momentan nicht genutzt...
# calcMLE_1 <- function(nextWord, df_1, V) {
#     #print(ngram)
#     c_x <- df_1$count[df_1$ngram==nextWord]
#     mle <- c_x / V
#     data.frame(nextWord, mle)
# }


#
# ----------------- La Place Smoothing ---------
#

### approach
## 1. Use Markov chains to calculate the Maximum Likelihood estimate 
##    P(C|AB) = count(ABC)/count(AB)
##
## 2. For previous terms whose count is 0, perform Laplace Add - 1 smoothing 
##    Padd-1(C|AB) = (count(ABC) + 1)/(count(AB) + V)
##
## V is the number of unique n-1 grams you have in the corpus
## MLE = (Count(n grams) + 1)/ (Count(n-1 grams) + V)
###
buildSmoothedBOTables <- function(df_1, df_2, df_3, df_4) {
    useAdd1Smoothing <- FALSE
    flog.trace("buildSmoothedBOTables: primCalculateSmoothedBOTables 4")
    sdf_4 <- primCalculateSmoothedBOTables(df_4, df_3, useAdd1Smoothing)
    flog.trace("buildSmoothedBOTables: primCalculateSmoothedBOTables 3")
    sdf_3 <- primCalculateSmoothedBOTables(df_3, df_2, useAdd1Smoothing)
    flog.trace("buildSmoothedBOTables: primCalculateSmoothedBOTables 2")
    sdf_2 <- primCalculateSmoothedBOTables(df_2, df_1, useAdd1Smoothing)
    flog.trace("buildSmoothedBOTables: primCalculateSmoothedBO_1Table 1")
    sdf_1 <- primCalculateSmoothedBO_1Table(df_1)
    
    tsbo <- list(sdf_1, sdf_2, sdf_3, sdf_4)
    saveRDS(tsbo,  file="data/temp/tsbo.rds")
    tsbo
}

#helper function
restartBuildSmoothedBOTables <- function(file="tcdf.rds") {
    tcdfList <- readRDS(file=paste0("data/temp/", file))
    buildSmoothedBOTables(tcdfList[[1]], tcdfList[[2]], tcdfList[[3]], tcdfList[[4]])
}

#helper function
restartBuildTermCountDF <- function(file="dfms.rds", minFreq=5) {
    dfmsList <- readRDS(file=paste0("data/temp/", file))
    buildTermCountDF(dfmsList[[1]], dfmsList[[2]], dfmsList[[3]], dfmsList[[4]], minFreq)
}


primCalculateSmoothedBOTables <- function(df_n, df_n_1, useAdd1Smoothing) {
    #evaluate the n of the ngram to setup regex
    flog.trace("primCalculateSmoothedBOTables")
    n <- length(strsplit(df_n[1,1], " ")[[1]])
    if (n==4) ngram_1Regex <- "^[^ ]+ [^ ]+ [^ ]+"
    else if(n==3) ngram_1Regex <- "^[^ ]+ [^ ]+"
    else if(n==2) ngram_1Regex <- "^[^ ]+"
    df <- df_n %>% mutate(ngram_1 = str_extract(ngram, ngram_1Regex), nextWord = str_extract(ngram, "[^ ]+$")) %>% select(count, ngram_1, nextWord)
    
    distinceNGram_1 <- df %>% distinct(ngram_1)
    df_n_1_f <- df_n_1 %>% filter(ngram %in% distinceNGram_1$ngram_1) %>% rename(count_1=count)
    if(nrow(df_n_1_f) != nrow(distinceNGram_1)) {
        stop("primCalculateSmoothedBO_1Table - issue mit ngrams...")
    }
    df <- df %>% left_join(df_n_1_f, by=c("ngram_1" = "ngram"))
    
    # apply add+1 smoothing, divide by V_1 (the number of n-1 grams)
    if (useAdd1Smoothing) {
        V_1 <- nrow(df_n_1)
        df <- df %>% mutate(mle=(count + 1)/(count_1 + V_1)) 
    } else {
        df <- df %>% mutate(mle=count/count_1)
    }
    
    # #apply katzbach backoff
    # gtTable <- getGoodTouringTable(k, df)
    
    #select relevant columns
    df <- df %>% select(ngram_1, nextWord, mle, count)
    
    #filter ngrams die auf ein Stopword enden...
    flog.trace("primCalculateSmoothedBOTables - nrows before filtering: %i", nrow(df))
    sw <- stopwords("en")
    df <- df %>% filter(!(nextWord %in% sw))
    flog.trace("primCalculateSmoothedBOTables - nrows after filtering stopwords: %i", nrow(df))
    
    #keep for sbo only the ngrams with the highest probability per ngram_1
    #don't filter for the quiz 3 as i need the other probabilities
    # df <- df %>% group_by(ngram_1) %>% filter(mle==max(mle))
    # flog.trace("primCalculateSBOTables - nrows after filtering non max mle: %i", nrow(df))
    
    df
}

# as for SBO...
primCalculateSmoothedBO_1Table <- function(df1) {
    V <- as.integer(count(df1))
    df <- df1 %>% mutate(mle=count/V) %>% select(nextWord=ngram, mle, count) 

    #filter ngrams die auf ein Stopword enden...
    flog.trace("primCalculateSmoothedBO_1Table - nrows before filtering: %i", nrow(df))
    sw <- stopwords("en")
    df <- df %>% filter(!(nextWord %in% sw))
    flog.trace("primCalculateSmoothedBO_1Table - nrows after filtering stopwords: %i", nrow(df))
    
    #keep for sbo only tghe ngrams with the highest probability
    #todo: man könnte noch optimieren und nur im das ngram mit dem höchsten MLE sammeln...
    #don't filter for the quiz 3 as i need the other probabilities
    df <- df %>% filter(mle==max(mle))
    # flog.trace("primCalculateSmoothedBO_1Table - nrows after filtering non max mle: %i", nrow(df))
    df
}

#based on slide 71 in http://gki.informatik.uni-freiburg.de/teaching/ws0405/advanced/AAI1.ppt
calculateGoodTouringEstimator <- function(k=5, c, kboTable) {
    
    # N1 is the number of ngrams that occur 1 time, N2 ...2 times etc.
    if (c > k) {cStar <- c}
    else if (c==0) {cStar <- 0}   #just take 0
    else {
        Nk1 <- nrow(kboTable %>% filter(count==(k+1)))  #Nk
        N1 <- nrow(kboTable %>% filter(count==1))  #N1
        Nc1 <- nrow(kboTable %>% filter(count==(c+1)))  #Nc+1
        Nc <- nrow(kboTable %>% filter(count==c))  #Nc
        cStar <- ((c + 1)*Nc1/Nc - c*(k+1)*Nk1/N1) / (1 - (k+1) * Nk1/N1)
    }
    cStar <- cStar / c
    flog.trace("Good Touring Estimator for c=%i and k=%i :%f", c, k, cStar)
    cStar
}
    
getGoodTouringTable <- function(k=5, kboTable) {
    gttable <- numeric(k)
    if (k > 0) {
        Nk1 <- nrow(kboTable %>% filter(count==(k+1)))  #Nk
        N1 <- nrow(kboTable %>% filter(count==1))  #N1
        for (c in 1:k) {
            Nc1 <- nrow(kboTable %>% filter(count==(c+1)))  #Nc+1
            Nc <- nrow(kboTable %>% filter(count==c))  #Nc

            cstar <- ((c + 1)*Nc1/Nc - c*(k+1)*Nk1/N1) / (1 - (k+1) * Nk1/N1)
            gttable[c] <- cstar/c
            flog.trace("Good Touring Table entry for c=%i and k=%i :%f", c, k, gttable[c])
        }
    }
    gttable
}




