

generateSampleFiles <- function(p=0.01) {
    fileNames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
    for(fileName in fileNames) {
        generateSampleFile(fileName, p)
    }
}

generateSampleFile <- function(fileName, p=0.01) {
    print(fileName)
    set.seed(12345)
    numLines <- 10000
    inFile <- file(paste("data/final/en_US", fileName, sep="/"), "r")
    outFile <- file(paste("data/samples", fileName, sep="/"), "w")
    txtLines <- readLines(inFile, numLines)
    while (length(txtLines)) {
        sample <- rbinom(length(txtLines),1,p)
        writeLines(txtLines[sample==1],outFile)
        txtLines <- readLines(inFile,numLines)
    }
    close(inFile)
    close(outFile)
}

splitUpFiles <- function(pTest=0.05, pVerif=0.05) {
    fileNames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
    for(fileName in fileNames) {
        splitUpFile(fileName, pTest, pVerif)
    }
}

splitUpFile <- function(fileName, pTest=0.05, pVerif=0.05) {
    print(fileName)
    set.seed(12345)
    numLines <- 10000
    inFile <- file(paste("data/final/en_US", fileName, sep="/"), "r")
    outFileTrain <- file(paste("data/split/train", fileName, sep="/"), "w")
    outFileVerif <- file(paste("data/split/verif", fileName, sep="/"), "w")
    outFileTest <- file(paste("data/split/test", fileName, sep="/"), "w")
    pTrain <- 1 - pTest - pVerif
    
    txtLines <- readLines(inFile, numLines, skipNul = TRUE)
    while (length(txtLines)) {
        sample <- runif(length(txtLines))
        writeLines(txtLines[sample<=pTrain],outFileTrain)
        writeLines(txtLines[(sample>pTrain) && (sample <= (pTrain+pVerif))],outFileVerif)
        writeLines(txtLines[sample>(pTrain+pVerif)],outFileTest)
        txtLines <- readLines(inFile,numLines, skipNul = TRUE)
    }
    close(inFile)
    close(outFileTrain)
    close(outFileVerif)
    close(outFileTest)
}



getProfanityWords <- function() { 
    pFile <- file("data/Google-profanity-words-master/list.txt", "r")
    profanityWords <- readLines(pFile)
    close(pFile)
    profanityWords
}