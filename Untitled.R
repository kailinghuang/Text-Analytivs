install.packages("tm")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("hash")

library(tm)
library(ggplot2)

## corpus of 50 documents
DocData<-data("acq")

########################################
## Q(a) try function in lecture
inspect(acq)
## the length of specific document
test11<-acq[[1]]
test11
## sparsity/ Max length term
ACQdtm<-DocumentTermMatrix(acq)
ACQdtm
## inspect term
inspect(ACQdtm[1:15, 1:6])
## frequency of term
test1tf <- termFreq(test11)
test1tf
## convert to a dataFrame
test1df <- as.data.frame(test1tf)
test1df
## Convert the corpus to lower case
ACQlow<- tm_map(acq, content_transformer(tolower))
ACQlow
## remove anything other than English letters or spaces
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
ACQcl <- tm_map(ACQlow, content_transformer(removeNumPunct))
## remove stop words from the corpus
myStopword <- c(stopwords('english'))
ACQstop <- tm_map(ACQcl, removeWords, myStopword)
inspect(ACQstop[1:2])
## find terms with a frequency of 5 or more
ACQtdm2 <- TermDocumentMatrix(ACQstop, control = list(wordLengths = c(1, Inf)))
ACQtdm2
freq.terms <- findFreqTerms(ACQtdm2, lowfreq = 5)
freq.terms
## find words associated with "states"
findAssocs(ACQtdm2, "states", 0.25)
## term frequency
term.freq <- rowSums(as.matrix(ACQtdm2))
term.freq <- subset(term.freq, term.freq >= 5)
df <- data.frame(term = names(term.freq), freq = term.freq)
term.freq
df


###########################################
## Q(b) use inspect
## 15 largest document
## 50:1068, 47:3013, 44:1022, 42:1607, 36:1043, 34:1465, 29:3109, 25:3516, 22:1873, 20:1009, 19:2457
## 18:871, 7:3635, 4:2308, 1:1287
inspect(acq)

############################################
## Q(c) 
## dendrogram
tdm2 <- removeSparseTerms(ACQtdm2, sparse = 0.50)
tdm2
dd <- dist(scale(tdm2), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc)
## WordCloud
m1 <- as.matrix(tdm2)
word.freq <- sort(rowSums(m1), decreasing = T)
word.freq
library(wordcloud)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)

######################################################
install.packages("textreuse")
install.packages("wordnet")
install.packages("zipfR")
install.packages("tidyverse")
install.packages("tokenizers")
## see the content of document
as.character(acq[[7]])
## Q(d)
library(textreuse)
library(tidyverse)
library(tokenizers)
## get one of 15 largest documents
docI <- acq[[7]]
charDoc <- as.character(docI)
max_word1 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word1)){
    max_word1 = word
   }
}
docI <- acq[[50]]
charDoc <- as.character(docI)
max_word2 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word2)){
    max_word2 = word
  }
}
docI <- acq[[47]]
charDoc <- as.character(docI)
max_word3 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word3)){
    max_word3 = word
  }
}


docI <- acq[[44]]
charDoc <- as.character(docI)
max_word4 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word4)){
    max_word4 = word
  }
}

docI <- acq[[42]]
charDoc <- as.character(docI)
max_word5 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word5)){
    max_word5 = word
  }
}


docI <- acq[[36]]
charDoc <- as.character(docI)
max_word6 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word6)){
    max_word6 = word
  }
}


docI <- acq[[34]]
charDoc <- as.character(docI)
max_word7 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word7)){
    max_word7 = word
  }
}


docI <- acq[[29]]
charDoc <- as.character(docI)
max_word8 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word8)){
    max_word8 = word
  }
}


docI <- acq[[25]]
charDoc <- as.character(docI)
max_word9 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word9)){
    max_word9 = word
  }
}


docI <- acq[[22]]
charDoc <- as.character(docI)
max_word10 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word10)){
    max_word10 = word
  }
}


docI <- acq[[20]]
charDoc <- as.character(docI)
max_word11 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word11)){
    max_word11 = word
  }
}


docI <- acq[[19]]
charDoc <- as.character(docI)
max_word12 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word12)){
    max_word12 = word
  }
}


docI <- acq[[18]]
charDoc <- as.character(docI)
max_word13 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word13)){
    max_word13 = word
  }
}

docI <- acq[[4]]
charDoc <- as.character(docI)
max_word14 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word14)){
    max_word14 = word
  }
}

docI <- acq[[1]]
charDoc <- as.character(docI)
max_word15 = ""
for (word in tokenize_words(charDoc)){
  if(nchar(word) > nchar(max_word15)){
    max_word15 = word
  }
}




print(max_word1)
print(max_word2)
print(max_word3)
print(max_word4)
print(max_word5)
print(max_word6)
print(max_word7)
print(max_word8)
print(max_word9)
print(max_word10)
print(max_word11)
print(max_word12)
print(max_word13)
print(max_word14)
print(max_word15)





# print the longest sentence in every file
docI <- acq[[7]]
charDoc <- as.character(docI)
max_sentence_len1 = 0
max_sentence1 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len1){
    max_sentence1 = line
    max_sentence_len1 = count
  }
}



docI <- acq[[50]]
charDoc <- as.character(docI)
max_sentence_len2 = 0
max_sentence2 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len2){
    max_sentence2 = line
    max_sentence_len2 = count
  }
}



docI <- acq[[47]]
charDoc <- as.character(docI)
max_sentence_len3 = 0
max_sentence3 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len3){
    max_sentence3 = line
    max_sentence_len3 = count
  }
}




docI <- acq[[44]]
charDoc <- as.character(docI)
max_sentence_len4 = 0
max_sentence4 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len4){
    max_sentence4 = line
    max_sentence_len4 = count
  }
}



docI <- acq[[42]]
charDoc <- as.character(docI)
max_sentence_len5 = 0
max_sentence5 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len5){
    max_sentence5 = line
    max_sentence_len5 = count
  }
}



docI <- acq[[36]]
charDoc <- as.character(docI)
max_sentence_len6 = 0
max_sentence6 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len6){
    max_sentence6 = line
    max_sentence_len6 = count
  }
}



docI <- acq[[34]]
charDoc <- as.character(docI)
max_sentence_len7 = 0
max_sentence7 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len7){
    max_sentence7 = line
    max_sentence_len7 = count
  }
}



docI <- acq[[29]]
charDoc <- as.character(docI)
max_sentence_len8 = 0
max_sentence8 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len8){
    max_sentence8 = line
    max_sentence_len8 = count
  }
}



docI <- acq[[25]]
charDoc <- as.character(docI)
max_sentence_len9 = 0
max_sentence9 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len9){
    max_sentence9 = line
    max_sentence_len9 = count
  }
}



docI <- acq[[22]]
charDoc <- as.character(docI)
max_sentence_len10 = 0
max_sentence10 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len10){
    max_sentence10 = line
    max_sentence_len10 = count
  }
}



docI <- acq[[20]]
charDoc <- as.character(docI)
max_sentence_len11 = 0
max_sentence11 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len11){
    max_sentence11 = line
    max_sentence_len11 = count
  }
}

docI <- acq[[19]]
charDoc <- as.character(docI)
max_sentence_len12 = 0
max_sentence12 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len12){
    max_sentence12 = line
    max_sentence_len12 = count
  }
}




docI <- acq[[18]]
charDoc <- as.character(docI)
max_sentence_len13 = 0
max_sentence13 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len13){
    max_sentence13 = line
    max_sentence_len13 = count
  }
}


docI <- acq[[4]]
charDoc <- as.character(docI)
max_sentence_len14 = 0
max_sentence14 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len14){
    max_sentence14 = line
    max_sentence_len14 = count
  }
}



docI <- acq[[1]]
charDoc <- as.character(docI)
max_sentence_len15 = 0
max_sentence15 = ""
for (line in tokenize_sentences(charDoc)){
  count = 0
  for (words in tokenize_words(line)){
    count=count+1
  }
  #print(line)
  if(count > max_sentence_len15){
    max_sentence15 = line
    max_sentence_len15 = count
  }
}



print(max_sentence1)
print(max_sentence2)
print(max_sentence3)
print(max_sentence4)
print(max_sentence5)
print(max_sentence6)
print(max_sentence7)
print(max_sentence8)
print(max_sentence9)
print(max_sentence10)
print(max_sentence11)
print(max_sentence12)
print(max_sentence13)
print(max_sentence14)
print(max_sentence15)
#######################################################
## Q(e)
## draw a table show the length of longest sentence
length_array <- c(max_sentence_len1,max_sentence_len2,max_sentence_len3,max_sentence_len4,max_sentence_len5,max_sentence_len6,max_sentence_len7,max_sentence_len8,max_sentence_len9,max_sentence_len10,max_sentence_len11,max_sentence_len12,max_sentence_len13,max_sentence_len14,max_sentence_len15)    ## change length here
length_data <- data.frame(len = length_array[1:2])
mytable <- cbind(sites = c("file 7", "file 50", "file 47", "file 44", "file 42", "file 36", "file 34", "file 29", "file 25", "file 22", "file 20", "file 19", "file 18", "file 4", "file 1"), length_data[1:2,])   ## change file name here
rownames(mytable) <- c("No1", "No2", "No3", "No4", "No5", "No6", "No7", "No8", "No9", "No10", "No11", "No12", "No13", "No14", "No15")

#############################################################
## Q(f)
## remove punctuation
fileNoPun <- tm_map(acq, content_transformer(removeNumPunct))
DocINoPun <- fileNoPun[[7]]
tokenize_sentences(as.character(DocINoPun))


#############################################################
## Q(g)
## print part of speech of every word
library(wordnet)
docI <- acq[[7]]
charDoc <- as.character(docI)
sentences <- tokenize_sentences(charDoc)
sentences_words <- sapply(sentences, tokenize_words)
sentences


##############################################################
## Q(h)
## print word frequency
library(zipfR)
testFre <- termFreq(acq[[7]])
rt_pos = as.data.frame(testFre)
Vm = rt_pos[1]
testFre

###############################################################
library(tm)
library(textreuse)

target = "current"
file_index = 1
find = 0
result_index = 1
for(i in 1:50){
  File <- acq[[file_index]]
  charFile <- as.character(File)
  line_index = 1
  for(line in tokenize_sentences(charFile)){
    
    word_index = 1
    for(word in tokenize_words(line)){
      if(word == target){
        print(paste(paste("No.", as.character(result_index)), "result"))
        print(paste(paste("No.", as.character(file_index)), "file"))
        print(paste(paste("No.", as.character(line_index)), "line"))
        print(paste(paste("No.", as.character(word_index)), "word"))
        find = 1
        result_index = result_index + 1
        print('---------------')
      }
      word_index = word_index + 1
    }
    line_index = line_index + 1
  }
  file_index = file_index + 1
}

if(find == 0){
  print("no such word!") 
}



