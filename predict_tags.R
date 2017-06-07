rm(list = ls(all = TRUE))

setwd("D:/rakesh/data science/Project/Predicting_tags")

#load data into R
#data1 = read.csv(c("biology.csv","cooking.csv", header = T))

data1 = read.csv("cooking.csv", header = T , stringsAsFactors = FALSE)[1:1000,]
data1$category = 'cooking'
data2 = read.csv("biology.csv", header = T , stringsAsFactors = FALSE)[1:1000,]
data2$category = 'biology'
data3 = read.csv("crypto.csv", header = T , stringsAsFactors = FALSE)[1:1000,]
data3$category = 'crypto'
data4 = read.csv("diy.csv", header = T , stringsAsFactors = FALSE)[1:1000,]
data4$category = 'diy'
data5 = read.csv("robotics.csv", header = T , stringsAsFactors = FALSE)[1:1000,]               
data5$category = 'robotics'
data6 = read.csv("travel.csv", header = T , stringsAsFactors = FALSE)[1:1000,]
data6$category = 'travel'

# test = read.csv("test.csv", header = T , stringsAsFactors = F)


all_data = rbind(data1 , data2 , data3 , data4 , data5 , data6)

remove(data1 , data2 , data3 , data4 , data5 , data6)

# str(data)


#Text Minning
#load libraries
library("stringr")
library(tm)
library(slam)
library(sampling)
#Load text
all_text = all_data[,2:4]

# stratas = strata(all_data, c("category"), size = c(1000, 1000, 1000, 1000, 1000, 1000), method = "srswor")
# 
# stratified_data = getdata(all_data, stratas)


#Removing HTML tags
all_text = data.frame(lapply(all_text, function(y) 
                                         gsub("<.*?>", " ",y)))

#Removing punctuation marks except '-' sign for compound words
all_text = data.frame( lapply(all_text, function(y)
        gsub("[^[:^punct:]-]", " ", y, perl = T)))

# all_text = data.frame( apply(all_text, 2, function(y)
#         gsub("[[:punct:]]", " ", y, perl = T)))

#all_text$tags

#Removing numbers
all_text = data.frame(lapply(all_text, function(y)
                                       gsub("[0-9]", " ", y)))

#data1
#class(data1)  

#Stemming the Document
all_text = data.frame(apply(all_text,2 , function(y) stemDocument(y)))

#Converting all words to lower case
all_text = data.frame(tolower(as.matrix(all_text)) , stringsAsFactors = FALSE)

# str(all_text)

#Remove stopwords
all_text = data.frame(lapply(all_text , 
        function(x){removeWords(x , stopwords("english"))}), stringsAsFactors = FALSE)


#Delete extra spaces
all_text = data.frame(lapply(all_text, function(y) 
  gsub("^ *|(?<= ) | *$", "", y, perl = TRUE)), stringsAsFactors = FALSE)


# class(all_text)
# str(all_text)


#Replacing missing values with NA
all_text = data.frame(apply(all_text, 2, 
                         function(x) gsub("^$|^ $", NA, x)),stringsAsFactors = FALSE)
# sum(is.na(data))

tf_idf = function(corp)
{
        corpus = Corpus(VectorSource(corp))
        
        corpus = tm_map(corpus , tolower)
        
        tdm = TermDocumentMatrix(corpus)
        tf_mat =as.matrix(tdm)
        tf = tf_mat

        idf = log(nrow(tf_mat)/colSums(tf_mat))
        tfidf = tf_mat

        for(i in names(idf)){
                tfidf[,i] <- tf[,i] * idf[i]
        }
        #rm(tf_mat,tf,idf,tdm,corpus)
        return(tfidf)
}


content_tf_idf = tf_idf(all_text[1:6000,2])

x = as.matrix(content_tf_idf[content_tf_idf[,3]>0,3])  
y[1] = list(rownames(x))


#Splitting each word in Tag
tag_matrix = matrix("", ncol = 20, nrow(all_text))
tag_title_word_count = matrix("", ncol = 20, nrow(all_text))
tag_content_word_count = matrix("", ncol = 20, nrow(all_text))

library(stringr)
for( j in 1:nrow(all_text))
{
        len = length(strsplit(all_text$tags[j] , " ")[[1]])
        
        for( i in 1:len)
        { 
                tag_matrix[j,i] = strsplit(all_text$tags[j] , " ")[[1]][i]
                
                tag_title_word_count[j,i] = sum(str_count(all_text$title[j], tag_matrix[j,i]))
                tag_content_word_count[j,i] = sum(str_count(all_text$content[j], tag_matrix[j,i]))
                
        }
}
storage.mode(tag_title_word_count) = "numeric"
storage.mode(tag_content_word_count) = "numeric"

library(wordcloud)
#Word cloud
wordcloud(tag_matrix, max.words = 200, scale=c(5, .2), colors=brewer.pal(8, "Dark2"))


#Number of words in title, content & tag columns
length_count = matrix("",nrow(all_text) , ncol = 9)
for (i in c(1:3))
{
length_count[,i] = sapply(all_text[,i],
                               function(x)(length(strsplit(x , " ")[[1]])))

}

storage.mode(length_count) = "numeric"

#Tag in title count
length_count[,4] = apply(tag_title_word_count ,1,
          function(x) sum(x>0,na.rm=TRUE))

#Tag in content count
length_count[,5] = apply(tag_content_word_count ,1,
                         function(x) sum(x>0,na.rm=TRUE))

#Count of Tag present in both Title & Content
m = tag_content_word_count + tag_title_word_count


# #Tags present both in title & content
length_count[,6] = apply(m ,1,
                         function(x) sum(x>0,na.rm=TRUE))


#Percentage of tag in title & content

length_count[,7] = length_count[,4] / length_count[,3] * 100 
length_count[,8] = length_count[,5] / length_count[,3] * 100
length_count[,9] = length_count[,6] / length_count[,3] * 100



length_count[,7:9] =  round(length_count[,7:9] , digits = 2)

colnames(length_count) = c("title_len" , "cont_len" , "tag_len" , "tagintitle_count" , 
                           "tagincont_count", "tagincont_title" , "per_tagintitle" , 
                           "per_tagincont" , "per_tagincont_title")

# class(length_count)
# str(length_count)
all_text = cbind(all_text , length_count , all_data[,5])


colnames(all_text)[13] = 'category'
# colnames(all_text)
# save.image("preprocessing")
# rm(data,data1,test)
  load('preprocessing')
rm(all_data,length_count,m,tag_content_word_count,tag_title_word_count,tag_matrix,x,b)
  # 
# library('ggplot2')
# library('ggthemes')
# library('tm')
# bar = function(dat,X,Y,Name,title){
#         p = ggplot(dat, aes_string(x = X, fill = Y)) +
#                 geom_bar(stat = 'count', position = 'dodge') + labs(x = Name) +
#                 ggtitle(title) + theme(text=element_text(size=10))
#         return(p)
# }
# 
# b1 = bar(all_text[which(all_text[12]==0),],"per_tagincont_title","category","Categories",
#         "Tags which are not present in Title & Content ")
# 
# b2 = bar(all_text[which(all_text[12]==0),],"per_tagincont_title","category","Categories",
#         "Tags which are not present in Title & Content ")
# 
# b3 = bar(all_text[which(all_text[12]==0),],"per_tagincont_title","category","Categories",
#         "Tags which are not present in Title & Content ")
# 
# all_data = cbind(all_data, all_text[12])
# 
# x = all_data[which(all_data[6]==0),2:4]        
# 
# tagcorpus = Corpus(VectorSource(all_text[,3]))
# writeLines(as.character(tagcorpus[[8]]))
# 
# tagcorpus
# tdm = TermDocumentMatrix(tagcorpus)
# 
#          
# all_data[,6] = all_text[,12]
# x = all_data[which(all_data$V6==100),c(2:5)]
# table(x$category)
# write.table(table(x$category),"data.xls" ,sep = '\t', row.names = F, append = T)
# 
# # #write.table(x[30000:60055,],"match2.txt",sep = "\n" , row.names = T)
# # stemDocument("Isss's lovess india's" , language = 'english')
# # stemCompletion("Is lonesasd ndia's" , language = 'english')
# # y = all_data[1:5,]

