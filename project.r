setwd('D:/Coding track/Data Analytics/Machine-Learning-with-R-datasets-master')
getwd()
#install.packages("data.table")
library(data.table)

input<-as.data.frame(fread(file="groceries.csv",header=FALSE,fill=TRUE, stringsAsFactors = FALSE))
str(input)
print(input)
View(input)
is.na(input)
colSums(is.na(input))
summary(input)
str(input)
row<-nrow(input)
col=ncol(input)
head(input)
data<-data.frame()
rows<-c(1:row)
cols<-c(1:col)

for(i in rows){
  for(j in cols){
    if(input[i,j]==""){
      break
    }else{
      new<-data.frame(id=i,products=input[i,j])
      data<-rbind(data,new)
    }
  }
} 

print(data)
View(data)


#install.packages("dplyr") 
library(dplyr)
new_result<- data %>% group_by(products)%>% summarise(count  = n())%>% arrange(desc(count))
print(tail(new_result,n=20))
print(head(new_result,n=20))
nrow(new_result)
new_results<-new_result[1:10,]
barplot(new_results$count,names.arg=new_results$products,col=blues9)



new_contribution<- data %>% group_by(products)%>% summarise(per = n()/nrow(data))%>% arrange(desc(per))
print(tail(new_contribution,n=20))
print(head(new_contribution,n=20))
nrow(new_contribution)
new_contribution<-new_contribution[1:10,]
barplot(new_contribution$per,names.arg=new_contribution$products,col=blues9,ylim=c(0,0.06)) #for top10


sample<-split(data$product,data$id,"transactions")
head(sample)
typeof(sample)
class(sample)
#install.packages("arules")
library(arules)


rule1=apriori(sample,parameter=list(support=0.001,confidence=0.8,minlen=2)) #
inspect(rule1[1:20])
rule2=apriori(sample,parameter=list(support=0.02,confidence=0.1,maxlen=2,minlen=2))
inspect(rule2)
rule3=apriori(sample,parameter=list(support=0.009,confidence=0.5,maxlen=2,minlen=2))
inspect(rule3)
rules4 = apriori(sample,parameter = list(support=0.006,confidence=0.25,minlen=2))
inspect(rules4)
sorted<-(head(sort(rules4,by="lift",descending=TRUE),20))
install.packages("arulesViz")
library(arulesViz)
plot(sorted,method="grouped")
dev.off()
sorted<-(head(sort(rule1,by="lift",descending=TRUE),20))
plot(sorted,method="grouped")
