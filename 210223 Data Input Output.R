no<- c(1,2,3,4)
name<-c("Apple","Banana","Peach","Berry")
prices<-c(500,200,200,50)
qty<-c(5,2,7,9)
fruit<-data.frame(No=no, Name=name,Price = prices,QTY=qty) ;fruit

save(no,name,prices,qty,fruit,file="test.data")

ls()

load("test.data")

getwd()

list.files()

file.exists("test.data")

file.exists("123")

install.packages('svDialogs')
library(svDialogs)

svDialogs::dlg_input()

user.input<-dlgInput('Input income')$res
user.input

income <- as.numeric(user.input)
income

tax <- income *0.05; tax
cat('세금 : ', tax)

weight.input<-dlgInput('Input weight')$res
weight <- as.numeric(weight.input)
height.input<-dlgInput('Input height')$res
height <- as.numeric(height.input)

BMI <- weight/(height/100)^2
cat("your bmi is", BMI, sep=" ")


test<-dlgInput('Input weight')$res; test

install.packages("xlsx")
library(xlsx)

a<-scan("testdata.txt",what="")
a
a[4]
str(a)

c<-read.table("testdata.txt", header=T)
c

write.xlsx(iris,"iris.xlsx",row.names=F)

fruit

clipd <- read.table(file="clipboard",
                  header=T,
                  sep="\t",
                  stringsAsFactors=F)
clipd

print('Begin work')
a<- 10; b<-20
sink('result.txt', append =T)
cat('a+b=',a+b, '\n')
sink()
cat('hello world \n')
sink('result.txt', append =F)
cat('a*b=',a*b,'\n')
sink()

install.packages('ggplot2')
library(ggplot2)

str(diamonds)

levels(diamonds$cut)

new<-subset(diamonds, cut =="Premium" & carat >=2)
write.csv(new, 'test2.csv',row.names=F)

diamonds2<-read.csv('test2.csv', header =T)
Not_d<-subset(diamonds2, color!='D')
write.xlsx(Not_d, 'test3.xlsx', row.names=F)

url<-"https://vincentarelbundock.github.io/Rdatasets/csv/datasets/Titanic.csv"
titanic<-read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/Titanic.csv")
head(titanic)

carprice.new<-read.csv('data/carprice.csv', header=T); carprice.new
str(carprice.new)
head(carprice.new)

library(svDialogs)

type_input<-dlg_input('Input type')$res
MPG.city_input<-dlg_input('Input MPG.city')$res
result<-subset(carprice.new, Type==type_input & MPG.city>=MPG.city_input); result
write.csv(result, 'result.csv')
print(result)
sink('result2.xlsx', append=T)
print(result)
sink()

a<-c(3,1,5,2,7,8,10)
ifelse(a%%2==0, "even", "odd")

a<-10
b<-20

c<-ifelse(a>b, a,b)
print(c)

library(svDialogs)
amount_input<-as.numeric(dlg_input('purchase amount')$res)

for(i in 1:14){
  print(i)
}

iris
  
lb<-c()
norow<-nrow(iris); norow

for (i in 1:norow) {
  if (iris$Petal.Length[i]>=5.1) {
    lb[i]<- 'High'
  } else if(iris$Petal.Length[i]<=1.6) {
    lb[i]<- 'Low'
  } else {
    lb[i]<- 'Medium'
  }
}

new<-data.frame(iris,lb)
head(new)

x<-data.frame(a=c(1,2,3), b=c('a',NA,"c"), c=c(2,3,NA))
x

na.fail(x)
na.omit(x)
na.exclude(x)
na.pass(x)
