setwd(paste(getwd(), "MachineLearning", sep="/"))
getwd()

temp=tempfile()
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", temp)
temp1=tempfile()
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", temp1)
train<-read.csv(temp, na.strings=c("NA","#DIV/0!", ""))
test=read.csv(temp1, na.strings=c("NA","#DIV/0!", ""))


library(foreach)
library(doParallel)
registerDoParallel()

load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
} 

packages <- c("data.table", "caret", "randomForest", "foreach", "doParallel")
load(packages)

RF<-train(class~., data=training, method="rf", prox=TRUE, ntree=)


#Columns with all NAs:  
    
    
na.test <-  function (x) {
    w <- sapply(x, function(x)all(is.na(x)))
    if (any(w)) {
        stop(paste("All NA in columns", paste(which(w), collapse=", ")))
    }
}  

na.test(train)


#Columns 14, 17, 89, 92, 127 and 130 have all NA. 

data(mtcars)
cars=mtcars
str(cars)
cars$cyl=as.factor(cars$cyl)
cars$am=as.factor(cars$am)
 cars$gear=as.factor(cars$gear)
 car$carb=as.factor(car$carb)
round(cor(mtcars)[,1], 3)
levels(cars$am)=c("Automatic", "Manual")
plot(cars$am, cars$mpg, data=cars, col="blue", 
     bg="grey", xlab="Transmission", ylab="MPG")

library(ggplot2)

qplot(am, mpg, data = cars, geom = "boxplot", 
      xlab="Transmission", fill = I("salmon"))

a<-subset(cars, am=="Automatic", select=mpg)
b<-subset(cars, am=="Manual", select=mpg)
t.test(a, b)

by(cars$mpg, cars$am, summary)

mod1=lm(mpg~am, data=cars)
summary(mod1)

# Let's compare the summaries of mpg by transmission:  

```{r echo=FALSE}
by(mtcars$mpg, mtcars$am, summary)  
```

# Like the plot, the summaries seem to indicate that cars with 
# manual transmission tend to have higher mpg than cars with 
# automatic transmission. 

## !!!!! To see element names from a model fit, do:

str(summary(model))