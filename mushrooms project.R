library(tidyverse)
library(readxl)
## reading dataset
mush <- read_csv("mushrooms.csv")

# ** data ananlysis ** #

# checking dimensions
nrow(mush)
ncol(mush)
dim(mush)
View(mush)

# checking the structure of dataset
str(mush)

# checking the summary of the dataset
summary(mush)
head(mush)

# what variable names are there in our dataset
names(mush)
glimpse(mush)
View(mush)
colnames(mush)

# checking the missing values in our dataset
is.na(mush)

# ommiting the 'NA' values from dataset
  #data(airquality)
sum(is.na(mush))
colSums(is.na(mush))
clean_data <- na.omit(mush)
clean_data

# accessing elements at a particular index
mush[2,3]
mush[,1:3]
length(mush)
mush[,c("class","odor","bruises")]

head(mush)
pclass<-mush$class
pclass
max(mush$class)
min(mush$bruises)

# what's in the package
vignette(package="tidyverse")

# ** filtering ** #

filter(mush,class=="e")
filter(mush,class=="p",bruises=="TRUE")
filter(mush,class=="p" & population=="s")
filter(mush,bruises=="FALSE"|population=="n")
filter(mush,odor>"n")
filter(mush,mush$`cap-surface`=="y")

#logical tests
filter(mush,class%in% c("p"))

# ** using the 'select' in dataset ** #
select(mush,`cap-color`,`cap-shape`,`cap-surface`)
select(mush,`gill-attachment`,`gill-size`,`gill-color`)

#now using the 'deselect'
select(mush,-(class:odor))
# renaming a variable
rename(mush,Environments=habitat)

# arranging our dataset
arrange(mush,`gill-size`)
arrange(mush,`gill-spacing`,`odor`)
arrange(mush,desc(`gill-size`))

# reducing a dataset
red<- select(mush,class:odor,ends_with("p"),bruises)
red


# doing the substitutiono
mush1<-mush
mush1$class=gsub("p","a",mush1$class)
mush1$class
mush1$`gill-attachment`=gsub("f","z",mush1$`gill-attachment`)
mush1$`gill-attachment`

# ** summarizing the grouped data ** #
by_gill<- group_by(mush,`gill-attachment`,`gill-size`,`gill-color`)
by_gill
summarise(by_gill)


# ** visualization ** #
library(ggplot2)
ggplot(data =mush, aes(x=`cap-shape`))+
  geom_bar()
ggplot(data = mush,aes(x=`cap-shape`,y=odor))+
  geom_point(color="red",size=2,shape=19,alpha=.3)+
  ggtitle("The graph between `cap-shape` and `odor`", subtitle = "cap-shape vs odor")+
  facet_wrap(~ `cap-shape`)

ggplot(data = mush,aes(x=`gill-color`,y=`gill-size`))+
  geom_point(color="red",size=2,shape=19,alpha=.3)+
  ggtitle("The graph between `gill-color` and `gill-size`", subtitle = "`gill-color` vs `gill-size`")+
  facet_grid(`gill-color` ~ `gill-size`)


# ** Linear Regression ** #
# Simple Linear Regression

# Importing the dataset
dataset<-read.csv('mushrooms.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$class, SplitRatio = 2/3)
MushSubset1 = subset(dataset, split == TRUE)
MushSubset2 = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = class ~ odor,
               data = MushSubset1)

# Predicting the Test set results
y_pred = predict(regressor, newdata = MushSubset2)


# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = MushSubset1$class, y = MushSubset1$population),
             colour = 'red') +
  geom_line(aes(x = MushSubset1$class, y = predict(regressor, newdata = MushSubset1)),
            colour = 'blue') +
  ggtitle('Population vs class (MushSubset1)') +
  xlab('class') +
  ylab('population')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = MushSubset2$class, y = MushSubset2$population),
             colour = 'red') +
  geom_line(aes(x = MushSubset1$class, y = predict(regressor, newdata = MushSubset1)),
            colour = 'blue') +
  ggtitle('Population vs class (Test set)') +
  xlab('class') +
  ylab('Population')


