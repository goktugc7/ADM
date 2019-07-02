#libraries
library(ggplot2) 
library(dplyr)
library(VIM) 
library(corrplot) 
library(cowplot) 
library(missForest) 
library(caret)
library(gridExtra)
library(ISLR)
library(tree)
library(ROCR)
library(rpart)
library(rpart.plot)
library(rattle)

#Heart Dataset
# age: age in years
# sex: (1 = male; 0 = female)
# cp: chest pain type
# trestbps: resting blood pressure (in mm Hg on admission to the hospital)
# chol: serum cholestoral in mg/dl
# fbs: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
# restecg: resting electro cardio graphic results
# thalach: maximum heart rate achieved
# exang: exercise induced angina (1 = yes; 0 = no)
# oldpeak: ST depression induced by exercise relative to rest
# slope: the slope of the peak exercise ST segment
# ca: number of major vessels (0-3) colored by flourosopy
# thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
# target: 1 or 0
#Read Data
heartData = read.csv("/Users/goktugcengiz/Desktop/Datasets/heart.csv")
colnames(heartData)
colnames(heartData)[1]<-"age"
#First Summary of Data
head(heartData, 10)
str(heartData)
summary(heartData)
#Data Transformation
heartData$sex <- ifelse(heartData$sex=="0",'female','male')
heartData$fbs <- ifelse(heartData$fbs=="0",'false','true')
heartData$exang <- ifelse(heartData$exang=="0",'no','yes')
heartData = heartData %>% mutate(sex = as.character(sex),
                                 cp = as.factor(cp),
                                 fbs = as.factor(fbs),
                                 restecg = as.factor(restecg),
                                 exang = as.factor(exang),
                                 ca = as.factor(ca),
                                 slope = as.factor(slope),
                                 thal = as.factor(thal),
                                 target = as.factor(target))
#Missing Values
apply(heartData, 2, function(x) sum(is.na(x))) #Missing Value Detection
aggr_plot <- aggr(heartData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(heartData), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern")) #Missing Value Visualization
#Outliers
#Outlier Detection by Boxplot
plot_theme = theme_classic() +  
  theme(plot.title = element_text(hjust = 0.5, size = 14,face = 'bold'), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12)) 

#Continuous Univariate plots  

CUV = function(yfeature, ylabel) { 
  ggplot(heartData, aes(x = "", y = yfeature)) + 
    geom_boxplot(fill = "#0000FF", outlier.colour = "red", outlier.shape = 1) + 
    stat_boxplot(geom = "errorbar", width = 0.5) + 
    labs( y = ylabel, title = paste(ylabel, "Distribution")) + 
    plot_theme 
} 

p1 = CUV(heartData$age, "Age")
p2 = CUV(heartData$trestbps, "Resting Blood Pressure")
p3 = CUV(heartData$chol, "Serum Cholestoral")
p4 = CUV(heartData$thalach, "Maximum Heart Rate")
p5 = CUV(heartData$oldpeak, "Depression Induced")

plot_grid(p1, p2, p3, p4, p5) 

#Outlier Treatment
oiRBP = which(heartData$trestbps %in% boxplot.stats(heartData$trestbps)$out)
oiSC = which(heartData$chol %in% boxplot.stats(heartData$chol)$out)
oiMHR = which(heartData$thalach %in% boxplot.stats(heartData$thalach)$out)
oiDI = which(heartData$oldpeak %in% boxplot.stats(heartData$oldpeak)$out)

heartData[oiRBP, ]$trestbps = NA
heartData[oiSC, ]$chol = NA
heartData[oiMHR, ]$thalach = NA
heartData[oiDI, ]$oldpeak = NA

hd = data.frame(heartData$trestbps, heartData$chol, heartData$thalach, heartData$oldpeak)
heartDataImp = missForest(hd)
heartDataImp = heartDataImp$ximp
sum(is.na(heartDataImp))
heartData$trestbps = heartDataImp$heartData.trestbps
heartData$chol = heartDataImp$heartData.chol
heartData$thalach = heartDataImp$heartData.thalach
heartData$oldpeak = heartDataImp$heartData.oldpeak
sum(is.na(heartData))

a1 = CUV(heartData$age, "Age")
a2 = CUV(heartData$trestbps, "Resting Blood Pressure")
a3 = CUV(heartData$chol, "Serum Cholestoral")
a4 = CUV(heartData$thalach, "Maximum Heart Rate")
a5 = CUV(heartData$oldpeak, "Depression Induced")

plot_grid(a1, a2, a3, a4, a5) 

#Explatory Data Analysis
#Function for displaying histograms using ggplot2 
g1 = ggplot(heartData, aes(x=age)) + ggtitle("Age") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=5, colour="black", fill="red") + ylab("Percentage") 
g2 = ggplot(heartData, aes(x=trestbps)) + ggtitle("Resting Blood Pressure") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=5, colour="black", fill="green") + ylab("Percentage") 
g3 = ggplot(heartData, aes(x=chol)) + ggtitle("Serum Cholestoral") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=5, colour="black", fill="blue") + ylab("Percentage") 
g4 = ggplot(heartData, aes(x=thalach)) + ggtitle("Maximum Heart Rate") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=5, colour="black", fill="yellow") + ylab("Percentage") 
g5 = ggplot(heartData, aes(x=oldpeak)) + ggtitle("Depression Induced") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=5, colour="black", fill="grey") + ylab("Percentage") 
plot_grid(g1, g2, g3, g4, g5)

#Sort categorical variables in descending order
cat.sort = function(x){reorder(x,x,function(y){-length(y)})} ## Sorting function for categorical variables
cat.var = which(sapply(heartData, is.factor)) ## Find the categorical variables
for (i in cat.var){  ## Apply the sort function on each categorical variable
  heartData[,i] = cat.sort(heartData[,i])   
}
attach(heartData)

#Bar plots of categorical variables 

c1 = ggplot(heartData, aes(x=sex)) + ggtitle("Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(heartData$sex)))

c2 = ggplot(heartData, aes(x=cp)) + ggtitle("Chest Pain Type") + xlab("Chest Pain Type") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(heartData$cp)))

c3 = ggplot(heartData, aes(x=fbs)) + ggtitle("Fasting Blood Sugar") + xlab("Fasting Blood Sugar") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(heartData$fbs)))

c4 = ggplot(heartData, aes(x=restecg)) + ggtitle("Resting Electrocardiographic Results") + xlab("Resting Electrocardiographic Results") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(heartData$restecg)))

c5 = ggplot(heartData, aes(x=exang)) + ggtitle("Exercise Induced Angina") + xlab("Exercise Induced Angina") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(heartData$exang)))

c6 = ggplot(heartData, aes(x=slope)) + ggtitle("Slope") + xlab("Slope") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(heartData$slope)))

c7 = ggplot(heartData, aes(x=ca)) + ggtitle("Number of major vessels (0-3) colored by flourosopy") + xlab("Number of Major Vessels") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(heartData$ca)))

c8 = ggplot(heartData, aes(x=thal)) + ggtitle("Thal") + xlab("Thal") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(heartData$thal)))

grid.arrange(c1, c2, c3, c4, ncol=2)
grid.arrange(c5, c6, c7, c8, ncol=2)

#Pie charts of categorical variables 

pc1 = ggplot(heartData, aes(x=factor(1), fill=sex)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Sex") 

pc2 = ggplot(heartData, aes(x=factor(1), fill=cp)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Chest Pain Type")

pc3 = ggplot(heartData, aes(x=factor(1), fill=fbs)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Fasting Blood Sugar")

pc4 = ggplot(heartData, aes(x=factor(1), fill=restecg)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Resting Electrocardiographic Results")

pc5 = ggplot(heartData, aes(x=factor(1), fill=exang)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Exercise Induced Angina")

pc6 = ggplot(heartData, aes(x=factor(1), fill=slope)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Slope")

pc7 = ggplot(heartData, aes(x=factor(1), fill=ca)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Number of major vessels (0-3) colored by flourosopy")

pc8 = ggplot(heartData, aes(x=factor(1), fill=thal)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Thal")
grid.arrange(pc1, pc2, pc3, pc4, ncol=2) 
grid.arrange(pc5, pc6, pc7, pc8, ncol=2) 

#Correlation between numerical variables 
hdNum = data.frame(heartData$trestbps, heartData$chol, heartData$thalach, heartData$oldpeak)
colnames(hdNum) = c("trestbps","chol","thalach", "oldpeak")
correlations = cor(hdNum) # calculate a correlation matrix for numeric variables 
print(correlations) # display the correlation matrix 
corrplot(correlations) 
palette = colorRampPalette(c("blue", "white", "red")) (20) 
heatmap(x = correlations, col = palette, symm = TRUE) 
##Multivariate Analysis (Continuous Variables)
#Target vs <rest of continuous variables>
CONT = function(xfeature, yfeature, xlabel, ylabel) {
  ggplot(hdNum, aes(x = xfeature, y = yfeature)) +
    geom_point() +
    geom_smooth() +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "vs", xlabel)) +
    plot_theme
}
co1 = CONT(heartData$age, heartData$target, 
          "Age", "Target")
co2 = CONT(heartData$trestbps, heartData$target, 
          "Resting Blood Pressure", "Target")
co3 = CONT(heartData$chol, heartData$target, 
          "Serum Cholestoral", "Target")
co4 = CONT(heartData$thalach, heartData$target, 
          "Maximum Heart Rate", "Target")
co5 = CONT(heartData$oldpeak, heartData$target, 
          "Depression Induced", "Target")
plot_grid(co1, co2, co3, co4, co5) 

# Target vs <rest of categorical variables>
#Sex vs Target
qp1 = qplot(target, data = heartData, fill = sex) + facet_grid (. ~ sex)
#Chest Pain Type vs Target
qp2 = qplot(target, data = heartData, fill = cp) + facet_grid (. ~ cp)
#Fasting Blood Sugar vs Target
qp3 = qplot(target, data = heartData, fill = fbs) + facet_grid (. ~ fbs)
#Resting Electrocardiographic Results vs Target
qp4 = qplot(target, data = heartData, fill = restecg) + facet_grid (. ~ restecg)
#Exercise Induced Angina vs Target 
qp5 = qplot(target, data = heartData, fill = exang) + facet_grid (. ~ exang)
#Slope vs Target
qp6 = qplot(target, data = heartData, fill = slope) + facet_grid (. ~ slope)
#Number of major vessels (0-3) colored by flourosopy vs Target
qp7 = qplot(target, data = heartData, fill = ca) + facet_grid (. ~ ca)
#Thal vs Target
qp8 = qplot(target, data = heartData, fill = thal) + facet_grid (. ~ thal)
plot_grid(qp1, qp2, qp3, qp4)
plot_grid(qp5, qp6, qp7, qp8)

#Data partition

train_index = createDataPartition(heartData$target,
                                  p = .8,
                                  list = FALSE,
                                  times = 1)
head(train_index)
train = heartData[train_index, ]
test = heartData[-train_index, ]
train_x = train %>% dplyr::select(-target)
train_y = train$target
test_x = test %>% dplyr::select(-target)
test_y = test$target

#single data set
training = data.frame(train_x, target = train_y)

#Data Modeling

#Model is done by Decision Trees (CART)
heart_tree <- rpart(target ~ ., 
                    data=train,  
                    control=rpart.control(cp=0.001, xval=10),
                    method = "class")
# cp = complexity parameter = min value of alpha, when growing the initial tree
# xval = numbr of CV runs performed on training data
printcp(heart_tree)

# We obtain the optimal tree by pruning the maximal one up to the minimal crossvalidation error.
# Depending on the context, we could take the minimum xerror.
heart_tree$cptable = as.data.frame(heart_tree$cptable)

# Decide the cutoff value
ind = which.min(heart_tree$cptable$xerror) # Index of the minimum xerror
xerr <- heart_tree$cptable$xerror[ind] # Minimum xerror value
xstd <- heart_tree$cptable$xstd[ind] # Xstd value of the element with smaller xerror 
i = 1
while (heart_tree$cptable$xerror[i] > xerr+xstd) i = i+1
alfa = heart_tree$cptable$CP[i]
# Cut the tree acording to the alpha value
tree.optimal <-prune(heart_tree,cp=alfa)
tree.optimal
printcp(tree.optimal)

#Plot the importance of variables in the prediction.
fancyRpartPlot(tree.optimal)
impVar = tree.optimal$variable.importance

varImpPlot <- barplot(impVar,
                      main="Importance of variables",
                      xaxt="n",
                      xlab="Variables",
                      ylab="Importance",
                      las=1)
text(cex=1, x=varImpPlot-.25, y=-10, labels(impVar), xpd=TRUE, srt=45, col = "blue")


#Compute the accuracy, precision, recall and AUC on the test individuals.
# We first do the prediction with the optimal tree model obtained from the training dataset.
prediction = predict(tree.optimal, newdata = test, type = "class")

# We show the confusion matrix
cm <- confusionMatrix(prediction,
                      factor(test_y),
                      positive="1",
                      dnn = c("Prediction", "Reference")
)
cm$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = c("0", "1"))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  mutate(frac_directed = if_else(Prediction == "0", frac_fill * - 1, frac_fill)) %>%
  ggplot(aes(Prediction, Reference, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
  scale_x_discrete(position = "top")

accuracy = cm$overall[1]
precision = floor(cm$table[1,1]) / floor(cm$table[1,1] + cm$table[2,1])
recall = floor(cm$table[1,1]) / floor(cm$table[1,1] + cm$table[1,2])

## AUC
probPredict <- as.data.frame(predict(tree.optimal,
                                     newdata = test, 
                                     type = "prob")
)

pred_heart = prediction(probPredict$`1`, test_y)
roc = performance(pred_heart,measure="tpr",x.measure="fpr")
plot(roc, col=2, lwd=3, main="Decision Tree ROC curve")
legend(.5,.4, "AUC = 0.8") 
abline(0,1)

