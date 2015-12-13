
library(xgboost)
library(data.table)
library(Ckmeans.1d.dp)
for (i in 1:100) {
  
  filename = paste('APIpg', i, '.xml', sep = '')
  doc = xmlInternalTreeParse(filename)
  for (d in 1:100) {
    p = xmlChildren(getNodeSet(doc, path = '//listing')[[d]])  
    tCount = (i*100) + d -100

    mastertable$price[tCount] = as.integer(html_text(p$price))
                                   print(tCount)
  }
  print(i)
}

mastertableSpaital[,price:=mastertable$price[1:9800]]

#so 50k increments up to 

table(mastertableSpaital$price > 1000000)
table(mastertableSpaital$price > 200000 & mastertableSpaital$price < 250000)



catos = c(seq(0,1000000,100000),seq(1250000,2000000,250000),10000000, 100000000)

mastertableSpaital[,priceCat:=(cut(mastertableSpaital$price,catos))]



###############

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

head(train)
str(train)
###############################
       
mastertableCleaned = mastertableSpaital #to ready for XGBoost

#masionette = flat with two floors
#studio = flat
#end terrace house = terraced house
#link-detached house = semi detached house
# time contrainst so will remove less that 40


mastertableCleaned = mastertableCleaned[(Borough != 'NA')]
mastertableCleaned = mastertableCleaned[(grep(FALSE,is.na(mastertableCleaned$priceCat)))]
 # variables needed: borough, Dist2Stn, property_type, num_bedrooms, num_bathrooms,
#num_recepts, central
xVars = mastertableCleaned[,.(Borough, Dist2Stn, property_type, num_bedrooms, num_bathrooms,
                      num_recepts, central)]


xVars$Borough = as.integer(factor(xVars$Borough))
xVars$property_type = as.integer(factor(xVars$property_type))


labeldata = 
classifiaz  =  levels(mastertableCleaned$priceCat)
mastertableCleaned[mastertableCleaned$priceCat == classifiaz[2]]
logis = data.table(x = 1:9715)
for (i in 1:length(classifiaz)) {
#  assign(paste('logis',i),as.integer((mastertableCleaned$priceCat == classifiaz[i])))
#  logis[,paste('logis',i,sep = ''):=as.integer((mastertableCleaned$priceCat == classifiaz[i]))]
 logis$newcol1 = as.integer((mastertableCleaned$priceCat == classifiaz[i]))
  setnames(logis, 'newcol1', paste("clas", i, sep = ""))
}
logis[,x:=NULL]
logis = as.matrix(logis, byrow=TRUE)

#as.integer(mastertableCleaned$priceCat == classifiaz[2])
#mastertableCleaned[164]
#table(mastertableCleaned$priceCat)table(is.na(mastertableCleaned$priceCat)) length(complete.cases(mastertableCleaned$priceCat))
#check out how to discriminate between linear and cato numerical vars
#labeldata = as.integer(factor(mastertableCleaned$priceCat))

#table(labeldata) mastertableCleaned[grep(TRUE,is.na(labeldata))]

xVars = as.matrix(xVars)

#mastertableCleaned[(grep(FALSE,is.na(mastertableCleaned$priceCat)))]
trainSamp = length(logis[,1]) * 0.7
trainSamprn = round(runif(trainSamp,min = 1, max = length(logis[,1])))

trainsplitBin = logis[,1]
trainsplitBin = trainsplitBin * 0
for (i in 1:length(trainSamprn)) {
  y = trainSamprn[i]
  trainsplitBin[y] = 1
}
testSplitBin = trainsplitBin
for (i in 1:length(trainsplitBin)) {
  if (trainsplitBin[i] == 1) {
    testSplitBin[i] = 0
  } else if (trainsplitBin[i] == 0) {
    testSplitBin[i] = 1
  } 

}

#create a uniform dist
#and then a selector vector to split the trainin and test set 

predErr = list()
xVars[grep(1,testSplitBin)]
for (i in 1:(length(classifiaz))) {
 xVarTrain = xVars[grep(1,trainsplitBin),]
  xVarTest = (xVars[grep(1,testSplitBin),])
  logisTrain = logis[,i][grep(1,trainsplitBin)]
  logisTest = logis[,i][grep(1,testSplitBin)]
pred =  xgboost(data = xVarTrain, label = logisTrain, nrounds = 2000, objective = 'binary:logistic', verbose = 1)
predErr[i] = sum(as.integer(round(predict(pred,xVarTest)) != logisTest))
}
#pred error as a total of all binary classifications
logisticpred = logis[1:length(logisTest),] * 0
predictedCato = as.integer(0)
for (i in 1:(length(classifiaz))) {
  xVarTrain = xVars[grep(1,trainsplitBin),]
  xVarTest = (xVars[grep(1,testSplitBin),])
  logisTrain = logis[,i][grep(1,trainsplitBin)]
  logisTest = logis[,i][grep(1,testSplitBin)]
  pred =  xgboost(data = xVarTrain, label = logisTrain, nrounds = 2000, objective = 'binary:logistic', verbose = 1)
  logisticpred[,i] = predict(pred,xVarTest)
}

predictedCato = max.col(logisticpred)
answerlist = (mastertableCleaned[testSplitBin]$priceCat)
fpredlist = (mastertableCleaned[testSplitBin]$priceCat)

for (i in length(predictedCato)) {
   fpredlist[i] = classifiaz[predictedCato[i]] 
   fpredlist == 
}
table(fpredlist == answerlist)[2]/(table(fpredlist == answerlist)[2] + table(fpredlist == answerlist)[1])


table()
predic
#need dimensions of logis but length of logisTest
as.integer(mastertableCleaned[testSplitBin]$priceCat)

predict

xgboost::xgb.importance(filename_dump = 'wot')
xgboost::xgb.plot.tree(filename_dump = 'wot')
library
install.packages('DiagrammeR')
library(DiagrammeR)
0.007309 * 10000
select.list(logis, 2)
?select_vars(logis, 2)
?select.list()
save.image()



###############
xVarTrain = xVars[grep(1,trainsplitBin),]
xVarTest = xVars[grep(0,trainsplitBin),]
labeldata = as.integer(factor(mastertableCleaned$priceCat))
labeldata = labeldata - 1
y = labeldata[grep(1,trainsplitBin)]

numberOfClasses <- max(y) + 1

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 10

bst.cv = xgb.cv(param=param, data = xVarTrain, label = y, 
                nfold = cv.nfold, nrounds = cv.nround, verbose = 2)
nround <- 10000
bstfull = xgboost(param=param, data = xVarTrain, label = y, nrounds=nround)

model <- xgb.dump(bst, with.stats = T)
model[1:10]
# Get the feature real names
names <- dimnames(xVarTrain)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:5,])
xgb.plot.tree(feature_names = names, model = bstlite, n_first_tree = 2)
xgb.importance(feature_names = names, model = bst)
save.image()

install.packages('Ckmeans.1d.dp')
library(DiagrammeR)

# save model to R's raw vector
rawVec <- xgb.save.raw(bst)

# print class
print(class(rawVec))

# load binary model to R
bst3 <- xgb.load(rawVec)
pred3 <- predict(bst3, xVarTest)

# pred2 should be identical to pred
print(paste("sum(abs(pred3-pred))=", sum(abs(pred2-pred))))
library(caret)


data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
bst <- xgboost(data = train$data, label = train$label, max.depth = 2,
               eta = 1, nthread = 2, nround = 2,objective = "binary:logistic")
pred <- predict(bst, test$data)
#so maybe pred is a vector of preds for each classifer

pred <- predict(bstlite, xVarTest)
err <- mean(as.numeric(pred > 0.5) != y)
str(test$data)

#############

pred <- predict(bstlite, xVarTest)
predmatr = matrix(pred, nrow = 16, ncol = 4803)

table(max.col(t(predmatr)) != labeldata[grep(0,trainsplitBin)])

871/(871+3932) #lite version gives .18% error rate
#trying a more extensive ensemble

red <- predict(bstfull, xVarTest)
predmatr = matrix(red, nrow = 16, ncol = 4803)

table(max.col(t(predmatr)) != labeldata[grep(0,trainsplitBin)])

736/(4067+736) #full version gives .15% error rate


#############

nround <- 10000

param2 <- list("objective" = "multi:softprob",
              "eval_metric" = "merror",
              "num_class" = numberOfClasses)
bst2 = xgboost(param=param2, data = xVarTrain, label = y, nrounds=nround)
str(xVarTrain)
pred2(bst2, xVarTest)

predmatr = matrix(pred2, nrow = 16, ncol = 4803)

table(max.col(t(predmatr)) != labeldata[grep(0,trainsplitBin)])

736/(4067+736)  #evalmetric = merror version gives .15% error rate
#gonna try changing the training split

##########################

#table(labeldata) mastertableCleaned[grep(TRUE,is.na(labeldata))]

xVars = as.matrix(xVars)

#mastertableCleaned[(grep(FALSE,is.na(mastertableCleaned$priceCat)))]
trainSamp = length(logis[,1]) * 0.7
#trainSamprn = round(runif(trainSamp,min = 1, max = length(logis[,1])))
trainSamprn = sample(1:length(logis[,1]),trainSamp,replace = FALSE)

trainsplitBin = logis[,1]
trainsplitBin = trainsplitBin * 0
for (i in 1:length(trainSamprn)) {
  y = trainSamprn[i]
  trainsplitBin[y] = 1
}
testSplitBin = trainsplitBin
for (i in 1:length(trainsplitBin)) {
  if (trainsplitBin[i] == 1) {
    testSplitBin[i] = 0
  } else if (trainsplitBin[i] == 0) {
    testSplitBin[i] = 1
  } 
  
}

#retry with 50% split 
str(xVars[grep(1,trainsplitBin),])
str(xVars[grep(0,trainsplitBin),])
xVarTrain = xVars[grep(1,trainsplitBin),]
xVarTest = xVars[grep(0,trainsplitBin),]

y = labeldata[grep(1,trainsplitBin)]

numberOfClasses <- max(y) + 1

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 10

bst.cv = xgb.cv(param=param, data = xVarTrain, label = y, 
                nfold = cv.nfold, nrounds = cv.nround, verbose = 2)
nround <- 2000
bstfull50 = xgboost(param=param, data = xVarTrain, label = y, nrounds=nround)

pred50 = predict(bstfull50, xVarTest)


predmatr = matrix(pred50, nrow = 16, ncol = 4803)

table(max.col(t(predmatr)) != labeldata[grep(0,trainsplitBin)])

743 /(4115+743)

#15.2% failure rate nbay 

# xVarTest gonna try feeding it a random new one
xAppTest = xVarTest[1,]

xAppTest[1] = 10
xAppTest[2] = 150
xAppTest[3] = 3
xAppTest[4] = 2
xAppTest[5] = 1
xAppTest[6] = 0
xAppTest[7] = 1
 predict(xAppTest, bstfull50)
 p = predict(xVarTest, bstfull50)
x
 xAppTest = vector(length = 7, mode = 'integer')
q = predict(bstfull50,t(as.matrix(xAppTest)))
shinyClassifiaz[max.col(t(q))]
shinyClassifiaz = as.character(c("1-100,000", "100,000-200,000","200,000-300,000","300,000-400,000","400,000-500,000",     
"500,000-600,000",      "600,000-700,000",      "700,000-800,000",      "800,000-900,000",      "900,000-1,000,000",     
"1-1.25 million",   "1.25 - 1.5 million", "1.5 - 1.75 million", "1.75 - 2 million",   "2 - 10 million",     
"10 - 100 million"))
shinyClassifiaz[max.col(t(q))]
matrix(q, nrow = 16, ncol = 1)
q
112/16

xQuery <- reactive({ 
  xQuery = xAppTest
  xQuery[1] = input$borough
  xQuery[2] = input$dist2stn
  xQuery[3] = input$prop_type
  xQuery[4] = input$num_bedrooms
  xQuery[5] = input$num_bathrooms
  xQuery[6] = input$num_recepts
  xQuery[7] = input$central  
  xQuery
}) 

xAppTest = vector(length = 7, mode = 'numeric')


#retry with 50% split ###########
str(xVars[grep(1,trainsplitBin),])
str(xVars[grep(0,trainsplitBin),])
xVarTrain = xVars[grep(1,trainsplitBin),]
xVarTest = xVars[grep(0,trainsplitBin),]

y = labeldata[grep(1,trainsplitBin)]

numberOfClasses <- max(y) + 1

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 10

bst.cv = xgb.cv(param=param, data = xVarTrain, label = y, 
                nfold = cv.nfold, nrounds = cv.nround, verbose = 2)
nround <- 2000
bst2000w70split = xgboost(param=param, data = xVarTrain, label = y, nrounds=nround)

pred50 = predict(bstfull50, xVarTest)


predmatr = matrix(pred50, nrow = 16, ncol = 4803)

table(max.col(t(predmatr)) != labeldata[grep(0,trainsplitBin)])

#########


mastertableCleaned = mastertableCleaned[(Borough != 'NA')]
mastertableCleaned = mastertableCleaned[(grep(FALSE,is.na(mastertableCleaned$priceCat)))]
# variables needed: borough, Dist2Stn, property_type, num_bedrooms, num_bathrooms,
#num_recepts, central
xVarsChk = mastertableCleaned[,.(Borough, Dist2Stn, property_type, num_bedrooms, num_bathrooms,
                              num_recepts, central, myRef, priceCat)]


xVarsChk[,Borlist:=as.integer(factor(xVarsChk$Borough))]
xVarsChk[,propnum:=as.integer(factor(xVarsChk$property_type))]

table(xVarsChk[,Borlist, by=Borough]) #shows codes for each
table(xVarsChk[,propnum, by=property_type])
table(xVarsChk[,Borough, by=central])
# Bungalow = 3, Detached House = 7, End terrace house = 296,
#Flat = 9, Masionette = 12, Town house = 20, Terraced house = 19, Studio = 17, Semi-detached house = 16,


length(grep(1,xVarsChk$propnum))
table(xVarsChk$propnum)

table(xVarsChk[,propnum == '1']) #893 of no cato

xVarsChk = xVarsChk[order(xVarsChk[,propnum]), ][894:(9715),] # now taken out no cato houses

labeldata = as.integer(factor(xVarsChk$priceCat))
labeldata = labeldata - 1
xVarsChk = xVarsChk[,.(Borlist, Dist2Stn, propnum, num_bedrooms, num_bathrooms,
            num_recepts, central)]
xVarsChk = as.matrix(xVarsChk)

#mastertableCleaned[(grep(FALSE,is.na(mastertableCleaned$priceCat)))]

trainSamp = round((length(xVarsChk[,1]) * 0.7))
trainSamprn = sample.int(size = trainSamp, length(xVarsChk[,1]))

trainsplitBin = vector(mode = 'integer', length(xVarsChk[,1]))
length(trainsplitBin)
trainsplitBin = (trainsplitBin * 0)
length(trainsplitBin)
for (i in 1:length(trainSamprn)) {
  p = trainSamprn[i]
  trainsplitBin[p] = 1
}
length(trainsplitBin) #so coz trainSamprn is long it 
 #so label data comes from 
#xVars$Borough = as.integer(factor(xVars$Borough))
#xVars$property_type = as.integer(factor(xVars$property_type))

xVarsTrain = as.matrix(as.data.table(xVarsChk)[grep(1,trainsplitBin)])
y = labeldata[grep(1,trainsplitBin)]
xTest = as.matrix(as.data.table(xVarsChk)[grep(0,trainsplitBin)])
yTest = labeldata[grep(0,trainsplitBin)]
table(labeldata)

#so the model stil isnt giving the correct sample size? 4454 is only .5 of whole sample (-low info rows) 
#it should be around 6175



#retry with 50% split ###########


numberOfClasses <- max(y) + 1

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 10
nround <- 5000
y =  matrix(data = y, ncol = 1)
yTest =  matrix(data = yTest, ncol = 1)

bst.cv = xgb.cv(param=param, data = xVarsTrain, label = y, 
                nfold = cv.nfold, nrounds = cv.nround, verbose = 2)

bst70split = xgboost(param=param, data = xVarsTrain, label = y, nrounds=nround)

pred70 = predict(bst70split, xTest)


predmatr = matrix(pred70, nrow = 16, ncol = nrow(yTest))

table(max.col(t(predmatr)) != yTest)

xgboost::xgb.save(bst70split,'bstMed1')

deployApp()
