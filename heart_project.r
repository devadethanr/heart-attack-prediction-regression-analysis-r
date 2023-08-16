#libraries
library("plotrix")

#reading csv file
heartdata=read.csv("heart.csv")

#------------------------------------------------------------------------------

#Q1.heart attack occurrence by age (regression analysis)

  age=12 #age for prediction 
  x <-heartdata$age #age group
  y <- heartdata$target  #occurrence of heart attack
 
   #summary of data
  relation <- lm(y~x)
  print(summary(relation))

  #prediction.
  a <- data.frame(x = age)
  result <-  predict(relation,a)
  print(result)
  plot(result)
  
#-------------------------------------------------------------------------------
  
#Q2.heart attack occurrence by cholesterol (regression analysis)
  
  cholm=17 #cholesterol measurement
  col <- heartdata$chol
  
    #summary of data
  relation2 <- lm(y~col)
  print(summary(relation2))
  
  #prediction.
  a2 <- data.frame(x=cholm)
  result <- predict(relation,a2)
  print(result)
  plot(result)
  
#-------------------------------------------------------------------------------

#Q3.probability of heart attack by a person's resting blood pressure(mm Hg) (chi-square test)
  
  rbps <- heartdata$trestbps

  
  #creating data frame for bps and heart attack occurrence
  
  rbps.data <- data.frame(heartdata$target,rbps)
  print(rbps.data)
  plot(heartdata$trestbps)
  
  #chi-square for finding the significant correlation between sex and heart attack
  
  print(chisq.test(rbps.data,correct = FALSE)) #for large samples chi square almost always statistically significant
  
  #plot line
  xy=heartdata
  xy%>%
  ggplot(aes(x=heartdata$trestbps,y=heartdata$age))+
    geom_line()
  
  
#-------------------------------------------------------------------------------   
 
#Q4.heart attack occurrence by heart rate (regression analysis)
  
  thalach=168 #heart rate for prediction 
  x <-heartdata$thalach #heart rate group
  y <- heartdata$target  #occurrence of heart attack
  
  #summary of data
  relation <- lm(y~x)
  print(summary(relation))
  
  #prediction.
  a <- data.frame(x = thalach)
  result <-  predict(relation,a)
  print(result)
  plot(result)
  
#-------------------------------------------------------------------------------   
  
#Q5.heart attack occurrence by thalassemia value (regression analysis)
  
  thal=3 #thalassemia value for prediction 
  x <-heartdata$thalach #thalassemia value group
  y <- heartdata$target  #occurrence of heart attack
  
  #summary of data
  relation <- lm(y~x)
  print(summary(relation))
  
  #prediction.
  a <- data.frame(x = thal)
  result <-  predict(relation,a)
  print(result)
  pie(result)
  
#-------------------------------------------------------------------------------   
  
#Q6.heart attack occurrence by old peak value (regression analysis)
  
  oldpeak=4.5 #old peak value for prediction 
  x <-heartdata$thalach #old peak value group
  y <- heartdata$target  #occurrence of heart attack
  
  #summary of data
  relation <- lm(y~x)
  print(summary(relation))
  
  #prediction.
  a <- data.frame(x = oldpeak)
  result <-  predict(relation,a)
  print(result)
  plot(result)
  
  
  
  