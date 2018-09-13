
#############################################################
# Logistic Regression Assignment: Employee attrition Model

#############################################################


#Load Libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(MASS)
library(car)
library(caret)
library(ROCR)


#############################################################
##      Load Data Files                                    ##
#############################################################
Employee.Survey.Data<-read.csv("employee_survey_data.csv",stringsAsFactors = TRUE)
General.Data<-        read.csv("general_data.csv",stringsAsFactors = TRUE)
Manager.Survey.Data<- read.csv("manager_survey_data.csv",stringsAsFactors = TRUE)
In_time.Data<-        read.csv("in_time.csv",stringsAsFactors = TRUE)
Out_time.Data<-       read.csv("out_time.csv",stringsAsFactors = TRUE)

#############################################################
##      Calculate Avg Working Hours                        ##
#############################################################

#As dataset is not sorted checking the emp id in intime an out time 
length(unique(In_time.Data$X))          #4410
length(unique(Out_time.Data$X))         #4410  
emp.id.in<-In_time.Data[,1]
emp.id.out<-Out_time.Data[,1]
sum(emp.id.in-emp.id.out) #0 Same EMP ID sorting, Can perform Vector Operation 


#Removing the Emp ID column from In and OUT Time dataframe
In_time.Data<-In_time.Data[-1]
Out_time.Data<-Out_time.Data[-1]

#Convert the format in Time
in_time <- as.data.frame(sapply(In_time.Data, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S")))
out_time <- as.data.frame(sapply(Out_time.Data, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S")))
Office.Time<-out_time-in_time
#COnvert all the columns to numeric for Mean Calculation
Office.Time<-as.data.frame(sapply(Office.Time,function(x) as.numeric(x)))
Office.Time$AvgWorkHrs<-round(apply(Office.Time,1,mean,na.rm=TRUE),digits = 2)
AvgWorkHrs<-data.frame("EmployeeID"= emp.id.in,"AvgWorkHrs"= Office.Time$AvgWorkHrs)


#Check for the Unique Employee in each data sets
length(unique(Employee.Survey.Data$EmployeeID))#4410
length(unique(General.Data$EmployeeID))#4410
length(unique(Manager.Survey.Data$EmployeeID))#4410
#Merge all the data sets in a single file
Attrition.Master.Data<-merge(Employee.Survey.Data,General.Data,by="EmployeeID")
Attrition.Master.Data<-merge(Attrition.Master.Data,Manager.Survey.Data,by="EmployeeID")
Attrition.Master.Data<-merge(Attrition.Master.Data,AvgWorkHrs,by="EmployeeID")


#############################################################
##                Data Cleansing                           ##
#############################################################
# Remove Columns having all same row Values
cols.to.Remove<-apply(Attrition.Master.Data,2,function(x) length(unique(x)))
cols.to.Remove<-names(cols.to.Remove)[which(cols.to.Remove==1)]
Attrition.Master.Data<-Attrition.Master.Data[ , !names(Attrition.Master.Data) %in% cols.to.Remove] 

# Check for NA ,Blank and Duplicate

sum(is.na(Attrition.Master.Data)) #111 Values in Total ~ 2% of Data
#Removing the NA Value Rows from the data
Attrition.Master.Data<-na.omit(Attrition.Master.Data)
colSums(Attrition.Master.Data =="") #No Blank Values 
sum(duplicated(Attrition.Master.Data))#No Duplicates



#############################################################
##         Convert Variables to Factors                    ##
#############################################################    

#Remove the Employee ID Column 

Attrition.Master.Data<- Attrition.Master.Data[,-1]

#Columns to be converted to Factors

Fact.Cols<-c("Education","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","JobInvolvement","PerformanceRating","JobLevel")

#Converting Columns to Factor as per Data dictionary

for(f in 1:length(Fact.Cols)){
  
  Attrition.Master.Data[,Fact.Cols[f]] <- as.factor(Attrition.Master.Data[,Fact.Cols[f]])
}  

levels(Attrition.Master.Data$Education) <- c("Below College","College","Bachelor","Master","Doctor")
levels(Attrition.Master.Data$EnvironmentSatisfaction) <- c("Low","Medium","High","Very High")
levels(Attrition.Master.Data$JobInvolvement) <- c("Low","Medium","High","Very High")
levels(Attrition.Master.Data$JobSatisfaction) <- c ("Low","Medium","High","Very High")
levels(Attrition.Master.Data$WorkLifeBalance) <- c("Bad","Good","Better","Best")
levels(Attrition.Master.Data$PerformanceRating) <-c ("Low","Good","Excellent","Outstanding")



#############################################################
##               Exploratory Data Analysis                 ##
#############################################################        
#Plots of Employee Behaviour

plot_grid(ggplot(Attrition.Master.Data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(Attrition.Master.Data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(),
          ggplot(Attrition.Master.Data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(),
          ggplot(Attrition.Master.Data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(),
          align = "h")   
#Plots of Organizational Behaviour 
plot_grid(ggplot(Attrition.Master.Data, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(), 
          ggplot(Attrition.Master.Data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(),
          ggplot(Attrition.Master.Data, aes(x=Department,fill=Attrition))+ geom_bar(),
          ggplot(Attrition.Master.Data, aes(x=Education,fill=Attrition))+ geom_bar(),
          ggplot(Attrition.Master.Data, aes(x=JobLevel,fill=Attrition))+ geom_bar(),
          align = "h",nrow=3) 
#Plots of Facts    
plot_grid(ggplot(Attrition.Master.Data, aes(x=EducationField,fill=Attrition))+ geom_bar(), 
          ggplot(Attrition.Master.Data, aes(x=Gender,fill=Attrition))+ geom_bar(),
          ggplot(Attrition.Master.Data, aes(x=JobRole,fill=Attrition))+ geom_bar(),
          ggplot(Attrition.Master.Data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
          align = "h") 


#Function to Extract Numeric COlumn

Specific_Class_Columns <- function(x,y){
  Col.Class<- sapply(x,class)
  Col.List<-grep(y,Col.Class)
  return((Col.List))
}


Numeric.Cols<- c(Specific_Class_Columns(Attrition.Master.Data,"integer")
                 ,Specific_Class_Columns(Attrition.Master.Data,"numeric")) 
Character.cols<-Specific_Class_Columns(Attrition.Master.Data,"factor")



#Adding Attrition column for plotting
Plot.Data<-c(Numeric.Cols,5)
class(Attrition.Master.Data$Age)      


#plot all the numeric columns against Attrition for Outliers
Attrition.Master.Data.Numeric<-Attrition.Master.Data[ ,Plot.Data ]
Attrition.Master.Data.Numeric.melted<-melt(Attrition.Master.Data.Numeric,id.vars = "Attrition")


plot_grid(ggplot(Attrition.Master.Data.Numeric.melted,aes(Attrition,value))+
            geom_boxplot(outlier.colour = "red")+
            facet_wrap(~variable)+
            coord_cartesian(ylim = c(0,40)),
          ggplot(Attrition.Master.Data.Numeric.melted, aes(value,fill=Attrition)) +
            geom_histogram()+
            facet_wrap(~variable)+xlim(0,70)+ylim(0,400))



#############################################################
##                Outlier Treatment                        ##
#############################################################


Outlier_Treatment<- c("NumCompaniesWorked","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager")

# Function to fix Outliers
Append_outliers <- function(x,Min_Cap=.05,Max_Cap=.95){
  qnt <- quantile(x, probs=c(.25, .75))
  max_and_min <- quantile(x, probs=c(Min_Cap, Max_Cap))
  Inter_Q_Range <- 1.5 * IQR(x)
  x[x < (qnt[1] - Inter_Q_Range)] <- max_and_min[1]
  x[x > (qnt[2] + Inter_Q_Range)] <- max_and_min[2]
}

#Outlier Treatment for All the analysed Column
for(z in 1:length(Outlier_Treatment)){
  
  Attrition.Master.Data[,Outlier_Treatment[z]] <- sapply(Attrition.Master.Data[,Outlier_Treatment[z]], Append_outliers)
}


#############################################################
##               Scaling Continous Variables               ##
#############################################################    

for(a in 1:length(Numeric.Cols)){
  
  Attrition.Master.Data[,Numeric.Cols[a]] <- scale(Attrition.Master.Data[,Numeric.Cols[a]])
}
#############################################################
##               Dummy Variable Creation                   ##
#############################################################   

Two_level_Treatment<-function(x){
  levels(x)<-c(0,1) 
  x <- as.numeric(levels(x))[x] 
  
}

#Convert Factor with 2 levels to numerical variables
# Gender, Attrition 

level_2<-c('Gender','Attrition')

Attrition.Master.Data[,level_2]<-sapply(Attrition.Master.Data[,level_2],Two_level_Treatment)


#Extract Character Columns

Char.Cols<- Specific_Class_Columns(Attrition.Master.Data,"character")
Fact.Cols<- Specific_Class_Columns(Attrition.Master.Data,"factor")
Multilevel_Data<-cbind(Attrition.Master.Data[,Char.Cols],Attrition.Master.Data[,Fact.Cols])

length.multilevel.data<-apply(Multilevel_Data,2,function(x) length(unique(x)))
length.multilevel.data[12]<-4 

Multilevel_Dummy<-names(length.multilevel.data)[which(length.multilevel.data > 2)]


Empty_DF <- data.frame(matrix(ncol = 0, nrow = 4300)) # To Store the Dummy Variables

Dummy_Multilevel <- function(x) {
  Dummy_Variable <- data.frame(model.matrix(~x,data = Multilevel_Data))
  Dummy_Ready<-Dummy_Variable[,-1]
  Empty_DF<-cbind(Empty_DF,Dummy_Ready)
  return(Dummy_Ready) 
}

Dummy_Combined<-as.data.frame(sapply(Multilevel_Data[,Multilevel_Dummy],Dummy_Multilevel))

Attrition.Model.Data<-Attrition.Master.Data[ , !names(Attrition.Master.Data) %in% Multilevel_Dummy] 


# Combine the dummy variables and the numeric columns
Attrition.Model.Data<-cbind(Attrition.Model.Data,Dummy_Combined)
names(Attrition.Model.Data)<-gsub("x","",names(Attrition.Model.Data))# handle extra x added due to custom funciton      


#############################################################
##               Model Preparation                         ##
#############################################################   



set.seed(123)
Training_indice<- sample(1: nrow(Attrition.Model.Data),(.7*nrow(Attrition.Model.Data)))
Training_data<-Attrition.Model.Data[Training_indice,]
Test_data<-Attrition.Model.Data[-Training_indice,]      


model1<-glm(Attrition~.,data = Training_data,family = "binomial")
summary(model1)

model2<-stepAIC(model1,direction="both") 


model2<-glm(Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
              TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
              YearsWithCurrManager + AvgWorkHrs + EnvironmentSatisfaction.Medium + 
              EnvironmentSatisfaction.High + EnvironmentSatisfaction.Very.High + 
              JobSatisfaction.Medium + JobSatisfaction.High + JobSatisfaction.Very.High + 
              WorkLifeBalance.Good + WorkLifeBalance.Better + WorkLifeBalance.Best + 
              BusinessTravel.Travel_Frequently + BusinessTravel.Travel_Rarely + 
              Department.Research...Development + Department.Sales + EducationField.Other + 
              JobLevel.5 + JobRole.Manager + JobRole.Manufacturing.Director + 
              JobRole.Research.Director + JobRole.Research.Scientist + 
              JobRole.Sales.Eecutive + MaritalStatus.Single + JobInvolvement.High,data = Training_data,family = "binomial")
summary(model2)
vif(model2)
#All the Values have low VIFs, Proceeeding with removing Least SIgnificant Variable

model3<-update(model2,~.-AvgWorkHrs,family = "binomial" )
summary(model3)
vif(model3)
# AvgWorkHrs  0.04354    0.05518   0.789 0.430120

model4<-update(model3,~.-JobRole.Manufacturing.Director) 
summary(model4)
vif(model4)

#JobRole.Manufacturing.Director    -0.29964    0.20977  -1.428 0.153181

model5<-update(model4,~.-JobRole.Manager)
summary(model5)
vif(model5)

#JobRole.Manager  -0.33203    0.25007  -1.328 0.184261 

model6<-update(model5,~.-YearsAtCompany)
summary(model6)
vif(model6)

# YearsAtCompany    0.23678    0.14669   1.614 0.106491

model7<-update(model6,~.-MonthlyIncome)
summary(model7)
vif(model7)  

# MonthlyIncome     -0.09561    0.05892  -1.623 0.104653

model8<-update(model7,~.-JobLevel.5)
summary(model8)
vif(model8)  

# JobLevel.5   -0.57436    0.30162  -1.904 0.056878 . 

model9<-update(model8,~.-JobInvolvement.High)
summary(model9)
vif(model9) 
# JobInvolvement.High   -0.27502    0.11345  -2.424 0.015341

model10<-update(model9,~.-EducationField.Other)
summary(model10)
vif(model10)
# EducationField.Other    -0.67499    0.27704  -2.436 0.014830 

model11<-update(model10,~.-JobRole.Sales.Eecutive)
summary(model11)
vif(model11)

# JobRole.Sales.Eecutive 0.39277    0.14160   2.774 0.005539

model12<-update(model11,~.-JobRole.Research.Director)
summary(model12)
vif(model12)

# JobRole.Research.Director   0.58527    0.21899   2.673 0.007527

model13<-update(model12,~.-JobRole.Research.Scientist)
summary(model13)
vif(model13)

# JobRole.Research.Scientist         0.33368    0.13345   2.500 0.012402

model14<-update(model13,~.-WorkLifeBalance.Best)
summary(model14)
vif(model14)

# WorkLifeBalance.Best  -0.83459    0.24769  -3.369 0.000753

model15<-update(model14,~.-WorkLifeBalance.Good)
summary(model15)
vif(model15)
# WorkLifeBalance.Good    -0.48938    0.16496  -2.967 0.003011

model16<-update(model15,~.-JobSatisfaction.Medium)
summary(model16)
vif(model16)

# JobSatisfaction.Medium    -0.54955    0.16682  -3.294 0.000987

model17<-update(model16,~.-JobSatisfaction.High)#0.001828
summary(model17)
vif(model17)

#JobSatisfaction.High    -0.25996    0.12714  -2.045 0.040891
#############################################################
##               Model Evaluation                          ##
#############################################################   

#Predict the Probabilty of Attrition
Attrition.Predicted = predict(model17,type="response",Test_data[,-2])


Test_data$prob <-Attrition.Predicted


# COnvert Probabilty to category 

Att.Pred.Probabilty<-factor(ifelse(Attrition.Predicted >=.5,"Yes","No"))
Actual_Attrition<-factor(ifelse(Test_data$Attrition==1,"Yes","No"))


Conf.Matrix.data<-data.frame("Actual"=Actual_Attrition,"Predicted"=Att.Pred.Probabilty)
names(Conf.Matrix.data)
Confusion.Matrix<-confusionMatrix(Conf.Matrix.data$Predicted,Conf.Matrix.data$Actual, positive = "Yes")
Accuracy<-Confusion.Matrix$overall[1]
Sensitivity<-Confusion.Matrix$byClass[1]
Specificity<-Confusion.Matrix$byClass[2]

#################################################################
###### ROC CURVE,GAIN AND LIFT CHART ############################
#################################################################

ROC.Label<-ifelse(Actual_Attrition=="Yes",1,0)
ROC.Pred.Data<-ifelse(Att.Pred.Probabilty =="Yes",1,0)
Pred.Object<-prediction(ROC.Pred.Data,ROC.Label)
class(Pred.Object)
Perf.Meas<-performance(Pred.Object,"tpr","fpr")
plot(Perf.Meas, col="red",main="ROC- Employe Attrition")

Perf.Meas1<-performance(Pred.Object,"tpr","rpp")
plot(Perf.Meas1, main = "Gain Chart")

Perf.Meas2<-performance(Pred.Object,"tpr","lift")
plot(Perf.Meas2, main = "Lift Chart")


###########################################################################


###########################################################################
# Finding optimal probability cuttoff 

perform_fun <- function(cutoff) 
{
  Att.Pred.Probabilty<-ifelse(Attrition.Predicted >=cutoff,"Yes","No")
  Conf.Matrix.data<-data.frame("Actual"=Actual_Attrition,"Predicted"=Att.Pred.Probabilty)
  names(Conf.Matrix.data)
  Confusion.Matrix<-confusionMatrix(Conf.Matrix.data$Predicted,Conf.Matrix.data$Actual, positive = "Yes")
  Accuracy<-Confusion.Matrix$overall[1]
  Sensitivity<-Confusion.Matrix$byClass[1]
  Specificity<-Confusion.Matrix$byClass[2]
  out <- t(as.matrix(c(Sensitivity, Sensitivity, Accuracy))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



prob_seq = seq(.01,.80,length=100)

OUTPUT_MTRX = matrix(0,100,3)


for(i in 1:100)
{
  OUTPUT_MTRX[i,] = perform_fun(prob_seq[i])
} 

cutoff <- prob_seq[which(abs(OUTPUT_MTRX[,1]-OUTPUT_MTRX[,2])<0.01)]


plot(prob_seq, OUTPUT_MTRX[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(prob_seq,OUTPUT_MTRX[,2],col="darkgreen",lwd=2)
lines(prob_seq,OUTPUT_MTRX[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


###############################################################################################
############### from the above matric the cutoff prob is 0.16959596  ##########################
###############################################################################################

#Confusion Matrix

Att.Pred.Probabilty<-ifelse(Attrition.Predicted >=0.18,"Yes","No")

Conf.Matrix.data<-data.frame("Actual"=Actual_Attrition,"Predicted"=Att.Pred.Probabilty)
names(Conf.Matrix.data)
Confusion.Matrix<-confusionMatrix(Conf.Matrix.data$Predicted,Conf.Matrix.data$Actual, positive = "Yes")
Accuracy<-Confusion.Matrix$overall[1]
Sensitivity<-Confusion.Matrix$byClass[1]
Specificity<-Confusion.Matrix$byClass[2]


#################################################################
###### ROC CURVE,GAIN AND LIFT CHART ############################
#################################################################
ROC.Label<-ifelse(Actual_Attrition=="Yes",1,0)
ROC.Pred.Data<-ifelse(Att.Pred.Probabilty =="Yes",1,0)

Pred.Object<-prediction(ROC.Pred.Data,ROC.Label)
class(Pred.Object)
Perf.Meas<-performance(Pred.Object,"tpr","fpr")
plot(Perf.Meas, col="red",main="ROC- Employe Attrition")

Perf.Meas1<-performance(Pred.Object,"tpr","rpp")
plot(Perf.Meas1, main = "Gain Chart")

Perf.Meas2<-performance(Pred.Object,"tpr","lift")
plot(Perf.Meas2, main = "Lift Chart")

Sens.Spec.plot<-performance(Pred.Object,"sens","spec")
plot(Sens.Spec.plot, main = "Sensitivity vs Specificity")

lift.Plot<- performance(Pred.Object,measure = "lift",x.measure = "rpp")
plot(lift.Plot, main = "Lift")

Acc.Graph<-performance(Pred.Object,measure = "err")
plot(Acc.Graph)

######################################################################################

####################################################################################
############ Model 2 with probability above 0.3 ###################################
####################################################################################


#Confusion Matrix

Att.Pred.Probabilty<-ifelse(Attrition.Predicted >=0.3,"Yes","No")

Conf.Matrix.data<-data.frame("Actual"=Actual_Attrition,"Predicted"=Att.Pred.Probabilty)
names(Conf.Matrix.data)
Confusion.Matrix<-confusionMatrix(Conf.Matrix.data$Predicted,Conf.Matrix.data$Actual, positive = "Yes")
Accuracy<-Confusion.Matrix$overall[1]
Sensitivity<-Confusion.Matrix$byClass[1]
Specificity<-Confusion.Matrix$byClass[2]


#################################################################
###### ROC CURVE,GAIN AND LIFT CHART ############################
#################################################################
ROC.Label<-ifelse(Actual_Attrition=="Yes",1,0)
ROC.Pred.Data<-ifelse(Att.Pred.Probabilty =="Yes",1,0)

Pred.Object<-prediction(ROC.Pred.Data,ROC.Label)
class(Pred.Object)
Perf.Meas<-performance(Pred.Object,"tpr","fpr")
plot(Perf.Meas, col="red",main="ROC- Employe Attrition")

Perf.Meas1<-performance(Pred.Object,"tpr","rpp")
plot(Perf.Meas1, main = "Gain Chart")

Perf.Meas2<-performance(Pred.Object,"tpr","lift")
plot(Perf.Meas2, main = "Lift Chart")

Sens.Spec.plot<-performance(Pred.Object,"sens","spec")
plot(Sens.Spec.plot, main = "Sensitivity vs Specificity")

lift.Plot<- performance(Pred.Object,measure = "lift",x.measure = "rpp")
plot(lift.Plot, main = "Lift")

Acc.Graph<-performance(Pred.Object,measure = "err")
plot(Acc.Graph)

##################################################################################
