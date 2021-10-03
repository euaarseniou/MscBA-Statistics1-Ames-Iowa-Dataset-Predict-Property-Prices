#Reading the data
setwd("C:/Users/Eva/Desktop/aueb/Statistics 1/Main Assignment Ntzoufras")
data<-read.csv(file = 'ames_iowa_housing_29.csv', header= TRUE, sep=";")

head(data)
#--------Drop x, order and pid variables
data<-subset(data,select=-c(X, Order, PID))
str(data)      

require(psych)

#--------------------------------------------------------------#
#                                                              #
#             Find missing values in the data set              #
#                                                              #
#--------------------------------------------------------------#

NAs <- which(colSums(is.na(data)) > 0)
sort(colSums(sapply(data[NAs], is.na)), decreasing = TRUE)
#Pool.QC is the variable with the most missing values
length(NAs)
#19 variables have NAs

require(plyr)
#---------------------------------------------------------------------------
data$Pool.QC[is.na(data$Pool.QC)] <- 'NA'
table(data$Pool.QC)
PoolQC <- c('NA' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Pool.QC<-as.integer(revalue(data$Pool.QC, PoolQC))
table(data$Pool.QC)
#----------------------------------------------------------------------------
data$Misc.Feature[is.na(data$Misc.Feature)] <- 'NA'
data$Misc.Feature <- as.factor(data$Misc.Feature)
table(data$Misc.Feature)
#---------------------------------------------------------------------------
data$Alley[is.na(data$Alley)] <- 'NA'
data$Alley <- as.factor(data$Alley)
table(data$Alley)
#---------------------------------------------------------------------------
data$Fence[is.na(data$Fence)] <- 'NA'
table(data$Fence)
data$Fence <- as.factor(data$Fence)
#---------------------------------------------------------------------------
data$Fireplace.Qu[is.na(data$Fireplace.Qu)] <- 'NA'
Fireplace <- c('NA' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Fireplace.Qu<-as.integer(revalue(data$Fireplace.Qu, Fireplace))
table(data$Fireplace.Qu)
#---------------------------------------------------------------------------
for (i in 1:nrow(data)){
  if(is.na(data$Lot.Frontage[i]==TRUE)){
    data$Lot.Frontage[i] <- as.integer(median(data$Lot.Frontage[data$Neighborhood==data$Neighborhood[i]], na.rm=TRUE)) 
  }
}
i<-which(is.na(data$Lot.Frontage))
#there is only 1 GrnHill in our data set so we cannot take the median for this neighborhood
#we are going to use the mode for lot.frontage
require(modeest)
data$Lot.Frontage[is.na(data$Lot.Frontage)] <- mlv(data$Lot.Frontage[-i], method = "mfv")

Lot.Shape<-c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)
data$Lot.Shape<-as.integer(revalue(data$Lot.Shape,Lot.Shape ))
table(data$Lot.Shape)

data$Lot.Config <- as.factor(data$Lot.Config)
table(data$Lot.Config)
#----------------------------------------------------------------------------
data$Garage.Yr.Blt[is.na(data$Garage.Yr.Blt)] <- data$Year.Built[is.na(data$Garage.Yr.Blt)]

length(which(is.na(data$Garage.Type) & is.na(data$Garage.Finish) & is.na(data$Garage.Cond) & is.na(data$Garage.Qual)))

data$Garage.Type[is.na(data$Garage.Type)] <- 'NA'
data$Garage.Type<- as.factor(data$Garage.Type)
table(data$Garage.Type)

data$Garage.Finish[is.na(data$Garage.Finish)] <- 'NA'
GarageFinish <- c('NA'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
data$Garage.Finish<-as.integer(revalue(data$Garage.Finish, GarageFinish))
table(data$Garage.Finish)

data$Garage.Qual[is.na(data$Garage.Qual)] <- 'NA'
GarageQual <- c('NA' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Garage.Qual<-as.integer(revalue(data$Garage.Qual, GarageQual))
table(data$Garage.Qual)

data$Garage.Cond[is.na(data$Garage.Cond)] <- 'NA'
GarageCond <- c('NA' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Garage.Cond<-as.integer(revalue(data$Garage.Cond, GarageCond))
table(data$Garage.Cond)
#------------------------------------------------------------------------------
#check if 47 NAs are the same for all basement variables
length(which(is.na(data$Bsmt.Qual) & is.na(data$Bsmt.Cond) & is.na(data$Bsmt.Exposure) & 
               is.na(data$BsmtFin.Type.1) & is.na(data$BsmtFin.Type.2)))

#they are all the same so we have to find which 2 nas are in bsmt.exposure

same<-which(is.na(data$Bsmt.Qual) & is.na(data$Bsmt.Cond) & 
              is.na(data$BsmtFin.Type.1) & is.na(data$BsmtFin.Type.2))
bsmtexposure_nas<-which(is.na(data$Bsmt.Exposure))

#compare to find which are the 2 extra NAs
y<-bsmtexposure_nas[!(bsmtexposure_nas %in% same)]
#rows 174 and 1154

#select only columns for basement with NAs
bmst<-subset(data,select=c(Bsmt.Exposure,Bsmt.Qual, Bsmt.Cond, BsmtFin.Type.1,BsmtFin.Type.2))
#show only the rows with extra 2 NAs
bmst[y,]

#filter the data set 
bmst<-bmst[which((data$Bsmt.Qual=='Gd')&(data$Bsmt.Cond=='TA')&(data$BsmtFin.Type.1=='Unf')&(data$BsmtFin.Type.2=='Unf')),]
#find the most common bsmt.exposure 
table(bmst$Bsmt.Exposure)
#so we are going to replace NAs with NA

data$Bsmt.Qual[is.na(data$Bsmt.Qual)] <- 'NA'
BsmtQual <- c('NA' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Bsmt.Qual<-as.integer(revalue(data$Bsmt.Qual, BsmtQual))
table(data$Bsmt.Qual)

data$Bsmt.Cond[is.na(data$Bsmt.Cond)] <- 'NA'
BsmtCond <- c('NA' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Bsmt.Cond<-as.integer(revalue(data$Bsmt.Cond, BsmtCond))
table(data$Bsmt.Cond)

data$Bsmt.Exposure[is.na(data$Bsmt.Exposure)] <- 'NA'
BsmtExposure <- c('NA'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
data$Bsmt.Exposure<-as.integer(revalue(data$Bsmt.Exposure, BsmtExposure))
table(data$Bsmt.Exposure)

data$BsmtFin.Type.1[is.na(data$BsmtFin.Type.1)] <- 'NA'
BsmtFinType <- c('NA'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
data$BsmtFin.Type.1<-as.integer(revalue(data$BsmtFin.Type.1,BsmtFinType))
table(data$BsmtFin.Type.1)

data$BsmtFin.Type.2[is.na(data$BsmtFin.Type.2)] <- 'NA'
BsmtFinType <- c('NA'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
data$BsmtFin.Type.2<-as.integer(revalue(data$BsmtFin.Type.2, BsmtFinType))
table(data$BsmtFin.Type.2)
#----------------------------------------------------------------------------
data$Mas.Vnr.Type[is.na(data$Mas.Vnr.Type)] <- 'Missing'
data$Mas.Vnr.Type<- as.factor(data$Mas.Vnr.Type)
table(data$Mas.Vnr.Type)
data$Mas.Vnr.Area[is.na(data$Mas.Vnr.Area)] <-0
#-----------------------------------------------------------------------
which(is.na(data$Electrical))
#only one NA so it is better to replace it with the most common category and 
#not create a new level missing
data$Electrical[is.na(data$Electrical)] <- names(sort(table(data$Electrical),decreasing=TRUE))[1]
data$Electrical <- as.factor(data$Electrical)
table(data$Electrical)
#-------------------------------------------------------------------------
which(is.na(data)==TRUE) #we have finished with NAs

#----------------------------------------------------------------------------#
#                                                                            #
#             Convert remaining Variables in the correct type                #
#                                                                            #
#----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
data$MS.SubClass <- as.factor(data$MS.SubClass)
#-----------------------------------------------------------------------------
data$MS.Zoning <- as.factor(data$MS.Zoning)
table(data$MS.Zoning)
#-----------------------------------------------------------------------------
data$Street <- as.factor(data$Street)
table(data$Street)
#-----------------------------------------------------------------------------
data$Land.Contour <- as.factor(data$Land.Contour)
table(data$Land.Contour)

landslope<-c('Sev'=0, 'Mod'=1, 'Gtl'=2)
data$Land.Slope<-as.integer(revalue(data$Land.Slope, landslope))
table(data$Land.Slope)
#-----------------------------------------------------------------------------
table(data$Utilities)
#utilities has only 2 nosewr. it seems that this variable does not add any significant 
#information in our model. we keep this in mind for later on
#-----------------------------------------------------------------------------
data$Neighborhood <- as.factor(data$Neighborhood)
table(data$Neighborhood)
#-----------------------------------------------------------------------------
data$Condition.1 <- as.factor(data$Condition.1)
table(data$Condition.1)

data$Condition.2 <- as.factor(data$Condition.2)
table(data$Condition.2)
#-----------------------------------------------------------------------------
data$Bldg.Type <- as.factor(data$Bldg.Type)
table(data$Bldg.Type)

data$House.Style<- as.factor(data$House.Style)
table(data$House.Style)
#-----------------------------------------------------------------------------
data$Roof.Style <- as.factor(data$Roof.Style)
table(data$Roof.Style)

data$Roof.Matl <- as.factor(data$Roof.Matl)
table(data$Roof.Matl)
#-----------------------------------------------------------------------------
data$Exterior.1st <- as.factor(data$Exterior.1st)
table(data$Exterior.1st)

data$Exterior.2nd <- as.factor(data$Exterior.2nd)
table(data$Exterior.2nd)

ExterQual <- c('Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Exter.Qual<-as.integer(revalue(data$Exter.Qual, ExterQual))

ExterCond <- c('Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Exter.Cond<-as.integer(revalue(data$Exter.Cond, ExterCond))
#----------------------------------------------------------------------------
data$Foundation <- as.factor(data$Foundation)
table(data$Foundation)
#----------------------------------------------------------------------------
data$Heating <- as.factor(data$Heating)
table(data$Heating)

HeatingQC <- c('Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Heating.QC<-as.integer(revalue(data$Heating.QC, HeatingQC))

data$Central.Air <- as.factor(data$Central.Air)
table(data$Central.Air)
#----------------------------------------------------------------------------
kitchenqual <- c('Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$Kitchen.Qual<-as.integer(revalue(data$Kitchen.Qual, kitchenqual))
table(data$Kitchen.Qual)
#----------------------------------------------------------------------------
functional<- c('Sal'=1, 'Sev'=2, 'Maj2'=3, 'Maj1'=4, 'Mod'=5, 'Min2'=6, 'Min1'=7, 'Typ'=8)
data$Functional <- as.integer(revalue(data$Functional,functional))
table(data$Functional)
#---------------------------------------------------------------------------
paveddrive<-c('N'=1, 'P'=2, 'Y'=3)
data$Paved.Drive<-as.integer(revalue(data$Paved.Drive, paveddrive))
table(data$Paved.Drive)
#---------------------------------------------------------------------------
data$Sale.Type <- as.factor(data$Sale.Type )
table(data$Sale.Type)

data$Sale.Condition <- as.factor(data$Sale.Condition)
table(data$Sale.Condition)

data$Yr.Sold <- as.factor(data$Yr.Sold)
table(data$Yr.Sold)

data$Mo.Sold<- as.factor(data$Mo.Sold)
table(data$Mo.Sold)
#---------------------------------------------------------------------------
#split data to numeric and categorical variables
index <- (sapply(data, class) == "integer") |(sapply(data, class) == "numeric")
datanum <- data[,index]
datafact <- data[,!index]

ncol(datanum) # 52 numeric variables
ncol(datafact) # 28 categorical variables

#----------------------------------------------------------------------------#
#                                                                            #
#                             univariate analysis                            #
#                                                                            #
#----------------------------------------------------------------------------#

#--------------------------------------------------------------#
#                   descriptive statistics
#--------------------------------------------------------------#

require(summarytools)
dsc <- round(describe(datanum),2)
View(dsc)

#we found that garage.yr.build has a maximum value 2027. this is not reasonable
#we are going to replace it with the year.remod.add 

row<-which((datanum$Garage.Yr.Blt)==2207)
datanum$Garage.Yr.Blt[row]<-datanum$Year.Remod.Add[row]

#--------------Separate Discrete variables-------------------------------------
datadiscrete<-subset(datanum,select=c(Garage.Yr.Blt,Bsmt.Full.Bath,Bsmt.Half.Bath,Full.Bath,Half.Bath,
                                      Bedroom.AbvGr,Kitchen.AbvGr,Kitchen.Qual,Fireplaces,Fireplace.Qu,
                                      Garage.Cars,Year.Built,Year.Remod.Add,Lot.Shape, Land.Slope, Overall.Cond, Exter.Qual,
                                      Exter.Cond,Bsmt.Qual,Bsmt.Cond,Bsmt.Exposure,BsmtFin.Type.1,BsmtFin.Type.2,
                                      Heating.QC,TotRms.AbvGrd, Functional, Garage.Finish, Garage.Qual,Garage.Cond,
                                      Paved.Drive, Pool.QC,Overall.Qual))

#--------------Separate Continuous variables-----------------------------------
datanumeric<-subset(datanum,select=-c(Garage.Yr.Blt,Bsmt.Full.Bath,Bsmt.Half.Bath,Full.Bath,Half.Bath,
                                      Bedroom.AbvGr,Kitchen.AbvGr,Kitchen.Qual,Fireplaces,Fireplace.Qu,
                                      Garage.Cars,Year.Built,Year.Remod.Add,Lot.Shape, Land.Slope, Overall.Cond, Exter.Qual,
                                      Exter.Cond,Bsmt.Qual,Bsmt.Cond,Bsmt.Exposure,BsmtFin.Type.1,BsmtFin.Type.2,
                                      Heating.QC,TotRms.AbvGrd, Functional, Garage.Finish, Garage.Qual,Garage.Cond,
                                      Paved.Drive, Pool.QC,Overall.Qual))

#find the mode for categorical variables and mean for numeric
meanvalues<-sapply(datanumeric,mean)
meanvalues<-as.data.frame(round(meanvalues,2))

Mode = function(x){
  tab = table(x)
  maxim = max(tab)
  if (all(tab == maxim))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(tab)[tab == maxim])
  else
    mod = names(tab)[tab == maxim]
  return(mod)
}
modefact<-rep(NA,ncol(datafact))
for (i in 1:ncol(datafact)){
  modefact[i]<-Mode(datafact[,i])
}
modefact<-cbind(colnames(datafact),modefact)

modediscrete<-rep(NA,ncol(datadiscrete))
for (i in 1:ncol(datadiscrete)){
  modediscrete[i]<-Mode(datadiscrete[,i])
}
modediscrete<-cbind(colnames(datadiscrete),modediscrete)

#--------------------------------------------------------------#
#                     Visual Analysis
#--------------------------------------------------------------#

#useful visuals for report
options(scipen = 999)

par(mfrow=c(1,3))
hist(datanum$SalePrice,main="SalePrice",xlab="",ylab="Frequency",col=5,cex.axis=1.5,cex.lab=1.5)
hist(datanum$Year.Built,main="Year.Built",xlab="",ylab="Frequency",col=5,cex.axis=1.5,cex.lab=1.5)
hist(datanum$Gr.Liv.Area,main="Gr.Liv.Area",xlab="",ylab="Frequency",col=5,cex.axis=1.5,cex.lab=1.5)

par(mfrow=c(1,2))
barplot(table(datafact$Mo.Sold),main="Mo.Sold",xlab="",ylab="Frequency", col="brown",cex.axis=1.5,cex.lab=1.5)
barplot(sort(table(datafact$Neighborhood)),xlim=c(0,250),horiz=TRUE,las=1,ylab=names(datafact$Neighborhood), col="brown",cex.axis=1,cex.names=0.5, main="Neighborhoods of Houses")

#---------------discrete variables-----------
n<-nrow(datadiscrete)
par(mfrow=c(2,3)); 

for (i in 1:ncol(datadiscrete)) { 
  plot(table(datadiscrete[,i])/n, type='h', main=names(datadiscrete)[i], ylab='Relative frequency',col="deepskyblue4")
}

#-------------continuous variables-----------

par(mfrow=c(2,3));
for (i in 1:ncol(datanumeric)) { 
  hist(datanumeric[,i], main=names(datanumeric)[i], xlab="",col="deepskyblue4")
}

par(mfrow=c(2,3));
for (i in 1:ncol(datanumeric)) { 
  qqnorm(datanumeric[,i],main=names(datanumeric)[i],col="magenta")
  qqline(datanumeric[,i], col="black")
}

#----------categorical variables------------


#Visual Analysis for factors 
par(mfrow=c(2,3)); 

for (i in 1:ncol(datafact)){
  barplot(table(datafact[,i]),ylab=names(datafact)[i], col="brown",cex.axis=1.5,cex.lab=1.5)
}

#----------------------------------------------------------------------------# 
#                                                                            #
#                              Bivariate analysis                            #
#                                                                            #
#----------------------------------------------------------------------------#

#------------Pairwise comparisons-----------

#numeric variables
require(corrplot)
head(datanumeric)
par(mfrow=c(2,3))
for (i in 1:(ncol(datanumeric)-1)){
  plot(datanumeric[,i],datanum$SalePrice,ylab="SalePrice",xlab=names(datanumeric)[i],col="magenta")
}

n<-which(colnames(datanum)=='SalePrice')
x<-cor(datanum$SalePrice, datanum[,-n])
x <- x[,order(x[1,])]
barplot(x, horiz=T, las = 1, xlim=c(-0.4,0.8), cex.axis=1, cex.names=0.5, col="mediumorchid1",)
#overallqual has the strongest correlation with saleprice

cor_numVar <- cor(datanum, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.53)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
length(CorHigh)

#drop variables with cor>0.80
#drop garage.cars, Garage.Yr.Blt, Garage.Cond, Pool.Area, BsmtFin.SF.2

datanum<-subset(datanum,select=-c(Garage.Yr.Blt, Garage.Cars,
                                  Garage.Cond,Pool.Area,
                                  BsmtFin.SF.2))

#--------------------Saleprice~categorical variables-------------------------------

#SalePrice (our response) on factor variables 
par(mfrow=c(2,3))
for(i in 1:ncol(datafact)){
  boxplot(datanum$SalePrice~datafact[,i], xlab=names(datafact)[i], ylab='SalePrice',cex.lab=1.5, col=4)
}
#----------------------------------------------------------------------------------
#Saleprice - Neighborhood
options(scipen = 0)
#Tests for k>2 independent samples - 1 quantitative & 1 categorical factor
anova<-aov(data$SalePrice~data$Neighborhood)
summary(anova)
#Can we assume normality?
require(nortest)
shapiro.test(anova$res)
lillie.test(anova$res)
qqnorm(anova$res)
qqline(anova$res)
#no 
#large sample?
#yes
#is the mean a sufficient descriptive measure for central location for all groups?
require(lawstat)
symmetry.test(anova$res)
#no
#test for equality of medians (kruskall-wallis test)
kruskal.test(data$SalePrice~data$Neighborhood)
#reject Ho 
boxplot(data$SalePrice~data$Neighborhood,ylab="SalePrice",xlab="",main="Neighborhood",boxcol=1,boxfill=2,las=2,medlwd=3,medcol="black")
#---------------------------------------------------------------------------------
#Saleprice - YrSold
#Tests for k>2 independent samples - 1 quantitative & 1 categorical factor
anova<-aov(data$SalePrice~data$Yr.Sold)
summary(anova)
#Can we assume normality?
require(nortest)
shapiro.test(anova$res)
lillie.test(anova$res)
qqnorm(anova$res)
qqline(anova$res)
#no 
#large sample?
#yes
#is the mean a sufficient descriptive measure for central location for all groups?
require(lawstat)
symmetry.test(anova$res)
#nO
#test for equality of medians (kruskall-wallis test)
kruskal.test(data$SalePrice~data$Yr.Sold)
#we do not reject Ho 
boxplot(data$SalePrice~data$Yr.Sold, ylab="SalePrice",xlab="",main="Yr.Sold",boxcol=1,boxfill=2,medlwd=3,medcol="black")
#---------------------------------------------------------------------------------------
#Sales - Street
#test for 2 independent samples - 1 quantitative & 1 binary
#can we assume the normality?
table(data$Street)
by(data$SalePrice,data$Street,shapiro.test)
by(data$SalePrice,data$Street, lillie.test)
#no
#large samples?
#yes
#is the mean a sufficient descriptive measure for central location for both groups?
symmetry.test(data$SalePrice)
#no
#test for zero difference between the medians
wilcox.test(data$SalePrice~data$Street)
#we reject Ho: M1=M2 significance difference is found about the median of the saleprice between grvl and pave
boxplot(data$SalePrice~data$Street,ylab="SalePrice",xlab="",main="Street",boxfill=2,medlwd=3)
#---------------------------------------------------------------------------------------
#Sales -Central Air
#test for 2 independent samples - 1 quantitative & 1 binary
#can we assume the normality?
table(data$Central.Air)
by(data$SalePrice,data$Central.Air,shapiro.test)
by(data$SalePrice,data$Central.Air, lillie.test)
#no
#large samples?
#yes
#is the mean a sufficient descriptive measure for central location for both groups?
symmetry.test(data$SalePrice)
#no
#test for zero difference between the medians
wilcox.test(data$SalePrice~data$Central.Air)
#we reject Ho: M1=M2 significance difference is found 
boxplot(data$SalePrice~data$Central.Air,ylab="SalePrice",xlab="",main="Central.Air",boxfill=2,medlwd=3)
par(mfrow=c(2,2))

#----------------------------------------------------------------------------# 
#                                                                            #
#                                 Model Selection                            #
#                                                                            #
#----------------------------------------------------------------------------#

table(data$sa)
require(fastDummies)
datafact <- dummy_cols(datafact, remove_selected_columns = TRUE,remove_first_dummy = TRUE)
colnames(datafact)
sapply(datafact,as.numeric)
data<-cbind(datanum,datafact)
ncol(data)
#after dummy creation variables are 235

# Dropping highly correlated variables
cor_numVar <- cor(data, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.25)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
length(CorHigh)

data<-subset(data,select=c(SalePrice, Overall.Qual, Gr.Liv.Area, Exter.Qual,Kitchen.Qual,             
                           Total.Bsmt.SF, X1st.Flr.SF, 
                           Garage.Area, Bsmt.Qual,Year.Built, Garage.Finish,
                           Year.Remod.Add, Full.Bath,             
                           Fireplace.Qu, Foundation_PConc, Mas.Vnr.Area, 
                           TotRms.AbvGrd, Fireplaces, Heating.QC,             
                           BsmtFin.SF.1, Bsmt.Exposure, Neighborhood_NridgHt,   
                           Lot.Frontage, MS.SubClass_60, Garage.Type_Attchd,     
                           Sale.Type_New, Sale.Condition_Partial, Exterior.1st_VinylSd,   
                           Exterior.2nd_VinylSd, Wood.Deck.SF, BsmtFin.Type.1,         
                           Neighborhood_NoRidge, Mas.Vnr.Type_Stone, Open.Porch.SF,          
                           Paved.Drive, Central.Air_Y, Garage.Qual,            
                           X2nd.Flr.SF, Half.Bath, Roof.Style_Hip,         
                           Bsmt.Full.Bath, MS.Zoning_RM,           
                           Lot.Shape, Foundation_CBlock, Garage.Type_Detchd,
                           Mas.Vnr.Type_None ))     
ncol(data)      


#we end up with 46 variables


#-----------------------------------#
#                                   #
#             LASSO                 #
#                                   #
#-----------------------------------#

mfull <- lm(data$SalePrice ~ . ,data)
n<-which(colnames(data)=="SalePrice")

require(glmnet)
X <- model.matrix(mfull)[,-n]

lasso <- glmnet(X, data$SalePrice)
plot(lasso, xvar = "lambda", label = T)

#Use cross validation to find a reasonable value for lambda - 10-fold CV
lasso1 <- cv.glmnet(X, data$SalePrice, alpha = 1)
lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se
plot(lasso1, cex.lab = 2, cex.axis = 2)

coef(lasso1, s = "lambda.min")
coef(lasso1, s = "lambda.1se") #keep lasso with 64 variables

coefs <- as.matrix(coef(lasso1)) # convert to a matrix (618 by 1)
ix <- which(abs(coefs[,1]) > 0)
length(ix)
coefs<-coefs[ix,1, drop=FALSE]
coefs
plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)

#-----model with 25 variables of lasso---------
model<-lm(SalePrice~Overall.Qual+Gr.Liv.Area+Exter.Qual+Kitchen.Qual+
            Total.Bsmt.SF+X1st.Flr.SF+Garage.Area+Bsmt.Qual+
            Year.Built+Year.Remod.Add+Fireplace.Qu+Foundation_PConc+
            Mas.Vnr.Area+Fireplaces+Heating.QC+BsmtFin.SF.1+Bsmt.Exposure+
            Neighborhood_NridgHt+Lot.Frontage+Sale.Type_New+Sale.Condition_Partial+
            Wood.Deck.SF+Neighborhood_NoRidge+Lot.Shape,data)


#-----------------------------------#
#                                   #
#             STEPWISE              #
#                                   #
#-----------------------------------#

step<-step(model, direction='both') #Stepwise

#after stepwise model has left with 19 variables


#--------------------------------------------------------------------#
#                   Model1  - Initial variables                      #
#--------------------------------------------------------------------#

model1<-lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Exter.Qual + Kitchen.Qual + 
             Total.Bsmt.SF + X1st.Flr.SF + Garage.Area + Year.Built + 
             Foundation_PConc + Mas.Vnr.Area + Fireplaces + Heating.QC + 
             BsmtFin.SF.1 + Bsmt.Exposure + Neighborhood_NridgHt + Lot.Frontage + 
             Sale.Condition_Partial + Wood.Deck.SF + Neighborhood_NoRidge + 
             Lot.Shape, data=data)
options(scipen = 0)

summary(model1)


#------foundation.pcon not significant


model1<-lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Exter.Qual + Kitchen.Qual + 
             Total.Bsmt.SF + X1st.Flr.SF + Garage.Area + Year.Built + 
             Mas.Vnr.Area + Fireplaces + Heating.QC + 
             BsmtFin.SF.1 + Bsmt.Exposure + Neighborhood_NridgHt + Lot.Frontage + 
             Sale.Condition_Partial + Wood.Deck.SF + Neighborhood_NoRidge + 
             Lot.Shape, data=data)

summary(model1)

#exlude constant 
model1<-lm(SalePrice ~ -1 + Overall.Qual + Gr.Liv.Area + Exter.Qual + Kitchen.Qual + 
             Total.Bsmt.SF + X1st.Flr.SF + Garage.Area + Year.Built + 
             Mas.Vnr.Area + Fireplaces + Heating.QC + 
             BsmtFin.SF.1 + Bsmt.Exposure + Neighborhood_NridgHt + Lot.Frontage + 
             Sale.Condition_Partial + Wood.Deck.SF + Neighborhood_NoRidge + 
             Lot.Shape, data=data)

summary(model1)

true.r2 <- 1-sum(model1$res^2)/((1500-1)*var(data$SalePrice))
true.r2
#radj lower so keep the constant 

#after stepwise the model left with 19 variables

#--------------------------------------------#
#      Checking the assumptions of model1    #
#--------------------------------------------#

par(mfrow=c(2,2),pch=16,bty='l')

# --------------------------------------------
#                   normality
# --------------------------------------------

plot(model1,which=2,cex=1.5,col='blue',main="Normality Assumption")
#normality is rejected

# --------------------------------------------
#                Homoscedasticity 
# --------------------------------------------

Stud.residuals <- rstudent(model1)
yhat <- fitted(model1)
plot(yhat, Stud.residuals, main="Homoscedasticity assumption")
abline(h=c(-2,2),lty=2,col='red',lwd=2)

plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2,lwd=2)
# ------------------
library(car)
ncvTest(model1)
# ------------------
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(model1)~yhat.quantiles)
boxplot(rstudent(model1)~yhat.quantiles)

# ------------------------------------------
#                non linearity
# ------------------------------------------

library(car)
residualPlot(model1, type='rstudent', main="Linearity assumption")
residualPlots(model1, plot=F, type = "rstudent")

# ------------------------------------------
#                Independence 
# ------------------------------------------
plot(rstudent(model1), type='l',main="Independence Assumption")
library(randtests); runs.test(model1$res)
library(car); durbinWatsonTest(model1)
# ------------------------------------------
#             multicollinearity 
# ------------------------------------------
library(car)
vif(model1)
alias(model1)
#no collinearity problems

# ------------------------------------------
#cook's distance
plot(model1,pch=16,cex=2,col='blue',which=4)
abline(h=4/5,col='red',lty=2,lwd=2)
# ------------------------------------------


#----------------------------------------------------------------------------#
#                                                                            #
#                             evaluate model1                                #
#                                                                            #
#----------------------------------------------------------------------------#

require(caret)
options(scipen = 0)

#--------------------------------------------------------#
#        Leave one out cross validation - LOOCV          #
#--------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model1a <- train(SalePrice ~ Overall.Qual + Gr.Liv.Area + Exter.Qual + Kitchen.Qual + 
                   Total.Bsmt.SF + X1st.Flr.SF + Garage.Area + Year.Built + 
                   Mas.Vnr.Area + Fireplaces + Heating.QC + 
                   BsmtFin.SF.1 + Bsmt.Exposure + Neighborhood_NridgHt + Lot.Frontage + 
                   Sale.Condition_Partial + Wood.Deck.SF + Neighborhood_NoRidge + 
                   Lot.Shape, data=data, method = "lm", trControl = train.control)
# Summarize the results
print(model1a)
#RMSE 27901.26  

#--------------------------------------------------------#
#               10-fold cross-validation                 #
#--------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model1b <- train(SalePrice ~ Overall.Qual + Gr.Liv.Area + Exter.Qual + Kitchen.Qual + 
                   Total.Bsmt.SF + X1st.Flr.SF + Garage.Area + Year.Built + 
                   Mas.Vnr.Area + Fireplaces + Heating.QC + 
                   BsmtFin.SF.1 + Bsmt.Exposure + Neighborhood_NridgHt + Lot.Frontage + 
                   Sale.Condition_Partial + Wood.Deck.SF + Neighborhood_NoRidge + 
                   Lot.Shape, data=data, method = "lm",trControl = train.control)
# Summarize the results
print(model1b)
#RMSE 27283.15  


#--------------------------------------------------------------------#
#                   Model2  - Log(SalePrice)                         #
#--------------------------------------------------------------------#

par(mfrow=c(1,2))
qqnorm(data$SalePrice, col="blue",main="SalePrice")
qqline(data$SalePrice, lwd = 2)
qqnorm(log(data$SalePrice), col="blue",main="Log(SalePrice)")
qqline(log(data$SalePrice), lwd = 2)

model2<-lm(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + Exter.Qual + Kitchen.Qual + 
             Total.Bsmt.SF + X1st.Flr.SF + Garage.Area + Year.Built + 
             Mas.Vnr.Area + Fireplaces + Heating.QC + 
             BsmtFin.SF.1 + Bsmt.Exposure + Neighborhood_NridgHt + Lot.Frontage + 
             Sale.Condition_Partial + Wood.Deck.SF + Neighborhood_NoRidge + 
             Lot.Shape, data=data)

summary(model2)

#drop exter.qual, X1st.Flr.SF, mas.vnr.area,Neighborhood_NridgHt,Neighborhood_NoRidge

model2<-lm(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + Kitchen.Qual + 
             Total.Bsmt.SF +  Garage.Area + Year.Built + 
              Fireplaces + Heating.QC + 
             BsmtFin.SF.1 + Bsmt.Exposure + Lot.Frontage + 
             Sale.Condition_Partial + Wood.Deck.SF + 
             Lot.Shape, data=data)

summary(model2)

#-------------------------------------------------#
#             Checking the assumptions            #
#-------------------------------------------------#

par(mfrow=c(2,2))
# ------------------------------------------------
#                   normality
# ------------------------------------------------

plot(model2,which=2,cex=1.5,col='blue')
#normality is rejected

# ------------------------------------------------
#                Homoscedasticity 
# ------------------------------------------------

Stud.residuals <- rstudent(model2)
yhat <- fitted(model2)
plot(yhat, Stud.residuals)
abline(h=c(-2,2),lty=2,col='red',lwd=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2,lwd=2)

# ------------------
library(car)
ncvTest(model2)
# ------------------
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(model2)~yhat.quantiles)
boxplot(rstudent(model2)~yhat.quantiles)

# -----------------------------------------------
#                  non linearity
# -----------------------------------------------

library(car)
residualPlot(model2, type='rstudent')
residualPlots(model2, plot=F, type = "rstudent")

# -----------------------------------------------
#                  Independence 
# -----------------------------------------------

plot(rstudent(model2), type='l')
library(randtests); runs.test(model2$res)
library(car); durbinWatsonTest(model2)

# -----------------------------------------------
#               multicollinearity 
# -----------------------------------------------

library(car)
vif(model2)
alias(model2)
#no collinearity problems

#-------------------
#cook's distance
plot(model2,pch=16,cex=2,col='blue',which=4)
abline(h=4/5,col='red',lty=2,lwd=2)
#--------------------------------------------------

#--------------------------------------------------------#
#        Leave one out cross validation - LOOCV          #
#--------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model2a <- train(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + Kitchen.Qual + 
                   Total.Bsmt.SF +  Garage.Area + Year.Built + 
                   Fireplaces + Heating.QC + 
                   BsmtFin.SF.1 + Bsmt.Exposure + Lot.Frontage + 
                   Sale.Condition_Partial + Wood.Deck.SF + 
                   Lot.Shape, data=data, method = "lm", trControl = train.control)
# Summarize the results
print(model2a)
#RMSE 0.1457159  

#--------------------------------------------------------#
#                10-fold cross-validation                #
#--------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model2b <- train(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + Kitchen.Qual + 
                   Total.Bsmt.SF +  Garage.Area + Year.Built + 
                   Fireplaces + Heating.QC + 
                   BsmtFin.SF.1 + Bsmt.Exposure + Lot.Frontage + 
                   Sale.Condition_Partial + Wood.Deck.SF + 
                   Lot.Shape, data=data, method = "lm",trControl = train.control)
# Summarize the results
print(model2b)
#RMSE 0.1420607 

#--------------------------------------------------------------------#
#                           Model3                                   #
#--------------------------------------------------------------------#

#--------------------------------------------------------#
#                  Log and Polynomials                   #
#--------------------------------------------------------#

options(scipen = 0)
data2<-data[-c(787),]
model3<-lm(log(SalePrice) ~ poly(Overall.Qual,5)+
             poly(Gr.Liv.Area,2)+ Kitchen.Qual + Total.Bsmt.SF + 
             poly(Garage.Area,4)+
             Year.Built + Fireplaces + Heating.QC + 
             BsmtFin.SF.1 + Bsmt.Exposure  + 
             poly(Lot.Frontage,3) + 
             Sale.Condition_Partial + Wood.Deck.SF  + 
             Lot.Shape, data=data2)

summary(model3)

# model with centered covariates
#---------------------------------------------------------------------
data2centered <- as.data.frame(scale(data2, center = TRUE, scale = F))
data2centered$SalePrice<-data2$SalePrice
model3centered<-lm(log(SalePrice) ~ poly(Overall.Qual,5)+
             poly(Gr.Liv.Area,2)+ Kitchen.Qual + Total.Bsmt.SF + 
             poly(Garage.Area,4)+
             Year.Built + Fireplaces + Heating.QC + 
             BsmtFin.SF.1 + Bsmt.Exposure  + 
             poly(Lot.Frontage,3) + 
             Sale.Condition_Partial + Wood.Deck.SF  + 
             Lot.Shape, data=data2centered)
summary(model3centered)

#-------------------------------------------------#
#           Checking the assumptions              #
#-------------------------------------------------#

par(mfrow=c(2,2))
# ------------------------------------------------
#                  normality
# ------------------------------------------------

plot(model3,which=2,cex=1.5,col='blue', main="Normality Assumption")
#normality is rejected

# ------------------------------------------------
#                Homoscedasticity 
# ------------------------------------------------

Stud.residuals <- rstudent(model3)
yhat <- fitted(model3)
plot(yhat, Stud.residuals,main="Homoscedasticity Assumption")
abline(h=c(-2,2),lty=2,col='red',lwd=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2,lwd=2)
# ------------------
library(car)
ncvTest(model3)
# ------------------
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(model3)~yhat.quantiles)
boxplot(rstudent(model3)~yhat.quantiles)

# -----------------------------------------------
#                 non linearity
# -----------------------------------------------

library(car)
residualPlot(model3, type='rstudent', main="Linearity Assumption")
residualPlots(model3, plot=F, type = "rstudent")

# -----------------------------------------------
#                   Independence 
# -----------------------------------------------

plot(rstudent(model3), type='l',main="Independence Assumption")
library(randtests); runs.test(model3$res)
library(car); durbinWatsonTest(model3)

# -----------------------------------------------
#                 multicollinearity 
# -----------------------------------------------
library(car)
vif(model3)
alias(model3)
#no collinearity problems

# -----------------------------------------------
#cook's distance
plot(model3,pch=16,cex=2,col='blue',which=4)
abline(h=4/5,col='red',lty=2,lwd=2)
#--------------------------------------------------


#--------------------------------------------------------#
#         Leave one out cross validation - LOOCV         #
#--------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model3a <- train(log(SalePrice) ~ poly(Overall.Qual,5)+
                   poly(Gr.Liv.Area,2)+ Kitchen.Qual + Total.Bsmt.SF + 
                   poly(Garage.Area,4)+
                   Year.Built + Fireplaces + Heating.QC + 
                   BsmtFin.SF.1 + Bsmt.Exposure  + 
                   poly(Lot.Frontage,3) + 
                   Sale.Condition_Partial + Wood.Deck.SF  + 
                   Lot.Shape, data=data2,
                   method = "lm", trControl = train.control)
# Summarize the results
print(model3a)
#RMSE 0.1356781  

#--------------------------------------------------------#
#                 10-fold cross-validation               #
#--------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model3b <- train(log(SalePrice) ~ poly(Overall.Qual,5)+
                   poly(Gr.Liv.Area,2)+ Kitchen.Qual + Total.Bsmt.SF + 
                   poly(Garage.Area,4)+
                   Year.Built + Fireplaces + Heating.QC + 
                   BsmtFin.SF.1 + Bsmt.Exposure  + 
                   poly(Lot.Frontage,3) + 
                   Sale.Condition_Partial + Wood.Deck.SF  + 
                   Lot.Shape, data=data2,
                   method = "lm",trControl = train.control)
# Summarize the results
print(model3b)
#RMSE 0.1339403      

#--------------------------------------------------------------#
#                       TEST DATA FROM FILE                    #
#--------------------------------------------------------------#

setwd("C:/Users/Eva/Desktop/aueb/Statistics 1/Main Assignment Ntzoufras")
testdata<-read.csv(file = "ames_iowa_housing_test.csv", header= TRUE, sep=";")

#keep only the remaining variables after lasso 
testdata<-subset(testdata,select=c(SalePrice, Overall.Qual , Gr.Liv.Area , Exter.Qual , Kitchen.Qual , 
                                   Total.Bsmt.SF , X1st.Flr.SF , Garage.Area , Year.Built , 
                                   Foundation, Mas.Vnr.Area , Fireplaces , Heating.QC , 
                                   BsmtFin.SF.1 , Bsmt.Exposure , Neighborhood , Lot.Frontage , 
                                   Sale.Condition , Wood.Deck.SF, 
                                   Lot.Shape))

#-----find missing values and define the correct type as train data
#-------------------------------------------------------------------------------
NAs <- which(colSums(is.na(testdata)) > 0)
sort(colSums(sapply(testdata[NAs], is.na)), decreasing = TRUE)
length(NAs)
#6 variables have NAs
#-------------------------------------------------------------------------------
testdata$Bsmt.Exposure[is.na(testdata$Bsmt.Exposure)] <- 'NA'
BsmtExposure <- c('NA'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
testdata$Bsmt.Exposure<-as.integer(revalue(testdata$Bsmt.Exposure, BsmtExposure))
table(testdata$Bsmt.Exposure)
#-------------------------------------------------------------------------------
for (i in 1:nrow(testdata)){
  if(is.na(testdata$Lot.Frontage[i]==TRUE)){
    testdata$Lot.Frontage[i] <- as.integer(median(testdata$Lot.Frontage[testdata$Neighborhood==testdata$Neighborhood[i]], na.rm=TRUE)) 
  }}
#-------------------------------------------------------------------------------
testdata$Mas.Vnr.Area[is.na(testdata$Mas.Vnr.Area)] <-0
#-------------------------------------------------------------------------------
testdata$Garage.Area[is.na(testdata$Garage.Area)]<-0
i<-which(is.na(testdata$Lot.Frontage))
require(modeest)
testdata$Lot.Frontage[is.na(testdata$Lot.Frontage)] <- mlv(testdata$Lot.Frontage[-i], method = "mfv")
#-------------------------------------------------------------------------------
which(is.na(testdata))
#-------------------------------------------------------------------------------
testdata$Sale.Condition <- as.factor(testdata$Sale.Condition)
table(testdata$Sale.Condition)
#-------------------------------------------------------------------------------
ExterQual <- c('Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
testdata$Exter.Qual<-as.integer(revalue(testdata$Exter.Qual, ExterQual))
#-------------------------------------------------------------------------------
kitchenqual <- c('Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
testdata$Kitchen.Qual<-as.integer(revalue(testdata$Kitchen.Qual, kitchenqual))
table(testdata$Kitchen.Qual)
#-------------------------------------------------------------------------------
HeatingQC <- c('Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
testdata$Heating.QC<-as.integer(revalue(testdata$Heating.QC, HeatingQC))
#-------------------------------------------------------------------------------
Lot.Shape<-c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)
testdata$Lot.Shape<-as.integer(revalue(testdata$Lot.Shape,Lot.Shape ))
table(testdata$Lot.Shape)
#-------------------------------------------------------------------------------
indextestdata <- (sapply(testdata, class) == "integer") |(sapply(testdata, class) == "numeric")
datanumtestdata <- testdata[,indextestdata]
datafacttestdata <- testdata[,!indextestdata]

#-------------------------------------------------------------------------------
#                               Create Dummies
#-------------------------------------------------------------------------------

require(fastDummies)
datafacttestdata <- dummy_cols(datafacttestdata, remove_selected_columns = TRUE,remove_first_dummy = TRUE)
colnames(datafacttestdata)
sapply(datafacttestdata,as.numeric)
testdata<-cbind(datanumtestdata,datafacttestdata)

#-------------------------------------------------------------------------------
#keep only those which included in model1
testdata<-subset(testdata,select=c(SalePrice, Overall.Qual , Gr.Liv.Area , Exter.Qual , Kitchen.Qual , 
                                     Total.Bsmt.SF , X1st.Flr.SF , Garage.Area , Year.Built , 
                                     Foundation_PConc , Mas.Vnr.Area , Fireplaces , Heating.QC , 
                                     BsmtFin.SF.1 , Bsmt.Exposure , Neighborhood_NridgHt , Lot.Frontage , 
                                     Sale.Condition_Partial , Wood.Deck.SF , Neighborhood_NoRidge , 
                                     Lot.Shape))

require(caret)

#----------------------------------------------------------------------------#
#                                                                            #
#                             evaluate model1                                #
#                                                                            #
#----------------------------------------------------------------------------#

options(scipen = 0)

#--------------------------------------------------------------#
#          Leave one out cross validation - LOOCV              #
#--------------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model1aa <- train(SalePrice ~ Overall.Qual + Gr.Liv.Area + Exter.Qual + Kitchen.Qual + 
                    Total.Bsmt.SF + X1st.Flr.SF + Garage.Area + Year.Built + 
                    Mas.Vnr.Area + Fireplaces + Heating.QC + 
                    BsmtFin.SF.1 + Bsmt.Exposure + Neighborhood_NridgHt + Lot.Frontage + 
                    Sale.Condition_Partial + Wood.Deck.SF + Neighborhood_NoRidge + 
                    Lot.Shape, data=testdata, method = "lm", trControl = train.control)
# Summarize the results
print(model1aa)
#RMSE  43684.65 

#--------------------------------------------------------------#
#                   10-fold cross-validation                   #
#--------------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model1bb <- train(SalePrice ~ Overall.Qual + Gr.Liv.Area + Exter.Qual + Kitchen.Qual + 
                    Total.Bsmt.SF + X1st.Flr.SF + Garage.Area + Year.Built + 
                    Mas.Vnr.Area + Fireplaces + Heating.QC + 
                    BsmtFin.SF.1 + Bsmt.Exposure + Neighborhood_NridgHt + Lot.Frontage + 
                    Sale.Condition_Partial + Wood.Deck.SF + Neighborhood_NoRidge + 
                    Lot.Shape, data=testdata, method = "lm",trControl = train.control)
# Summarize the results
print(model1bb)
#RMSE 41080.06     

#----------------------------------------------------------------------------#
#                                                                            #
#                             evaluate model2                                #
#                                                                            #
#----------------------------------------------------------------------------#

#--------------------------------------------------------------#
#         Leave one out cross validation - LOOCV               #
#--------------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model2aa <- train(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + Kitchen.Qual + 
                    Total.Bsmt.SF +  Garage.Area + Year.Built + 
                    Fireplaces + Heating.QC + 
                    BsmtFin.SF.1 + Bsmt.Exposure + Lot.Frontage + 
                    Sale.Condition_Partial + Wood.Deck.SF + 
                    Lot.Shape, data=testdata, method = "lm", trControl = train.control)
# Summarize the results
print(model2aa)
#RMSE 0.2166  

#--------------------------------------------------------------#
#                 10-fold cross-validation                     #
#--------------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model2bb <- train(log(SalePrice) ~ Overall.Qual + Gr.Liv.Area + Kitchen.Qual + 
                    Total.Bsmt.SF +  Garage.Area + Year.Built + 
                    Fireplaces + Heating.QC + 
                    BsmtFin.SF.1 + Bsmt.Exposure + Lot.Frontage + 
                    Sale.Condition_Partial + Wood.Deck.SF + 
                    Lot.Shape, data=testdata, method = "lm",trControl = train.control)
# Summarize the results
print(model2bb)
#RMSE 0.2046978  

#----------------------------------------------------------------------------#
#                                                                            #
#                             evaluate model3                                #
#                                                                            #
#----------------------------------------------------------------------------#

#--------------------------------------------------------------#
#            Leave one out cross validation - LOOCV            #
#--------------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model3aa <- train(log(SalePrice) ~ poly(Overall.Qual,5)+
                    poly(Gr.Liv.Area,2)+ Kitchen.Qual + Total.Bsmt.SF + 
                    poly(Garage.Area,4)+
                    Year.Built + Fireplaces + Heating.QC + 
                    BsmtFin.SF.1 + Bsmt.Exposure  + 
                    poly(Lot.Frontage,3) + 
                    Sale.Condition_Partial + Wood.Deck.SF  + 
                    Lot.Shape, data=testdata, method = "lm", trControl = train.control)
# Summarize the results
print(model3aa)
#RMSE 0.1929567    

#--------------------------------------------------------------#
#                   10-fold cross-validation                   #
#--------------------------------------------------------------#

set.seed(2822026)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model3bb <- train(log(SalePrice) ~ poly(Overall.Qual,5)+
                    poly(Gr.Liv.Area,2)+ Kitchen.Qual + Total.Bsmt.SF + 
                    poly(Garage.Area,4)+
                    Year.Built + Fireplaces + Heating.QC + 
                    BsmtFin.SF.1 + Bsmt.Exposure  + 
                    poly(Lot.Frontage,3) + 
                    Sale.Condition_Partial + Wood.Deck.SF  + 
                    Lot.Shape, data=testdata, method = "lm",trControl = train.control)
# Summarize the results
print(model3bb)
#RMSE 0.1876193  Rsquared  0.7915076  