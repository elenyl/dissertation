## PCA and Factor analysis

setwd("~/R/Trinity/Session3") # 


## Example 1#########################################################################
datNoOmit = read.table("treasury_yields.txt",header=T) # import the dataset
diffdatNoOmit = diff(as.matrix(datNoOmit[,2:12])) # diff the data
dat=na.omit(datNoOmit) # omit all NAs in initial dataset
diffdat = na.omit(diffdatNoOmit) # omit all NAs in diff dataset
n = dim(diffdat)[1] # a number of observations

pca = prcomp(diffdat) # Performs a principal components analysis on the given data matrix and returns the results as an object of class prcomp.
summary(pca) # return the statistics



# look at the figures at home
par(mfrow=c(2,2))
time = c(1/12,.25,.5,1,  2, 3, 5, 7, 10, 20, 30) # maturity: X1mo X3mo X6mo X1yr X2yr X3yr X5yr X7yr X10yr X20yr X30yr (as in dat)
plot(time, as.vector(dat[1,2:12]),ylim=c(0,6),type="b",lty=1,lwd=2, ylab="Yield",xlab="T",main="(a)") #,log="x",xaxs="r")
lines(time,as.vector(dat[486,2:12]),type="b",lty=2,lwd=2,col="red")
lines(time,as.vector(dat[n+2,2:12]),type="b",lty=3,lwd=2,col="blue")
legend("bottomright",c("07/31/01","07/02/07","10/31/08"),lty=c(1,2,3),lwd=2, cex=1, col=c("black","red","blue"))

plot(pca,main="(b)")

plot(time,pca$rotation[,1],,ylim=c(-.8,.8),type="b",lwd=2,ylab="PC",xlab="T", main="(c)")
lines(time,pca$rotation[,2],lty=2,type="b",lwd=2,col="red")
lines(time,pca$rotation[,3],lty=3,type="b",lwd=2,col="blue")
lines(0:30,0*(0:30),lwd=1)
legend("bottomright",c("PC 1","PC 2","PC 3"),lty=c(1,2,3),lwd=2,col=c("black","red","blue"))


plot(time,pca$rotation[,1],ylim=c(-.8,.8),type="b",lwd=2,ylab="PC",xlab="T", xlim=c(0,3),main="(d)")
lines(time, pca$rotation[,2],lty=2,type="b",lwd=2,col="red")
lines(time, pca$rotation[,3],lty=3,type="b",lwd=2,col="blue")
lines(0:30,0*(0:30),lwd=1)
legend("bottomright",c("PC 1","PC 2","PC 3"),lty=c(1,2,3),lwd=2,col=c("black","red","blue"))



## Example 2: equity funds
equityFunds = read.csv("equityFunds.csv")
equityFunds[1:10,]
pairs(equityFunds[,2:9]) # Scatterplot Matrices
pcaEq = prcomp(equityFunds[,2:9])
summary(pcaEq)


par(mfrow=c(1,2))
plot(pcaEq,main="(a)")
Names = names(equityFunds)[2:9]
plot(pcaEq$rotation[,1],type="b",ylab="PC",lwd=2,ylim=c(-1.4,2),main="(b)")
lines(pcaEq$rotation[,2],type="b",lty=2,lwd=2,col="red")
lines(pcaEq$rotation[,3],type="b",lty=3,lwd=2,col="blue")
lines(0:9,0*(0:9))
legend("top",c("PC1","PC2","PC3"),lty=c(1,2,3),lwd=2,cex=.65,col=c("black", "red", "blue"))
text(4.35,-1.25, "   EASTEU   LATAM   CHINA   INDIA   ENERGY   MINING   GOLD   WATER",cex=.38)


##Exercise: the Dow Jones 30
DowJones30 = read.csv("DowJones30.csv")
pcaDJ = prcomp(DowJones30[,2:31])
summary(pcaDJ)


## FACTOR Analysis
CPI.dat = read.csv("CPI.dat.csv") # Inflation 
IP.dat = read.csv("IP.dat.csv") # Industrial production
berndtInvest = read.csv("berndtInvest.csv") #Monthly returns on  stocks  
berndt = as.matrix(berndtInvest[,-1])   #  1978-01-01 to 1987-12-01
CPI.dat = read.csv("CPI.dat.csv")
IP.dat = read.csv("IP.dat.csv")
berndt = as.matrix(berndtInvest[,-1])   #  1978-01-01 to 1987-12-01
CPI2 = as.matrix(CPI.dat$CPI[775:900]) #  1977-07-30  to 1987-12-31
CPI = as.data.frame(diff(log(CPI2)))  # log difference
names(CPI)[1]="CPI" #we name the column of the CPI dataframe
IP2 = as.matrix(IP.dat$IP)[703:828,]   #  1977-07-28 to 1987-12-28 
IP = as.data.frame(diff(log(IP2))) #calculte log difference 
names(IP)[1] = "IP"
CPI_IP = cbind(CPI,IP)

arFit = ar(cbind(CPI,IP)) # autoregressive model - by default selecting the complexity by AIC. Here it is AR(5)

res = arFit$resid[6:125,] # residuals of the AR (5) modle (the interpretations of the residuals is that they are unexpected shocks)
lmfit = lm(berndt[,2:10]~res[,1]+res[,2]) #fit a regression Y is a set of teh stock returns and X1 and X2 - residuals from the AR modles
slmfit = summary(lmfit) #summary of the regression

rsq = rep(0,9) #create a variable rsq with nine 0 values

for (i in 1:9){rsq[i]= slmfit[[i]][[8]]} # we substruct the values of R2 from each of 9 models
beta_CPI = lmfit$coef[2,] # extract all CPI betas (b1)
beta_IP = lmfit$coef[3,] # extract all IP betas (b2)

par(mfrow=c(1,3)) # building three graphs in a row
barplot(rsq,horiz=T,names=names(beta_CPI),main="R squared") #Creates a bar plot with vertical or horizontal bars.
barplot(beta_CPI,hori=T,main="beta CPI") #Creates a bar plot with vertical or horizontal bars.
barplot(beta_IP,hori=T,main="beta IP") #Creates a bar plot with vertical or horizontal bars.




# Example : Factor analysis of equity funds
equityFunds = read.csv("equityFunds.csv")
fa_none = factanal(equityFunds[,2:9],4,rotation="none") # 4 factor model without rotation
print(fa_none,cutoff=0.1) # By convention, any loading with an absolute value less than the parameter cutoff is not printed, and the default value of cutoff is 0.1



# VARIMAX rotation
fa_vari = factanal(equityFunds[,2:9],4,rotation="varimax") #factor model with rotation

print(fa_vari,cutoff=0.1,sort=T)
print(fa_vari,cutoff=0.1)
sum(fa_vari$loadings[,1]^2)
B=fa_vari$loadings[,] # factor loadings



# Going into details

library (psych) # the library for factor analysis
library (GPArotation) # to estimate oblimin rotation
equities = equityFunds[,2:9]
describe(equities) # general description of the data 

##Assessing the Factorability of the Data
#Bartlett's Test of Sphericity
cortest.bartlett(equities)

#KMO
KMO(equities)


##Determining the Number of Factors to Extract
# scree plot
scree(equities)

#Parallel Analysis
fa.parallel (equities) #


# estimation factor model
factor.model <- fa(equities, nfactors = 3, fm="ols", max.iter = 100, rotate = "oblimin")
# we estimate a factor model with 3 factors, estimated by means of OLS, 100 is the number of iterations or attempts to use when identifying the "best"" solution
# rotate - we apply oblimin rotation, allowing factors to correlate.


# make it visual
fa.diagram(factor.model) # 
# we can see that factor 1 is the common factor for GOld and mining equities
# Factor 2 affects energy, Latam and water equities
# and Factor 3 affects India and China
# it means that the respective groups of the equities have something in common. 

# Communality 
factor.model$communality

#Eeigenvalues
factor.model$e.values

#Percentage of Variance Accounted For
100*factor.model$e.values/length(factor.model$e.values)


print(factor.model$loadings, cutoff=0, digits=3)
print(factor.model$Structure, cutoff=0, digits=3)

