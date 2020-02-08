#Setting the directory
setwd("C:/Users/Shravya/Documents/College Stuff/Data Analytics/dataAnalyticsProject/Cleaned")


#Loading the data
gdpnc <- read.csv("GDPInPercent.csv")
netdebtnc <- read.csv("netDebtToGDP.csv")


#Converting to transpose
trgdpnc <- t(gdpnc)
trnetdebtnc <- t(netdebtnc)


#Converting each transpose from character to numeric data types
z<-matrix(0,10,94)
for ( i in 1:10)
{
	for(j in 1:94)
	{
		z[i,j] <- as.numeric(trgdpnc[(i+1),j])
	}
}

zz <- matrix(0,10,94)
for ( i in 1:10)
{
	for(j in 1:94)
	{
		zz[i,j] <- as.numeric(trnetdebtnc[(i+1),j])
	}
}

#Next I am going to fill in the values of Net Debt for Greece, Austria, South Sudan and
#Nigeria using imputeTS

library(imputeTS)
fullNetDebt <- na.interpolation(zz)


#Fill in the values of GDP for South Sudan
fullGDP <- na.interpolation(z)


#Find the correlation matrix between countries for GDP
corMatGDP <- matrix(0,94,94)
for( i in 1:94)
{
	for(j in 1:94)
	{
		corMatGDP[i,j]<-(cor(fullGDP[,i], fullGDP[,j]))
	}
} 


#See which country Syria is most similar to in terms of GDP
maxValue <- corMatGDP[1,83]
country <- 0
for( i in 2:94)
{
	if(i!=83)
	{
		if(corMatGDP[i,83] > maxValue)
		{
			maxValue <- corMatGDP[i,83]
			country <- i
		}
	}
}


#Filling in missing data values for Syria
for( i in 1:10)
{
	if(fullGDP[i,83] == 0)
	{
		fullGDP[i,83] = fullGDP[i,country]
	}
} 


#Find the correlation matrix between countries for Net Debt
corMatNDebt <- matrix(0,94,94)
for( i in 1:94)
{
	for(j in 1:94)
	{
		corMatNDebt[i,j]<-(cor(fullNetDebt[,i], fullNetDebt[,j]))
	}
} 


#See which country Syria is most similar to in terms of Net Debt
maxValue <- corMatNDebt[1,83]
country2N <- 0
for( i in 2:94)
{
	if(i!=83)
	{
		if(corMatNDebt[i,83] > maxValue)
		{
			maxValue <- corMatNDebt[i,83]
			country2N <- i
		}
	}
}


#Filling in missing data values for Syria
for( i in 1:10)
{
	if(fullNetDebt[i,83] == 0)
	{
		fullNetDebt[i,83] = fullNetDebt[i,country2N]
	}
} 

#NOW ALL THE DATA VALUES IN TERMS OF NATIONAL CURRENCY HAVE BEEN LOADED AND THE MISSING 
#VALUES HAVE BEEN ACCOUNTED FOR


#FORMULATING THE FEATURE VECTORS

fvmat <- matrix(0,94,2)
for(i in 1:94)
{
	sumgdp <- 0
	sumnd <- 0
	for(j in 1:10)
	{
		sumgdp <- sumgdp + fullGDP[j,i]
		sumnd <- sumnd + fullNetDebt[j,i]	
	}
	avggdp <- sumgdp/10
	avgnd <- sumnd/10
	fvmat[i,1] = avggdp
	fvmat[i,2] = avgnd
}



#LOADING THE PARAMETER DATA 

po1 <- read.csv("p1_Population.csv")
po2 <- read.csv("p2_CurrentAccountBalance.csv")
po3 <- read.csv("p3_GeneralGovernmentRevenue.csv")
po4 <- read.csv("p4_unemploymentRate.csv")
po5 <- read.csv("p5_volOfExports.csv")
po6 <- read.csv("p6_volOfImports.csv")
po7 <- read.csv("p7_grossNationalSavings.csv")
po8 <- read.csv("p8_totalInvestment.csv")


#Find the transpose of the data

hp1 <- t(po1)
hp2 <- t(po2)
hp3 <- t(po3)
hp4 <- t(po4)
hp5 <- t(po5)
hp6 <- t(po6)
hp7 <- t(po7)
hp8 <- t(po8)

#Converting each transpose from character to numeric data types
p1<-matrix(0,10,94)
p2<-matrix(0,10,94)
p3<-matrix(0,10,94)
p4<-matrix(0,10,94)
p5<-matrix(0,10,94)
p6<-matrix(0,10,94)
p7<-matrix(0,10,94)
p8<-matrix(0,10,94)
for ( i in 1:10)
{
	for(j in 1:94)
	{
		p1[i,j] <- as.numeric(hp1[(i+1),j])
		p2[i,j] <- as.numeric(hp2[(i+1),j])
		p3[i,j] <- as.numeric(hp3[(i+1),j])
		p4[i,j] <- as.numeric(hp4[(i+1),j])
		p5[i,j] <- as.numeric(hp5[(i+1),j])
		p6[i,j] <- as.numeric(hp6[(i+1),j])
		p7[i,j] <- as.numeric(hp7[(i+1),j])
		p8[i,j] <- as.numeric(hp8[(i+1),j])
	}
}

#FILLING IN THE MISSING VALUES THROUGH INTERPOLATION
p2c <- na.interpolation(p2)
p3c <- na.interpolation(p3)
p4c <- na.interpolation(p4)

p6c <- na.interpolation(p6)
p7c <- na.interpolation(p7)
p8c <- na.interpolation(p8)


#NOW THAT THE MISSING VALUES HAVE BEEN FILLED, CALCULATE THE FEATURE VECTOR 
#MATRIX FOR THE PARAMETERS


pfv <- matrix(0,94,8)
for(i in 1:94)
{
	sump1 <- 0
	sump2 <- 0
	sump3 <- 0
	sump4 <- 0
	sump5 <- 0
	sump6 <- 0
	sump7 <- 0
	sump8 <- 0
	for(j in 1:10)
	{
		sump1 <- sump1 + p1[j,i]
		sump2 <- sump2 + p2c[j,i]
		sump3 <- sump3 + p3c[j,i]
		sump4 <- sump4 + p4c[j,i]
		sump5 <- sump5 + p5[j,i]
		sump6 <- sump6 + p6c[j,i]
		sump7 <- sump7 + p7c[j,i]
		sump8 <- sump8 + p8c[j,i]	
	}
	avgp1 <- sump1/10
	avgp2 <- sump2/10
	avgp3 <- sump3/10
	avgp4 <- sump4/10
	avgp5 <- sump5/10
	avgp6 <- sump6/10
	avgp7 <- sump7/10
	avgp8 <- sump8/10
	pfv[i,1] = avgp1
	pfv[i,2] = avgp2
	pfv[i,3] = avgp3
	pfv[i,4] = avgp4
	pfv[i,5] = avgp5
	pfv[i,6] = avgp6
	pfv[i,7] = avgp7
	pfv[i,8] = avgp8
}

#print(pfv)


#NOW THE FEATURE VECTOR MATRIX FOR THE PARAMETERS HAVE BEEN CALCULATED

#PCA
 pfv_new<-scale(pfv)
 s<-cov(pfv)
 r<-cor(pfv_new)
 pca_job1<-princomp(s,cor=FALSE)
 pca_job2<-princomp(r,cor=TRUE)
 plot(pca_job1,type="lines")
 plot(pca_job2,type="lines")
 pca_job1.m=as.matrix(pca_job1)
 pfv.m<-as.matrix(pfv.m)
for ( i in 1:8)
{
	for(j in 1:8)
	{
		s[i,j] <- as.numeric(s[i,j])
	}
}
 scores<-pfv.m%*%(eigen(s)$vector[,1:2])
 plot(scores[,1], scores[,2], main="PC score plot")
 scores<-pfv.m%*%(eigen(s)$vector[,1:2])
 plot(scores[,1], scores[,2], main="PC score plot")
 screeplot(pca_job1)
 screeplot(pca_job2)
 biplot(pca_job1,cex=0.8)
 biplot(pca_job2,cex=0.8)
#As we can see from the biplots PCA doesnt seem like a proper tool to analyse
#this dataset

#So we use another feature subset selection. LVQ(Learning Vector Quantization)
install.packages('mlbench')  
install.packages('caret')
library('mlbench')
library('caret')
corrplot(r,method=c("number"),title="cor matrix",diag=T)
extra<-read.csv('extra.csv')
pfv1<-pfv
pfv1<-cbind(pfv1,extra['Income'])
control<- trainControl(method="repeatedcv",number=10,repeats=3)
model<-train((Income)~.,data=pfv1,method="lvq",preProcess="scale",trControl=control)
imp <- varImp(model, scale=FALSE)
plot(imp)

write.csv(pfv, file = "yourfile.csv", row.names = FALSE)
#and then  copied 
#the important attributes selcted by lvq in impFeatures.csv 

pfv2<-read.csv('impFeatures.csv')
#then we read the file back to an R dataframe called pfv2

km <- kmeans(log(pfv2+10), 4, nstart = 25)
#we use log to scale out for better visualisation and we add 10 to all the values 
#so we dont get NAN since log(0)=NAN.
#4 iterations decided by the elbow method

write.csv(km$cluster, file = "yourfile2.csv", row.names = FALSE)
#we copy the cluster values to a file

plot(log(pfv2+10),col=km$cluster)
#this is the plot showing the clusters
#CORRELATION 

#fvmat is my feature vector containing feature vectors of Net Debt and GDP

write.csv(fvmat, file = "featureForCor.csv")

cluster1 <- read.csv("cluster1.csv")
cluster2 <- read.csv("cluster2.csv")
cluster3 <- read.csv("cluster3.csv")
cluster4 <- read.csv("cluster4.csv")

#print(cluster1)
for ( i in 1:10)
{
	for(j in 2:3)
	{
		cluster1[i,j] <- as.numeric(cluster1[i,j])
	}
}
for ( i in 1:15)
{
	for(j in 2:3)
	{
		cluster2[i,j] <- as.numeric(cluster2[i,j])
	}
}
for ( i in 1:nrow(cluster3))
{
	for(j in 2:3)
	{
		cluster3[i,j] <- as.numeric(cluster3[i,j])
	}
}
for ( i in 1:nrow(cluster4))
{
	for(j in 2:3)
	{
		cluster4[i,j] <- as.numeric(cluster4[i,j])
	}
}
#Find the correlation matrix for the clusters and display the final result
corclus1 <- 0
corclus1<-(cor(cluster1[,2], cluster1[,3]))
print("The correlation between Net Debt and GDP for Cluster 1")
print(corclus1)

corclus2 <- 0
corclus2<-(cor(cluster2[,2], cluster2[,3]))
print("The correlation between Net Debt and GDP for Cluster 2")
print(corclus2)

corclus3<- 0
corclus3<-(cor(cluster3[,2], cluster3[,3]))
print("The correlation between Net Debt and GDP for Cluster 3")
print(corclus3)

corclus4 <- 0
corclus4<-(cor(cluster4[,2], cluster4[,3]))
print("The correlation between Net Debt and GDP for Cluster 4")
print(corclus4)
