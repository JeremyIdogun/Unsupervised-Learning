#load libraries for cluster analysis 
library(cluster)

#import dataset into R
load("/Users/jeremyidogun/Library/CloudStorage/OneDrive-UniversityofHertfordshire
     /Data Analytics/Unsupervised Learning Assignment/wine.20058459.RData")

#summary statistics for the imported dataset
View(wine.20058459)
summary(wine.20058459)

#load dataset with all variables into a dataframe
#since there are no missing values in the dataset
df <- wine.20058459

#create distance matrix
df.dist <- daisy(df, metric = "gower")

#pam analysis for a 2 cluster solution
pam2.out<- pam(df.dist,2)
pam2.out$clusinfo

#pam analysis for a 3 cluster solution
pam3.out <- pam(df.dist, 3)
pam3.out$clusinfo

#pam analysis for a 4 cluster solution
pam4.out <- pam(df.dist, 4)
pam4.out$clusinfo

#pam analysis for a 5 cluster solution
pam5.out <- pam(df.dist, 5)
pam5.out$clusinfo

#pam analysis for a 6 cluster solution
pam6.out <- pam(df.dist, 6)
pam6.out$clusinfo

#pam analysis for a 7 cluster solution
pam7.out <- pam(df.dist, 7)
pam7.out$clusinfo

#pam analysis for a 8 cluster solution
pam8.out <- pam(df.dist, 8)
pam8.out$clusinfo

#hierarchical clustering using ward's method
ward.D2.out <- hclust(d=df.dist, method = "ward.D2")
plot(ward.D2.out)
abline(h=1.6,lty=2)
abline(h=0.75,lty=2)


#summary stats for a 3 cluster solution
#to obtain a mean score of all the continuous variables 
aggregate(df[,-8], by = list(cluster=pam3.out$clustering), mean)


#to obtain the standard deviation for some "variables of importance"
aggregate(df[, c("fixed.acidity","chlorides","total.sulphur.dioxide")], 
          by = list(cluster=pam3.out$clustering), sd)

#to obtain summary stats for the categorical variable
aggregate(df[,8], by = list(cluster=pam3.out$clustering),table)

