######################### DEVUL - ASSIGNMENT 2 ################################

######################  INSTALL LIBRARIES AND IMPORT DATA #####################


#install.packages("mice")
#install.packages("VIM")
#install.packages("psych")
#install.packages("factoextra")
#install.packages("FactoMineR")
#install.packages("tidyverse")
#install.packages("cluster")

library(mice)
library(VIM)
library(psych)
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(cluster)

# Read in CSV as df
df = read.csv("/Users/apple/Downloads/mali.csv", header = TRUE)

############################ EDA & OUTLIERS ###################################

# Pairs Plot
pairs.panels(df,
             method = "pearson", 
             hist.col = "cornflowerblue",
             density = T, 
             ellipses = F,
             pch = 16)

### Family v DistRD
par(mfrow=c(1,2))

# Change Colours for Outliers
colours = c(rep("black", nrow(df)))
colours[c(which(df$Family > 100))] <- "red"
colours[c(which(df$DistRD > 200))] <- "red"

plot(df$Family, df$DistRD,pch = 16,
     xlab = "Family - Total Number of Individuals in Household",
     ylab = "DistRD - Distance to Nearest Passable Road (km)",
     col = colours)

### DistRD v Cattle 

# Change Colours for Outliers
colours = c(rep("black", nrow(df)))
colours[c(which(df$DistRD >200))] <- "red"
colours[c(which(df$Cattle >80))] <- "red"

plot(df$DistRD, df$Cattle, pch = 16,
     xlab = "DistRd - Distance to Nearest Passable Road (km)",
     ylab = "Cattle - Total Number of Cattle",
     col = colours)


# Find Outliers
df[c(which(df$Family > 100)),] # row 25 
df[c(which(df$DistRD > 200)),] # row 69, 72 
df[c(which(df$Cattle >80)),] # row 34 

# Remove Outliers
df = df[-c(25,34,69,72),]

dev.off()

############################# MISSING DATA ####################################

sum(!complete.cases(df)) # 22 rows contain missing data: ~31%

# Missingness Proportion and Pattern
aggr(df, col=c("cornflowerblue", "red"), numbers=TRUE, sortVars=TRUE, labels=names(df),
     cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# Single Mean Imputation
mice_df = mice(df, m=1, maxit = 1, method = "mean")

# Density Plot
densityplot(mice_df)

# Assign
imputed_df = complete(mice_df, 1)

########################## DIMENSIONALITY REDUCTION ###########################

# Create components
pr_components <- prcomp(imputed_df, scale = TRUE)

# Summarise Components
summary(pr_components)

# Screeplot
fviz_screeplot(pr_components, addlabels = T, barfill = "cornflowerblue",
               barcolor = "black", main="")

# SD
std_dev_components = pr_components$sdev

# Variance
components_var = std_dev_components^2

# Variance Explained
var_components_explained = components_var/sum(components_var)

# Cumulatives Variance 
var_components_explained_cumsum = cumsum(var_components_explained)

# Cumulative Variance Plot
plot(var_components_explained_cumsum, 
     xlab= "Principal Component",ylab = "Cumulative Variance Explained by Principal Components",
     main = "",
     type = "b", pch = 16, )
abline(v=0, h=0.8, lty = 1, col = "red")
abline(v=0, h=0.9, lty = 1, col = "red") 


### Naming of Components -look at loadings for each component

for (i in 1:5){

print(c("Principal Component", i))
print(pr_components$rotation[,i])

}

########################### HIERARCHIACAL CLUSTERING ##########################

# Create 5 PC components
pca_results <- PCA(imputed_df, ncp = 5, graph = F)

# Hierarchical Clustering on the 5 PC components
hcpc_results = HCPC(pca_results, graph = F)

# Inertia Gain
inertia = hcpc_results$call$t$inert.gain

inertia_bp = barplot(inertia[1:10], col = "cornflowerblue",
                     names.arg = c("2","3","4","5","6","7","8","9","10","11"),
                     xlab = "Addition of Cluster Number", 
                     ylab = "Inertia Gain")
text(inertia_bp, 0,round(inertia[0:10],2),cex=1.0,pos = 3) 

# Dendrogram
fviz_dend(hcpc_results, 
          cex = 0.7, palette = "jco",               
          rect = TRUE, rect_fill = TRUE, rect_border = "jco", 
          main = "",
          xlab = "Observation Number"
)

# 3d plot
plot(hcpc_results, choice = "3D.map")

# Cluster
fviz_cluster(hcpc_results,
             repel = F,            
             show.clust.cent = TRUE,
             axes = 1:2,
             ggtheme = theme_minimal(),
             main = "", 
             xlab = "Dim 1 (44.4%) - Family and Farm Size Component",
             ylab =  "Dim 2 (15.4%) - Distance from Road, Crop Choice Component"
)

# Cluster Assignments
assignments = hcpc_results$call$X

clust1_observations = rownames(assignments[c(which(assignments$clust == 1)),])
clust2_observations = rownames(assignments[c(which(assignments$clust == 2)),])
clust3_observations = rownames(assignments[c(which(assignments$clust == 3)),])

# Biplot grouped by clusters
colours = rep("black", nrow(imputed_df))
colours[as.integer(clust1_observations)] <- "1"
colours[as.integer(clust2_observations)] <- "2"
colours[as.integer(clust3_observations)] <- "3"

fviz_pca_biplot(pr_components, axes = 1:2, col.ind = colours,title = "",
                xlab = "Dim 1 (44.4%) - Family and Farm Size Component",
                ylab = "Dim 2 (15.4%) - Distance from Road, Crop Choice Component"
                )

# Find Mean Values for Each Cluster from Original Variables

colMeans(imputed_df[clust1_observations,])
colMeans(imputed_df[clust2_observations,])
colMeans(imputed_df[clust3_observations,])


