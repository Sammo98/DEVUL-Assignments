################################### DATA AND LIBRARIES ##################################

### Import Libraries
                                
library(scales)
library(lattice)
library(vcd)
library(gplots)

### Import Plastic Data (saved locally) and Assign as "df"

df = plastic

# Look at Data

head(df)

# Check For missing

for (i in 1:ncol(df)){
  print(sum(is.na(df[,i])))
}


##### 1. Exploration two plastic waste per capita distributions, including outliers #####

par(mfrow=c(1,2))

### Plastic Hist

hist(df$PlasticWaste, breaks = 20, main = "",
     xlab = "Plastic Waste Per Capita (kg/day)", col = "cornflowerblue")
rug(df$PlasticWaste)

### Plastic Box

boxplot(
  df$PlasticWaste,
  col = "cornflowerblue", outcol = "red", pch = 16,
  main = "",
  ylab = "Plastic Waste Per Capita (kg/day)"
    )
mtext("Plastic Waste Per Capita Distribution - Histogram and Boxplot",
      side = 3, line = -2, outer = TRUE, cex=1.5)


# Outlier statistics for colouring

stats_plastic = fivenum(df$PlasticWaste)
iqr = stats_plastic[4] - stats_plastic[2]
outlier = stats_plastic[4]+1.5*iqr
extreme_outlier= stats_plastic[4]+3*iqr

outliers = df$Country[(which(df$PlasticWaste > outlier))]
extreme_outliers = df$Country[(which(df$PlasticWaste > extreme_outlier))]

### Plastic Scatter

colours = c(rep("black", nrow(df)))
colours[c(which(df$PlasticWaste >123))] <- "orange"
colours[c(which(df$PlasticWaste > 187.63))] <- "red"

dev.off()

plot(df$PlasticWaste, pch = 16, col = colours, main = "Distribution of Plastic Waste by Country",
     xlab = "Country Index No. in Data Frame", ylab = "Plastic Waste Per Capita (kg/day)")
legend(1,310, legend = c("Normal Observations", "Outliers", "Extreme Outliers"),
       fill = c("Black", "Orange", "Red"),
       text.font =4, cex = 0.60)

### Mismanaged Hist

par(mfrow=c(1,2))

hist(df$MismanagedPW,breaks = 20, main = "",
     xlab = "Mismanged Plastic Waste Per Capita (kg/day)", col = "cornflowerblue")
rug(df$MismanagedPW)

### Mismanged Box

boxplot(
  df$MismanagedPW,
  col = "cornflowerblue", outcol = "red", pch = 16,
  main = "",
  ylab = "Mismanaged Plastic Waste Per Capita (kg/day)"
)
mtext("Mismanaged Plastic Waste Per Capita Distribution - Histogram and Boxplot",
      side = 3, line = -2, outer = TRUE, cex=1.5)

stats_mpw = fivenum(df$MismanagedPW)
iqr = stats_mpw[4] - stats_mpw[2]
outlier = stats_mpw[4]+1.5*iqr
extreme_outlier= stats_mpw[4]+3*iqr

### Mismanged Scatter

colours = c(rep("black", nrow(df)))
colours[c(which(df$MismanagedPW > 44.01))] <- "orange"
colours[c(which(df$MismanagedPW > 68.55))] <- "red"

dev.off()

plot(df$MismanagedPW, pch = 16, col = colours, main = "Distribution of Mismanaged Plastic Waste",
     xlab = "Country Index in Data Frame", ylab = "Mismanaged Plastic Waste Per Capita (kg/day)")
legend(160,140, legend = c("Normal Observations", "Outliers", "Extreme Outliers"),
       fill = c("Black", "Orange", "Red"),
       text.font =4, cex = 0.60)

##### 2. Exploration of the distributions of plastic waste by region and income status #####

### Region

region_matrix = matrix(nrow = 2, ncol = 7)
colnames(region_matrix) <- levels(df$Region)
rownames(region_matrix) <- c("Plastic Waste", "Mismanaged Plastic Waste")
region_matrix[1,] = xtabs(PlasticWaste~Region, data = df)
region_matrix[2,] = xtabs(MismanagedPW~Region, data = df)

bp_waste_region = barplot(
  region_matrix,
  beside = T,
  col = c("cornflowerblue", "red"),
  legend = rownames(region_matrix),
  main = "Amount of Plastic Waste and Mismanged Plastic Waste Produced Per Capita by Region",
  xlab = "Region", ylab = "Waste Produced Per Capita (kg/day)",
  cex.names = 0.7
)
text(bp_waste_region, 0,region_matrix,cex=0.8,pos = 3) 

### Income Status

income_matrix = matrix(nrow = 2, ncol = 4)
colnames(income_matrix) <- levels(df$IncomeStatus)
rownames(income_matrix) <- c("Plastic Waste", "Mismanaged Plastic Waste")
income_matrix[1,] = xtabs(PlasticWaste~IncomeStatus, data = df)
income_matrix[2,] = xtabs(MismanagedPW~IncomeStatus, data = df)

bp_waste_income = barplot(
  income_matrix,
  beside = T,
  col = c("cornflowerblue", "red"),
  legend = rownames(income_matrix),
  args.legend = list(x = "topleft", inset=c(0.05, 0)),
  main = "Amount of Plastic Waste and Mismanged Plastic Waste Produced Per Capita by Income Status",
  xlab = "Region", ylab = "Waste Produced Per Capita (kg/day)"
)
text(bp_waste_income, 0,income_matrix,cex=1,pos = 3) 

##### 3. Relationships between variables, general goal to explain/understand plastic waste  in terms of other quantities ####

cont_variables = df[,c(4,6,7,8,9,10)]

### Scatter plot Matrix

colours = rep("black", nrow(df))
colours <- alpha(colours,0.4)
pairs(cont_variables, col = colours, pch = 16)

# Pop Outliers

colours[which(df$Population>500000000)] <- "red"
pairs(cont_variables, col = colours, pch = 16)

# GP Outlier

colours[which(df$GDP>125000)] <- "red"
pairs(cont_variables, col = colours, pch = 16,
      main = "Scatter Plot Matrix (SPLOM) for all Continuous Variables")

# Plastic Waste Outliers

colours[which(df$PlasticWaste>300)] <- "red"
pairs(cont_variables, col = colours, pch = 16)

# Panels

pairs(cont_variables, col = colours, pch = 16,
             main = "Scatter Plot Matrix (SPLOM) for all Continuous Variables")


### Plastic Waste against Mismanaged PW

plot(df$PlasticWaste, df$MismanagedPW, pch = 16,
     main = "Scatter Plot of Plastic Waste against Mismanaged Plastic Waste",
     xlab = "Plastic Waste Per Capita (kg/day)", 
     ylab = "Mismanaged Plastic Waste Per Capita (kg/day)"
)
segments(0,0,x1=150, y1=150, col = "red")
segments(0,0,x1=350, y1=7, col = "red")

### Loess GDP v. Urban Pop PC

lfit <- loess(GDP~UrbanPopPC, data=df, span=0.66) 
xs <- seq(min(df$UrbanPopPC), max(df$UrbanPopPC), length=200)

lpred <- predict(lfit, data.frame(UrbanPopPC=xs), se = TRUE)
upr <- lpred$fit + 1.95*lpred$se.fit
lwr <- lpred$fit - 1.95*lpred$se.fit

plot(x=df$UrbanPopPC, y=df$GDP, xlab="% of Population Living in Urban Environment",
     ylab="GDP", main = "Scatter Plot of UrbanPopPC Against GDP with LOESS Curve",
     pch=16)
lines(x=xs, y=lpred$fit,col='red',lwd=4)
lines(x=xs, y=upr,col='red',lwd=1, lty=2)
lines(x=xs, y=lwr,col='red',lwd=1, lty=2)

### Urban Pop vs Plastic Waste with Loess Regression

lfit <- loess(PlasticWaste~UrbanPopPC, data=df, span=0.66) 
xs <- seq(min(df$UrbanPopPC), max(df$UrbanPopPC), length=200)

lpred <- predict(lfit, data.frame(UrbanPopPC=xs), se = TRUE)
upr <- lpred$fit + 1.95*lpred$se.fit
lwr <- lpred$fit - 1.95*lpred$se.fit

plot(x=df$UrbanPopPC, y=df$PlasticWaste, xlab="% of Population Living in Urban Environment",
     ylab="Plastic Waste Per Capita (kg/day)", 
     main = "Scatter Plot of UrbanPopPC Against PlasticWaste with LOESS Curve",
     pch=16)
lines(x=xs, y=lpred$fit,col='red',lwd=4)
lines(x=xs, y=upr,col='red',lwd=1, lty=2)
lines(x=xs, y=lwr,col='red',lwd=1, lty=2)




### Effect of population

par(mfrow=c(1,2))

colours_parallel = rep("black", nrow(df))
colours_parallel <- alpha(colours_parallel,0.4)

plot(x=df$Population, y = df$PlasticWaste, pch = 16, col = colours_parallel,
     xlab ="Population Size", ylab= "Plastic Waste Per Capita (kg/day)"
)

plot(x=df$Population, y = df$MismanagedPW, pch = 16,col = colours_parallel,
     xlab ="Population Size", ylab = ''
)
mtext("Mismanaged Plastic Waste Per Capita (kg/day)", side=2, line=2.2, cex=0.80)
mtext("Scatter Plots showing Population against both Plastic Waste and Mismanaged Plastic Waste",
      side = 3, line = -2, outer = TRUE, cex=0.95)

dev.off()

### Parallel Coordinate Plots

parallelplot(cont_variables,horizontal=FALSE, col = colours,
             main = 'Parallel Coordinate Plot for Continuous Variabes',
             xlab = "Variables")



############# 4. Impact of Region and Income Status on those relationships######

#### Waste against Waste

par(mfrow= c(1,2))

# Region

plot(df$PlasticWaste, df$MismanagedPW, pch = 16, col = df$Region,
     xlab = "Plastic Waste Per Capita (kg/day)",
     ylab = "Mismanaged Plastic Waste Per Capita (kg/day)")
legend(x='topright', legend=levels(df$Region), pch=16, col=1:nlevels(df$Region), 
        cex = 0.8, text.width = 100)

# Income

plot(df$PlasticWaste, df$MismanagedPW, pch = 16, col = df$IncomeStatus,
     xlab = "Plastic Waste Per Capita (kg/day)",
     ylab = "Mismanaged Plastic Waste Per Capita (kg/day)")
legend(x='topright', legend=levels(df$IncomeStatus), pch=16, col=1:nlevels(df$IncomeStatus),
       cex = 0.8)

# Title

mtext("Scatter Plots showing Plastic Waste against Mismanaged Plastic Waste by Region and Income Status",
      side = 3, line = -2, outer = TRUE, cex=1.5)


####GDP against UrbanPopPC

# LOESS

lfit <- loess(GDP~UrbanPopPC, data=df, span=0.66) 
xs <- seq(min(df$UrbanPopPC), max(df$UrbanPopPC), length=200)
lpred <- predict(lfit, data.frame(UrbanPopPC=xs), se = TRUE)
upr <- lpred$fit + 1.95*lpred$se.fit
lwr <- lpred$fit - 1.95*lpred$se.fit

# Region

plot(x=df$UrbanPopPC, y=df$GDP, xlab="% of Population Living in Urban Environment",
     ylab="GDP",
     pch=16, col = df$Region)
lines(x=xs, y=lpred$fit,col='red',lwd=4)
lines(x=xs, y=upr,col='red',lwd=1, lty=2)
lines(x=xs, y=lwr,col='red',lwd=1, lty=2)
legend(x=30,y=175000, legend=levels(df$Region), pch=16, col=1:nlevels(df$Region), 
       cex = 0.8, text.width = 32)

# Income 

plot(x=df$UrbanPopPC, y=df$GDP, xlab="% of Population Living in Urban Environment",
     ylab="GDP",
     pch=16, col = df$IncomeStatus)
lines(x=xs, y=lpred$fit,col='red',lwd=4)
lines(x=xs, y=upr,col='red',lwd=1, lty=2)
lines(x=xs, y=lwr,col='red',lwd=1, lty=2)
legend(x=30,y=175000, legend=levels(df$IncomeStatus), pch=16, col=1:nlevels(df$IncomeStatus), 
       cex = 0.8, text.width = 16)

# Title

mtext("Scatter Plots showing Urban Population Percentage against GDP by Region and Income Status",
      side = 3, line = -2, outer = TRUE, cex=1.5)


#### Urban Pop PC against Plastic Waste

par(mfrow=c(1,2))

# LOESS

lfit <- loess(PlasticWaste~UrbanPopPC, data=df, span=0.66) 
xs <- seq(min(df$UrbanPopPC), max(df$UrbanPopPC), length=200)
lpred <- predict(lfit, data.frame(UrbanPopPC=xs), se = TRUE)
upr <- lpred$fit + 1.95*lpred$se.fit
lwr <- lpred$fit - 1.95*lpred$se.fit

# Region

plot(x=df$UrbanPopPC, y=df$PlasticWaste, xlab="% of Population Living in Urban Environment",
     ylab="Plastic Waste Per Capita (kg/day)",
     pch=16, col = df$Region)
lines(x=xs, y=lpred$fit,col='red',lwd=4)
lines(x=xs, y=upr,col='red',lwd=1, lty=2)
lines(x=xs, y=lwr,col='red',lwd=1, lty=2)
legend(x="topright", legend=levels(df$Region), pch=16, col=1:nlevels(df$Region), 
       cex = 0.6, text.width = 25)

# Income 

plot(x=df$UrbanPopPC, y=df$PlasticWaste, xlab="% of Population Living in Urban Environment",
     ylab="Plastic Waste Per Capita (kg/day)",
     pch=16, col = df$IncomeStatus)
lines(x=xs, y=lpred$fit,col='red',lwd=4)
lines(x=xs, y=upr,col='red',lwd=1, lty=2)
lines(x=xs, y=lwr,col='red',lwd=1, lty=2)
legend(x="topright", legend=levels(df$IncomeStatus), pch=16, col=1:nlevels(df$IncomeStatus), 
       cex = 0.7, text.width = 8)

# Title

mtext("Scatter Plots showing Urban Population Percentage against Plastic Waste Per Capita by Region and Income Status",
      side = 3, line = -2, outer = TRUE, cex=1.3)

#### Parallel Coordinate Plot

# Region

p1 = parallelplot(cont_variables,horizontal=FALSE, col = df$Region)
# Income Status

p2 = parallelplot(cont_variables,horizontal=FALSE, col = df$IncomeStatus)

# Print with Grid

grid::textGrob(expression(frac(abs( bold(c) ),abs( bold(b) ))), 
               gp = gpar(fontsize = 25))
print(p1, position = c(0.0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1), more = TRUE)
trellis.focus("toplevel") 
panel.text(0, 0.02, "Variables", cex = 1.2, font = 2)
panel.text(0, 0.98, "Parallel Coordinate Plots Grouped by Region and Income Status", cex = 1.2, font = 2)
trellis.unfocus()


############## 5. Any other insightful discoveries not covered above ##########

dev.off()


### Income Status and Region

income_region_table = xtabs(~IncomeStatus + Region, data = df)

bp_income_region = barplot(income_region_table, beside=TRUE, col=2:nlevels(df$IncomeStatus),
                           xlab = "Regions", ylab = "Frequency",
                           main= "Grouped Barplot Showing the Number of Countries in Each Income Status Level by Region",
                           cex.names = 0.7)
legend(x=14, y= 35, legend=levels(df$IncomeStatus), pch=16, col=2:nlevels(df$IncomeStatus), 
       cex = 0.7, text.width = 8)
text(bp_income_region, 0,income_region_table,cex=0.7,pos = 3)


### Mean of Each Region

region_matrix_mean = matrix(nrow = 2, ncol = 7)
colnames(region_matrix_mean) <- levels(df$Region)
rownames(region_matrix_mean) <- c("Plastic Waste", "Mismanaged Plastic Waste")

for (i in 1:nlevels(df$Region)){
  
  region = levels(df$Region)[i]
  
  subsetdf = df[c(df$Region == region),]
  
  region_matrix_mean[1,i] = round(mean(subsetdf$PlasticWaste, na.rm = T), digits = 1)
  region_matrix_mean[2,i] = round(mean(subsetdf$MismanagedPW, na.rm = T), digits = 1)
  
}
  

bp_mean_region = barplot(
  region_matrix_mean,
  beside = T,
  col = c("cornflowerblue", "red"),
  legend = rownames(region_matrix_mean),
  main = "Mean Plastic Waste and Mismanged Plastic Waste Produced Per Capita by Region",
  xlab = "Region", ylab = "Mean Waste Produced Per Capita (kg/day)",
  args.legend = list(text.width = 4.5),
  cex.names = 0.7
)
text(bp_mean_region, 0,region_matrix_mean,cex=0.8,pos = 3) 

