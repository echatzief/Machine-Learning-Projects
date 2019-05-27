# We will do Principal Component analysis
# to two dataset that we will load.

# Install some libraries
#install.packages(c("FactoMineR", "factoextra"))

# Load the libraries we installed.
library("FactoMineR")
library("factoextra")

# Load the data from the csv files
firstDataset <- read.csv("./greek_regions_preprocessed.csv")
secDataset <- read.csv("./job_performance_preprocessed.csv")

# PCA with automatic data standarization
resFirstDataset <- PCA(firstDataset,graph = FALSE)
resSecDataset <- PCA(secDataset,graph = FALSE)
print(resFirstDataset)
print(resSecDataset)

#Get the eigenvalues of each dataset
firstDatasetEigenValues <- get_eigenvalue(resFirstDataset)
print(firstDatasetEigenValues)
secDatasetEigenValues <- get_eigenvalue(resSecDataset)
print(secDatasetEigenValues)

#Visualize the eigenvalues with Scree Plot
fviz_eig(resFirstDataset, addlabels = TRUE)
fviz_eig(resSecDataset, addlabels = TRUE)

# Graph of variables
firstVar <- get_pca_var(resFirstDataset)
secVar <- get_pca_var(resSecDataset)

#Coordinates
head(firstVar$coord)
head(secVar$coord)
# Cos2: quality on the factore map
head(firstVar$cos2)
head(secVar$cos2)
# Contributions to the principal components
head(firstVar$contrib)
head(secVar$contrib)

# Correlation circle
# Coordinates of variables
head(firstVar$coord, 4)
head(secVar$coord, 4)

#Plot Variables
fviz_pca_var(resFirstDataset, col.var = "black")
fviz_pca_var(resSecDataset, col.var = "black")

# Quality of representation
head(firstVar$cos2, 4)
head(secVar$cos2, 4)

# Visualize the cos2

# Install corrplot
#install.packages("corrplot")\

# Load corrplot
library(corrplot)


# Plot the cos2 with different ways
# For first dataset
corrplot(firstVar$cos2, is.corr=FALSE)
fviz_cos2(resFirstDataset, choice = "var", axes = 1:2)
fviz_pca_var(resFirstDataset, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
fviz_pca_var(resFirstDataset, alpha.var = "cos2")

# For second dataset
corrplot(secVar$cos2, is.corr=FALSE)
fviz_cos2(resSecDataset, choice = "var", axes = 1:2)
fviz_pca_var(resSecDataset, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
fviz_pca_var(resSecDataset, alpha.var = "cos2")

# Contributions of variables to PCs
head(firstVar$contrib,4)
head(secVar$contrib,4)

# Plot the contribution to the PC of each dataset
library(corrplot)
corrplot(firstVar$contrib,is.corr = FALSE)
corrplot(secVar$contrib,is.corr = FALSE)

# Plot the contribution of variables with bar plot

# Contributions of variables to PC1
fviz_contrib(resFirstDataset, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(resFirstDataset, choice = "var", axes = 2, top = 10)

# Contributions of variables to PC1
fviz_contrib(resSecDataset, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(resSecDataset, choice = "var", axes = 2, top = 10)

# Plot the total contribution
fviz_contrib(resFirstDataset, choice = "var", axes = 1:2, top = 10)
fviz_contrib(resSecDataset, choice = "var", axes = 1:2, top = 10)

# The most important (or, contributing) variables
fviz_pca_var(resFirstDataset, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_pca_var(resSecDataset, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Change the transparency by contrib values
fviz_pca_var(resFirstDataset, alpha.var = "contrib")
fviz_pca_var(resSecDataset, alpha.var = "contrib")

# Color by a custom continuous variable
# Create a random continuous variable of length 10
set.seed(123)
my.cont.var <- rnorm(16)
# Color variables by the continuous variable
fviz_pca_var(resFirstDataset, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

set.seed(123)
my.cont.var <- rnorm(14)
fviz_pca_var(resSecDataset, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")
# Color by groups

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
firstMean <- kmeans(firstVar$coord, centers = 3, nstart = 25)
grp <- as.factor(firstMean$cluster)

# Color variables by groups
fviz_pca_var(resFirstDataset, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
secMean <- kmeans(secVar$coord, centers = 3, nstart = 25)
grp <- as.factor(secMean$cluster)

# Color variables by groups
fviz_pca_var(resSecDataset, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

# Dimension description

firstDesc <- dimdesc(resFirstDataset, axes = c(1,2), proba = 0.05)
# Description of dimension 1
firstDesc$Dim.1
# Description of dimension 2
firstDesc$Dim.2

secDesc <- dimdesc(resSecDataset, axes = c(1,2), proba = 0.05)
# Description of dimension 1
secDesc$Dim.1
# Description of dimension 2
secDesc$Dim.2

## Graph of individuals

firstInd <- get_pca_ind(resFirstDataset)
secInd <- get_pca_ind(resSecDataset)

# Coordinates of individuals
head(firstInd$coord)
head(secInd$coord)
# Quality of individuals
head(firstInd$cos2)
head(secInd$cos2)
# Contributions of individuals
head(firstInd$contrib)
head(secInd$contrib)

# Plots: quality and contribution
fviz_pca_ind(resFirstDataset)

# Color by their cos2 values
fviz_pca_ind(resFirstDataset, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Change the point size according the cos2
fviz_pca_ind(resFirstDataset, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Change both point size and color by cos2
fviz_pca_ind(resFirstDataset, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Create a bar plot of the quality(cos2)
fviz_cos2(resFirstDataset, choice = "ind")

# Total contribution on PC1 and PC2
fviz_contrib(resFirstDataset, choice = "ind", axes = 1:2)

fviz_pca_ind(resSecDataset)

# Color by their cos2 values
fviz_pca_ind(resSecDataset, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Change the point size according the cos2
fviz_pca_ind(resSecDataset, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Change both point size and color by cos2
fviz_pca_ind(resSecDataset, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Create a bar plot of the quality(cos2)
fviz_cos2(resSecDataset, choice = "ind")

# Total contribution on PC1 and PC2
fviz_contrib(resSecDataset, choice = "ind", axes = 1:2)

# Color by groups

head(firstDataset,3)
head(secDataset,3)

# The first column will be used as grouping variable
firstDatasetPCA <- PCA(firstDataset[,-2],graph = FALSE)
secDatasetPCA <- PCA(secDataset[,-2],graph = FALSE)

# Dimensions

# Variables on dimensions 2 and 3
fviz_pca_var(resFirstDataset, axes = c(2, 3))
# Individuals on dimensions 2 and 3
fviz_pca_ind(resSecDataset, axes = c(2, 3))

# Plot elements: point, text, arrow

# Show variable points and text labels
fviz_pca_var(resFirstDataset, geom.var = c("point", "text"))

# Show variable points and text labels
fviz_pca_var(resSecDataset, geom.var = c("point", "text"))

# Show individuals text labels only
fviz_pca_ind(resFirstDataset, geom.ind =  "text")

# Show individuals text labels only
fviz_pca_ind(resSecDataset, geom.ind =  "text")

# Size and shape of plot elements

# Change the size of arrows an labels
fviz_pca_var(resFirstDataset, arrowsize = 1, labelsize = 5, 
             repel = TRUE)
# Change points size, shape and fill color
# Change labelsize
fviz_pca_ind(resFirstDataset, 
             pointsize = 3, pointshape = 21, fill = "lightblue",
             labelsize = 5, repel = TRUE)

# Change the size of arrows an labels
fviz_pca_var(resSecDataset, arrowsize = 1, labelsize = 5, 
             repel = TRUE)
# Change points size, shape and fill color
# Change labelsize
fviz_pca_ind(resSecDataset, 
             pointsize = 3, pointshape = 21, fill = "lightblue",
             labelsize = 5, repel = TRUE)

# Group mean points
fviz_pca_ind(firstDatasetPCA,
             geom.ind = "point", # show points only (but not "text")
             group.ind = firstDataset$IONIAN.ISLANDS, # color by groups
             legend.title = "Groups",
             mean.point = FALSE)
fviz_pca_ind(secDatasetPCA,
             geom.ind = "point", # show points only (but not "text")
             group.ind = secDataset$Problem.Solving, # color by groups
             legend.title = "Groups",
             mean.point = FALSE)

## Axis lines
fviz_pca_var(resFirstDataset, axes.linetype = "blank")
fviz_pca_var(resSecDataset, axes.linetype = "blank")

# Graphical parameters
firstDatasetInd <- fviz_pca_ind(firstDatasetPCA, geom = "point", col.ind = firstDataset$IONIAN.ISLANDS)
ggpubr::ggpar(firstDatasetInd,
              title = "Principal Component Analysis",
              subtitle = "Greek Regions",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "IONIAN.ISLANDS", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco"
)

secDatasetInd <- fviz_pca_ind(secDatasetPCA, geom = "point", col.ind = secDataset$Problem.Solving)
ggpubr::ggpar(secDatasetInd,
              title = "Principal Component Analysis",
              subtitle = "Job performance",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Problem.Solving", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco"
)

# Biplot

fviz_pca_biplot(firstDatasetPCA, 
                col.ind = firstDataset$IONIAN.ISLANDS, palette = "jco", 
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "IONIAN.ISLANDS") 

fviz_pca_biplot(secDatasetPCA, 
                col.ind = secDataset$Problem.Solving, palette = "jco", 
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Problem.Solving") 

# To customize individuals and variable colors, we use the helper functions fill_palette() and color_palette()
fviz_pca_biplot(firstDatasetPCA, 
                col.ind = firstDataset$IONIAN.ISLANDS, palette = "jco", 
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "IONIAN.ISLANDS")+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors 

fviz_pca_biplot(secDatasetPCA, 
                col.ind = secDataset$Problem.Solving, palette = "jco", 
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Problem.Solving")+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors 


# We'll change the transparency of variables by their contributions

fviz_pca_biplot(firstDatasetPCA, 
                # Individuals
                geom.ind = "point",
                fill.ind = firstDataset$IONIAN.ISLANDS, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = FALSE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "IONIAN.ISLANDS", color = "Contrib",
                                    alpha = "Contrib")
)

fviz_pca_biplot(secDatasetPCA, 
                # Individuals
                geom.ind = "point",
                fill.ind = secDataset$Problem.Solving, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = FALSE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Problem.Solving", color = "Contrib",
                                    alpha = "Contrib")
)

# Specification in PCA

firstDatasetPCA <- PCA(firstDataset, ind.sup = 24:27, 
               quanti.sup = 11:12, quali.sup = 13, graph=FALSE)
firstDatasetPCA$quanti.sup
fviz_pca_var(firstDatasetPCA)

secDatasetPCA <- PCA(secDataset, ind.sup = 24:27, 
                       quanti.sup = 11:12, quali.sup = 13, graph=FALSE)
secDatasetPCA$quanti.sup
fviz_pca_var(secDatasetPCA)

# Further customization

# Change color of variables
fviz_pca_var(resFirstDataset,
             col.var = "black",     # Active variables
             col.quanti.sup = "red" # Suppl. quantitative variables
)
# Hide active variables on the plot, 
# show only supplementary variables
fviz_pca_var(resFirstDataset, invisible = "var")
# Hide supplementary variables
fviz_pca_var(resFirstDataset, invisible = "quanti.sup")

# Change color of variables
fviz_pca_var(resSecDataset,
             col.var = "black",     # Active variables
             col.quanti.sup = "red" # Suppl. quantitative variables
)
# Hide active variables on the plot, 
# show only supplementary variables
fviz_pca_var(resSecDataset, invisible = "var")
# Hide supplementary variables
fviz_pca_var(resSecDataset, invisible = "quanti.sup")


# Plot of active variables
pFirst <- fviz_pca_var(firstDatasetPCA, invisible = "quanti.sup")
# Add supplementary active variables
fviz_add(pFirst, firstDatasetPCA$quanti.sup$coord, 
         geom = c("arrow", "text"), 
         color = "red")

# Individuals
resFirstDataset$ind.sup

# Plot of active variables
pSec <- fviz_pca_var(secDatasetPCA, invisible = "quanti.sup")
# Add supplementary active variables
fviz_add(pSec, secDatasetPCA$quanti.sup$coord, 
         geom = c("arrow", "text"), 
         color = "red")

# Individuals
firstDatasetPCA$ind.sup
secDatasetPCA$ind.sup

fviz_pca_ind(firstDatasetPCA, habillage = 13,
             addEllipses =FALSE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE) 

fviz_pca_ind(secDatasetPCA, habillage = 13,
             addEllipses =FALSE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE) 

# Scree plot
scree.plot <- fviz_eig(firstDatasetPCA)
# Plot of individuals
ind.plot <- fviz_pca_ind(firstDatasetPCA)
# Plot of variables
var.plot <- fviz_pca_var(firstDatasetPCA)

pdf("PCA_Greek_Regions.pdf") # Create a new pdf device
print(scree.plot)
print(ind.plot)
print(var.plot)
dev.off() # Close the pdf device

# Print scree plot to a png file
png("pca-scree-plot_Greek_Regions.png")
print(scree.plot)
dev.off()
# Print individuals plot to a png file
png("pca-variables_Greek_Regions.png")
print(var.plot)
dev.off()
# Print variables plot to a png file
png("pca-individuals_Greek_Regions.png")
print(ind.plot)
dev.off()

# Export individual plots to a pdf file 
library(ggpubr)

gexport(plotlist = list(scree.plot, ind.plot, var.plot), 
        nrow = 2, ncol = 2,
        filename = "PCA_Greek_Regions_2.pdf")
ggexport(plotlist = list(scree.plot, ind.plot, var.plot),
         filename = "PCA_Greek_Regions.png")


# Scree plot
scree.plot <- fviz_eig(secDatasetPCA)
# Plot of individuals
ind.plot <- fviz_pca_ind(secDatasetPCA)
# Plot of variables
var.plot <- fviz_pca_var(secDatasetPCA)

pdf("PCA_Job_Performance.pdf") # Create a new pdf device
print(scree.plot)
print(ind.plot)
print(var.plot)
dev.off() # Close the pdf device

# Print scree plot to a png file
png("pca-scree-plot_Job_Performance.png")
print(scree.plot)
dev.off()
# Print individuals plot to a png file
png("pca-variables_Job_Performance.png")
print(var.plot)
dev.off()
# Print variables plot to a png file
png("pca-individuals_Job_Performance.png")
print(ind.plot)
dev.off()

# Export individual plots to a pdf file 
library(ggpubr)

gexport(plotlist = list(scree.plot, ind.plot, var.plot), 
        nrow = 2, ncol = 2,
        filename = "PCA_Job_Performance_2.pdf")
ggexport(plotlist = list(scree.plot, ind.plot, var.plot),
         filename = "PCA_Greek_Regions.png")



# Export into a TXT file
write.infile(secDatasetPCA, "PCA_Job_Performance.txt", sep = "\t")
# Export into a CSV file
write.infile(secDatasetPCA, "PCA_Job_Performance.csv", sep = ";")

# Export into a TXT file
write.infile(firstDatasetPCA, "PCA_Greek_Regions.txt", sep = "\t")
# Export into a CSV file
write.infile(firstDatasetPCA, "PCA_Greek_Regions.csv", sep = ";")
