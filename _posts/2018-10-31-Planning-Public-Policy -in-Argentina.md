---
title: "Planning Public Policy in Argentina"
author: "Chris DeAngelis, CFA"
date: "8/29/2019"
output:
  pdf_document:
    toc: yes
  html_document:
    code_folding: hide
    toc: yes
    toc_float: yes
---

<style type="text/css">

h1.title {
  font-size: 38px;
  color: Black;
  text-align: center;
}
h4.author { /* Header 4 - and the author and data headers use this too  */
    font-size: 18px;
  font-family: "Times New Roman", Times, serif;
  color: Black;
  text-align: center;
}
h4.date { /* Header 4 - and the author and data headers use this too  */
  font-size: 18px;
  font-family: "Times New Roman", Times, serif;
  color: Black;
  text-align: center;
}
</style>
With almost 40 million inhabitants and a diverse geography that encompasses the Andes mountains, glacial lakes, and the Pampas grasslands, Argentina is the second largest country (by area) and has one of the largest economies in South America. It is politically organized as a federation of 23 provinces and an autonomous city, Buenos Aires.
We will analyze ten economic and social indicators collected for each province. Because these indicators are highly correlated, we will use principal component analysis (PCA) to reduce redundancies and highlight patterns that are not apparent in the raw data. After visualizing the patterns, we will use k-means clustering to partition the provinces into groups with similar development levels.
These results can be used to plan public policy by helping allocate resources to develop infrastructure, education, and welfare programs.
```{r setup}
library(tidyverse) # Tidying, manipulating, and charting data
library(factoextra) # Extract and Visualize Multiple Factor Analysis
library(FactoMineR) # Multiple Factor exploratory analysis and data mining
library(ggrepel) # ggplot formatting for labels
# Read in the dataset
argentina <- read_csv("datasets/argentina.csv")
# Inspect the first rows of the dataset
nrow(argentina)
head(argentina, 6)
```
Argentina ranks third in South America in total population, but the population is unevenly distributed throughout the country. Sixty percent of the population resides in the Pampa region (Buenos Aires, La Pampa, Santa Fe, Entre Ríos and Córdoba) which only encompasses about 20% of the land area.
GDP is a measure of the size of a province's economy. To measure how rich or poor the inhabitants are, economists use per capita GDP, which is GDP divided by the province's population.
```{r exploratory}
# Add gdp_per_capita column to argentina
argentina <- argentina %>% 
  mutate(gdp_per_cap = gdp / pop) 
# Find the four richest provinces
( rich_provinces  <- argentina %>% 
                         arrange(desc(gdp_per_cap)) %>%                         
                         select(province, gdp_per_cap) %>%
                         top_n(4))
 
# Find the provinces with populations over 1 million
( bigger_pops <- argentina %>% 
                     arrange(desc(pop)) %>%
                     select(province, pop) %>%
                     filter(pop > 1000000))
```
PCA Matrix
Principal Component Analysis (PCA) is an unsupervised learning technique that summarizes multivariate data by reducing redundancies (variables that are correlated). New variables (the principal components) are linear combinations of the original data that retain as much variation as possible. We would imagine that some aspects of economic and social data would be highly correlated, so let's see what pops out. But first, we need to do some data preparation.
R makes it easy to run a PCA with the PCA() function from the FactoMineR package. The first argument in PCA() is a data frame or matrix of the data where the rows are "individuals" (or in our case, provinces) and columns are numeric variables. To prepare for the analysis, we will remove the column of province names and build a matrix from the dataset.
```{r PCA}
# Select numeric columns and cast to matrix
argentina_matrix  <- argentina  %>% 
                         select_if(is.numeric) %>% 
                         as.matrix()
# Print the first lines of the result
head(argentina_matrix, 6)
```
Reducing Dimensions
o go from eleven dimensions (eleven original variables) down to two dimensions (two variables that are summaries of the original eleven).
To run PCA, we need to make sure all the variables are on similar scales. Otherwise, variables with large variance will be overrepresented. In PCA() setting scale.unit = TRUE ensures that variables are scaled to unit variance before crunching the numbers.
Feel free to explore the output!
```{r dimensionReduction}
# Apply PCA and print results
( argentina_pca  <- PCA(argentina_matrix, scale.unit = TRUE) )
# Set the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=5)
# Plot the original variables and the first 2 components and print the plot object.
( pca_var_plot <- fviz_pca_var(argentina_pca))
# Sum the variance preserved by the first two components. Print the result.
( variance_first_two_pca <- argentina_pca$eig[1, 2] + argentina_pca$eig[2, 2] )
# Visualize Dim2 vs. Dim1
fviz_pca_ind(argentina_pca, title = "Provinces - PCA")
```
PCA: Variables & Components¶
Now that we have the principal components, we can see how the original variables are correlated among themselves and how the original variables are correlated with the principal components. We will build a plot using the factoextra package to help us understand these relationships. A correlation circle plot (also known as a variable correlation plot) shows the relationship among all variables as they are plotted on the first two principal components (Dimension 1 and Dimension 2).
To understand the plot, note that:
Positively correlated variables have similar vectors.
The vectors of negatively correlated variables are on opposite sides of the plot origin (opposite quadrants).
Each axis represents a principal component. Vectors pointing in the direction of the component are correlated with that component.
The percentage of the original variance explained by each component (dimension) is given in parentheses in the axes labels.
Plotting the components¶
With the first two principal components representing almost 65% of the variance, most of the information we are interested in is summarized in these two components. From the variable correlation plot, we can see that population and GDP are highly correlated; illiteracy, poverty, no healthcare, school dropout, and deficient infrastructure are correlated; and GDP per capita and movie theaters per capita are correlated.
But how do these correlations map to the provinces? To dive into that question, let's plot the individual principal components for each province and look for clusters.
```{r kMeans}
# Set seed to 1234 for reproducibility
set.seed(1234)
head(argentina_pca)
# Create an intermediate data frame with pca_1 and pca_2
argentina_comps <- tibble(pca_1 = argentina_pca$ind$coord[ ,1],  
                          pca_2 = argentina_pca$ind$coord[ ,2])
argentina_comps
# Cluster the observations using the first 2 components and print its contents
( argentina_km <- kmeans(argentina_comps, centers = 4, nstart = 20, iter.max = 50))
# Convert assigned clusters to factor
clusters_as_factor <- factor(argentina_km$cluster)
# Plot individulas colored by cluster
fviz_pca_ind(argentina_pca, 
             title = "Clustered Provinces - PCA", 
             habillage = clusters_as_factor) 
```
Components with colors
Now that we have cluster assignments for each province, we will plot the provinces according to their principal components coordinates, colored by the cluster.
Buenos Aires, in a league of its own¶
A few things to note from the scatter plot:
Cluster 1 includes only Buenos Aires and has a large positive value in Dimension 2 with an intermediate negative value in Dimension 1.
Cluster 2 has the greatest negative values in Dimension 1.
Cluster 3 has the greatest positive values in Dimension 1.
Cluster 4 has small absolute values in Dimension 1.
Clusters 2, 3, and 4, all have small absolute values in Dimension 2.
We will focus on exploring clusters 1, 2, and 3 in terms of the original variables in the next few tasks.
As we noted earlier, Buenos Aires is in a league of its own, with the largest positive value in Dimension 2 by far. The figure below is a biplot, a combination of the individuals plot from Task 6 and the circle plot from Task 5.
```{r plots}
# Add cluster column to argentina
argentina <- argentina %>%
               mutate(cluster = clusters_as_factor)
# Make a scatterplot of gdp vs. cluster, colored by cluster
ggplot(argentina, aes(y = gdp, x = cluster, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP") +
  ggtitle("Argentina's GDP per capita vs Province Clusters")
# Make a scatterplot of GDP per capita vs. cluster, colored by cluster
ggplot(argentina, aes(y = gdp_per_cap, x = cluster, color = cluster)) +
    geom_point() +
    geom_text_repel(aes(label = province), show.legend = FALSE) +
    labs(x = "Cluster", y = "GDP per capita") +
    ggtitle("Argentina's GDP per Capita vs Province Clusters")
# Make scatterplot of poverty vs. cluster, colored by cluster
ggplot(argentina, aes(x = cluster, y = poverty, color = cluster)) +
    geom_point() +
    labs(x = "Cluster", y = "Poverty rate") +
    geom_text_repel(aes(label = province), show.legend = FALSE) +
    ggtitle("Argentina's Poverty vs Province Clusters")
```
The rich provinces¶
Provinces in cluster 2 have large negative values in Dimension 1. The biplot shows that gdp_per_cap, movie_theaters_per_cap and doctors_per_cap also have high negative values in Dimension 1.
If we plot gdp_per_cap for each cluster, we can see that provinces in this cluster 2, in general, have greater GDP per capita than the provinces in the other clusters. San Luis is the only province from the other clusters with gdp_per_cap in the range of values observed in cluster 2. We will see similar results for movie_theaters_per_cap and doctors_per_cap.
The poor provinces
Provinces in Cluster 3 have high positive values in Dimension 1. As shown in the biplot, provinces with high positive values in Dimension 1 have high values in poverty, deficient infrastructure, etc. These variables are also negatively correlated with gdp_per_cap, so these provinces have low values in this variable.
Planning for public policy¶
Now that we have an idea of how social and economic welfare varies among provinces, we've been asked to help plan an education program. A pilot phase of the program will be carried out to identify design issues. Our goal is to select the proposal with the most diverse set of provinces:
Tucumán, San Juán, and Entre Ríos
Córdoba, Santa Fé, and Mendoza
Buenos Aires, Santa Cruz, and Misiones
Which proposal includes the most diverse set of provinces? #3
