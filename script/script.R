library(haven)
library(factoextra)
library(ggplot2)
library(plotly)

#------------------------ import datasets, left join ---------------------------
dep = read_xpt("data/P_DPQ.xpt")
demo = read_xpt("data/P_DEMO.xpt")

df = merge(dep, demo[ , c(1,4,5,8,10,29)], by = "SEQN", all.x = TRUE) #only merging Gender, Age, Race, Country, and Income-Poverty Ratio from the demo table
head(df)
#---------------------------- data preprocessing -------------------------------
### remove NAs
df = na.omit(df)

### remove rows with certain responses: "Refused" or "Don't Know"
df = df[ !(df$DPQ010 == 7 | df$DPQ010 == 9) , ]
df = df[ !(df$DPQ020 == 7 | df$DPQ020 == 9) , ]
df = df[ !(df$DPQ030 == 7 | df$DPQ030 == 9) , ]
df = df[ !(df$DPQ040 == 7 | df$DPQ040 == 9) , ]
df = df[ !(df$DPQ050 == 7 | df$DPQ050 == 9) , ]
df = df[ !(df$DPQ060 == 7 | df$DPQ060 == 9) , ]
df = df[ !(df$DPQ070 == 7 | df$DPQ070 == 9) , ]
df = df[ !(df$DPQ080 == 7 | df$DPQ080 == 9) , ]
df = df[ !(df$DPQ090 == 7 | df$DPQ090 == 9) , ]
df = df[ !(df$DPQ100 == 7 | df$DPQ100 == 9) , ]
df = df[ !(df$DMDBORN4 == 77 | df$DMDBORN4 == 99) , ]

### clean demographic variables
#Gender
df$RIAGENDR = as.factor(df$RIAGENDR)
levels(df$RIAGENDR) = c("Male", "Female")

#Race
df$RIDRETH3[which(df$RIDRETH3 == 1)] <- 2 #Unify "Mexican American" and "Other Hispanic" into one unified "Hispanic" label (2)
df$RIDRETH3 = as.factor(df$RIDRETH3)
levels(df$RIDRETH3) = c("Hispanic", "White", "Black", "Asian", "Other")

#Country of Birth
df$DMDBORN4 = as.factor(df$DMDBORN4)
levels(df$DMDBORN4) = c("United States", "Other")

#----------------------- exploratory data analysis (EDA) -----------------------
### Gender
ggplot(data = df, aes(RIAGENDR)) + 
  geom_bar(fill="red", color="black") + 
  xlab("Gender") +
  ylab("Count of Respondents") + 
  ggtitle("Distribution of Respondents across Gender")

### Age
ggplot(df, aes(RIDAGEYR)) + 
  geom_histogram(fill="red", color="black") + 
  xlab("Age of Respondents") + 
  ylab("Count of Respondents") + 
  ggtitle("Age Distribution of Respondents")

### Race
ggplot(df, aes(RIDRETH3)) + 
  geom_bar(fill="red", color="black", position = "identity") + 
  xlab("Race of Respondents") +
  ylab("Count of Respondents") +
  ggtitle("Racial Distribution of Respondents")

### Country of Birth
ggplot(df, aes(as.factor(DMDBORN4))) + 
  geom_bar(fill="red", color="black", position = "identity") + 
  xlab("Country of Birth") + 
  ylab("Count of Respondents") +
  ggtitle("Respondents' Country of Birth")

### Income-to-Poverty Ratio
ggplot(df, aes(INDFMPIR)) + 
  geom_histogram(fill="red", color="black") + 
  xlab("Income-to-Poverty Ratio Distributions") + 
  ylab("Count of Respondents") + 
  ggtitle("Income-to-Poverty Ratio Distributions")


#---------------------- K-Means Clustering without PCA -------------------------
### Selecting K
set.seed(123)
fviz_nbclust(df[ , 2:11], kmeans, nstart=10, k.max = 10, method = "wss") #elbow bends at k=2
fviz_nbclust(df[ , 2:11], kmeans, nstart=10, k.max = 10, method = "silhouette") # k=2

### Cluster using k=2
set.seed(123)
cluster = kmeans(df[ , 2:11], centers = 2, nstart = 10)

### Check the cluster object and compare ratio of betweenSS to withinnss
cluster$betweenss / cluster$tot.withinss #very low ratio, which means low separation of clusters. This is bad.

fviz_cluster(cluster, geom="point", data = df[, 2:11]) + 
  ggtitle("K Means Clustering, Given Features, K=2")

#------------------------ K-Means Clustering with PCA --------------------------
### PCA
pca = prcomp(df[ , 2:11])
summary(pca)

### Create scores table
scores = as.data.frame(pca$x) # the "x" table is the PCA scores

### Observe weights from the rotation matrix
rot = as.data.frame(pca$rotation)

### Selecting K
set.seed(123)
fviz_nbclust(scores[ , 1:2], kmeans, nstart=10, k.max = 10, method = "wss") #elbow bends at 2 and 4
fviz_nbclust(scores[ , 1:2], kmeans, nstart=10, k.max = 10, method = "silhouette") #k=2

### Cluster using k=2
set.seed(123)
cluster2 = kmeans(scores[ , 1:2], centers = 2, nstart = 10)

### Check the cluster object and compare ratio of betweenSS to withinnss
cluster2$betweenss / cluster2$tot.withinss #ratio is higher but still not great

### Cluster using k=4
set.seed(123)
cluster2 = kmeans(scores[ , 1:2], centers = 4, nstart = 10)
cluster2$betweenss / cluster2$tot.withinss #ratio is much higher and therefore better separation between clusters

### Visualizing the Clusters
#Option 1: GGPlot
scores$cluster = as.character(cluster2$cluster) #add cluster label to the scores table
ggplot(scores, aes(x=PC1, y=PC2)) + 
  geom_point(aes(color=cluster)) + 
  scale_color_manual(values = c("red", "gold", "dark green", "blue")) + 
  ggtitle("K Means Clustering, Principal Components, K=4")


#Option 2: 3D (Plotly)
plot_ly(data = scores, x=scores$PC1, y=scores$PC2, z=scores$PC3, color = scores$cluster)


#-------------------------- Interpreting Clusters -----------------------------
### create data frame using df and a cluster column
survey = cbind(df, cluster = cluster2$cluster)
survey$cluster = as.factor(survey$cluster)

### create a column "PHQ9 Score", adding up questions 1-9 from the questionnaire in each row
survey$PHQ9_score = rowSums(survey[ , 2:10])

### create a column "Depressive Symptoms Category" to interpret the Kroenke scores (https://bmcpsychiatry.biomedcentral.com/articles/10.1186/s12888-019-2262-9#:~:text=Regarding%20severity%2C%20PHQ%2D9%20comprises,severe%20depressive%20symptoms%20%5B25%5D.)
survey$depressive_symptoms_cat = ""
survey$depressive_symptoms_cat[survey$PHQ9_score >= 0 & survey$PHQ9_score<=4] <- "None"
survey$depressive_symptoms_cat[survey$PHQ9_score >= 5 & survey$PHQ9_score<=9] <- "Mild"
survey$depressive_symptoms_cat[survey$PHQ9_score >= 10 & survey$PHQ9_score<=14] <- "Moderate"
survey$depressive_symptoms_cat[survey$PHQ9_score >= 15 & survey$PHQ9_score<=19] <- "Moderately-Severe"
survey$depressive_symptoms_cat[survey$PHQ9_score >= 20 & survey$PHQ9_score<=27] <- "Severe"

### Condense data frame
survey = survey[ , c(1, 12:19)]

#Visualize distribution of PHQ-9 scores per cluster
ggplot(survey, aes(cluster, PHQ9_score)) + 
  geom_boxplot(outlier.color = "red") +
  xlab("Clusters") + 
  ylab("PHQ-9 Scores") +
  ggtitle("Distribution of Questionnaire Scores per Cluster")


### isolate Cluster 3
group3 = survey[ survey$cluster == 3 , ]

#Age
ggplot(data = group3, aes(RIDAGEYR)) + 
  geom_histogram(fill="red", color="black") + 
  geom_vline(xintercept = mean(group3$RIDAGEYR), linetype = "dashed", color="green") +
  geom_vline(xintercept = mean(df$RIDAGEYR), linetype = "dashed", color="blue") +
  ggtitle("Age Distribution of Respondents in Cluster 3") +
  xlab("Age") +
  ylab("Count of Respondents")

#Income-to-Poverty ratio
ggplot(data = group3, aes(INDFMPIR)) + 
  geom_histogram(fill="red", color="black") + 
  geom_vline(xintercept = mean(group3$INDFMPIR), linetype = "dashed", color="green", size=1) +
  geom_vline(xintercept = mean(df$INDFMPIR), linetype = "dashed", color="blue", size=1) +
  ggtitle("Distribution of Income-to-Poverty Ratios for Cluster 3") + 
  xlab("Income-to-Poverty Ratios") +
  ylab("Count of Respondents")

#Questionnaire Scores
ggplot(group3, aes(PHQ9_score)) + 
  geom_histogram(fill="red", color="black", binwidth = 1) + 
  geom_vline(xintercept = mean(group3$PHQ9_score), linetype = "dashed", color="green", size=1) + 
  ggtitle("Distribution of Questionnaire Scores for Cluster 3") +
  xlab("Questionnaire Scores") +
  ylab("Count of Respondents")

#Depressive Symptoms
ggplot(group3, aes(depressive_symptoms_cat)) + 
  geom_bar(fill="red", color="black", position = "identity") +
  ggtitle("Categorization of Depressive Symptoms for Cluster 3") + 
  xlab("Depressive Symptom Categories") + 
  ylab("Count of Respondents")

