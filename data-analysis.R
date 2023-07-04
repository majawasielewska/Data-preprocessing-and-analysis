#install.packages("readxl") # Install package readxl

library("readxl") # Load the library

### Data preprocessing

data = read_excel("D:/c/folder/Chwilowy folder/projekt z R/rzeczy_do_R/catsvdogs.xlsx")
data

head(data) # Display for the first 5 objects

table(is.na(data)) # Check for missing data, TRUE: 1, column: 'Procent właścicieli psów', Mean: 37.42 

summary(data) # Column: Procent właścicieli psów, Mean: 37.42
min(data$Populacja_psów) # Minimum value: 42

#install.packages("tidyverse")
library(tidyverse)

data_replace <- data %>% replace_na(list(Procent_właścicieli_psów=37.42)) # Replace missing values -> mean
data_replace

table(is.na(data_replace))

dataframe = data.frame(data_replace)
dataframe
dataframe$Liczba_gospodarstw_domowych
kids = c("1"="Powyżej 50% dzieci w gospodarstwie domowym", "0"="Poniżej 50% dzieci w gospodarstwie domowym") # Convert numbers to words
dataframe$Występowanie_powyżej_50_procent_dzieci_w_gospodarstwie_domowym = kids[as.character(dataframe$Występowanie_powyżej_50_procent_dzieci_w_gospodarstwie_domowym)]
head(dataframe)

dataframe$Populacja_psów

#install.packages("writexl")
write_xlsx(dataframe, "./mydata.xlsx") # Save a new dataset

arrange(dataframe, Populacja_kotów)

### Data visualization

png('scatterplot_dogscats.png')
plot(dataframe$Populacja_psów, dataframe$Populacja_kotów, xlab = "Populacja psów", ylab = "Populacja kotów", main = "Zależność pomiędzy populacją psów i populacją kotów", col = 'purple', pch = 20) # Scatterplot
abline(lm(Populacja_kotów~Populacja_psów,data=dataframe),col='pink') # Scatterplot with linear regression
dev.off()

hist(dataframe$Populacja_psów, main = "Histogram", ylab = "Częstość", xlab = "Populacja psów", col="blue")

hist(dataframe$Populacja_psów, xlim =c(0,10000),ylim=c(0.00,30),breaks = 8, main = "Zbiorczy histogram", ylab = "", xlab = "", col="orange")
hist(dataframe$Populacja_kotów, add = TRUE, ylab = "", xlab = "", col="pink")
hist(dataframe$Liczba_gospodarstw_domowych, add = TRUE, ylab = "", xlab = "", col="red")
legend("topright", c("Populacja_psów", "Populacja_kotów","Liczba_gospodarstw_domowych"), fill=c("orange", "pink","red"))

cor.test(data$Populacja_psów, data$Populacja_kotów, method = "pearson") # Correlation coefficient equal to 0.96
cor.test(data$Populacja_psów, data$Występowanie_powyżej_50_procent_dzieci_w_gospodarstwie_domowym, method = "pearson")

df = dplyr::select_if(dataframe, is.numeric) # Correlation plots
r = cor(df)
round(r,2)

library(ggcorrplot)
ggcorrplot(r)

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

# Group rows by column values in the DataFrame

mydata_groupby <- dataframe %>% group_by(Występowanie_powyżej_50_procent_dzieci_w_gospodarstwie_domowym) %>% summarise(min_Liczba_gospodarstw_domowych = min(Liczba_gospodarstw_domowych), max_Liczba_gospodarstw_domowych = max(Liczba_gospodarstw_domowych), Liczba_stanów = n())
mydata_groupby # Boxplot

mydata_groupby_df = data.frame(mydata_groupby)
mydata_groupby_df

y = dataframe %>% arrange(desc(Populacja_psów))
y
png('boxplotyaaa.png')
par(mfrow = c(1,1))
boxplot(y$Populacja_kotów~y$Występowanie_powyżej_50_procent_dzieci_w_gospodarstwie_domowym, main = "Populacja kotów w zależności od ilości dzieci", ylab = "Populacja kotów", xlab = "", col=c("pink","red"), cex.axis=0.75)
dev.off()

#install.packages("dplyr")
library("dplyr")
#install.packages("ggpubr")

library("ggpubr")
ggdensity(dataframe$Procent_właścicieli_psów, 
          main = "Wykres gęstości dla % właścicieli psów",
          xlab = "% właścicieli psów",
          ylab = "Gęstość")
shapiro.test(dataframe$Procent_właścicieli_psów)
library(ggpubr)
ggqqplot(dataframe$Procent_właścicieli_psów,
         xlab = "Reszty",
         ylab = "Wartość oczekiwana")

### Hierarchical Cluster Analysis (HCA)

dataframe_scale = scale(dataframe[,2:9],center = TRUE, scale = TRUE)
dataframe_scale # Data scaling

dataframe_distance_matrix = dist(dataframe_scale, method = "euclidean")
dataframe_distance_matrix # Distance matrix calculation


object_names = dataframe$Lokalizacja
hc1 = hclust(dataframe_distance_matrix, method = "complete" )
plot(hc1, object_names, cex = 0.6,hang = -1, main = "Dendrogram\n(Odległość euklidesowa, metoda pełnego wiązania)", cex.main=0.7, xlab = "Obiekty", ylab = "Odleglość", cex.lab=0.7)
rect.hclust(hc1, k = 4, border = 2:5)
abline(h = 7, col = 'orange')


#install.packages("dendextend")
#install.packages("circlize")
library(dendextend)
library(circlize)

circ_den <- as.dendrogram(hclust(dataframe_distance_matrix)) # Circular dendrogram

circ_den <- circ_den %>%
  color_branches(k = 3) %>%
  color_labels(k = 3)

circlize_dendrogram(circ_den,
                    labels_track_height = NA,
                    dend_track_height = 0.5)

circlize_dendrogram(circ_den,
                    labels_track_height = NA,
                    dend_track_height = 0.5,
                    facing = "inside")

### K-means clustering

#install.packages("cluster")
#install.packages("factoextra")
#install.packages("gridExtra")
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

distance_ = get_dist(dataframe_scale)
fviz_dist(distance_, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Create charts

png("kmeans_4wykresy.png")
k2 = kmeans(dataframe_scale, centers = 2, nstart = 25)
k3 = kmeans(dataframe_scale, centers = 3, nstart = 25)
k4 = kmeans(dataframe_scale, centers = 4, nstart = 25)
k5 = kmeans(dataframe_scale, centers = 5, nstart = 25)

p1 = fviz_cluster(k2, geom = "point", data = dataframe_scale) + ggtitle("k = 2")
p2 = fviz_cluster(k3, geom = "point",  data = dataframe_scale) + ggtitle("k = 3")
p3 = fviz_cluster(k4, geom = "point",  data = dataframe_scale) + ggtitle("k = 4")
p4 = fviz_cluster(k5, geom = "point",  data = dataframe_scale) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

### Create easy functions
# 1 -> mathematical 

mathematical_operation = function(x,y,z,i){
  wynik = ((x^z)^i)/y
  print(paste("Działanie ((2^2)^3)/2 równa się: ",wynik))
}

mathematical_operation(2,2,2,3)

# 2 -> test the regularity of the level of thyroid stimulating hormone

TSH_test = function(TSH){ # Reference range for good TSH 0.27 - 4.20
  if(TSH > 4.20)
  {
    print(paste("TSH =",TSH,"->","TSH level elevated, go to the doctor!"))
  } else if(TSH < 0.27){
    print(paste("TSH =",TSH,"->","TSH level lowered, go to the doctor!"))
  } else {
    print(paste("TSH =",TSH,"->","Normal TSH level"))
  }
    
  }

TSH_test(4.25)
TSH_test(0.126)
TSH_test(8.5)
TSH_test(3.2)
TSH_test(0.36)