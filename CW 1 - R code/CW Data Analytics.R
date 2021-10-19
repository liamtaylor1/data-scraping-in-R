library(xml2)
library(rvest)
library(stringr)
library (lubridate)
library(robotstxt)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sentimentr)
library(FactoMineR)
library(factoextra)
library(kohonen)
library(corrplot)
library(cluster)
library(RColorBrewer)

#-------------------------------
#--------Data scraping:----------
#-------------------------------

#check is scrapping is allowed on this site, retuns True or False
paths_allowed("https://www.imdb.com/search/title?groups=top_250&sort=user_rating")

#Read HTML Contents from the DOM of the site, 50 films per page so multiple pages needed for top 250
imbd <- read_html("https://www.imdb.com/search/title?groups=top_250&sort=user_rating")
imbd2 <- read_html("https://www.imdb.com/search/title/?groups=top_250&sort=user_rating,desc&start=51&ref_=adv_nxt")
imbd3 <- read_html("https://www.imdb.com/search/title/?groups=top_250&sort=user_rating,desc&start=101&ref_=adv_nxt")
imbd4 <- read_html("https://www.imdb.com/search/title/?groups=top_250&sort=user_rating,desc&start=151&ref_=adv_nxt")
imbd5 <- read_html("https://www.imdb.com/search/title/?groups=top_250&sort=user_rating,desc&start=201&ref_=adv_nxt")

#bind all web scrapings into 1 imdb variable
doc2children <- xml_children(imbd2)

for (child in doc2children) {
  xml_add_child(imbd, child)
}

doc2children <- xml_children(imbd3)

for (child in doc2children) {
  xml_add_child(imbd, child)
}

doc2children <- xml_children(imbd4)

for (child in doc2children) {
  xml_add_child(imbd, child)
}

doc2children <- xml_children(imbd5)

for (child in doc2children) {
  xml_add_child(imbd, child)
}

#remove data that is no longer needed
remove(imbd2, imbd3, imbd4, imbd5, child, doc2children)

#----------------------------
#---- extraction of data ----
#----------------------------
#Extract film Ranking
imbd %>%
  html_nodes(".lister-item-content h3 .lister-item-index") %>%
  html_text() -> Rank

Rank

#Extract Movie title
imbd %>%
  html_nodes(".lister-item-content h3 a") %>%
  html_text() -> movie_title

movie_title

#Extract Release year
imbd %>%
  html_nodes(".lister-item-content h3 .lister-item-year") %>%
  html_text() %>%
  str_sub(start = 2, end = 5) %>%
  as.Date(format = "%Y") %>%
  year() -> Release_year

Release_year

#Extract certificate (age rating)
imbd %>%
  html_nodes(".lister-item-content p .certificate") %>%
  html_text() -> Certificate

Certificate

#Extract Runtime
imbd %>%
  html_nodes(".lister-item-content p .runtime") %>%
  html_text() -> Runtime

Runtime

#Extract Genre
imbd %>%
  html_nodes(".lister-item-content p .genre") %>%
  html_text() -> Genre

Genre

#Extract IMBD score 
imbd %>%
  html_nodes(".lister-item-content .ratings-bar strong") %>%
  html_text() -> IMBD_Score

IMBD_Score

#director,Actors, and description (they're in the same Tags)
imbd %>%
  html_nodes(".lister-item-content p") %>%
  html_text() -> IMBD_People

IMBD_People <- as.data.frame(IMBD_People)

#-------------------------------
#-------- Cleaning Data: -------
#-------------------------------
#separate people (actors and directors) and Descriptions.
Description = IMBD_People[seq(2, nrow(IMBD_People), 4), ]
Description <- as.data.frame(Description)

People = IMBD_People[seq(3, nrow(IMBD_People), 4), ]
People <- as.data.frame(People)

remove(IMBD_People)

#Separate director and actors into their own columns and clean data (remove un-needed text)
People[] <- lapply(People, gsub, pattern='Director:', replacement='')
People[] <- lapply(People, gsub, pattern='Directors:', replacement='')
People[] <- lapply(People, gsub, pattern='  ', replacement='')

People <- str_split_fixed(People$People ,"Stars:", 2)
People <- as.data.frame(People)

Directors <-as.data.frame(People$V1)  
Actors <-as.data.frame(People$V2) 
remove(People)

#remove "|" char from the end of the directors as gsub pattern (above) couldn't recognize it
Directors[] <- lapply(Directors, as.character)
Directors$`People$V1` = substr(Directors$`People$V1`,1,nchar(Directors$`People$V1`)-3)

names(Directors)[names(Directors) == "People$V1"] <- "Directors"
Directors[] <- lapply(Directors, as.character)

#split multiple directors into their own column
Directors <- separate(
  Directors,
  Directors,
  c("Director1","Director2","Director3"),
  sep = ",",
  remove = TRUE,
)

#Remove 2nd and 3rd directors as most of the list is NA
Directors <- subset(Directors , select =  c(1) )

#Separate multiple actors into their own columns and set data type
names(Actors)[names(Actors) == "People$V2"] <- "Actors"
Actors[] <- lapply(Actors, as.character)

Actors <- separate(
  Actors,
  Actors,
  c("Actor1","Actor2","Actor3","Actor4"),
  sep = ",",
  remove = TRUE,
)

#split multiple Genre into their own column
Genre <- as.data.frame(Genre)
Genre <- separate(
  Genre,
  Genre,
  c("Genre1","Genre2","Genre3"),
  sep = ",",
  remove = TRUE,
)

#set rank as numeric
Rank <- as.numeric(Rank)

#Set IMBD score as numeric
IMBD_Score <- as.numeric(IMBD_Score)

#Join Data to create complete data frame including NA's
Master <- cbind(Rank, IMBD_Score, Release_year, Runtime, movie_title, Genre, Directors,Actors, Description)

remove(Rank, movie_title, IMBD_Score, Release_year, Runtime, Genre, Directors,Actors, Description)

Master <- as.data.frame(Master)


#-----------------------------------------
#------Exploratory Analysis Here:---------
#-----------------------------------------

#------Initial exploratory analysis------
summary(Master)

#Average run time of best films
mean(Master$Runtime) 

#imbd score not evenly distributed
ggplot(Master,aes(x = Master$IMBD_Score))+ geom_histogram(binwidth= 0.1)

#IMBD score vs film year with median line plotted
ggplot(data=Master,aes(x= Release_year,y= IMBD_Score))+
  geom_point(alpha=1/2,position="jitter",scale=0.8)+
  geom_line(stat="summary",fun.y=median,col=2,linetype=2) # Adding median line


#----------------------------------------
#---sentiment analysis on descriptions:--
#----------------------------------------

#Sentiment analysis works sentence by sentence so we want to remove full stops so that
#it treats a description as a single entity 

#Master$Description <- lapply(Master$Description, gsub, pattern='.', replacement='')
SentimentAnlAysis <- sentiment(Master$Description)

#----------------------------------------
#------------------SOM:------------------
#----------------------------------------
set.seed(123)

#extract numeric data from master data set
Master_SOM <- subset(Master , select = c(1:4))

#make sure all data is numeric, so that no error occured during SOM
Master_SOM[] <- lapply(Master_SOM[], gsub, pattern='min', replacement='')

Master_SOM[] <- lapply(Master_SOM, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

Master_SOM <- mutate_all(Master_SOM, function(x) as.numeric(as.character(x)))

#scale data
Master_SOM <- scale(Master_SOM)

#Attributes for SOM grip
som.grid = somgrid(xdim = 4, ydim=4, topo="hexagonal", toroidal = TRUE)

#Train SOM Model
som_model <- som(Master_SOM,grid=som.grid, rlen=400, alpha=c(0.05,0.01), keep.data = TRUE)

#visualise training progress, once model plateau's it as train correctly
plot(som_model, type="changes")

#mapping plot to show films that are similar together

plot(som_model, type="mapping", border = "grey" , labels = Master$Rank)

#colors function for the charts
BlueToRedFade <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

#plot depending on distance between each node and nearest neighbor
plot(som_model, type = "dist.neighbours", palette.name = BlueToRedFade )

#colour pallet
bgcols <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

# Viewing WCSS to determine best cluster numbers
mydata <- getCodes(som_model)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 1:4) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

#SOM with hierarchica clustering using 3 clusters
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 3)
plot(som_model, type="mapping", bgcol = bgcols [som_cluster], main = "Hierarchical Clusters", labels = Master$Rank)
add.cluster.boundaries(som_model, som_cluster)


#loop for each column to create comparison plot
par(mfrow = c(5, 6), cex.main = 1.5)
for(i in 1:4) {
  var <- i
  plot(
    som_model,
    type = "property",
    property = getCodes(som_model)[, var],
    main = "",
    palette.name = BlueToRedFade
  )
  title(colnames(getCodes(som_model)) [var], line = 2.5)
  mtext(
    side = 2,
    text = "Map-Values",
    line = 0,
    7,
    cex = 0.8
  )
}

#clean uneeded data
remove(bgcols,i,var,BlueToRedFade)

#----------------------------------------
#--------Multiple Factor Analysis:-------
#----------------------------------------
#?MCA

#remove description,title,2nd 3rd and 4th actors and NA heavy columns
Master <- subset(Master, select = -c(5,7,8,11,12,13,14)) 
Master <- drop_na(Master)

##broken - to fix errror
# for (i in 2:6) {
#   plot(Master[,i], main=colnames(Master)[i],
#        ylab = "Count", col="steelblue", las = 2)
# }


#number of categories per variable
cats = apply(Master, 2, function(x) nlevels(as.factor(x)))
cats

# #reduce catagories
# Master <- Master %>% group_by(Director1) %>% filter(n()>1)
# Master <- Master %>% group_by(Actor1) %>% filter(n()>1)

#Convert Master data to factors as it is needed for Multiple Correspondence Analysis (MCA)
i=0
while(i < ncol(Master)){
  i=i+1 
  Master[i] = as.factor(Master[,i])
}

remove(i)

#Multiple Correspondence Analysis (MCA), PCA equivalent for non numeric
dev.off()
mca1 = MCA(Master, ncp = 5, graph = TRUE)
print(mca1)

#get eigenvalue and plot on spree plot
eig.val <- get_eigenvalue(mca1)
eig.val

fviz_eig(mca1, addlabels = TRUE, ylim = c(0, 1))

# #biplot - #Coordinates of variable categories
# fviz_mca_biplot(mca1 ,
#                 repel = FALSE,
#                 ggtheme = theme_minimal()) #too much info - TO FIX!! (repel = TRUE breaks R studio)

#-----MCA results--------
var <- get_mca_var(mca1)

# Coordinates
head(var$coord)

# Cos2: quality on the factor map
head(var$cos2)

# Contributions to the principal components
head(var$contrib)
#------------------------

#visualize the correlation between variables and MCA principal dimensions
fviz_mca_var(mca1, choice = "mca.cor", 
             col.var="black",
             shape.var = 15,
             repel = TRUE, # Avoid text overlapping 
             ggtheme = theme_minimal())

# #Quality of representation of variable categories
# fviz_mca_var(mca1, col.var = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#              repel = FALSE, #too much info - TO FIX!! (repel = TRUE breaks R studio)
#              ggtheme = theme_minimal())

# #visualize the cos2 of row categories
# corrplot(var$cos2 , is.corr=FALSE) #too much info - TO FIX!!

# # Cos2 of variable categories on Dim.1 and Dim.2
# fviz_cos2(mca1, choice = "var", axes = 1:2)#too much info - TO FIX!!

# Contributions of rows to dimension 1
fviz_contrib(mca1, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(mca1, choice = "var", axes = 2, top = 15)

# #
# fviz_mca_var(mca1, col.var = "contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = FALSE, #too much info - TO FIX!!
#              ggtheme = theme_minimal()
# )

# fviz_mca_ind(mca1, col.ind = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE, #too much info - TO FIX!!
#              ggtheme = theme_minimal())

fviz_mca_ind(mca1, 
             label = "ind.sup", #Show the label of ind.sup only
             ggtheme = theme_minimal())

#Visualize variable categories with cos2 >= 0.1
fviz_mca_var(mca1, select.var = list(cos2 = 0.1))

# Top 10 active variables with the highest cos2
fviz_mca_var(mca1, select.var= list(cos2 = 10), repel = TRUE)