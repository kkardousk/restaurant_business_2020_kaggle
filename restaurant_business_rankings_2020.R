rm(list = ls(all.names = TRUE))
library(tidyverse)
library(janitor)
library(tidytext) 
library(textclean)
library(ggplot2)
library(stringi)
library(stringr)
library(scales)
options(scipen = 999)

# reading files after extracting all files (originally zipped)
setwd("C://Documents and Settings/kardo/Documents/kaggle_files_data/restaurant_business_2020/data/")
list.files()

## exploring datasets
# future50
future_50 <- read.csv("Future50.csv", stringsAsFactors = FALSE)
# Independence100
independence100 <- read.csv("Independence100.csv", stringsAsFactors = FALSE)
# Top250
top250 <- read.csv("Top250.csv", stringsAsFactors = FALSE)

# using independce rating study first to try to visualize map based upon states - to  see any relationship between geography and sales

fct_count(independence100$State)
# cleaning up state acronyms (also a chance to consolidate them - florida is spelled differently because of whitespace)

independence100_cleaned <- 
  independence100 %>%
  mutate(state_cleaned = gsub( "\\.", "", str_squish(str_to_lower(State)) ),
         state_cleaned = case_when(state_cleaned == "calif" ~ "CA",
                                   state_cleaned == "colo"  ~ "CO",
                                   state_cleaned == "dc"    ~ "DC",
                                   state_cleaned == "fla"   ~ "FL",
                                   state_cleaned == "ga"    ~ "GA",
                                   state_cleaned == "ill"   ~ "IL",
                                   state_cleaned == "ind"   ~ "IN",
                                   state_cleaned == "mass"  ~ "MA",
                                   state_cleaned == "mich"  ~ "MI",
                                   state_cleaned == "nc"    ~ "NC",
                                   state_cleaned == "nj"    ~ "NJ",
                                   state_cleaned == "ny"    ~ "NY",
                                   state_cleaned == "nev"   ~ "NV",
                                   state_cleaned == "ore"   ~ "OR",
                                   state_cleaned == "pa"    ~ "PA",
                                   state_cleaned == "tenn"  ~ "TN",
                                   state_cleaned == "texas" ~ "TX",
                                   state_cleaned == "va"    ~ "VA")
  )

# using maps package nad ggplot2 data to extract long and lat for drawing map
library(maps)
long_lat <- map_data("state") # from ggplot - user has to have "maps" installed 
long_lat$region <- str_to_title(long_lat$region) # to match string format in independence100 df

# adding state abbs to join on state abbs from independence100_cleaned
independence100_duped <- # note that this will create a duped record because of cartesian join so we will just use the mean to plot numeric data on the map so to not inflate numbers
  independence100_cleaned %>%
  group_by(Rank, Restaurant) %>%
  right_join(data.frame(state_abb = state.abb, # not a fan of right join but this is so all the us map gets drawn/delimited (as we have many states for which we have no data in this small dataset)
                       state_name = state.name), by = c("state_cleaned" = "state_abb")) %>%
  right_join(long_lat, by = c("state_name" = "region")) %>%
  mutate(rn = row_number()) %>%
  mutate(rn_max = max(rn)) %>%
  mutate(normalized_sales = Sales / rn_max) %>%
  select(-c(rn, rn_max)) %>%
  ungroup() # we will keep this for futher use; but we need to reaggreate and generate aggregation of sales per state

# aggregating at a state level
independence100_map_ready <- 
  independence100_duped %>%
  group_by(state_cleaned) %>%
  summarise(Sales = sum(normalized_sales)) %>%
  right_join(data.frame(state_abb = state.abb, # not a fan of right join but this is so all the us map gets drawn/delimited (as we have many states for which we have no data in this small dataset)
                        state_name = state.name), by = c("state_cleaned" = "state_abb")) %>%
  right_join(long_lat, by = c("state_name" = "region"))

# reordering factor levels and sorting order (for accurate drawing if state bounderies)
independence100_map_ready$state_cleaned <- factor(independence100_map_ready$state_cleaned)
independence100_map_ready <- independence100_map_ready[order(independence100_map_ready$order), ] # sorting by order (for drawing states)

p <- ggplot(independence100_map_ready, aes(long, lat, group = group, fill = Sales)) +
  geom_polygon(color = "black") 
  

# adding state abbs
centroids <- data.frame(region = tolower(state.name), long = state.center$x, lat = state.center$y)
centroids$abb <- state.abb[match(centroids$region,tolower(state.name))]

# joining map ready df with centroids
independence100_map_ready <- 
  independence100_map_ready %>%
  left_join(centroids %>%
              mutate(state_name = str_to_title(region)),
            by = c("state_name" = "state_name"))

# adding on state lable layer 
map_with_state_labels <-
  p +
  with(centroids, 
       annotate(geom = "text", x = long, y = lat, label = abb, 
                size = 4, color = "white", family = "Times")
  )
map_with_state_labels +
  scale_fill_continuous(labels = comma) + # adding commas to sales figures
  scale_fill_continuous(labels = dollar) + # and adding dollars to know it's revenue
  ggtitle("Independence Result of Top 100 Restaurant in 2020 (by Sales figures)") +
  theme(plot.title = element_text(hjust = 0.5)) 
  

# NY surprsignly still fared better than expected (being the epicenter of Covid in US for quite some time in 2020);
# followed closely by Illinois and Nevada - it's important to note that this data is filtering to top 100 restaurants in the US; 
# so not representative of restaurant business in general 

# let's now plot a correlation matrix of independence100_cleaned numeric variables to see relationship between features
corr_independence <- 
  independence100_cleaned %>%
  keep(is.numeric)

library(corrplot)
cor_mat <- cor(corr_independence)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")) # color palette custom

corrplot(cor_mat, method="color", col=col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag = FALSE 
)
# above correlation plot shows that highest rank (lowest numbers) restaurant produce the most sales; there is a strong negative correlation of (negative) 92%

## now we explore what makes a small versus big business - clustering comes to mind 
# clustering to see if unsupervised method gives us a clear distinction between small and big entitites
 
sapply(top250, class)
# we first need to convert tot_sales and yoy_units to numeric
top250_cleaned <- 
  top250 %>%
  mutate(yoy_sales = as.numeric(str_replace(YOY_Sales, "%", "")),
         yoy_units = as.numeric(str_replace(YOY_Units, "%", "")))

#keeping only numeric features now to be readu for clustering
top250_cluster_ready <- 
  top250_cleaned %>%
  keep(is.numeric) %>%
  select(-1) # rank is not needed - and might throw off algoithm even when we scale

# rescaling data now 
top250_cluster_ready <- scale(top250_cluster_ready) # standardize variables
set.seed(123) # for reproducibility 
 
clust_20 <- kmeans(top250_cluster_ready, centers = 2, nstart = 20) # let's start with 2 clusters (small and big business as the assumption + 20 iterations)

clust_20 # flairly low within cluster score "compacteness" score ~= 60%
# let's use below function to then plot optimal no. of clusters to choose from 

wssplot <- function(data, nc = 15, seed = 123){
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of clusters",
       ylab = "Sum of squares within a group")}

wssplot(top250_cluster_ready, nc = 20) # 7 is acutally not a bad start

clust_20_4_clusters <- kmeans(top250_cluster_ready, centers = 4, nstart = 20) # we still reduce wihtin group variance untill we reach 4 clusters - there are diminishing 
# returns in paritioning the data with more clusters (beyond 4)

library(cluster)
library(factoextra)

sil <- silhouette(clust_20_4_clusters$cluster, dist(top250_cluster_ready))
fviz_silhouette(sil)

clusplot(top250_cluster_ready, clust_20_4_clusters$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
top250_cleaned$cluster <- clust_20_4_clusters$cluster

ggplot(top250_cleaned, aes(x = log(Units), y = log(Sales), label = Restaurant)) + 
  geom_point(aes(col = as.character(cluster)), size = 2) +
  geom_text(size = 2) + 
  labs(col = "Cluster",
       x = "Log of Units",
       y = "Log of Sales") + 
  ggtitle("Sales Against Units by Cluster") +
  theme(plot.title = element_text(hjust = .5)) 

# even with a significant 4% decrease in units sold YoY (so compared to 2019), Subway still outperformed the competition by leading in number of units sold in 2020
# it's also clear that, based on sales and units sold, McDonalds, Starbucks, and Subway are on a "league" of their own and that they are pandemic "proof" although more data is 
# porbbaly needed accross time to confirm this 

## Future 50 dataset
# let's run a annova to see whether franc hises were "spared" relative to non-franchises - since it's fairly balanced proportions (21 non franchises vs 29 franchises)
table(future_50$Franchising)
# let's convert a few columns (with % symbol) to numeric first
future_50_cleaned <- 
  future_50 %>%
  mutate(yoy_sales = as.numeric(str_replace(YOY_Sales, "%", "")),
         yoy_units = as.numeric(str_replace(YOY_Units, "%", "")))

anova_units_sold <- aov(Units ~ Franchising, data = future_50_cleaned)
anova_revenue_sold <- aov(Sales ~ Franchising, data = future_50_cleaned)
anova_units_sold$coefficients # 
anova_revenue_sold$coefficients
summary(anova_units_sold) # signficant mean difference in sold for franchises vs non franchises
summary(anova_revenue_sold) # no significant difference

# pandemic "spared" number of units sold for franchisers versus non-franchises but pandemic hit franchiser and non franchiser revenue alike
## this makes sense in the sense that franchises typically have a much bigger share of the market than non franchisers
