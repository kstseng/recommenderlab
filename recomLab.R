library(RODBC)
conn <- odbcConnect(dsn = "ACLSQL7_SQL2008R2", uid = "twWebApp", pwd = "twweb")
dat <- sqlQuery(conn, "SELECT [UserID], [URL], [RequestTime] 
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-07-19'")
dataInfo_id <- as.matrix(read.csv("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\advantechProject\\Recommendation_CF\\dataInfo_id.csv", header=T))

library(recommenderlab)
library(plyr); library(dplyr)
library(reshape2)
##
ml <- sapply(1:nrow(dataInfo_id), function(i){
  tmpRow <- dataInfo_id[j, ]
  blank <- as.numeric(which(tmpRow == ""))
  if (length(blank) != 0){
    out <- as.character(tmpRow[min(blank) - 1])
  }else{
    out <- as.character(tmpRow[length(tmpRow)])
  }
  return(out)
})

##
## split the url
##
dat <- as.data.frame(dat)
if(length(which(dat$UserID == "00000000-0000-0000-0000-000000000000")) > 0)dat <- dat[-which(dat$UserID == "00000000-0000-0000-0000-000000000000"), ]
url <- as.character(select(dat, URL)[, 1])
splitURL <- function(url_i){
  tmp1 <- strsplit(url_i, "\\://|\\.|\\/")[[1]]
  #   if ("products" %in% tmp1 & "aspx" %in% tmp1){
  if (sum(grepl("products", tmp1)) > 0 & sum(grepl("aspx", tmp1)) > 0){
    num <- which(substring(tmp1, first = 1, 3) == "mod" | substring(tmp1, first = 1, 3) == "sub") 
    if (length(num) != 0){
      num <- max(num)
      mer <- toupper(substring(tmp1[num], first = 5))
    }else{
      mer <- toupper(tmp1[length(tmp1) - 2])
    }
  }else{
    mer <- NA
  }
  return(mer)
}
product <- unlist(sapply(1:length(url), function(i){
  print(i/length(url))
  splitURL(url[i])
}))
if(length(which(product == "ETHERNET")) > 0)product[which(product == "ETHERNET")] <- "EN50155_INDUSTRIAL_ETHERNET_SWITCHES"
if(length(which(product == "BACNET_REMOTE_I")) > 0)product[which(product == "BACNET_REMOTE_I")] <- "BACNET_REMOTE_I/O_MODULES"
UserProduct <- cbind(as.character(dat$UserID), product)
UserProduct <- UserProduct[which(UserProduct[, 2] %in% dataInfo_id[, 1]), ]
colnames(UserProduct) <- c("UserID", "Product")
UserProduct <- as.data.frame(UserProduct)
# UserProductRM <- as.data.frame(UserProduct %>% group_by(UserID, Product) %>% mutate(rating = n()))
idTab <-  as.data.frame(sort(table(UserProduct$UserID),decreasing=T))
colnames(idTab) <- "count"
rollInID <- rownames(idTab[which(idTab$count >= 3), ])
UserProductRollIn <- UserProduct[(which(UserProduct$UserID %in% rollInID)), ]
UserProductRM <- as.data.frame(UserProductRollIn %>% group_by(UserID, Product) %>% mutate(rating = n()))
###
###
###
# g <- acast(UserProductRM, UserID ~ Product)
g <- acast(UserProductRM, UserID ~ Product, fun.aggregate = sum, fill = as.numeric(NA))
# g <- dcast(UserProductRM, UserID ~ Product, value.var = 'rating', fun.aggregate = sum)

R <- as.matrix(g)

# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
UP.rm <- as(R, "realRatingMatrix") # UP.rm -> user product rating matrix

# Draw an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items
image(UP.rm, main = "Raw Ratings")       

hist(log(rowCounts(UP.rm)), breaks = 10)
image(sample(UP.rm, 200))
hist(colCounts(UP.rm), breaks = 25)
#
# remove users which their rowCounts are strangely large.
#
#
length(which(rowCounts(UP.rm) >= 10))

# r <- Recommender(UP.rm[1:300], method = "POPULAR")
num <- 15000
r <- Recommender(UP.rm[1:num],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
r <- Recommender(UP.rm[1:num],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
r <- Recommender(UP.rm[1:num],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
r <- Recommender(UP.rm[1:num],method="POPULAR")

r
getModel(r)
recom <- predict(r, UP.rm[(num + 1):(num + 5)], n = 5)
recom <- predict(r, UP.rm[(num + 1)], n = 5)
recom
as(recom, "list")
recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")


scheme <- evaluationScheme(UP.rm, method = "cross", k = 4, given = 1, goodRating=3)
scheme
# results <- evaluate(scheme, method = "POPULAR", n = c(1, 3, 5, 10))
# results
# getConfusionMatrix(results)[[1]]
# avg(results)
# plot(results, annotate = TRUE)
# plot(results, "prec/rec", annotate = TRUE)
algorithms <- list(`random items` = list(name = "RANDOM",param = NULL), 
                   `popular items` = list(name = "POPULAR",param = NULL), 
                   `user-based CF` = list(name = "UBCF",param = list(method = "Jaccard", nn = 10)), 
                   `item-based CF` = list(name = "IBCF",param = list(method = "Jaccard", k = 10)))
results <- evaluate(scheme, algorithms, n = c(1, 3, 5, 10))
results
getConfusionMatrix(results)[[1]]

avg(results)


plot(results, annotate = c(1, 3), legend = "right")
plot(results, "prec/rec", annotate = 3)

scheme1 <- evaluationScheme(UP.rm, method = "cross", k = 4, given = 1)
scheme1
results1 <- evaluate(scheme1, algorithms, n = c(1, 3, 5, 10))
plot(results1, annotate = c(1, 3), legend = "right")



