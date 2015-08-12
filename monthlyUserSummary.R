library(RODBC)
conn <- odbcConnect(dsn = "ACLSQL7_SQL2008R2", uid = "twWebApp", pwd = "twweb")
dat1 <- sqlQuery(conn, "SELECT [UserID]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-01-01' AND RequestTime < '2015-02-01'")
dat2 <- sqlQuery(conn, "SELECT [UserID]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-02-01' AND RequestTime < '2015-03-01'")
dat3 <- sqlQuery(conn, "SELECT [UserID]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-03-01' AND RequestTime < '2015-04-01'")
dat4 <- sqlQuery(conn, "SELECT [UserID]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-04-01' AND RequestTime < '2015-05-01'")
dat5 <- sqlQuery(conn, "SELECT [UserID]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-05-01' AND RequestTime < '2015-06-01'")
dat6 <- sqlQuery(conn, "SELECT [UserID]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-06-01' AND RequestTime < '2015-07-01'")
dat7 <- sqlQuery(conn, "SELECT [UserID]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-07-01' AND RequestTime < '2015-08-01'")
nrow(unique(dat4))
nrow(unique(dat5))
nrow(unique(dat6))
nrow(unique(dat7))

User1 <- as.character((dat1[, 1])); U1 <- as.data.frame((dat1[, 1])); colnames(U1) <- 'user'
User2 <- as.character((dat2[, 1])); U2 <- as.data.frame((dat2[, 1])); colnames(U2) <- 'user'
User3 <- as.character((dat3[, 1])); U3 <- as.data.frame((dat3[, 1])); colnames(U3) <- 'user'
User4 <- as.character((dat4[, 1])); U4 <- as.data.frame((dat4[, 1])); colnames(U4) <- 'user'
User5 <- as.character((dat5[, 1])); U5 <- as.data.frame((dat5[, 1])); colnames(U5) <- 'user'
User6 <- as.character((dat6[, 1])); U6 <- as.data.frame((dat6[, 1])); colnames(U6) <- 'user'
User7 <- as.character((dat7[, 1])); U7 <- as.data.frame((dat7[, 1])); colnames(U7) <- 'user'

uniUser1 <- as.character(unique(dat1[, 1]))
uniUser2 <- as.character(unique(dat2[, 1]))
uniUser3 <- as.character(unique(dat3[, 1]))
uniUser4 <- as.character(unique(dat4[, 1]))
uniUser5 <- as.character(unique(dat5[, 1]))
uniUser6 <- as.character(unique(dat6[, 1]))
uniUser7 <- as.character(unique(dat7[, 1]))
alluni <- c(uniUser1, uniUser2, uniUser3, uniUser4, uniUser5, uniUser6, uniUser7)
alluni <- c(uniUser5, uniUser6, uniUser7)
# alluser <- c(as.character(dat5[, 1]), as.character(dat6[, 1]), as.character(dat7[, 1]))
# alluserF <- c(as.character(dat6[, 1]), as.character(dat7[, 1]))
######################################
###################################### active user
######################################

userMatrix <- matrix(0, ncol = 4, nrow = length(alluni))
colnames(userMatrix) <- c("May", "Jun", "Jul", "number of month")
for(i in 1:length(alluni)){
  print(i/length(alluni))
  tmpid <- alluni[i] 
  #   monthly <- c(length(which(tmpid == User1)), length(which(tmpid == User2)), 
  #                length(which(tmpid == User3)), length(which(tmpid == User4)), 
  #                length(which(tmpid == User5)), length(which(tmpid == User6)), 
  #                length(which(tmpid == User7)))
  #   monthly <- c(length(which(tmpid == User5)), length(which(tmpid == User6)), 
  #                length(which(tmpid == User7)))
  monthly <- c(nrow(filter(U5, user == tmpid)), nrow(filter(U6, user == tmpid)), nrow(filter(U7, user == tmpid)))
  numOfMonth <- length(which(monthly > 0))
  userMatrix[i, ] <- c(monthly, numOfMonth)
}

length(which(userMatrix[, 1] > 0 & userMatrix[, 2] > 0 & userMatrix[, 3] > 0))
length(which(userMatrix[, 1] > 0 & userMatrix[, 2] > 0 & userMatrix[, 3] == 0))
length(which(userMatrix[, 1] > 0 & userMatrix[, 2] == 0 & userMatrix[, 3] > 0))
length(which(userMatrix[, 1] == 0 & userMatrix[, 2] > 0 & userMatrix[, 3] > 0))
length(which(userMatrix[, 1] > 0 & userMatrix[, 2] == 0 & userMatrix[, 3] == 0))
length(which(userMatrix[, 1] == 0 & userMatrix[, 2] > 0 & userMatrix[, 3] == 0))
length(which(userMatrix[, 1] == 0 & userMatrix[, 2] == 0 & userMatrix[, 3] > 0))

userMatrix[head(which(userMatrix[, 4] == 3), 10), ]
userMatrix[head(which(userMatrix[, 4] == 2), 10), ]
# TorN <- sapply(1:length(uniUser7), function(i){
#   print(i/length(uniUser7))
#   return ((uniUser7[i] %in% uniUser6 | uniUser7[i] %in% uniUser5))
# })
# 
# numOfMulUser <- sapply(1:sum(TorN), function(j){
#   print(j/sum(TorN))
#   return(length(which(uniUser5[TorN][j] == alluserF)))
# #   return(length(which(uniUser5[TorN][j] == alluni)))
# })
# sum(TorN)/length(TorN)
######################################
###################################### active product
######################################
dat1 <- sqlQuery(conn, "SELECT [URL]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-01-01' AND RequestTime < '2015-02-01'")
dat2 <- sqlQuery(conn, "SELECT [URL]
                 FROM [WebTracking].[dbo].[RequestLog]
                 WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-02-01' AND RequestTime < '2015-03-01'")
dat3 <- sqlQuery(conn, "SELECT [URL]
                 FROM [WebTracking].[dbo].[RequestLog]
                 WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-03-01' AND RequestTime < '2015-04-01'")
dat4 <- sqlQuery(conn, "SELECT [URL]
                 FROM [WebTracking].[dbo].[RequestLog]
                 WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-04-01' AND RequestTime < '2015-05-01'")
dat5 <- sqlQuery(conn, "SELECT [URL]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-05-01' AND RequestTime < '2015-06-01'")
dat6 <- sqlQuery(conn, "SELECT [URL]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-06-01' AND RequestTime < '2015-07-01'")
dat7 <- sqlQuery(conn, "SELECT [URL]
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-07-01' AND RequestTime < '2015-08-01'")
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
url1 <- c(as.character(dat1[, 1]));url2 <- c(as.character(dat2[, 1]));url3 <- c(as.character(dat3[, 1]));url4 <- c(as.character(dat4[, 1]))
url5 <- c(as.character(dat5[, 1]));url6 <- c(as.character(dat6[, 1]));url7 <- c(as.character(dat7[, 1]))
product1 <- unlist(sapply(1:length(url1), function(i){
  print(i/length(url1))
  splitURL(url1[i])
}))
product2 <- unlist(sapply(1:length(url2), function(i){
  print(i/length(url2))
  splitURL(url2[i])
}))
product3 <- unlist(sapply(1:length(url3), function(i){
  print(i/length(url3))
  splitURL(url3[i])
}))
product4 <- unlist(sapply(1:length(url4), function(i){
  print(i/length(url4))
  splitURL(url4[i])
}))
product5 <- unlist(sapply(1:length(url5), function(i){
  print(i/length(url5))
  splitURL(url5[i])
}))
product6 <- unlist(sapply(1:length(url6), function(i){
  print(i/length(url6))
  splitURL(url6[i])
}))
product7 <- unlist(sapply(1:length(url7), function(i){
  print(i/length(url7))
  splitURL(url7[i])
}))

allProduct <- c(product1, product2, product3, product4, product5, product6, product7)
allUniProduct <- unique(c(product1, product2, product3, product4, product5, product6, product7))
productMatrix <- matrix(0, nrow = length(allUniProduct), ncol = 8)
colnames(productMatrix) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "number")
for(j in 1:length(allUniProduct)){
  print(j/length(allUniProduct))
  tmpPro <- allUniProduct[j] 
  m <- c(length(which(tmpPro == product1)), length(which(tmpPro == product2)), length(which(tmpPro == product3)), length(which(tmpPro == product4)), 
         length(which(tmpPro == product5)), length(which(tmpPro == product6)), length(which(tmpPro == product7)))
  n <- length(which(m > 0))
  productMatrix[j, ] <- c(m, n)
}




