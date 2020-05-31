
DataSet <- read.csv(file="FEI_CITY_200531102910.csv",header=T,fileEncoding="SJIS")

head(DataSet)
DataToCluster <- DataSet[, c("A4200_死亡数.人.", "A9101_婚姻件数.組.", "A4101_出生数.人.", "A9201_離婚件数.組.")]
head(DataToCluster)

year = c()

for (variable in DataSet$調査年) {
  print(sub("年度", "", variable))
  year = c(year, c(strtoi(sub("年度", "", variable))))
}


year
length(year)

DataToCluster

DataToCluster$A4200_死亡数.人. = as.numeric(DataToCluster$A4200_死亡数.人.)
DataToCluster$A9101_婚姻件数.組. = as.numeric(DataToCluster$A9101_婚姻件数.組.)
DataToCluster$A4101_出生数.人. = as.numeric(DataToCluster$A4101_出生数.人.)
DataToCluster$A9201_離婚件数.組. = as.numeric(DataToCluster$A9201_離婚件数.組.)

DataToCluster$year = year
DataToCluster
distance <- dist(DataToCluster) #ユークリッド距離を求める
# 樹形図作成
hc <- hclust(distance, "ward.D2")
hc

plot(hc)

result <- cutree(hc,k=3)
result

result_df <- DataToCluster

result_df$result = result
result_df$area = DataSet$地域
result_df
head(DataToCluster)


pca = prcomp(DataToCluster)

biplot(pca)
