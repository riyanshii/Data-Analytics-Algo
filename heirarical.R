df <- USArrests
df <- na.omit(df)
df <- scale(df)

d <- dist(df, method = "euclidean")
hc1 <- hclust(d, method = "complete" )

plot(hc1, cex = 0.6, hang = -1)

