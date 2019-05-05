library(ggplot2)
library(data.table)
library(matrixStats)

# convert "gracefully" from the CSV
data <- t(fread("ValarMorghulis/predictionMatrix.csv"))
colnames(data) <- data[1,]
data <- data[-1,]

write.csv2(data, file = "processed.csv",
           quote = FALSE)

processedData <- data.frame(fread("processed.csv"))
processedData$score <- rep(0, nrow(processedData))

sameSign <- function(x1, x2) {
  if (x1 * x2 > 0) {
    return(TRUE)
  }
  return(FALSE)
}

for (i in 2:nrow(processedData)) {
  for (j in 2:ncol(processedData)) {
    if (sameSign(processedData[i,j],processedData[1,j])) {
      processedData[i,27] <- processedData[i,27] + abs(processedData[i,j])
    }
  }
}

scores <- data.frame(name = processedData$V1,
                     score = processedData$score)

plotScores <- scores[order(scores$score),]


ggplot(plotScores[2:19,],
       aes(x = reorder(name, score),
           y = score,
           fill = score)) +
  geom_bar(stat = "identity") + 
  scale_fill_continuous(low = "black",
                    high = "green") +
  coord_flip() + 
  labs(fill = "Score") + 
  ggtitle("All men must die... but most of them lived.")

sdData <- as.matrix(processedData[2:19,2:26])

means <- colMeans(sdData)
sds <- colSds(sdData)

characterNames <- colnames(processedData)[2:26]

stats <- data.frame(name <- rep(0, 25),
                    sd <- rep(0, 25),
                    mean <- rep(0, 25))
stats$name <- characterNames
stats$sd <- sds
stats$mean <- means

stats$sd <- as.numeric(stats$sd)
stats$mean <- as.numeric(stats$mean)

ggplot(stats,
       aes(x = reorder(name, sd),
           y = sd,
           fill = sd)) +
  geom_bar(stat = "identity") + 
  scale_fill_continuous(low = "white",
                        high = "purple") +
  coord_flip() + 
  labs(fill = "Standard Deviation") + 
  ylab("Standard Deviation") + 
  xlab("Character") + 
  ggtitle("We were divided on some importnat characters... and some less important ones.")

  
stats$mean <- as.numeric(stats$mean)

ggplot(stats,
       aes(x = reorder(name, mean),
           y = mean,
           fill = mean)) +
  geom_bar(stat = "identity") + 
  scale_fill_continuous(low = "red",
                        high = "blue") +
  coord_flip() + 
  labs(fill = "Mean") + 
  ylab("Mean bet (negative indicates death)") + 
  xlab("Character") + 
ggtitle("We were too bullish on death, but we at least caught most of the ones that happened")
