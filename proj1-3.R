####### Proj. 1 - Quest. 1 #######
library(OIdata)
library(ggplot2)
library(gridExtra)

alpha <- 5.4
beta <- 1

mu <- 5.4
sd <- sqrt(5.4)
meanData <- c()
sdData <- c()
sample_n <- c(1, 5, 10, 20, 30, 40)

for (n in sample_n) {  
  xbar <- 0.0
  mean_dist <- c() # all of the mean values for each sampling
  
  # calculate the sampling distribution of means and some other things
  for (i in 1:1000) {
    dist <- rgamma(n, alpha, rate=beta)
    mean <- mean(dist)
    mean_dist <- c(mean_dist, mean)
    xbar <- xbar + mean
  }
  xbar <- xbar / 1000.0 # calculate the average of the 1000 runs
  meanData <- c(meanData, xbar)
  sdData <- c(sdData, sd(mean_dist))

  # Make the graphs
  hist(mean_dist, xlab="Sample xbar", ylab="Frequency", 
       main=paste("Sample Mean Gamma Distribution for N=", n), las=1, xlim=c(0, 15))
  qqnorm(mean_dist, main=paste("Sample Mean Gamma Quantile Plot for N=", n), las=1)
  qqline(mean_dist)
}

# make table
mean_dist_table <- matrix(c(sample_n, meanData, rep(mu, length(sample_n)), sdData, 
                            rep(sd, length(sample_n))), ncol=5)
colnames(mean_dist_table) <- c("n", "mean of 1000 xbar's", "theoretical mean", 
                               "std. dev. of 1000 xbar's", "theoretical std dev.")
mean_dist_table <- as.table(mean_dist_table)#, col.names=colnames)

qplot(1:10, 1:10, geom="blank") +
theme_bw() + theme(line=element_blank(), text=element_blank()) +
  annotation_custom(grob=tableGrob(mean_dist_table, gpar.coltext=gpar(cex=1.2), 
                                   gpar.rowtext=gpar(cex=1.2)), xmin=-Inf, xmax=Inf, 
                                    ymin=-Inf, ymax=Inf)