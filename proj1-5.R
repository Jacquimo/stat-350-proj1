####### Proj. 1 - Quest. 1 #######
library(OIdata)
library(ggplot2)
library(gridExtra)

p <- 0.1

#mu <- 0.5
#sd <- 0.5
meanData <- c()
sdData <- c()
sample_n <- c(1, 5, 10, 20, 30, 40, 50)
mu_theory <- c()
sd_theory <- c()

for (n in sample_n) {  
  mu <- n * p
  mu_theory <- c(mu_theory, mu)
  sd <- sqrt(p * (1 - p))
  sd_theory <- c(sd_theory, sd)
  
  xbar <- 0.0
  mean_dist <- c() # all of the mean values for each sampling
  
  # calculate the sampling distribution of means and some other things
  for (i in 1:1000) {
    # dist <- rexp(n, rate=lambda)
    dist <- rbinom(n, n, p)
    mean <- mean(dist)
    mean_dist <- c(mean_dist, mean)
    xbar <- xbar + mean
  }
  xbar <- xbar / 1000.0 # calculate the average of the 1000 runs
  meanData <- c(meanData, xbar)
  sdData <- c(sdData, sd(mean_dist))

  # Make the graphs
  hist(mean_dist, xlab="Sample xbar", ylab="Frequency", 
       main=paste("Sample Mean Binomial Distribution for N=", n), las=1)#, xlim=c(0, 3))
  qqnorm(mean_dist, main=paste("Sample Mean Binomial Quantile Plot for N=", n), las=1)
  qqline(mean_dist)
}

# make table
mean_dist_table <- matrix(c(sample_n, meanData, mu_theory, sdData, 
                            sd_theory), ncol=5)
colnames(mean_dist_table) <- c("n", "mean of 1000 xbar's", "theoretical mean", 
                               "std. dev. of 1000 xbar's", "theoretical std dev.")
mean_dist_table <- as.table(mean_dist_table)#, col.names=colnames)

qplot(1:10, 1:10, geom="blank") +
theme_bw() + theme(line=element_blank(), text=element_blank()) +
  annotation_custom(grob=tableGrob(mean_dist_table, gpar.coltext=gpar(cex=1.2), 
                                   gpar.rowtext=gpar(cex=1.2)), xmin=-Inf, xmax=Inf, 
                                    ymin=-Inf, ymax=Inf)