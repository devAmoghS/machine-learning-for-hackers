# Loading the dataset
setwd('/home/amogh/Work/ML Projects/Machine Learning For Hackers/')
data.file<-file.path('Chapter2/data', 'gender_heights_weights.csv')
heights.weights<-read.csv(data.file, header=TRUE, sep=',')

head(heights.weights)
nrow(heights.weights)

# Making the heights vector
heights<-with(heights.weights, Height)
summary(heights)

# Creating our own mean and median methods
my.mean<-function(x) {
  return(sum(x) / length(x))
}

my.median<-function(x) {
  sorted.x<-sort(x)

  if(length(x) %% 2 == 0) {
    indices<-c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted.x[indices]))
  } else {
    index<-ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

# Testing out the methods
my.mean(heights)
my.median(heights)
mean(heights) - my.mean(heights)
median(heights) - median(heights)

# Range of the dataset
min(heights)
max(heights)
c(min(heights), max(heights))
range(heights)

# Deriving quantiles, default probs = 0.25
quantile(heights)
quantile(heights, probs=seq(0, 1, by=0.20))
# quantile(heights, probs=seq(0, 1, by=0.10))

# Looking at measure of spread
print("This is the range containing 50% of the data\n")
c(quantile(heights, probs=0.25), quantile(heights, probs=0.75))
print("This is the range containing 95% of the data\n")
c(quantile(heights, probs=0.025), quantile(heights, probs=0.975))

# Defining our own method for variance
my.var<-function(x) {
  m<-mean(x)
  return(sum((x-m)^2) / (length(x) - 1))
}

my.var(heights)
var(heights) - my.var(heights)

c(mean(heights) - var(heights), mean(heights) + var(heights))
range(heights)

my.sd<-function(x) {
  return(sqrt(my.var(x)))
}

sd(heights) - my.sd(heights)

c(mean(heights) - sd(heights), mean(heights) + sd(heights))
range(heights)

library("ggplot2")
hist.orig<-ggplot(heights.weights, aes(x=Height)) +
  geom_histogram(binwidth=1)
hist.undersmooth<-ggplot(heights.weights, aes(x=Height)) +
geom_histogram(binwidth=0.01)
hist.oversmooth<-ggplot(heights.weights, aes(x=Height)) +
  geom_histogram(binwidth=5)

heights.density<-ggplot(heights.weights, aes(x=Height)) + geom_density()

hgender.density<-ggplot(heights.weights, aes(x=Height, fill=Gender)) + geom_density()
wgender.density<-ggplot(heights.weights, aes(x=Weight, fill=Gender)) + geom_density()

hgender.facet<-ggplot(heights.weights, aes(x=Height, fill=Gender)) + geom_density() + facet_grid(Gender ~ .)
wgender.facet<-ggplot(heights.weights, aes(x=Weight, fill=Gender)) + geom_density() + facet_grid(Gender ~ .)

dir.create("Chapter2/images")
ggsave(plot = hist.orig,
  filename = file.path("Chapter2/images", "histogram_10000.pdf"),
  height = 6,
  width = 8)
ggsave(plot = hist.undersmooth,
  filename = file.path("Chapter2/images", "histogram_undersmooth_10000.pdf"),
  height = 6,
  width = 8)
ggsave(plot = hist.oversmooth,
  filename = file.path("Chapter2/images", "histogram_oversmooth_10000.pdf"),
  height = 6,
  width = 8)
ggsave(plot = heights.density,
  filename = file.path("Chapter2/images", "heights_density.pdf"),
  height = 6,
  width = 8)
ggsave(plot = hgender.density,
  filename = file.path("Chapter2/images", "gender_height_density.pdf"),
  height = 6,
  width = 8)
ggsave(plot = wgender.density,
  filename = file.path("Chapter2/images", "gender_weight_density.pdf"),
  height = 6,
  width = 8)
ggsave(plot = hgender.facet,
  filename = file.path("Chapter2/images", "gender_height_facet.pdf"),
  height = 6,
  width = 8)
ggsave(plot = wgender.facet,
  filename = file.path("Chapter2/images", "gender_weight_facet.pdf"),
  height = 6,
  width = 8)

# Plotting a standard normal distribution density plot
normal.mean<-0
normal.sd<-1
normal.curve<-ggplot(data.frame(X=rnorm(100000, normal.mean, normal.sd)), aes(x=X))+geom_density()

ggsave(plot = normal.curve,
  filename = file.path("Chapter2/images", "normal_dist_density.pdf"),
  height = 6,
  width = 8)

# Plotting a gamma distribution density plot
gamma.mean<-1
gamma.sd<-0.001
gamma.curve<-ggplot(data.frame(X=rgamma(100000, gamma.mean, gamma.sd)), aes(x=X))+geom_density()

ggsave(plot = gamma.curve,
  filename = file.path("Chapter2/images", "gamma_dist_density.pdf"),
  height = 6,
  width = 8)

# Plotting a standard cauchy distribution density plot
cauchy.mean<-1
cauchy.sd<-0
cauchy.curve<-ggplot(data.frame(X=rcauchy(250, cauchy.mean, cauchy.sd)), aes(x=X))+geom_density()

ggsave(plot = cauchy.curve,
  filename = file.path("Chapter2/images", "cauchy_dist_density.pdf"),
  height = 6,
  width = 8)

# Plotting a scaatterplot of heights and weights data
scatter.plot<-ggplot(heights.weights, aes(x=Height, y=Weight))+geom_point()
ggsave(plot = scatter.plot,
  filename = file.path("Chapter2/images", "scatter_plot.pdf"),
  height = 6,
  width = 8)

# Regression plot
regression.plot<-ggplot(heights.weights, aes(x=Height, y=Weight))+geom_point()+geom_smooth()
ggsave(plot = regression.plot,
  filename = file.path("Chapter2/images", "regression_plot.pdf"),
  height = 6,
  width = 8)

# Classification plot
heights.weights<-transform(heights.weights, Male = ifelse(Gender == 'Male', 1, 0))
head(heights.weights)
logit.model<-glm(Male ~ Height + Weight, data = heights.weights, family = binomial(link = 'logit'))

# using geom_abline, since stat_abline is depracated
classification.plot<-ggplot(heights.weights, aes(x=Weight, y=Height, color=Gender)) +
  geom_point() +
  geom_abline(intercept=-coef(logit.model)[1] / coef(logit.model)[2],
    slope = -coef(logit.model)[3] / coef(logit.model)[2],
    geom='abline',
    color='black')

ggsave(plot = classification.plot,
  filename = file.path("Chapter2/images", "classification_plot.pdf"),
  height = 6,
  width = 8)
