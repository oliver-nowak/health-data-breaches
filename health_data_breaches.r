
# install.packages('plotly')
# install.packages('plyr')
# install.packages('qwraps2')
# install.packages('tidyr')

library(dplyr)
library(tidyr)
library(plotly)
library(qwraps2)
library(sampling)

options(qwraps2_markup = "markdown")
options(warn=-1)

breach_df <- read.csv('/Users/nowak/Development/CS544/health-data-breaches/source_data/breach_report.csv')

# convert factor type to date type
breach_df$Breach.Submission.Date <- as.Date(breach_df$Breach.Submission.Date, "%m/%d/%y")

breach_df <- separate(breach_df, Breach.Submission.Date, into = c("Year", "Month", "Day"), sep="-") 
breach_df$Year <- as.factor(breach_df$Year)
breach_df$Month <- as.factor(breach_df$Month)
breach_df$Day <- as.factor(breach_df$Day)

breach_df$Type.of.Breach <- as.character(breach_df$Type.of.Breach)

breach_df$Type.of.Breach[grep("Hacking/IT Incident", breach_df$Type.of.Breach)] <- "Hacking/IT Incident"
breach_df$Type.of.Breach[grep("Improper Disposal", breach_df$Type.of.Breach)] <- "Improper Disposal"
breach_df$Type.of.Breach[grep("Loss,", breach_df$Type.of.Breach)] <- "Loss"
breach_df$Type.of.Breach[grep("Other,", breach_df$Type.of.Breach)] <- "Other"
breach_df$Type.of.Breach[grep("Theft,", breach_df$Type.of.Breach)] <- "Theft"
breach_df$Type.of.Breach[breach_df$Type.of.Breach==''] <- "Unknown"

breach_df$Type.of.Breach <- as.factor(breach_df$Type.of.Breach)

summary(breach_df$Type.of.Breach)

# Desktop Computer - fuzzy match; combine into 1 factor
breach_df$Location.of.Breached.Information <- as.character(breach_df$Location.of.Breached.Information)

breach_df$Location.of.Breached.Information[grep("Desktop Computer", breach_df$Location.of.Breached.Information)] <- "Desktop Computer"
breach_df$Location.of.Breached.Information[grep("Electronic Medical Record", breach_df$Location.of.Breached.Information)] <- "Electronic Medical Records"
breach_df$Location.of.Breached.Information[grep("Email", breach_df$Location.of.Breached.Information)] <- "Email"
breach_df$Location.of.Breached.Information[grep("Laptop", breach_df$Location.of.Breached.Information)] <- "Laptop"
breach_df$Location.of.Breached.Information[grep("Network Server", breach_df$Location.of.Breached.Information)] <- "Network Server"
breach_df$Location.of.Breached.Information[grep("Paper/Films", breach_df$Location.of.Breached.Information)] <- "Paper/Films"
breach_df$Location.of.Breached.Information[grep("Other", breach_df$Location.of.Breached.Information)] <- "Other"
breach_df$Location.of.Breached.Information[breach_df$Location.of.Breached.Information==''] <- "Unknown"

breach_df$Location.of.Breached.Information <- as.factor(breach_df$Location.of.Breached.Information)

summary(breach_df$Location.of.Breached.Information)

breach_df$Covered.Entity.Type <- as.character(breach_df$Covered.Entity.Type)
breach_df$Covered.Entity.Type[breach_df$Covered.Entity.Type==''] <- "Unknown"
breach_df$Covered.Entity.Type <- factor(breach_df$Covered.Entity.Type)
summary(breach_df$Covered.Entity.Type)

# clean NA's in raw data (needed for downstream plots that include the large 3x magnitude values)
breach_df$Individuals.Affected[is.na(breach_df$Individuals.Affected)] <- 0


# function for removing outliers 
# -- needed for density plots, since the outliers are 2-3 orders of magnitude larger than IQR
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# store the processed data
breach_df$Individuals.Affected.Normalized.Outliers <- remove_outliers(breach_df$Individuals.Affected)

glimpse(breach_df)

summary(breach_df[,names(breach_df) != "Web.Description"])

summary(breach_df$Individuals.Affected)
summary(breach_df$Individuals.Affected.Normalized.Outliers)

plot_ly(breach_df, x=na.omit(breach_df$Individuals.Affected.Normalized.Outliers), type='box', boxpoints = FALSE) %>%
  layout(title = "Boxplot of Individuals Affected (Normalized)",
         xaxis = list (title = "Number of Records"))

plot_ly(breach_df, y=na.omit(breach_df$Individuals.Affected.Normalized.Outliers), type='scatter', mode='markers', color = na.omit(breach_df$Individuals.Affected.Normalized.Outliers), colors = "Set1") %>%
  layout(title = "Individuals Affected - Color Gradient",
         xaxis = list (title = "Index"),
         yaxis = list (title = "Number of Records"))

dd <- arrange(breach_df, Individuals.Affected.Normalized.Outliers)

plot_ly(dd, y=~Individuals.Affected.Normalized.Outliers, type='scatter', mode='markers', color = ~Type.of.Breach, colors = "Set1") %>%
  layout(title = "Individuals Affected by Type of Breach - Index Ordered",
         xaxis = list (title = "Index"),
         yaxis = list (title = "Number of Records"))

fit <- density(na.omit(breach_df$Individuals.Affected.Normalized.Outliers))
plot_ly(x = na.omit(breach_df$Individuals.Affected.Normalized.Outliers), type = "histogram",  histnorm = "probability", name="Histogram") %>%
  add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
  layout(title="Density of Individuals Affected (Normalized)", yaxis2 = list(overlaying = "y", side = "right"))

plot_ly(breach_df, x=~Month, y=na.omit(breach_df$Individuals.Affected), type='bar') %>%
  layout(title="Individuals Affected per Month", yaxis = list(title = 'Records Compromised'), barmode = 'stack')

# source: https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html
# Summary Per Year
summary_data <- 
    list("Individuals Affected" = 
        list("min" = ~prettyNum(min(Individuals.Affected), big.mark=","),
             "max" = ~prettyNum(max(Individuals.Affected), big.mark=","),
             "mean (sd)" = ~ mean_sd(Individuals.Affected, denote_sd = "paren")))
summary_table(group_by(breach_df, Year), summary_data)

summary_table(group_by(breach_df, Month), summary_data)

df.2009 <- filter(breach_df, Year=='2009')
df.2010 <- filter(breach_df, Year=='2010')
df.2011 <- filter(breach_df, Year=='2011')
df.2012 <- filter(breach_df, Year=='2012')
df.2013 <- filter(breach_df, Year=='2013')
df.2014 <- filter(breach_df, Year=='2014')
df.2015 <- filter(breach_df, Year=='2015')
df.2016 <- filter(breach_df, Year=='2016')


p.2010 <- plot_ly(df.2010, x=~Month, y=~Individuals.Affected, type='bar', name='2010')
p.2011 <- plot_ly(df.2011, x=~Month, y=~Individuals.Affected, type='bar', name='2011')
p.2012 <- plot_ly(df.2012, x=~Month, y=~Individuals.Affected, type='bar', name='2012')
p.2013 <- plot_ly(df.2013, x=~Month, y=~Individuals.Affected, type='bar', name='2013')
p.2014 <- plot_ly(df.2014, x=~Month, y=~Individuals.Affected, type='bar', name='2014')
p.2015 <- plot_ly(df.2015, x=~Month, y=~Individuals.Affected, type='bar', name='2015')
p.2016 <- plot_ly(df.2016, x=~Month, y=~Individuals.Affected, type='bar', name='2016')

subplot(p.2010, p.2011, p.2012, p.2013, p.2014, p.2015, p.2016, nrows=7, shareX = TRUE) %>% 
layout(title="Individuals Affected per Month - Yearly")

plot_ly(df.2010, x=~Month, y=~Individuals.Affected, type='bar', name='2010') %>%
add_trace(x=df.2011$Month,y=df.2011$Individuals.Affected, name='2011') %>%
add_trace(x=df.2012$Month,y=df.2012$Individuals.Affected, name='2012') %>%
add_trace(x=df.2013$Month,y=df.2013$Individuals.Affected, name='2013') %>%
add_trace(x=df.2014$Month,y=df.2014$Individuals.Affected, name='2014') %>%
add_trace(x=df.2015$Month,y=df.2015$Individuals.Affected, name='2015') %>%
add_trace(x=df.2016$Month,y=df.2016$Individuals.Affected, name='2016') %>%
  layout(title="Individuals Affected per Month - Grouped", yaxis = list(title = 'Individuals Affected'))

plot_ly(df.2010, x=~Month, y=~Individuals.Affected, type='bar', name='2010') %>%
add_trace(x=df.2011$Month,y=df.2011$Individuals.Affected, name='2011') %>%
add_trace(x=df.2012$Month,y=df.2012$Individuals.Affected, name='2012') %>%
add_trace(x=df.2013$Month,y=df.2013$Individuals.Affected, name='2013') %>%
add_trace(x=df.2014$Month,y=df.2014$Individuals.Affected, name='2014') %>%
add_trace(x=df.2015$Month,y=df.2015$Individuals.Affected, name='2015') %>%
add_trace(x=df.2016$Month,y=df.2016$Individuals.Affected, name='2016') %>%
  layout(title="Individuals Affected per Month - Stacked", yaxis = list(title = 'Individuals Affected'), barmode='stack')

num_samples <- length(breach_df$Individuals.Affected.Normalized.Outliers)
sample_size_10 <- 10


data_10 <- numeric(num_samples)
for (i in 1:num_samples) {
    data_10[i] <- mean(sample(na.omit(breach_df$Individuals.Affected.Normalized.Outliers), size=sample_size_10, replace=TRUE))
}

n_10 <- plot_ly(x = data_10, type = "histogram",  histnorm = "probability") 

mean_10 <- mean(data_10)
std_10 <- sd(data_10)


conf <- c(80, 90)
alpha <- 1 - conf/100
xbar <- mean_10
sd.sample.means <- std_10 / sqrt(sample_size_10)

cat("Confidence Level - Sample Size = 10\n\n")

for (i in alpha) {
    str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f", 100*(1-i), i, 
                  xbar - qnorm(1-i/2) * sd.sample.means, 
                  xbar + qnorm(1-i/2) * sd.sample.means)
    cat(str, "\n")
}
cat("\n")
cat("Mean = ", mean_10, " SD = ", std_10, "\n")

num_samples <- length(breach_df$Individuals.Affected.Normalized.Outliers)
sample_size_100 <- 100


data_100 <- numeric(num_samples)
for (i in 1:num_samples) {
    data_100[i] <- mean(sample(na.omit(breach_df$Individuals.Affected.Normalized.Outliers), size=sample_size_100, replace=TRUE))
}

n_100 <- plot_ly(x = data_100, type = "histogram",  histnorm = "probability") 

mean_100 <- mean(data_100)
std_100 <- sd(data_100)

conf <- c(80, 90)
alpha <- 1 - conf/100
xbar <- mean_100
sd.sample.means <- std_100 / sqrt(sample_size_100)

cat("Confidence Level - Sample Size = 100\n\n")

for (i in alpha) {
    str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f", 100*(1-i), i, 
                  xbar - qnorm(1-i/2) * sd.sample.means, 
                  xbar + qnorm(1-i/2) * sd.sample.means)
    cat(str, "\n")
}
cat("\n")
cat("Mean = ", mean_100, " SD = ", std_100, "\n")

num_samples <- length(breach_df$Individuals.Affected.Normalized.Outliers)
sample_size_1000 <- 1000


data_1000 <- numeric(num_samples)
for (i in 1:num_samples) {
    data_1000[i] <- mean(sample(na.omit(breach_df$Individuals.Affected.Normalized.Outliers), size=sample_size_1000, replace=TRUE))
}

n_1000 <- plot_ly(x = data_1000, type = "histogram",  histnorm = "probability") 

mean_1000 <- mean(data_1000)
std_1000 <- sd(data_1000)

conf <- c(80, 90)
alpha <- 1 - conf/100
xbar <- mean_1000
sd.sample.means <- std_1000 / sqrt(sample_size_1000)

cat("Confidence Level - Sample Size = 1000\n\n")

for (i in alpha) {
    str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f", 100*(1-i), i, 
                  xbar - qnorm(1-i/2) * sd.sample.means, 
                  xbar + qnorm(1-i/2) * sd.sample.means)
    cat(str, "\n")
}
cat("\n")
cat("Mean = ", mean_1000, " SD = ", std_1000, "\n")

plot_ly(alpha=0.6) %>%
add_histogram(x=data_10, name='10 samples', type = "histogram",  histnorm = "probability") %>%
add_histogram(x=data_100, name='100 samples', type = "histogram",  histnorm = "probability") %>%
add_histogram(x=data_1000, name='1000 samples', type = "histogram",  histnorm = "probability") %>%
layout(title="Density Histogram of Samples", barmode='overlay',
      xaxis=list(title="Individuals Affected"), yaxis=list(title="Density"))

## Random Sampling Without Replacement
s <- srswor(500, nrow(breach_df))

# get row numbers from the sample bit field
rows <- (1:nrow(breach_df))[s!=0]
rows <- rep(rows, s[s != 0])
# store the sampled row data
sample.1 <- breach_df[rows, ]


## Systematic Sampling
# total rows
N <- nrow(breach_df)

# num samples
n <- 500

# number of rows per num samples
k <- ceiling(N / n)

# a sample
r <- sample(k, 1)

# select every kth item
s <- seq(r, by = k, length = n)

# store sampled data set
sample.2 <- breach_df[s, ]

## Sampling based on Inclusion Probabilities
# generate inclusion probs for every row, based on sample size
pik <- inclusionprobabilities(na.omit(breach_df$Individuals.Affected.Normalized.Outliers), 500)

# sample data based on inclusion probabilities and configured sample size
s <- UPsystematic(pik)

# store data
sample.3 <- breach_df[s != 0, ]

plot_ly(alpha=0.4) %>%
add_histogram(x=sample.1$Individuals.Affected.Normalized.Outliers, name='Random Sampling Without Replacement', type = "histogram",  histnorm = "probability") %>%
add_histogram(x=sample.2$Individuals.Affected.Normalized.Outliers, name='Systematic Sampling', type = "histogram",  histnorm = "probability") %>%
add_histogram(x=sample.3$Individuals.Affected.Normalized.Outliers, name='Inclusion Probabilities', type = "histogram",  histnorm = "probability") %>%
layout(title="Individuals Affected using Random Sampling Techniques (500 Samples)", barmode='overlay',
       xaxis=list(title="Individuals Affected"),
       yaxis=list(title="Density"))

fit.1 <- density(na.omit(sample.1$Individuals.Affected.Normalized.Outliers))
fit.2 <- density(na.omit(sample.2$Individuals.Affected.Normalized.Outliers))
fit.3 <- density(na.omit(sample.3$Individuals.Affected.Normalized.Outliers))

plot_ly() %>%
  add_trace(x = fit.1$x, y=fit.1$y, type='scatter', mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Random Sampling Without Replacement") %>% 
  add_trace(x = fit.2$x, y=fit.2$y, type='scatter', mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Systematic Sampling") %>% 
  add_trace(x = fit.3$x, y=fit.3$y, type='scatter', mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Inclusion Probabilities") %>% 
  layout(title="Density Curves (500 Samples)", yaxis2 = list(overlaying = "y", side = "right"))

breach_df$Name.of.Covered.Entity <- as.character(breach_df$Name.of.Covered.Entity)
x <- breach_df %>%
  group_by(Name.of.Covered.Entity) %>%
  count(Name.of.Covered.Entity) %>%
  arrange(desc(n))
x <- x[1:5,]

x <- arrange(x, desc(n))

plot_ly(x, y=x$Name.of.Covered.Entity, x=x$n, orientation = 'h', type='bar') %>%
  # labeling the y-axis
  add_annotations(xref = 'paper', yref = 'y', x = 0.5, y = x$Name.of.Covered.Entity,
                  xanchor = 'right',
                  text = x$Name.of.Covered.Entity,
                  font = list(family = 'Arial', size = 16,
                            color = 'rgb(255, 255, 255)'),
                  showarrow = FALSE, align = 'right') %>%
  layout(title = "Top 5 Breach Targets",
         xaxis = list(title = "Number of Breach"),
         yaxis = list(title = "")) 

breaches_per_year <- as.data.frame(count(breach_df, Year))
plot_ly(breaches_per_year, x=~Year, y=~n, type='bar', color=~Year, colors="Set1") %>%
layout(title="Breaches per Year",
       yaxis=list(title="Number of Breaches"))

breaches_per_month <- as.data.frame(count(breach_df, Month))
plot_ly(breaches_per_month, x=~Month, y=~n, type='bar', color=~Month, colors="Set1") %>%
layout(title="Breaches per Month",
       yaxis=list(title="Number of Breaches"))

breach_df %>%
group_by(Type.of.Breach) %>%
summarize(count=n()) %>%
plot_ly(labels = ~Type.of.Breach, values = ~count, insidetextfont = list(color = '#FFFFFF')) %>%
add_pie(hole = 0.6) %>%
layout(title="Type of Breach", showlegend = T,
       xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
       yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

breach_df %>%
group_by(Covered.Entity.Type) %>%
summarize(count=n()) %>%
plot_ly(labels = ~Covered.Entity.Type, values = ~count, insidetextfont = list(color = '#FFFFFF')) %>%
add_pie(hole = 0.6) %>%
layout(title="Covered Entity Type Involved in Breach", showlegend = T,
       xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
       yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

breach_df %>%
group_by(Location.of.Breached.Information) %>%
summarize(count=n()) %>%
plot_ly(labels = ~Location.of.Breached.Information, values = ~count, insidetextfont = list(color = '#FFFFFF')) %>%
add_pie(hole = 0.6) %>%
layout(title="Location of Records Involved in Breach", showlegend = T,
       xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
       yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

count_per_state <- count(breach_df, State)

# max number of breaches in US
max_idx <- row(count_per_state)[count_per_state==max(count_per_state$n)]

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_geo(count_per_state, locationmode = 'USA-states') %>%
  add_trace(
    z = ~n, locations = ~State,
    color = ~n, colors = 'Reds'
  ) %>%
  colorbar(title = "Number of Breaches") %>%
  layout(
    title = 'Breaches Per State',
    geo = g
  )




