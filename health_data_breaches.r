
# Breaches of HIPAA-protected health data affecting 500+ people.

## As required by section 13402(e)(4) of the HITECH Act, the Secretary [of the Department of Health and Human Services] must post a list of breaches of unsecured protected health information affecting 500 or more individuals.

## Source(s)
* Data Provider: [DataWorld](https://data.world/health/health-data-breaches)
* Reference Data Provider: [Dept of Health and Human Services](https://ocrportal.hhs.gov/ocr/breach/breach_report.jsf)


## Install / Source Dependencies

# wordcloud code: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# install.packages('plotly')
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# install.packages('plyr')
# install.packages('qwraps2')
install.packages('tidyr')

library(dplyr)
library(tidyr)
library(plotly)
library(qwraps2)
library(sampling)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

options(qwraps2_markup = "markdown")

### Load Source Data

breach_df <- read.csv('/Users/nowak/Development/CS544/health-data-breaches/source_data/breach_report.csv')

# Clean Source Data

### Breach Submission Date - Date Normalization

# convert factor type to date type
breach_df$Breach.Submission.Date <- as.Date(breach_df$Breach.Submission.Date, "%m/%d/%y")
# head(breach_df)

### Breach Submission Date - Split into Columns / Convert to Factors

breach_df <- separate(breach_df, Breach.Submission.Date, into = c("Year", "Month", "Day"), sep="-") 
breach_df$Year <- as.factor(breach_df$Year)
breach_df$Month <- as.factor(breach_df$Month)
breach_df$Day <- as.factor(breach_df$Day)

### Type of Breach - Factor Normalization

breach_df$Type.of.Breach <- as.character(breach_df$Type.of.Breach)

breach_df$Type.of.Breach[grep("Hacking/IT Incident", breach_df$Type.of.Breach)] <- "Hacking/IT Incident"
breach_df$Type.of.Breach[grep("Improper Disposal", breach_df$Type.of.Breach)] <- "Improper Disposal"
breach_df$Type.of.Breach[grep("Loss,", breach_df$Type.of.Breach)] <- "Loss"
breach_df$Type.of.Breach[grep("Other,", breach_df$Type.of.Breach)] <- "Other"
breach_df$Type.of.Breach[grep("Theft,", breach_df$Type.of.Breach)] <- "Theft"
breach_df$Type.of.Breach[breach_df$Type.of.Breach==''] <- "Unknown"

breach_df$Type.of.Breach <- as.factor(breach_df$Type.of.Breach)

summary(breach_df$Type.of.Breach)
# levels(breach_df$Type.of.Breach)

### Location of Breached Information - Factor Normalization

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

### Covered Entity Type - Factor Normalization

breach_df$Covered.Entity.Type <- as.character(breach_df$Covered.Entity.Type)
breach_df$Covered.Entity.Type[breach_df$Covered.Entity.Type==''] <- "Unknown"
breach_df$Covered.Entity.Type <- factor(breach_df$Covered.Entity.Type)
summary(breach_df$Covered.Entity.Type)

### Individuals Affected - Factor Normalization

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

# Summary

glimpse(breach_df)

summary(breach_df[,names(breach_df) != "Web.Description"])

# Individuals Affected Summary Stats

summary(breach_df$Individuals.Affected)
summary(breach_df$Individuals.Affected.Normalized.Outliers)

plot_ly(breach_df, y=breach_df$Individuals.Affected.Normalized.Outliers, type='box', name='Only Whiskers', boxpoints = FALSE) %>%
  layout(title = "Boxplot of Individuals Affected (Normalized)",
         yaxis = list (title = "Number of Records"))

plot_ly(breach_df, y=~Individuals.Affected.Normalized.Outliers, type='scatter', mode='markers', color = ~Individuals.Affected.Normalized.Outliers, colors = "Set1") %>%
  layout(title = "Individuals Affected - Color Gradient",
         xaxis = list (title = "Index"),
         yaxis = list (title = "Number of Records"))

dd <- arrange(breach_df, Individuals.Affected.Normalized.Outliers)

plot_ly(dd, y=~Individuals.Affected.Normalized.Outliers, type='scatter', mode='markers', color = ~Type.of.Breach, colors = "Set1") %>%
  layout(title = "Individuals Affected by Type of Breach - Index Ordered",
         xaxis = list (title = "Index"),
         yaxis = list (title = "Number of Records"))

plot_ly(x = breach_df$Individuals.Affected.Normalized.Outliers, type = "histogram",  histnorm = "probability") %>%
layout(title="Density of Individuals Affected (Normalized)",
       xaxis=list(title="Number of Individuals"),
       yaxis=list(title="Density"))

# Individuals Affected per Month

plot_ly(breach_df, x=~Month, y=~Individuals.Affected, type='bar') %>%
  layout(title="Individuals Affected per Month", yaxis = list(title = 'Records Compromised'), barmode = 'stack')

# Individuals Affected Summary (Yearly)

# source: https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html
# Summary Per Year
summary_data <- 
    list("Individuals Affected" = 
        list("min" = ~prettyNum(min(Individuals.Affected), big.mark=","),
             "max" = ~prettyNum(max(Individuals.Affected), big.mark=","),
             "mean (sd)" = ~ mean_sd(Individuals.Affected, denote_sd = "paren")))
summary_table(group_by(breach_df, Year), summary_data)

# Individuals Affected Summary (Monthly)

summary_table(group_by(breach_df, Month), summary_data)

# Individuals Affected per Month - Yearly

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

# Individuals Affected per Month - Grouped

plot_ly(df.2010, x=~Month, y=~Individuals.Affected, type='bar', name='2010') %>%
add_trace(x=df.2011$Month,y=df.2011$Individuals.Affected, name='2011') %>%
add_trace(x=df.2012$Month,y=df.2012$Individuals.Affected, name='2012') %>%
add_trace(x=df.2013$Month,y=df.2013$Individuals.Affected, name='2013') %>%
add_trace(x=df.2014$Month,y=df.2014$Individuals.Affected, name='2014') %>%
add_trace(x=df.2015$Month,y=df.2015$Individuals.Affected, name='2015') %>%
add_trace(x=df.2016$Month,y=df.2016$Individuals.Affected, name='2016') %>%
  layout(title="Individuals Affected per Month - Grouped", yaxis = list(title = 'Individuals Affected'))

# Individuals Affected per Month - Stacked

plot_ly(df.2010, x=~Month, y=~Individuals.Affected, type='bar', name='2010') %>%
add_trace(x=df.2011$Month,y=df.2011$Individuals.Affected, name='2011') %>%
add_trace(x=df.2012$Month,y=df.2012$Individuals.Affected, name='2012') %>%
add_trace(x=df.2013$Month,y=df.2013$Individuals.Affected, name='2013') %>%
add_trace(x=df.2014$Month,y=df.2014$Individuals.Affected, name='2014') %>%
add_trace(x=df.2015$Month,y=df.2015$Individuals.Affected, name='2015') %>%
add_trace(x=df.2016$Month,y=df.2016$Individuals.Affected, name='2016') %>%
  layout(title="Individuals Affected per Month - Stacked", yaxis = list(title = 'Individuals Affected'), barmode='stack')

# Individuals Affected - Central Limit Theorem Validation

### Sample Size = 10

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

### Sample Size = 100

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

### Sample Size = 1000

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

### Density Plot of Samples [10, 100, 1000]

plot_ly(alpha=0.6) %>%
add_histogram(x=data_10, name='10 samples', type = "histogram",  histnorm = "probability") %>%
add_histogram(x=data_100, name='100 samples', type = "histogram",  histnorm = "probability") %>%
add_histogram(x=data_1000, name='1000 samples', type = "histogram",  histnorm = "probability") %>%
layout(title="Density Histogram of Samples", barmode='overlay',
      xaxis=list(title="Individuals Affected"), yaxis=list(title="Density"))

# Individuals Affected - Random Sampling Techniques


## Random Sampling Without Replacement
s <- srswor(100, nrow(breach_df))

# get row numbers from the sample bit field
rows <- (1:nrow(breach_df))[s!=0]
rows <- rep(rows, s[s != 0])
# store the sampled row data
sample.1 <- breach_df[rows, ]


## Systematic Sampling
# total rows
N <- nrow(breach_df)

# num samples
n <- 100

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
pik <- inclusionprobabilities(na.omit(breach_df$Individuals.Affected.Normalized.Outliers), 100)

# sample data based on inclusion probabilities and configured sample size
s <- UPsystematic(pik)

# store data
sample.3 <- breach_df[s != 0, ]

plot_ly(alpha=0.4) %>%
add_histogram(x=sample.1$Individuals.Affected.Normalized.Outliers, name='Random Sampling Without Replacement', type = "histogram",  histnorm = "probability") %>%
add_histogram(x=sample.2$Individuals.Affected.Normalized.Outliers, name='Systematic Sampling', type = "histogram",  histnorm = "probability") %>%
add_histogram(x=sample.3$Individuals.Affected.Normalized.Outliers, name='Inclusion Probabilities', type = "histogram",  histnorm = "probability") %>%
layout(title="Individuals Affected using Random Sampling Techniques", barmode='overlay', 
       xaxis=list(title="Individuals Affected"),
       yaxis=list(title="Density"))

# Top 5 Breach Targets

breaches_entity <- as.data.frame(count(breach_df, Name.of.Covered.Entity))

top_5 <- arrange(breaches_entity, desc(n))[1:5,]
top_5 

# Breaches per Year

breaches_per_year <- as.data.frame(count(breach_df, Year))
plot_ly(breaches_per_year, x=~Year, y=~n, type='bar', color=~Year, colors="Set1") %>%
layout(title="Breaches per Year",
       yaxis=list(title="Number of Breaches"))

# Breaches Per Month

breaches_per_month <- as.data.frame(count(breach_df, Month))
plot_ly(breaches_per_month, x=~Month, y=~n, type='bar', color=~Month, colors="Set1") %>%
layout(title="Breaches per Month",
       yaxis=list(title="Number of Breaches"))

# Type of Breach Breakdown in Percent

breach_df %>%
group_by(Type.of.Breach) %>%
summarize(count=n()) %>%
plot_ly(labels = ~Type.of.Breach, values = ~count) %>%
add_pie(hole = 0.6) %>%
layout(title="Type of Breach", showlegend = T,
       xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
       yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

# Covered Entity Type Breakdown in Percent

breach_df %>%
group_by(Covered.Entity.Type) %>%
summarize(count=n()) %>%
plot_ly(labels = ~Covered.Entity.Type, values = ~count) %>%
add_pie(hole = 0.6) %>%
layout(title="Covered Entity Type Involved in Breach", showlegend = T,
       xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
       yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

# Location of Records Involved in Breach

breach_df %>%
group_by(Location.of.Breached.Information) %>%
summarize(count=n()) %>%
plot_ly(labels = ~Location.of.Breached.Information, values = ~count) %>%
add_pie(hole = 0.6) %>%
layout(title="Location of Records Involved in Breach", showlegend = T,
       xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
       yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

# Breaches per State (Chloropeth map)

count_per_state <- count(breach_df, State)

head(count_per_state)

sum(count_per_state$n)

max(count_per_state$n)

max_idx <- row(count_per_state)[count_per_state==max(count_per_state$n)]

# max number of breaches in US
count_per_state[which(count_per_state$n == max(count_per_state$n)),]

# source: https://plot.ly/r/choropleth-maps/

# df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
#                            "Fruits", total.fruits, "Veggies", total.veggies,
#                            "<br>", "Wheat", wheat, "Corn", corn))
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

# Web Description Word Cloud

web_descriptions <- paste(unlist(breach_df$Web.Description), collapse =" ")

# Load the data as a corpus
docs <- Corpus(VectorSource(web_descriptions))


# inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("breach", "health", "individuals", "information")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


