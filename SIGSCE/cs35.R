library(rockchalk)
library(ggplot2)
df_2017 <- read.csv("cs35_2017.csv")
df_2016 <- read.csv("cs35_2016.csv")
df <- rbind(df_2017, df_2016)

df$Major <- combineLevels(df$Major, c("French", "Spanish"),"Language")
df$Major <- combineLevels(df$Major, c("Linguistics & Cog. Sci.", "Linguistics", "Cognitive Science"), "Linguistics & Cog. Sci.")
df$Major <- combineLevels(df$Major, c("Biology", "Biochemistry", "Chemistry", "Environmental Analysis", "Molecular Biology"), "Natural Sciences")
df$Major <- combineLevels(df$Major, c("Public Policy-Politics", "Politics", "International Relations", "Government", "Political Studies"), "Politics & IR")
df$Major <- combineLevels(df$Major, c("Economics", "Economics - Accounting", "Mathematical Economics"), "Economics")
df$Major <- combineLevels(df$Major, c("Computer Science", "Computer Science & Math", "Mathematics"), "CS & Math")
df$Major <- combineLevels(df$Major, c("Psychology", "Sociology", "Science Tech & Society"), "Social Sciences")
df$Major <- combineLevels(df$Major, c("Environ., Econ., Politic", "Phil., Politics, Econ.", "Politics, Phil, Economic", "Philosophy"), "Politics, Philosophy, Econ")
df$Major <- combineLevels(df$Major, c("Undeclared", "Undecided"), "Undeclared")

#Drop Non-Degree Seeking Major and Undeclared
df <- subset(df, df$Major != "Non-Degree Seeking")
df$Major <- factor(df$Major)
cs35_major_freq <- table(df$Major)
ordered_cs35_major_freq <- sort(cs35_major_freq, decreasing = TRUE)
op <- par(mar = c(12,4,2,2) + 0.1)
barplot(ordered_cs35_major_freq, las=2)
par(op)

# Import project categories
project_categories_df <- read.csv("CS35_project_categories.csv")
categories <- project_categories_df$Motivation.Category
category_table <- table(categories)
