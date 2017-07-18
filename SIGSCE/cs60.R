library(rockchalk)
library(ggplot2)

#Set up column headers for data
df <- read.csv("cs60majors_combined.csv", header=FALSE)
data_names <- c("School", "Sex", "Major1", "Major2", "Course", "Year", "Semester", "Major_group")
colnames(df) <- data_names

#Take out CS, ENG, and UND majors
# df <- subset(df, (Major_group != "EGR"))
# df <- subset(df, (Major_group != "CSI"))
# df <- subset(df, (Major_group != "UND"))
# df <- subset(df, (Major_group != "CSM"))

years <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

# Separate data by year
data_2017 <- subset(df, Year == "2017")
data_2016 <- subset(df, Year == "2016")
data_2015 <- subset(df, Year == "2015")
data_2014 <- subset(df, Year == "2014")
data_2013 <- subset(df, Year == "2013")
data_2012 <- subset(df, Year == "2012")
data_2011 <- subset(df, Year == "2011")
data_2010 <- subset(df, Year == "2010")
data_2009 <- subset(df, Year == "2009")

tab <- table(df_no_cs$Sex, df_no_cs$Year)
proportion <- prop.table(tab, margin = 2)

op <- par(mar = c(5,6,4,8))
 colors <- c("darkslateblue","darkslategray1")
barplot(proportion, col = colors,  xlab="Year", ylab="Proportion",legend=rownames(tab), las=1, args.legend = list(x = 'right', bty='n', inset=c(-0.2,0), xpd = TRUE))
title(main="CS60 Enrollment by Gender")
par(op)

#Gender plot- add back in undeclared, cs, eng majors when running
# colors <- c("darkslateblue","darkslategray1")
# op <- par(mfrow=c(2,4))
# plot(data_2010$Sex,ylim=c(0, 140), col=colors, main="2010")
# plot(data_2011$Sex,ylim=c(0, 140), col=colors, main="2011")
# plot(data_2012$Sex,ylim=c(0, 140), col=colors, main="2012")
# plot(data_2013$Sex,ylim=c(0, 140), col=colors, main="2013")
# plot(data_2014$Sex,ylim=c(0, 140), col=colors, main="2014")
# plot(data_2015$Sex,ylim=c(0, 140), col=colors, main="2015")
# plot(data_2016$Sex,ylim=c(0, 140), col=colors, main="2016")
# plot(data_2017$Sex,ylim=c(0, 140), col=colors, main="2017")
# title("CS60 Enrollment by Gender", cex.main=2.0, side=3, outer = TRUE, line=-20 )
# par(op)

# Calculates the number of cs60 students with each major and sorts them by the least to most popular major
calculate_ordered_major_frequency <- function(data, min_num_majors = 1){
  major_frequency <- table(data$Major_group)
  sig_majors <- subset(major_frequency, major_frequency >= min_num_majors)
  sort(sig_majors, decreasing = TRUE)
}

#Calculates the top n majors by frequency
calculate_top_n_majors <- function(data, n){
  major_frequency <- table(data$Major_group)
  major_frequency <- sort(major_frequency, decreasing = TRUE)
  major_frequency[c(1:n)]
}

#Calculate the most common majors
#common_majors <- calculate_ordered_major_frequency(df)
par(mfrow=c(1,1))
#plot(ordered, las=2)

#Calculate the most common majors for non-mudd students
non_mudd_students <- subset(df, School != "HM  ")
freq_non_mudd <- calculate_ordered_major_frequency(non_mudd_students, 5)

#Calculate the most common majors for Pomona students
Pomona_students <- subset(df, School == "PO  ")
freq_Pomona <- calculate_ordered_major_frequency(Pomona_students, 2)

#Calculate the most common majors for Scripps students
Scripps_students <- subset(df, School == "SC  ")
freq_Scripps <- calculate_ordered_major_frequency(Scripps_students, 2)

#Calculate the most common majors for Pitzer students
Pitzer_students <- subset(df, School == "PZ  ")
freq_Pitzer <- calculate_ordered_major_frequency(Pitzer_students, 2)

#Calculate the most common majors for CMC students
CMC_students <- subset(df, School == "CM  ")
freq_CMC <- calculate_ordered_major_frequency(CMC_students, 5)

#Calculate the most common majors for males vs females
males <- subset(df, Sex == "M")
freq_M <- calculate_ordered_major_frequency(males, 5)

females <- subset(df, Sex == "F")
freq_F <- calculate_ordered_major_frequency(females, 5)



