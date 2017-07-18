library(rockchalk)
library(ggplot2)

#Set up column headers for data
df <- read.csv("cs60majors_combined.csv", header=FALSE)
data_names <- c("School", "Sex", "Major1", "Major2", "Course", "Year", "Semester", "Major_group")
colnames(df) <- data_names

#Take out CS, ENG, and UND majors
df <- subset(df, (Major_group != "EGR"))
df <- subset(df, (Major_group != "CSI"))
df <- subset(df, (Major_group != "UND"))
df <- subset(df, (Major_group != "CSM"))

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
plot(freq_non_mudd, las=2)

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

#Common majors by year
data_by_year <- c(data_2010, data_2011, data_2012, data_2013, data_2014, data_2015, data_2016, data_2017)
#major_data_by_year <- c(data_2010$Major_group, data_2011$Major_group, data_2012$Major_group, data_2013$Major_group, data_2014$Major_group, data_2015$Major_group, data_2016$Major_group, data_2017$Major_group)
#freq_data <- lapply(data_by_year, calculate_ordered_major_frequency)
#freq_2010 <- calculate_ordered_major_frequency(data_2010, 5)
#freq_2011 <- calculate_ordered_major_frequency(data_2011, 5)
#freq_2012 <- calculate_ordered_major_frequency(data_2012, 5)
#freq_2013 <- calculate_ordered_major_frequency(data_2013, 5)
#freq_2014 <- calculate_ordered_major_frequency(data_2014, 5)
#freq_2015 <- calculate_ordered_major_frequency(data_2015, 5)
#freq_2016 <- calculate_ordered_major_frequency(data_2016, 5)
#freq_2017 <- calculate_ordered_major_frequency(data_2017, 5)

top_5_2009 <- calculate_top_n_majors(data_2009, 5)
top_5_2010 <- calculate_top_n_majors(data_2010, 5)
top_5_2011 <- calculate_top_n_majors(data_2011, 5)
top_5_2012 <- calculate_top_n_majors(data_2012, 5)
top_5_2013 <- calculate_top_n_majors(data_2013, 5)
top_5_2014 <- calculate_top_n_majors(data_2014, 5)
top_5_2015 <- calculate_top_n_majors(data_2015, 5)
top_5_2016 <- calculate_top_n_majors(data_2016, 5)
top_5_2017 <- calculate_top_n_majors(data_2017, 5)

top_5_data_by_year <- c(top_5_2010, top_5_2011, top_5_2012, top_5_2013, top_5_2014,top_5_2015, top_5_2016, top_5_2017)

op <- par(mfrow=c(2,4))
plot(top_5_2010, las=2, ylab="", main="2010")
plot(top_5_2011, las=2,ylab="", main ="2011")
plot(top_5_2012, las=2,ylab="", main="2012")
plot(top_5_2013, las=2,ylab="", main="2013")
plot(top_5_2014, las=2,ylab="", main="2014")
plot(top_5_2015, las=2,ylab="", main="2015")
plot(top_5_2016, las=2,ylab="", main="2016")
plot(top_5_2017, las=2,ylab="", main="2017")
par(op)

par(mfrow=c(1,1))
#Need to set the colors to specific majors instead of just ordered.
colors <- c("darkslategrey","darkslategray4", "darkslategray3", "darkslategray2", "darkslategray1")
barplot(top_5_data_by_year, col=colors, border="white", space=0.04,  ylim = c(0,30), xlab=years, beside = FALSE)

theme_set(theme_classic())

# Histogram on a Categorical variable

# g <- ggplot(top_5_data_by_year, aes(years))
# g + geom_bar(aes(fill=), width = 0.5) + 
#   theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
#   labs(title="Histogram on Categorical Variable", ) 



# Population Pyramid for gender differences
