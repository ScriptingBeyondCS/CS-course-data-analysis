library(ggplot2)
library(rockchalk)

#Set up column headers for data
df <- read.csv("cs60majors_combined.csv", header=FALSE)
data_names <- c("School", "Sex", "Major1", "Major2", "Course", "Year", "Semester", "Major_group")
colnames(df) <- data_names

df_combined <- df
#Combine majors
df_combined$Major_group <- combineLevels(df$Major_group, c("MPH", "PHY"), "Physics")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("MAT"), "Math")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("MCB"), "Math/Comp. Bio.")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("NEU","BIO","MOB","MBI", "ORG", "HBI","BCH","BIC","BPH","CHB", "CHE", "GEO", "EVA", "SMG"), "Natural Sciences")
df_combined$Major_group <-combineLevels(df_combined$Major_group,c("E&E", "ECO", "EEP", "MEC", "ECA"),"Econ")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("PTS", "PIR", "POL", "GOV", "PPE", "PHI", "INT", "IPE","MEN","ARS"), "Politics, IR, Philosophy")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("PSY", "SOC", "ANT", "HIS", "RST", "STS", "CHS", "LCS", "LIN"), "Social Sciences")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("MUS", "ART", "FRE", "MDS", "SPA"), "Art, Language, Media")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("IPS"), "Independent Program")

df_combined$Major_group <- combineLevels(df_combined$Major_group, c("ENG", "MGE", "32E", "EGR"), "Engineering")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("CSI", "CSM"), "Computer Science")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("482", "518", "559", "MAB",
                                                                     "NON", "SDM"), "Don't Include")

df_cs_or_not$Major_group <- combineLevels(df_combined$Major_group, c("Physics", "Math", "Math/Comp. Bio.", "Natural Sciences", "Econ", "Politics, IR, Philosophy", "Social Sciences", 
                                                         "Art, Language, Media", "Independent Program"), "Not CS")
df_cs_or_not$Major_group <- combineLevels(df_cs_or_not$Major_group, c("Engineering", "Computer Science"), "CS")
df_cs_or_not$Major_group <- combineLevels(df_cs_or_not$Major_group, c("UND", "Don't Include"), "UND")

#Take out CS, ENG, and UND majors
df_no_cs <- subset(df_combined, (Major_group != "Engineering"))
df_no_cs <- subset(df_no_cs, (Major_group != "Computer Science"))
df_no_cs <- subset(df_no_cs, (Major_group != "UND"))
df_no_cs <- subset(df_no_cs, (Major_group != "Don't Include"))
df_no_cs <- droplevels(df_no_cs)

#Calculates the top n majors by frequency
calculate_top_n_majors <- function(data, n){
  major_frequency <- table(data$Major_group)
  major_frequency <- sort(major_frequency, decreasing = TRUE)
  major_frequency[c(1:n)]
}

# Separate data by year
data_2017 <- subset(df_no_cs, Year == "2017")
data_2016 <- subset(df_no_cs, Year == "2016")
data_2015 <- subset(df_no_cs, Year == "2015")
data_2014 <- subset(df_no_cs, Year == "2014")
data_2013 <- subset(df_no_cs, Year == "2013")
data_2012 <- subset(df_no_cs, Year == "2012")
data_2011 <- subset(df_no_cs, Year == "2011")
data_2010 <- subset(df_no_cs, Year == "2010")
data_2009 <- subset(df_no_cs, Year == "2009")

top_5_2009 <- calculate_top_n_majors(data_2009, 5)
top_5_2010 <- calculate_top_n_majors(data_2010, 5)
top_5_2011 <- calculate_top_n_majors(data_2011, 5)
top_5_2012 <- calculate_top_n_majors(data_2012, 5)
top_5_2013 <- calculate_top_n_majors(data_2013, 5)
top_5_2014 <- calculate_top_n_majors(data_2014, 5)
top_5_2015 <- calculate_top_n_majors(data_2015, 5)
top_5_2016 <- calculate_top_n_majors(data_2016, 5)
top_5_2017 <- calculate_top_n_majors(data_2017, 5)

#Create table- count of majors by year
tab <- table(df_no_cs$Major_group, df_no_cs$Year)
proportion <- prop.table(tab, margin = 2)*100

#Make the plot
op <- par(mar = c(5,6,4,12))
angle1 <- rep(c(45,45,135), length.out=9)
density1 <- c(seq(20,40,length.out=4), seq(80, 100, length.out=5))
clrs <- c("red", "orange", "yellow", "green", "cyan3", "dodgerblue2", "dodgerblue4", "darkslateblue", "darkorchid")
barplot(proportion, col = clrs,  xlab="Year", ylab="Percent of non-CS majors",legend=rownames(tab), las=1, 
        args.legend = list(x = 'right', bty='n', inset=c(-.7,0), xpd = TRUE), angle=angle1, density=density1)
title(main="Percent of CS60 Students with Non-CS Majors by Year",adj=0.3)
par(op)



