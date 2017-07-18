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
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("MUS", "ART", "FRE", "MDS"), "Art, Language, Media")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("IPS"), "Independent Program")

df_combined$Major_group <- combineLevels(df_combined$Major_group, c("ENG", "MGE", "32E", "EGR"), "Engineering")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("CSI", "CSM"), "Computer Science")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("482", "518", "559", "MAB",
                                                                     "NON", "SDM", "SPA"), "Don't Include")

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
#ordered_names <- c("PHY", "MAT","ECO","MCB","CHE","NEU","ORG","CHB","IPS","MGE","E&E","ANT")
tab <- table(df_no_cs$Major_group, df_no_cs$Year)
proportion <- prop.table(tab, margin = 2)
#tab <- tab[top_5_names,]


#Make the plot

op <- par(mar = c(5,6,4,12))
clrs <- c("red", "orange", "yellow", "green", "cyan", "dodgerblue2", "dodgerblue4", "darkslateblue", "darkorchid")
barplot(proportion, col = clrs, main="Proportion of CS60 Students with Non-CS Majors by Year",xlab="Year", ylab="Proportion of non-CS majors",legend=rownames(tab), las=1, args.legend = list(x = 'right', bty='n', inset=c(-.7,0), xpd = TRUE))
par(op)


