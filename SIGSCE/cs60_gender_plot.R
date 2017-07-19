flibrary(ggplot2)
library(rockchalk)

#Set up column headers for data
df <- read.csv("cs60majors_combined.csv", header=FALSE)
data_names <- c("School", "Sex", "Major1", "Major2", "Course", "Year", "Semester", "Major_group")
colnames(df) <- data_names

#Combine majors
df_combined <- df
df_combined$Major_group <- combineLevels(df$Major_group, c("CSI", "CSM"), "Computer Science")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("ENG", "MGE", "32E", "EGR"), "Engineering")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("MPH", "PHY"), "Physics")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("MAT"), "Math")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("MCB"), "Math/Comp. Bio.")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("NEU","BIO","MOB","MBI", "ORG", "HBI","BCH","BIC","BPH","CHB", "CHE", "GEO", "EVA", "SMG"), "Natural Sciences")
df_combined$Major_group <-combineLevels(df_combined$Major_group,c("E&E", "ECO", "EEP", "MEC", "ECA"),"Econ")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("PTS", "PIR", "POL", "GOV", "PPE", "PHI", "INT", "IPE","MEN","ARS"), "Politics, IR, Philosophy")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("PSY", "SOC", "ANT", "HIS", "RST", "STS", "CHS", "LCS", "LIN"), "Social Sciences")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("MUS", "ART", "FRE", "MDS"), "Art, Language, Media")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("IPS"), "Independent Program")
df_combined$Major_group <- combineLevels(df_combined$Major_group, c("482", "518", "559", "MAB",
                                                                    "NON", "SDM", "SPA"), "Don't Include")

#Take out CS, ENG, and UND majors
df_no_cs <- subset(df_combined, (Major_group != "UND"))
df_no_cs <- subset(df_no_cs, (Major_group != "Don't Include"))
df_no_cs <- droplevels(df_no_cs)

# Population Pyramid for gender differences
# Set up the dataframe in the format required by ggplot2
major_by_gender <- table(df_no_cs$Sex, df_no_cs$Major_group)
#major_by_gender <- prop.table(major_by_gender, margin = 2)
major_by_gender_df <- data.frame(major_by_gender)
colnames(major_by_gender_df) <- c("Sex", "Major", "Count")

# Make male counts negative so the bars will be centered
for(index in 1:nrow(major_by_gender_df)){
  row <- major_by_gender_df[index,]
  if(row["Sex"] == "M"){
    major_by_gender_df[index,]["Count"] <- row["Count"] * -1
  }
}

# X Axis Breaks and Labels 
brks <- seq(-200, 200, 50)
lbls = c(200, 150, 100, 50, 0, 50, 100, 150, 200)

# Plot
plot <- ggplot(major_by_gender_df, aes(x = Major, y = Count, fill = Sex)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                    labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="CS60 Majors by Gender from 2009 to 2017") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values = c("darkslateblue","darkslategray3"))
  #scale_fill_brewer(palette = "Set2")  # Color palette

print(plot)