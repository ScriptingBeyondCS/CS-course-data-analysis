library(BSDA)

# Read in Data
cs5_df <- read.csv("fulldata_42.csv")

# Take out CS5 and the labs that go along with CS courses
drops <- c("CSCI005..HM", "CSCI005L.HM", "CSCI005GLHM", "CSCI005GRHM", "CSCI105L.HM", "CSCI105.LPO", "CSCI105L.JT", "CSCI121L.HM", "CSCI070L.HM")
cs5_df <- cs5_df[,!(names(cs5_df) %in% drops)]

# Separate data into gold, black, and green
gold <- subset(cs5_df, cs5_df$X == "gold")
black <- subset(cs5_df, cs5_df$X == "black")
green <- subset(cs5_df, cs5_df$X == "green")

# Calculates the total number of CS or Bio courses taken in a gold, green, or black cohort
# department is either "CS" or "BIO" as a string
# data is gold, green, or black
# Returns an integer number of courses
calculate_num_courses <- function(department, data){
  courses_in_department <- data[,grepl(department, names(data))]
  sum(!is.na(courses_in_department))
}

# Calculates the number of CS or Bio courses taken by each student in a gold, green, or black cohort
# department is either "CS" or "BIO" as a string
# data is gold, green, or black
# returns a vector of integer numbers of courses by student
calculate_num_courses_by_student <- function(department, data){
  courses_in_department <- data[,grepl(department, names(data))]
  rowSums(!is.na(courses_in_department))
}

# Calculates the number of CS majors in a gold, green, or black cohort
# data is gold, green, or black
calculate_num_cs_majors <- function(data){
  courses_by_student <- calculate_num_courses_by_student("CS", data)
  sum(courses_by_student > 9)
}

# Calculates the number of Bio majors in a gold, green, or black cohort
# data is gold, green, or black
calculate_num_bio_majors <-function(data){
  courses_by_student <- calculate_num_courses_by_student("BIO", data)
  taken_191 <- data[,grepl("BIOL191", names(data))]
  taken_192 <- data[,grepl("BIOL192", names(data))]
  sum(!is.na(taken_191) | !is.na(taken_192) & (courses_by_student > 9))
}

#Calculate total number of CS and BIO courses taken by students in each CS5 course
total_gold_cs_courses <- calculate_num_courses("CS", gold)
total_gold_bio_courses <- calculate_num_courses("BIO", gold)

total_black_cs_courses <- calculate_num_courses("CS", black)
total_black_bio_courses <- calculate_num_courses("BIO", black)

total_green_cs_courses <- calculate_num_courses("CS", green)
total_green_bio_courses <- calculate_num_courses("BIO", green)

#Calculate number of CS and BIO courses taken by each student in each CS5 course
#Take out students with 0 courses (only taken cs5)
gold_cs_by_student <- calculate_num_courses_by_student("CS", gold)+1
gold_bio_by_student <- calculate_num_courses_by_student("BIO", gold)
#gold_cs_by_student <-subset(gold_cs_by_student, gold_cs_by_student > 0)
#gold_bio_by_student <-subset(gold_bio_by_student, gold_bio_by_student > 1)

black_cs_by_student <- calculate_num_courses_by_student("CS", black)+1
black_bio_by_student <- calculate_num_courses_by_student("BIO", black)
#black_cs_by_student <- subset(black_cs_by_student, black_cs_by_student >0 )
#black_bio_by_student <-subset(black_bio_by_student, black_bio_by_student > 1)

green_cs_by_student <- calculate_num_courses_by_student("CS", green)+1
green_bio_by_student <- calculate_num_courses_by_student("BIO", green)
#green_cs_by_student <- subset(green_cs_by_student, green_cs_by_student >0 )
#green_bio_by_student <-subset(green_bio_by_student, green_bio_by_student > 1)

# Calculate avg courses taken
avg_gold_cs <- mean(gold_cs_by_student)
sd_gold_cs <- sqrt(mean(gold_cs_by_student ^ 2) - mean(gold_cs_by_student)^2)
avg_gold_bio <- mean(gold_bio_by_student)
sd_gold_bio <-sqrt(mean(gold_bio_by_student ^ 2) - mean(gold_bio_by_student)^2)

avg_black_cs <- mean(black_cs_by_student)
sd_black_cs <- sqrt(mean(black_cs_by_student ^ 2) - mean(black_cs_by_student)^2)
avg_black_bio <- mean(black_bio_by_student)
sd_black_bio <- sqrt(mean(black_bio_by_student ^ 2) - mean(black_bio_by_student)^2)

avg_green_cs <- mean(green_cs_by_student)
sd_green_cs <- sqrt(mean(green_cs_by_student ^ 2) - mean(green_cs_by_student)^2)
avg_green_bio <- mean(green_bio_by_student)
sd_green_bio <- sqrt(mean(green_bio_by_student ^ 2) - mean(green_bio_by_student)^2)

print(c("Black", "Gold", "Green"))
print(c(sd_black_cs, sd_gold_cs, sd_green_cs))
print(c(sd_black_bio, sd_gold_bio, sd_green_bio))
# Z Test
z.test(gold_cs_by_student, green_cs_by_student, alternative="two.sided", mu = 0, sigma.x = sd_gold_cs, sigma.y =sd_green_cs , conf.level = 0.95)
z.test(gold_cs_by_student, black_cs_by_student, alternative="two.sided", mu = 0, sigma.x = sd_gold_cs, sigma.y =sd_black_cs , conf.level = 0.95)
par(mfrow=c(1,1))

# First three columns are shades of blue (CS5, CS60, CS70), everything after is white
colors <- c("darkslategrey","darkslategray4", "darkslategray3")
special <- c(colors, rep("white", 20))

hist(gold_cs_by_student, las=1, breaks=20, xlim = c(0, 18), ylim = c(0,50), xlab="Number of courses taken", ylab="Number of students", main="Number of Post-CS5 Courses Taken by CS5 Gold Students", col=special)
#hist(gold_bio_by_student, las=1, breaks=20, xlim = c(0, 18), ylim = c(0,500), xlab="Number of courses taken", ylab="Number of students", main="Number of Biology courses taken by CS5 Gold Students", col=special)
hist(black_cs_by_student, las=1, breaks=20, xlim = c(0, 20), ylim = c(0,50), xlab="Number of courses taken", ylab="Number of students", main="Number of Post-CS5 Courses Taken by CS5 Black Students", col=special)
#hist(black_bio_by_student, las=1, breaks=20, xlim = c(0, 20), ylim = c(0,200), xlab="Number of courses taken", ylab="Number of students", main="Number of Biology courses taken by CS5 Black Students", col=special)
hist(green_cs_by_student, las=1, breaks=20, xlim = c(0, 20), ylim = c(0,50), xlab="Number of courses taken", ylab="Number of students", main="Number of Post-CS5 Courses Taken by CS5 Green Students", col=special)
#hist(green_bio_by_student, las=1, breaks=20, xlim = c(0, 20), ylim = c(0,100), xlab="Number of courses taken", ylab="Number of students", main="Number of Biology courses taken by CS5 Green Students", col=special)
legend("topright", c("CS5","CS60", "CS70"), fill=colors)

#Calculate number of Bio majors
gold_bio_majors <- calculate_num_bio_majors(gold)
black_bio_majors <- calculate_num_bio_majors(black)
green_bio_majors <- calculate_num_bio_majors(green)

#Calculate number of CS majors
gold_cs_majors <- calculate_num_cs_majors(gold)
black_cs_majors <- calculate_num_cs_majors(black)
green_cs_majors <- calculate_num_cs_majors(green)





