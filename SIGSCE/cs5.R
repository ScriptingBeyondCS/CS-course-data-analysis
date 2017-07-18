cs5_df <- read.csv("fulldata_42.csv")

#Take out CS5 and the labs that go along with CS courses
drops <- c("CSCI005..HM", "CSCI005L.HM", "CSCI005GLHM", "CSCI005GRHM", "CSCI105L.HM", "CSCI105.LPO", "CSCI105L.JT", "CSCI121L.HM", "CSCI070L.HM")
cs5_df <- cs5_df[,!(names(cs5_df) %in% drops)]

gold <- subset(cs5_df, cs5_df$X == "gold")
black <- subset(cs5_df, cs5_df$X == "black")
green <- subset(cs5_df, cs5_df$X == "green")

calculate_num_courses <- function(department, data){
  courses_in_department <- data[,grepl(department, names(data))]
  sum(!is.na(courses_in_department))
}

calculate_num_courses_by_student <- function(department, data){
  courses_in_department <- data[,grepl(department, names(data))]
  rowSums(!is.na(courses_in_department))
}
calculate_num_cs_majors <- function(courses_by_student, data){
  sum(courses_by_student > 9)
}
calculate_num_bio_majors <-function(courses_by_student, data){
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
gold_cs_by_student <- calculate_num_courses_by_student("CS", gold)
gold_bio_by_student <- calculate_num_courses_by_student("BIO", gold)
gold_cs_by_student <-subset(gold_cs_by_student, gold_cs_by_student > 0)
gold_bio_by_student <-subset(gold_bio_by_student, gold_bio_by_student > 1)

black_cs_by_student <- calculate_num_courses_by_student("CS", black)
black_bio_by_student <- calculate_num_courses_by_student("BIO", black)
black_cs_by_student <- subset(black_cs_by_student, black_cs_by_student >0 )
black_bio_by_student <-subset(black_bio_by_student, black_bio_by_student > 1)

green_cs_by_student <- calculate_num_courses_by_student("CS", green)
green_bio_by_student <- calculate_num_courses_by_student("BIO", green)
green_cs_by_student <- subset(green_cs_by_student, green_cs_by_student >0 )
green_bio_by_student <-subset(green_bio_by_student, green_bio_by_student > 1)

par(mfrow=c(1,1))

colors <- c("darkslategrey","darkslategray4", "darkslategray3")
#special <- ifelse(sort(unique(gold_cs_by_student)) %in% c(0, 1, 2), "blue", "white")
special <- c(colors, rep("white", 20))

hist(gold_cs_by_student, las=1, breaks=20, xlim = c(0, 18), ylim = c(0,50), xlab="Number of courses taken", ylab="Number of students", main="Number of Post-CS5 Courses Taken by CS5 Gold Students", col=special)
#hist(gold_bio_by_student, las=1, breaks=20, xlim = c(0, 18), ylim = c(0,500), xlab="Number of courses taken", ylab="Number of students", main="Number of Biology courses taken by CS5 Gold Students", col=special)
hist(black_cs_by_student, las=1, breaks=20, xlim = c(0, 20), ylim = c(0,50), xlab="Number of courses taken", ylab="Number of students", main="Number of Post-CS5 Courses Taken by CS5 Black Students", col=special)
#hist(black_bio_by_student, las=1, breaks=20, xlim = c(0, 20), ylim = c(0,200), xlab="Number of courses taken", ylab="Number of students", main="Number of Biology courses taken by CS5 Black Students", col=special)
hist(green_cs_by_student, las=1, breaks=20, xlim = c(0, 20), ylim = c(0,50), xlab="Number of courses taken", ylab="Number of students", main="Number of Post-CS5 Courses Taken by CS5 Green Students", col=special)
#hist(green_bio_by_student, las=1, breaks=20, xlim = c(0, 20), ylim = c(0,100), xlab="Number of courses taken", ylab="Number of students", main="Number of Biology courses taken by CS5 Green Students", col=special)
legend("topright", c("CS5","CS60", "CS70"), fill=colors)

#Calculate number of Bio majors
gold_bio_majors <- calculate_num_bio_majors(gold_bio_by_student, gold)
black_bio_majors <- calculate_num_bio_majors(black_bio_by_student, black)
green_bio_majors <- calculate_num_bio_majors(green_bio_by_student, green)

#Calculate number of CS majors
gold_cs_majors <- calculate_num_cs_majors(gold_cs_by_student, gold)
black_cs_majors <- calculate_num_cs_majors(black_cs_by_student, black)
green_cs_majors <- calculate_num_cs_majors(green_cs_by_student, green)





