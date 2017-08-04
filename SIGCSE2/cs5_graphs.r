# Read in Data
cs5_df <- read.csv("~/Documents/sigcse/fulldata_42.csv")

# Take out CS5 and the labs that go along with CS courses
drops <- c("CSCI005..HM", "CSCI005L.HM", "CSCI005GLHM", "CSCI005GRHM", "CSCI105L.HM", "CSCI105.LPO", "CSCI105L.JT", "CSCI121L.HM", "CSCI070L.HM")
cs5_df <- cs5_df[,!(names(cs5_df) %in% drops)]

drops <- c("CSCI006..HM", "BIOL052..HM", "BIOL052R.HM")
cs5_df <- cs5_df[,!(names(cs5_df) %in% drops)]


#barplot(proportion, col = c("green", "black", "orange"))
###########################
#####bio courses####
bio <- c(1:68)

index <- bio + 2

times <- apply(cs5_df[index], 2, function(x) sum(!is.na(x)))
coursenames <- names(cs5_df)[index]
#students <- c(cs5_df$X.1[!is.na(cs5_df$coursenames)])

students <- vector("list", 68)
to_be_removed <- vector("list", 68)

for (i in bio) {
  students[[i]] <- cs5_df$X[!is.na(cs5_df[,i+2])]
  if (length(students[[i]])<15) {
    students[[i]] <- NULL
    to_be_removed[i] <- FALSE
  } else {
    to_be_removed[i] <- TRUE
  } 
}
to_be_removed <- to_be_removed == TRUE

times <- times[to_be_removed]
coursenames <- coursenames[to_be_removed]

times <- c(1329, times)
coursenames <- c(" CSCI005..HM",coursenames)

students <- unlist(students)
cs5_students <- c(rep(5, 297), rep(6, 839), rep(7, 193))
students <- c(cs5_students, students)

tab2 <- table(students, rep(coursenames, times=times))
prop2 <- prop.table(tab2, margin=2)

plot2 <- barplot(prop2, col=c("black", "orange", "green"), las=2, 
                 main="Proportion of Students Taking Bio Courses by CS5 Flavor", 
                 ylab="number of students", legend=c("black", "gold", "green"), xaxt='n')
text(plot2, labels = coursenames, srt=45, xpd=TRUE, par("usr")[3], adj=1, cex=1)
###################







########cs courses######
drops <- c("CSCI060..HM", "CSCI070..HM")
cs5_df <-cs5_df[,!(names(cs5_df) %in% drops)]

cs5_df_no_majors <- cs5_df[is.na(cs5_df$CSCI195..HM),]

cs <- c(69:151)

index2 <- cs + 2

times2 <- apply(cs5_df_no_majors[index2], 2, function(x) sum(!is.na(x)))
coursenames2 <- names(cs5_df_no_majors)[index2]

students <- vector("list", 83)
to_be_removed2 <- vector("list", 83)

for (i in cs) {
  students[[i]] <- cs5_df_no_majors$X[!is.na(cs5_df_no_majors[,i+2])]
  if (length(students[[i]])<10) {
    students[[i]] <- NULL
    to_be_removed2[i-68] <- FALSE
  } else {
    to_be_removed2[i-68] <- TRUE
  } 
}
to_be_removed2 <- to_be_removed2 == TRUE

times2 <- times2[to_be_removed2]
times2 <- times2[!is.na(times2)]
coursenames2 <- coursenames2[to_be_removed2]

students <- unlist(students)

tab3 <- table(students, rep(coursenames2, times=times2), useNA = 'no')
prop3 <- prop.table(tab3, margin=2)

plot3 <- barplot(tab3, col=c("black", "orange", "green"), las=2, main="Non-Majors in CS Courses by CS5 Flavor", 
                 ylab="number of students", xaxt = "n")
text(plot3, labels = coursenames2, srt=45, xpd=TRUE, par("usr")[3], adj=1, cex=1)


#####reference on proportions####
#tab4 <- table(c(rep("black", 297), rep("gold", 839), rep("green", 193)), rep("CS5", 1329))
#prop4 <- prop.table(tab4, margin = 2)
#barplot(prop4, col=c("black", "orange", "green"), las=2)
