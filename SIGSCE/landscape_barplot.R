# Set up data
categories <- c("CS+X double majors", "CS+X courses", "Interdisciplinary majors", "Flavored CS1", "CS2 for non-CS majors")
data <- c(rep("CS+X double majors", 50), rep("CS+X courses", 50), rep("Interdisciplinary majors", 27), 
          rep("Flavored CS1", 21), rep("CS2 for non-CS majors", 8))

angle1 <- rep(c(45,45,135), length.out=7)
density1 <- seq(5,35,length.out=5)
clrs <- c("red", "orange", "yellow", "green", "cyan", "dodgerblue2", "dodgerblue4", "darkslateblue", "darkorchid")
barplot(table(data), main="Number of schools with interdisciplinary efforts by category", col = rainbow(5), angle = angle1,density = density1, legend=categories)
#legend("topright", legend=categories, ncol=5, fill=TRUE, col=rainbow(5), angle=angle1, density=density1)