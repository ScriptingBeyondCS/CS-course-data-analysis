means <- c(3.300, 3.303, 2.835, 2.829)
names <- c("green60", "gold60", "green70", "gold70")
standardDevs <- c(.995, .994, .890, .854)
plotTop <- max(means+standardDevs*1.1)
barCenters <- barplot(means, names.arg=names, col=c('green','orange'), las=1, ylim=c(0,plotTop),
                      main='Average Grades in CS60 and CS70 by CS5 Flavor',ylab = 'Grades on a 4-point Scale')
arrows(barCenters, means-standardDevs, barCenters, means+standardDevs, lwd=2, angle=90, code=3)