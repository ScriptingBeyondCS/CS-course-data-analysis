means <- c(3.300, 3.303, 3.588)
names <- c("green", "gold", "black")
standardDevs <- c(.995, .994, .797)
plotTop <- max(means+standardDevs*1.1)
barCenters <- barplot(means, names.arg=names, col=c('green','orange','dimgray'), las=1, ylim=c(0,plotTop),
                      main='Average Grades in CS60 by CS5 Flavor',ylab = 'Grades on a 4-point Scale')
arrows(barCenters, means-standardDevs, barCenters, means+standardDevs, lwd=2, angle=90, code=3)