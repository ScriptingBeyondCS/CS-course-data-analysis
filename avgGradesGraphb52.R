means <- c(2.856, 2.623, 2.896)
names <- c("green", "gold", "black")
standardDevs <- c(.750, .848, 0.777)
plotTop <- 4
barCenters <- barplot(means, names.arg=names, col=c('green','orange','dimgray'), las=1, ylim=c(0,plotTop),
                      main='Average Grades in Bio52 by CS5 Flavor',ylab = 'Grades on a 4-point Scale')
arrows(barCenters, means-standardDevs, barCenters, means+standardDevs, lwd=2, angle=90, code=3)