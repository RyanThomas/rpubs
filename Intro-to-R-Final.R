IndivData <- read.table("http://esapubs.org/archive/ecol/E092/115/UPSP_Demo_data.txt", 
                        sep = '\t', 
                        header = TRUE)

SpData <- read.table("http://esapubs.org/archive/ecol/E092/115/UPSP_Species_list2.txt",
                     sep = '\t', header = TRUE)

head(IndivData)

plot(IndivData$Ysite ~ IndivData$Xsite, 
     xlab = "easting (m)", 
     ylab = "northing (m)",
     pch = 16,
     cex = .5)

par(mfrow = c(1,1),
mar = c(4,4,2,1),
las = 1)
plot(IndivData$Xsite[IndivData$Status0 == "A" | IndivData$Status5 =="D"], 
     IndivData$Ysite[IndivData$Status0 == "A" | IndivData$Status5 =="D"], 
     xlab = "easting (m)", 
     ylab = "northing (m)",
     pch = 1,
     cex = IndivData$GBH5/250,
     col = "grey")
points(IndivData$Xsite[IndivData$Status0 == "A" | IndivData$Status5 =="A"], 
       IndivData$Ysite[IndivData$Status0 == "A" | IndivData$Status5 =="A"], 
     xlab = "easting (m)", 
     ylab = "northing (m)",
     pch = 16,
     cex = IndivData$GBH5/250 )

table(IndivData$SpCode[IndivData$Status0 =="A"], IndivData$Status5[IndivData$Status0 == "A"])

dev.off()