#   Boxplot for selected GEO samples
library(Biobase)
library(GEOquery)
library(RColorBrewer)
# load series and platform data from GEO

gset <- getGEO("GSE24807", GSEMatrix =TRUE, getGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL2895", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# group names for all samples in a series
gsms <- "abcdefgABhijklCDE"
sml <- c()

for (i in 1:nchar(gsms)) { sml[i] <- substr(gsms,i,i) }

upperC = c()
lowC = c()
for(i in sml) {
    if(grepl('[A-Z]', i)) {
        upperC = c(upperC, c(i))
    } else {
        lowC = c(lowC, c(i))
    }
}

sml = c(lowC, upperC)
copySml = sml
sml <- paste("G", sml, sep="")  #set group names

# order samples by group
ex <- exprs(gset)[ ,order(sml)]
#sml <- sml[order(sml)]
fl <- as.factor(sml)
labels <- c("nash","control")

# set parameters and draw the plot
upperColor = colorRampPalette(brewer.pal(9,"YlOrRd"))(length(sml))
lowColor = colorRampPalette(brewer.pal(9,"BuGn"))(length(sml))
colors = c()
upper = 0
low = 0
for (i in copySml) {
  if (grepl('[A-Z]', i)) {
    colors = c(colors, upperColor[upper + 3])
    upper = upper + 1
  } else {
    colors = c(colors, lowColor[low + 3])
    low = low + 1
  }
}

palette(colors)
dev.new(width=4+dim(gset)[[2]]/5, height=6)
par(mar=c(2+round(max(nchar(sampleNames(gset)))/2),4,2,1))
title <- paste ("GSE24807", '/', annotation(gset), " selected samples", sep ='')
boxplot(ex, boxwex=0.6, notch=T, main=title, outline=FALSE, las=2, col=fl)
legend("topleft", labels, fill=c(lowColor[3], upperColor[3]), bty="n")
