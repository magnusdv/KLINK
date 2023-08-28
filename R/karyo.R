#' @importFrom graphics legend par plot.new points rect text.default
karyogram = function(markerdata, cols = KARYOPALETTE, bgcol = "gray92") {

  h = 0.7
  plot.new()
  par(mar = c(1,2,1,1), usr = c(0, max(CHROM.MB), 22+h, 1), xpd = TRUE)
  rect(xleft = 0, ybottom = 1:22, xright = CHROM.MB, ytop = 1:22 + h, col = bgcol)
  text.default(0, 1:22 + h/2, labels = 1:22, pos = 2)

  # Positions
  chr = markerdata$Chrom
  x = markerdata$PosCM * CHROM.MB[chr]/CHROM.CM[chr]
  y = chr + h/2
  points(x, y, bg = cols[markerdata$Pair], pch = 21, cex = 2.2)

  legend(max(CHROM.MB), 13+h/2, xjust = 1, y.intersp = 0, legend = 1:9,
         pch = 21, pt.cex = 2.2, pt.bg = cols[1:9], bty = "n")
}

CHROM.MB = c(247.2, 242.9, 199.5, 191.3, 180.9, 170.9, 158.8, 146.3, 140.3,
             135.4, 134.4, 132.3, 114.1, 106.4, 100.3,  88.8,  78.8, 76.1,
             63.8,  62.4,  46.9,  49.7)

CHROM.CM = c(267.8, 251.7, 218.3, 202.9, 197.1, 186.0, 178.4, 161.5, 157.3,
             169.3, 154.5, 165.5, 127.2, 116.0, 117.3, 126.6, 129.5, 116.5,
             106.4, 107.8,  62.9,  70.8)

KARYOPALETTE = c("#FD3216FF", "#00FE35FF", "#6A76FCFF", "#FED4C4FF", "#FE00CEFF", "#0DF9FFFF",
                 "#F6F926FF", "#FF9616FF", "#479B55FF", "#EEA6FBFF", "#DC587DFF", "#D626FFFF",
                 "#6E899CFF", "#00B5F7FF", "#B68E00FF", "#C9FBE5FF", "#FF0092FF", "#22FFA7FF",
                 "#E3EE9EFF", "#86CE00FF", "#BC7196FF", "#7E7DCDFF", "#FC6955FF", "#E48F72FF")
