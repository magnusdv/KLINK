#' @importFrom graphics par plot.new points rect segments text.default
karyogram = function(linkageMap, linkedPairs = NULL, bgcol = "gray92") {

  h = 0.7
  plot.new()

  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mar = c(1,1.5,1,1), usr = c(0, max(CHROM.MB), 22+h, 1), xpd = TRUE)

  rect(xleft = 0, ybottom = 1:22, xright = CHROM.MB, ytop = 1:22 + h, col = bgcol)
  text.default(0, 1:22 + h/2, labels = 1:22, pos = 2)

  # Linked pairs
  m = linkageMap$Marker
  if(is.null(linkedPairs))
    linkedPairs = getLinkedPairs(m, linkageMap)

  pp = lp2vec(m, linkedPairs)
  islinked = !is.na(pp)
  pair1 = islinked & !duplicated(pp)
  pair2 = islinked & duplicated(pp)

  # Positions
  chr = linkageMap$Chrom
  y = chr + h/2
  x = pmin(linkageMap$PosCM/CHROM.CM[chr], 1) * CHROM.MB[chr]  # cm -> mb

  # Segments first, to hide endpoints
  segments(x0 = x[pair1], x1 = x[pair2], y0 = y[pair1], y1 = y[pair2], col = 1, lwd = 1.3)

  points(x[islinked], y[islinked], bg = KARYOPALETTE[pp[islinked]], pch = 21, cex = 2.5)
  points(x[!islinked], y[!islinked], col = 1, pch = 21, cex = 2.5)

  # Labels inside points: White text on dark colours
  bw = rep(1, length(pp))
  bw[islinked] = DARKLIGHT[pp[islinked]]
  text.default(x, y, labels = seq_along(m), cex = 0.8, col = c("black", "white")[bw])
}



# Hard-coded parameters ----------------------------------------------------


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

DARKLIGHT = c(2,1,2,1,2,1,
              1,1,2,1,2,2,
              2,1,2,1,2,1,
              1,1,2,2,1,2)
