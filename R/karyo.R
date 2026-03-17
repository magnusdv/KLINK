#' @importFrom graphics par plot.new plot.window points rect segments text.default
karyogram = function(linkageMap, linkedPairs = NULL,
                     bgcol = "gray92", border = 1, cols = KARYOPALETTE, unlinkedCol = "gray100",
                     pch = 21, cexPoints = 2.75,
                     labs = seq_along(linkageMap$Marker), cexLabs = 0.8,
                     cexChr = 1,
                     mar = c(1, 1.5, 1, 1)) {

  h = 0.7
  op = par(mar = mar, xpd = TRUE)
  on.exit(par(op), add = TRUE)

  plot.new()
  plot.window(xlim = c(0, max(CHROM.MB)), ylim = c(22 + h, 1), xaxs = "i", yaxs = "i")

  rect(xleft = 0, ybottom = 1:22, xright = CHROM.MB, ytop = 1:22 + h, col = bgcol, border = border)
  text.default(0, 1:22 + h/2, labels = 1:22, pos = 2, cex = cexChr)

  # Linked pairs
  m = linkageMap$Marker
  if(length(m) == 0)
    return()

  if(is.null(linkedPairs))
    linkedPairs = getLinkedPairs(m, linkageMap)

  pp = lp2vec(m, linkedPairs)
  islinked = !is.na(pp)
  pair1 = islinked & !duplicated(pp)
  pair2 = islinked & duplicated(pp)

  # Positions
  chr = linkageMap$Chr
  y = chr + h/2
  x = pmin(linkageMap$cM/CHROM.CM[chr], 1) * CHROM.MB[chr]  # cm -> mb

  # Colors and symbols
  fills = rep(unlinkedCol, length(pp)) # cols[pp[islinked]]
  fills[islinked] = cols[pp[islinked]]
  pch = rep_len(pch, length.out = length(pp))

  # Text color (black or white, depending on background)
  bw = textBW(fills)

  # Connectors (before points, to hide endpoints)
  segments(x0 = x[pair1], x1 = x[pair2], y0 = y[pair1], y1 = y[pair2], col = 1, lwd = 1.3)

  # Linked points (unlinked points and first point of linked pairs)
  points(x[!pair2], y[!pair2], bg = fills[!pair2], pch = pch[!pair2], cex = cexPoints)

  # Labels inside points
  text.default(x[!pair2], y[!pair2], labels = labs[!pair2], col = bw[!pair2], cex = cexLabs, font = 2)

  # Second point of linked pairs (do these separately to avoid overlap)
  if(length(linkedPairs)) {
    points(x[pair2], y[pair2], bg = fills[pair2], pch = pch[pair2], cex = cexPoints)
    text.default(x[pair2], y[pair2], labels = labs[pair2], col = bw[pair2], cex = cexLabs, font = 2)
  }
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


# Utility for choose black or white text

#' @importFrom grDevices col2rgb
textBW = function(bg, threshold = 0.3) {
  rgb = col2rgb(bg) / 255

  f = function(x) ifelse(x <= 0.03928,
                         x / 12.92,
                         ((x + 0.055) / 1.055) ^ 2.4)

  L = 0.2126 * f(rgb[1, ]) +
    0.7152 * f(rgb[2, ]) +
    0.0722 * f(rgb[3, ])

  ifelse(L > threshold, "black", "white")
}
