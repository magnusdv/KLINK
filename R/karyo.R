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
    linkedPairs = getLinkedPairs(linkageMap)

  # Prepare colours for linked pairs; this gives same colours as the gt tables
  nPairs = length(linkedPairs)
  if(nPairs > length(cols)) {
    pairLevels = factor(seq_len(nPairs), levels = seq_len(nPairs))
    cols = scales::col_factor(cols, domain = pairLevels)(pairLevels)
  }

  pp = lp2vec(m, linkedPairs)
  islinked = !is.na(pp)
  pair1 = islinked & !duplicated.default(pp)
  pair2 = islinked & !duplicated.default(pp, fromLast = TRUE)

  # Positions
  chr = linkageMap$Chr
  y = chr + h/2
  x = if("Mb" %in% names(linkageMap))
    pmin(linkageMap$Mb, CHROM.MB[chr])
  else
    pmin(linkageMap$cM, CHROM.CM[chr]) / CHROM.CM[chr] * CHROM.MB[chr]

  # Colours and symbols
  ptcol = rep(1, length(pp))
  fills = rep(unlinkedCol, length(pp))
  fills[islinked] = cols[pp[islinked]]
  pch = rep_len(pch, length.out = length(pp))
  bw = textBW(fills) # black or white text, depending on background

  # Gray out empty/uninformative markers
  if("Typed" %in% names(linkageMap)) {
    fills[linkageMap$Typed == 0] = "gray90"
    ptcol[linkageMap$Typed < 2] = "gray40"
    bw[linkageMap$Typed < 2] = "gray40"
  }

  # Connectors (before points, to hide endpoints)
  segments(x0 = x[pair1], x1 = x[pair2], y0 = y[pair1], y1 = y[pair2], col = 1, lwd = 1.3)

  # Draw markers left-to-right, keeping right-most markers on top
  for(i in order(chr, x)) {
    points(x[i], y[i], bg = fills[i], col = ptcol[i], pch = pch[i], cex = cexPoints)
    text.default(x[i], y[i], labels = labs[i], col = bw[i], cex = cexLabs, font = 2)
  }
}



# Hard-coded parameters ----------------------------------------------------

# GRCh38/hg38 chromosome lengths
# (https://hgdownload.soe.ucsc.edu/goldenpath/hg38/bigZips/hg38.chrom.sizes)
CHROM.MB = c(249.0, 242.2, 198.3, 190.2, 181.5, 170.8, 159.3, 145.1, 138.4,
             133.8, 135.1, 133.3, 114.4, 107.0, 102.0,  90.3,  83.3, 80.4,
              58.6,  64.4,  46.7,  50.8)
# Old lengths
CHROM.MB37 = c(247.2, 242.9, 199.5, 191.3, 180.9, 170.9, 158.8, 146.3, 140.3,
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
