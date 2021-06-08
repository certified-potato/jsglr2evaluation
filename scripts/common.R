options(warn=1)

args = commandArgs(trailingOnly=TRUE)

if (Sys.getenv("JSGLR2EVALUATION_DATA_DIR") != "" && Sys.getenv("JSGLR2EVALUATION_FIGURES_DIR") != "") {
  dataDir    <- Sys.getenv("JSGLR2EVALUATION_DATA_DIR")
  figuresDir <- Sys.getenv("JSGLR2EVALUATION_FIGURES_DIR")
} else {
  dataDir    <- "~/jsglr2evaluation-data"
  figuresDir <- "~/jsglr2evaluation-data/figures"
}

savePlot <- function(plot, filename) {
  png(file=paste(filename, ".png", sep=""))
  plot()
  invisible(dev.off())
  
  pdf(file=paste(filename, ".pdf", sep=""))
  plot()
  invisible(dev.off())
}

# Color per parser variant, colorblind safe: http://colorbrewer2.org/#type=diverging&scheme=BrBG&n=9
colors      <- c("#bf812d",  "#c7eae5", "#80cdc1", "#35978f",         "#01665e", "#009FDA")
allVariants <- c("recovery","jsglr1",  "antlr",   "antlr-optimized", "tree-sitter", "recoveryInlined")
symbols     <- c(0,2,5,3) # Symbol per language