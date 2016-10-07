# this is for utilities that you'll use over and over

create_dir <- function(file, verbose) {
    if (!file.exists(dirname(file))) {
        if (verbose) {
            message("Create: ", dirname(file))
        }
        dir.create(dirname(file), recursive = TRUE)
    }
}

make_pdf_ggplot <- function (expr, filename, ..., verbose = TRUE){
  if (verbose){
    message("Generating PDF: ", filename) # this tells it to talk to us and let us know that it's generating a pdf
  }
  pdf(file = filename, ...) # create pdf file with argument filename that you'd provide. the ... means that anything else (height, width, etc) can be put there or the user can just use the default
  on.exit(dev.off()) # so do dev.off before you exit
  eval.parent(substitute(print(expr))) # black magic that takes the function you're using and expresss it inside the environment and makes it go in the pdf somehow
}

make_pdf_base <- function(expr, file, ..., verbose = TRUE) {
    create_dir(file = file, verbose = verbose)
    if (verbose) {
        message("Creating figure: ", file)
    }
    pdf(file = file, ...)
    on.exit(dev.off())
    eval.parent(substitute(expr))
}

make_csv <- function(obj, file, ...,  verbose = TRUE) {
    create_dir(file = file, verbose = verbose)
    if (verbose) {
        message("Creating csv file: ", file)
    }
    write.csv(obj, file = file, row.names = FALSE, ...)
}

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
  # from Hadley github for ggplot2
}
