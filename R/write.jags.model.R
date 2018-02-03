write.jags.model <-
function(model, filename = "model.txt",
digits = 5, dir = tempdir(),
overwrite = getOption("dcoptions")$overwrite)
{
    ext <- "txt"
    if (!inherits(filename, "connection") && !overwrite && file.exists(filename)) {
        sn <- unlist(strsplit(filename, "\\."))
        if (length(sn) > 2) {
            sn[(length(sn) - 1)] <- paste(sn[-length(sn)], collapse=".")
            sn <- sn[-c(1:(length(sn) - 2))]
        }
        ff <- tempfile("model","")
        filename2 <- paste(substr(ff, 2, nchar(ff)), ext, sep=".")
        if (file.exists(filename2)) {
            while (!file.exists(filename2)) {
                ff <- tempfile("model","")
                filename2 <- paste(substr(ff, 2, nchar(ff)), ext, sep=".")
            }
        }
    } else {
        filename2 <- filename
    }

    path <- if (is.null(dir))
        getwd() else as.character(dir)
    filename2 <- file.path(path, filename2)

    if (inherits(model, "custommodel")) {
        writeLines(model, filename2)
    } else {
        R2WinBUGS::write.model(model, filename2, digits = digits)
    }
    invisible(filename2)
}
