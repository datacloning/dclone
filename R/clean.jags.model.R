clean.jags.model <-
function(filename = "model.txt", dir = NULL)
{
    filepath <- if (is.null(dir))
        as.character(filename) else file.path(dir, filename)
    invisible(file.remove(filepath))
}

