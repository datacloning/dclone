clean.jags.model <-
function(filename = "model.bug", dir = NULL)
{
    filepath <- if (is.null(path))
        as.character(filename) else file.path(path, filename)
    invisible(file.remove(filepath))
}

