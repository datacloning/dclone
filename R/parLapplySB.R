parLapplySB <-
function(cl=NULL, x, size = 1, fun, ...)
{
    if (is.null(cl))
        stop("no cluster 'cl' supplied")
    if (!inherits(cl, "cluster"))
        stop("not a valid cluster")
    res <- do.call(c, clusterApply(cl, x = clusterSplitSB(cl, x, size),
        fun = lapply, fun, ...), quote = TRUE)
    res[order(unlist(clusterSplitSB(cl, 1:length(x), size)))]
}
