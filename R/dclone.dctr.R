dclone.dctr <-
function(x, n.clones = 1, attrib = TRUE, ...)
{
    if (n.clones == 1)
        return(x)
    rval <- t(dclone::dclone.default(t(x), n.clones, attrib, ...))
    attr(attr(rval, "n.clones"), "method") <- "tr"
    rval
}
