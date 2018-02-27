stan.model.mcmc.list.dc <- function (object, ...) {
    attr(attr(object, "stan.model"), "n.clones") <- nclones(object)
    attr(object, "stan.model")
}
