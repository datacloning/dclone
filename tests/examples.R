#devtools::install_github("datacloning/dclone")
library(dclone)

## --- run examples with \dontrun sections ---

help_pages <- c(
    "clusterSize",
    "clusterSplitSB",
    "codaSamples",
    "coef.mcmc.list",
#    "dc.fit",
#    "dc.parfit",
    "dclone",
    "DcloneEnv",
    "dcoptions",
    "dctable",
    "errlines",
    "evalParallelArgument",
    "jags.fit",
    "jags.parfit",
    "jagsModel",
    "lambdamax.diag",
    "make.symmetric",
    "mclapplySB",
    "mcmcapply",
    "nclones",
    "ovenbird",
    "pairs.mcmc.list",
    "parallel.inits",
    "parCodaSamples",
    "parDosa",
    "parJagsModel",
    "parLoadModule",
    "parSetFactory",
    "parUpdate",
    "regmod",
    "update.mcmc.list",
    "write.jags.model")

for (i in help_pages) {
    cat("\n\n---------- dclone example:", i, "----------\n\n")
    eval(parse(text=paste0("example(", i,
        ", package = 'dclone', run.dontrun = TRUE)")))
}
