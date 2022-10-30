# dclone package version history

## Version 2.3-1, July 11, 2022

* The strict uniqueness of n.clones in dc.fit can be relaxed 
  by the new check.nclones argument.
* The update argument of dc.fit can be of length 1 or more,
  updatefun must return a list when length(update) > 1.
* Bugfix: stan.fit and stan.parfit were not able to read in
  model file because model_chr was undefined.

## Version 2.3-0, Mar 21, 2019

* Support for Stan incorporated.
* New functions: stan.fit, stan.parfit, stan.model.
* dc.fit and dc.parfit accepts flavour="stan".

## Version 2.2-0, Feb 26, 2018

* Removed dependence on superseded package snow,
  now imports from parallel.
* Functions related to parallel load balancing (e.g. parLapplySB)
  have been rewritten.

## Version 2.1-3, Feb 2, 2018

* dc.fit: n.clones=1 is acceptable and will not throw error.
* dc.fit and dc.parfit gained return.all argument so that
  not only the mcmc.list corresponding to highest number of
  clones is returned.
* write.jags.model does not write into the user's working directory
  but uses tempdir() as per CRAN requirements. The function
  returns the full path for cleanup. Tested to work with
  sequential and parallel functions.

## Version 2.1-2, Jan 14, 2016

* snowWrapper deprecated.
* codaSamples and parCodaSamples gained na.rm argument
  following rjags::coda.samples (starting with rjags 4-4,
  thus the version dependency in DESCRIPTION).

## Version 2.1-1, Jan 11, 2016

* Extensive NAMESPACE changes to import from other packages.
* Import: rjags and snow, so that functions can be imported by NAMESPACE.

## Version 2.1-0, Nov 22, 2015

* Matrix as dependency: finding nearestPD in chisq.diag.

## Version 2.0-0, Sept 27, 2013

* Parallel functions (parJagsModel, parUpdate, parCodaSamples)
  triggered a Note by R CMD check for modifying the global
  environment. This is now resolved as described below.
* New function: parDosa (meaning: wolf spider). This
  replaces the snowWrapper. It comes with 2 hidden environments
  (.DcloneEnvModel, .DcloneEnvResults) defined as global variables.
  The .DcloneEnvModel is meant to store temporary objects
  for model fitting, and swiped clean after use.
  The .DcloneEnvResults is meant to store result objects on the
  workers.
* These environments come with functions for manipulating them:
  pullDcloneEnv, pushDcloneEnv, clearDcloneEnv, listDcloneEnv,
  existsDcloneEnv.
* snowWraper is now deprecated with a warning.
* R2OpenBUGS added to DESCRIPTION as suggested package.
* R2WinBUGS::* notation explicitly added to avoid clashes
  with R2OpenBUGS.
* The program="openbugs" in bugs.fit now calls R2OpenBUGS::bugs
  instead of R2WinBUGS::openbugs. The latter can be
  achieved by program="brugs" and gives a warning the
  "openbugs" via R2WinBUGS is the preferred interface.
  The reason is that as.mcmc.list.bug coercion produces
  slightly different ts attributes under BRugs.
  WinBUGS (via R2WinBUGS) and R2OpenBUGS implementations
  give consistent ts attributes and can be seamlessly coerced
  into mcmc.list object, which is the preferred format for dclone.
* bugs.parfit: new-old function from extras directory.
  This is for parallel computations. See help file for
  details on suggested best practices.
* dc.parfit: now allows flavour="bugs" for all partypes options.
* Removed reference to parallel:::.reg default cluster.
* SystemRequirements in DESCRIPTION changed to 'one or more of ...'.
* rjags related functions call requireNamespace("rjags")
  instead of require(rjags).
* Depends on R 2.15.1 because of the declaration of global
  variables .DcloneEnvModel and .DcloneEnvResults.

## Version 1.8-2, March 4, 2013

* custommodel: splits length 1 character model by '\n'.
  This is the type of model specification that is used
  in STAN related examples.
* New cloning option added: dctr, allows cloning column-wise.
  This change was motivated by some STAN related data specification
  difficulties.
* dctable: failed when mcmc.list had 1 parameter.
  This is now fixed by using varnames(x, allow.null=FALSE).

## Version 1.8-1, September 3, 2012

* clusterSplit now checks inheritance of its cl argument in
  R-devel (2.16.0 to be), as a result plotClusterSize
  threw an error when supplying a simple vector to clusterSplit
  (reported by B Ripley), this is now fixed.

## Version 1.8-0, July 6, 2012

* dciid: new function added, it is similar to dcdim.
  dciid attaches an attribute to data frames or matrices
  about columns to be treated as i.i.d. observations
  by dclone(). This aims to facilitates working with the INLA
  package to generate approximate marginals based on DC.
* dclone.dciid: proper cloning of objects with "dciid"
  tag. It respects possible groupings of observations.

## Version 1.7-2, March 21, 2012

* custommodel and write.jags.model fixed and modified.
  These now accept connection as filename argument.
* lambdamax.diag and chisq.diag are now generics.
  Methods for mcmc.list is defined.
* inst/COPYING file removed (standard license).

## Version 1.7-1, January 31, 2012

* Now that rjags is only suggested
  rjags is loaded by snowWrapper if JAGS is used
  (dc.parfit, jags.parfit, parJagsModel).
* n.chains argument was not passed by dc.parfit
  with partype %in% c("parchains", "balancing").
  Bug is now fixed, thanks to K. Nadeem (UofA).
* length(cl) >= n.chains is required in
  parJagsModel, parUpdate and parCodaSamples
  recognizes empty workers accordingly.
* lecuyer module is checked only for clusters
  and not for forking.
* Rd files cleaned up, documentation is up-to-date.

## Version 1.7-0, January 25, 2012

* Preparing for major CRAN release.

## Version 1.6-0, December 19, 2011

* Depends on parallel and R >= 2.14.0 for multicore type
  forking on Linux/Unix machines. Heavy lifting is
  still done by showWrapper for easy transitioning.
* parallel is now a dependency, snow is suggested only.
* list type declaration of params argument not working
  with partype="both" in dc.parfit. This option is
  defunct at the moment.
* Windows specific suggested packages removed from dependency
  list due to seamless Linux based testing of
  forking type parallel features (rsprng, BRugs).
* cl argument in parallelized functions that
  use snowWrapper can be NULL in which case it
  takes the value from the mc.cores global option
  getOption("mc.cores"). If mc.cores is not set (NULL)
  of 1, an error is produced.
* mclapplySB added to mimic size balancing with forking.
* parJagsModel, parUpdate and parCodaSamples works
  only with snow clusters.
* updated.model.mcmc.list.dc: keeps n.clones attribute.
* evalParallelArgument added for consistent handling
  of parallel arguments.
* jags.parfit and dc.parfit falls back to sequential
  evaluation if cl is NULL (and default options for
  parallel processing are unset) or 1.
* stack method for mcmc.list objects to facilitate
  creating graphics with lattice and ggplot2.
  Experimental ggplot2 based plotting functions are in
  dcextras package on R-Forge.
* The package was extensively tested on Windows XP and 7
  (32 and 64 bit) and Ubuntu Linux 11.10 using
  JAGS, OpenBUGS and WinBUGS (latter only on Windows).
  For testing suite see /devel on R-Forge.

## Version 1.5-1, October 18, 2011

* "[.mcmc.list.dc" added to extract parts of a fitted object
  and retain its n.clones attribute (no other attributes
  though, e.g. dcdiag, dctable, updated.model).
* params argument in dc.*fit can take a 2 element
  list. Union of the 2 vectors will be used as
  parameters to monitor (as in jags.fit),
  while the 2nd element of the list is used
  to calculate statistics in dcdiag().
  This way transformations of nodes or
  the deviance monitor won't be used in
  diagnostic tests.

## Version 1.5-0, October 17, 2011

* Major CRAN release with improved parallel features
  (double initialization overhead is removed);
  with bugfixes from devel versions.
* The package also has bee extensively tested using
  Classic BUGS examples (see ChangeLog for dcmle package).

## Version 1.4-2, October 16, 2011

* New function: parallel.inits make use of rjags:::parallelseeds
  to control RNG settings for initial values in parallel computations.
* parJagsModel: adapted to initial RNG setup by parallel.inits.
* jags.parfit: adapted to initial RNG setup by parallel.inits.
* dc.parfit: adapted to initial RNG setup by parallel.inits
  (partype=both, also partype=parchains via jags.parfit,
  partype=balancing depends on underlying JAGS settings
  as in jags.fit).

## Version 1.4-1, September 28, 2011

* plot.dcdiag: dcdiag colnames from .dcFit were non standard,
  so plot failed to select columns. Now fixed.
* dc.fit and dc.parfit: got n.chains argument for clean
  handling in parallel work. (dc.parfit failed with
  partype="both" when n.chains was other than 3.)
* plotClusterSize: can plot if length(size) < n
  without error/warnings.
* parLoadModule, parUnloadModule, parListModules,
  parSetFactory, parListFactories: workers returned values as
  expected, but did not actually made changes, bugs fixed,
  allowing full control of JAGS settings on workers.
* dcdiag and dctable: return NA when subroutines fail
  (gelman.diag, lambdamax.diag, chisq.diag).
  dc.fit and dc.parfit neglects these NA values
  when issuing warning based on Rhat.
* dcoptions()$overwrite changed to TRUE by default,
  this might spare some headaches when going parallel.
* lambdamax.diag returned sd instead of variance for
  one parameter case, bug fixed (reported by S Lele).

## Version 1.4-0, September 28, 2011

* Major CRAN release with new parallel features.

## Version 1.3-6, September 7, 2011

* jags.fit return jags model with n.clones
  attribute when n.iter=0.
* parJagsModel and parCodaSamples: passes the
  n.clones info from data argument.
* jagsModel and codaSamples added as wrapper
  around jags.model and coda.samples functions of
  rjags package to track dclone information passed
  through data argument and allow custommodel
  specification of model argument (similarly to par*
  versions of similar functions).
* .onLoad added to zzz.R

## Version 1.3-5, August 23, 2011

* n.update=1000 is now the default instead of 0
  to ensure independence after adaptation.
  Warning is issued if n.adapt>0 and n.update=0.
* New argument partype added to dc.parfit,
  possible options (balancing, parchains, both)
  are described on the help page.
* dc.parfit gained args (update, updatefun, initsfun)
  which are ignored if type is not "parchains".
* dc.fit and dc.parfit (with "parchains" type)
  now call new internal function .dcFit (former dc.fit reworked
  to allow parallel chains via jags.parfit).
  This function is not exported by namespace, thus
  not user visible and undocumented.
* dclone is loaded to workers by snowWrapper
  only if .packages() does not contain it yet.
* dclone method for environment added: it returns a cloned list.
* data argument can be environment,
  it is coerced to a list for handling n.clones attribute.
* jags.fit: quiet argument of jags.model used along with
  verbose dcoption to possibly suppress printout
  during compilation.
* Parallel functionality added replicating main rjags functions
  to allow easy updating of jags model objects.
  Functions added: parListModules, parLoadModule, parUnloadModule,
  parListFactories, parSetFactory, parJagsModel, parUpdate,
  parCodaSamples, parUpdate; with help pages taken and edited from
  rjags package of M Plummer.
* .DcloneEnv is created at startup, and removed on unloading
  dclone. This is intended to store temporary stuff passed
  to workers by snowWrapper.
* snowWrapper got new argument: unload=FALSE to unload
  packaged from pkg argument.
* snowWrapper: if name=NULL, .DcloneEnv is used to store
  temporary data.
* Data cloning compatible jagsModel and codaSamples
  function added for sequential computations.
  Both functions copied after rjags equivalents,
  jags.model and coda.samples.
* New method: pairs for mcmc.list objects. It plots
  bivariate kernel density estimates and scatterplots
  in a matrix with univariate densities as diagonal.
* dctable add object name even if length(n.clones)==1.
* initsfun handling in dc.parfit partly allowed,
  this new feature is now documented, testing, Example added.
* All documented S3 methods are now exported by NAMESPACE
  (dependent packages could not import them).

## Version 1.3-4, July 9, 2011

* dc.fit: updating fixed (only updated after 2nd iteration).
* dc.fit: updatefun and initsfun now accepts 2 arguments,
  1st is model, 2nd is n.clones to allow for cloning
  latent variable vectors on the go. Example reworked accordingly.
* snowWrapper: cleanup=TRUE cleans up after lib and dir args, too.
* New option 'overwrite' in dcoptions (used by write.jags.model).
* packageStartupMessage used in zzz.R .onAttach
* ... is now passed to snowWrapper in *.parfit (thanks to
  Emmanuel Charpentier for reporting the bug).

## Version 1.3-3, May 13, 2011

* *.parfit: write model to hard drive only if
  cluster type is SOCK (shared memory)
* snowWrapper: dir=NULL is set as the default.
* mcmcapply functions simplified, FUN can be missing.
* jags.parfit help notes that there is no update method
  for parallel mcmc.list objects.
* snowWrapper got cleanup=TRUE argument to remove
  data after evaluating main function.

## Version 1.3-2, March 2, 2011

* Partial argument match fixed (NOTE from R CMD CHECK).
* write.jags.model got new argument overwrite=FALSE.
* Typo in rng.type argument of snowWrapper fixed.
* plotClusterSize: col arg is reordered according to
  balancing type.

## Version 1.3-1, January 4, 2011

* JASA paper (Lele et al. 2010) citation added to Rd.
* loading rjags changed: it is now only suggested.
  Reason: some dclone functionality might be useful
  without JAGS/rjags present, and now loading
  rjags won't cause error if JAGS shared library
  not found.
* glm module not loading at startup because
  it cannot be nicely unloaded in parallel versions.
* dcoptions RNG got "none" value as default,
  *.parfit functions use dcoptions for snowWrapper.
  As a result, dependencies changed a bit.
* dc.parfit: none/both balancing fixed (were switched).

## Version 1.3-0, December 31, 2010

* preparing for major CRAN release.
* final RJ citation added.

## Version 1.2-3, December 16, 2010

* bugs.parfit: migrated into the new dcextras package
  where experimental and not fully supported functions
  will reside in the future.

## Version 1.2-2, October 21, 2010

* expanded examples for *.parfit functions.
* clusterSeed removed from package: use
  clusterSetupRNG in snow instead.

## Version 1.2-1, August 26, 2010

* experimenting with bugs.parfit:
  the different seeds approach is not satisfactory
  because it cannot guarantee independence of the chains
* seed arg added to bugs.fit
* RNG option added to work with snowWrapper
* set.rng arg added to snowWrapper that calls
  snow::clusterSetupRNG
* options for dclone reworked: can be accessed via
  the dcoptions function, and it sets the dcoptions option
* dclone.dcdim: dimnames bug fixed when drop=TRUE
* examples added on inits specifications for *.parfit

## Version 1.2-0, August 26, 2010

* polishing plot methods for R Journal paper.
* citation entry added.

## Version 1.1-1, May 6, 2010

* print.custommodel has deparse=FALSE argument.
* glm module for JAGS >2.0 is loaded at startup
* parallel RNG handling reworked.
* dctable fix for one parameter mcmc.list.
* dcdiag returns r.hat for one parameter models.

## Version 1.1-0, May 3, 2010

* dclone was tested with JAGS 2.0.
* major release submitted to CRAN.

## Version 1.0-7, April 7, 2010

* added flavour argument in dc.parfit.
* methods for mcmc objects added (coef, vcov, quantile).
* plot.dcdiag got log.lambda.max option.
* log.var type of plot.dctable changed.

## Version 1.0-6, March 14, 2010

* openbugs.seed removed.
* bugs.fit fixed, several arguments removed.
* dcpar package merged with dclone.
* WinBUGS/OpenBUGS related revision done:
  as.mcmc.list.bugs and dc.fit now works properly.
* dclone.ts removed, use dcdim instead.

## Version 1.0-5, January 25, 2010

* drop and perm arguments in dcdim.

## Version 1.0-4, January 14, 2010

* nclones.list: returns 'method' attributes.
* vcov: got invfisher argument.
* S3 methods added for diagnostic functions.
* dclone options restructured.
* dclone and dcpar split: parallel computing features
  in separate package (this decision changed v1.0-6).
* dcdiag: diagnostic tools for data cloning.
* chisq.diag: diagnostic tool added.

## Version 1.0-3, December 14, 2009

* lambdamax.diag: added back.
* errlines: got col argument.
* jags.fit: if n.iter=0, the (updated) JAGS model is returned
  instead of an MCMC object.
* update.mcmc.list: function to perform automatic updates of an
  MCMC object, until a desired statistics value
  (e.g. R_hat < 1.1) reached.

## Version 1.0-2, December 4, 2009

* updated.model attr in jags.fit output for further updating.
* bg arg in errlines and box.bg in plot.dctable for background
  color of boxes.

## Version 1.0-1, November 19, 2009

* custommodel function added, fitting functions modified.

## Version 1.0-0, November 16, 2009

* Unload problem fixed (reported by Brian D Ripley).
* dctable now can take more than one model as arguments.
* Code annotated, documentation revised.

## Version 0.9-0, October 19, 2009

* First CRAN release with basic functionality.
* Support for WinBUGS added.
* Initial functionality is stable.
