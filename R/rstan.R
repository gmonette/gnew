#' Get samples and sampler parameters from a stanfit object
#' 
#' @param sf a stanfit object
#' 
#' @export
getAll <- function(sf) {
  # function to create a data frame
  # from samples from a stanfit object
  # together with sampler parameters
  sam <- rstan:::as.data.frame.stanfit(sf)
  pars <- as.data.frame(do.call(rbind,rstan::get_sampler_params(sf, inc_warmup = FALSE)))
  cbind(sam, pars)
}