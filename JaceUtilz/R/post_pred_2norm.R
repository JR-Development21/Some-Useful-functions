#' A function that returns a list of two vectors containing posterior predictive parameters for a normal distribution with unknown variance and mean given a list of several vectors
#' @param data_list list of numerical vectors
#' @param mu the mean of the prior distribution for the mean
#' @param nu the prior parameter that adjusts the conjugate variance of the mean
#' @param b the prior beta parameter that adjusts the conjugate variance of the likelihood distribution variance
#' @param alpha the prior alpha parameter that adjusts the conjugate variance of the likelihood distribution variance
#' @import
#' @return returns the posterior predictive parameters given the data and the priors
#' @export
#' @examples
#' post_pred_2norm(data_list, mu, nu, b, alpha)

post_pred_2norm <- function(data_list, mu, nu, b, alpha){
  prior_mu <- mu
  prior_nu <- nu
  prior_beta <- b
  prior_alpha <- alpha
  posterior_mu <- (prior_mu * prior_nu + num_score * sapply(scores_2014,mean)) / (prior_nu + num_score)
  posterior_nu <- (prior_nu + num_score)
  posterior_alpha <- prior_alpha + num_score / 2
  posterior_beta <- prior_beta + .5 * sapply(sapply(scores_2014, function(x){(x - mean(x)) ^ 2}), sum) +
    num_score * prior_nu / (prior_nu + num_score) * (sapply(scores_2014, mean) - prior_mu)^2 / 2
  posterior_ncp <- posterior_mu
  posterior_df <- posterior_beta * (posterior_nu + 1) / (posterior_alpha)
  return(list(posterior_ncp, posterior_df))
}
