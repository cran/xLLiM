\name{gllim_inverse_map}
\alias{gllim_inverse_map}
\title{Inverse Mapping from gllim parameters
}
\usage{
gllim_inverse_map(y,theta,verb)
}
\arguments{
\item{y}{A \code{D x N} matrix of input observations with variables in rows and subjects on columns}
\item{theta}{An object returned by the \code{gllim} function corresponding to the learned GLLiM model}
\item{verb}{Verbosity: print out the progression of the algorithm. If \code{verb=0}, there is no print, if \code{verb=1}, the progression is printed out. Default is 0.}
}
\value{
Returns a list with the following elements:
\item{x_exp}{A \code{L x N} matrix of predicted responses by posterior mean. If \eqn{L_w} latent factors are added to the model, the first \eqn{Lt} rows (\eqn{1:Lt}) are predictions of responses and rows \eqn{(L_t+1):L} (recall that \eqn{L=L_t+L_w}) are estimations of latent factors.}
\item{alpha}{Weights of the posterior Gaussian mixture model}
}
\description{This function computes the prediction of a new response from the estimation of the GLLiM model, returned by the function \code{gllim}. Given an observed \eqn{Y}, the prediction of the corresponding \eqn{X} is obtained by setting \eqn{X} to the mean of the distribution \eqn{p(X \vert Y)}.
}
\details{This function computes the prediction of a new response from the estimation of a GLLiM model, returned by the function \code{gllim}.
Indeed, if the inverse conditional density \eqn{p(Y \vert X)} and the marginal density \eqn{p(X)} are defined according to a GLLiM model (as described on \code{\link{xLLiM-package}} and \code{\link{gllim}}), the forward conditional density \eqn{p(X \vert Y)} can be deduced.

Under GLLiM model, it is recalled that the inverse conditional \eqn{p(Y \vert X)} is a mixture of Gaussian regressions with parameters \eqn{(\pi_k,c_k,\Gamma_k,A_k,b_k,\Sigma_k)_{k=1}^K}. Interestingly, the forward conditional \eqn{p(X \vert Y)} is also a mixture of Gaussian regressions with parameters \eqn{(\pi_k,c_k^*,\Gamma_k^*,A_k^*,b_k^*,\Sigma_k^*)_{k=1}^K}. These parameters have a closed-form expression depending only on \eqn{(\pi_k,c_k,\Gamma_k,A_k,b_k,\Sigma_k)_{k=1}^K}. 

Finally, the forward density (of interest) has the following expression:
\deqn{p(X \vert Y=y) = \sum_{k=1}^K \frac{\pi_k N(y; c_k^*,\Gamma_k^*)}{\sum_j \pi_j N(y; c_j^*,\Gamma_j^*)} N(x; A_k^*y + b_k^*,\Sigma_k^*)}
and a prediction of a new vector of responses is computed as:
\deqn{E (X \vert Y=y) = \sum_{k=1}^K \frac{\pi_k N(y; c_k^*,\Gamma_k^*)}{\sum_j \pi_j N(y; c_j^*,\Gamma_j^*)} (A_k^*y + b_k^*)}
where \eqn{y} is a  new vector of observed covariates.
}
\references{
[1] A. Deleforge, F. Forbes, and R. Horaud. High-dimensional regression with Gaussian mixtures and partially-latent response variables. Statistics and Computing, 25(5):893--911, 2015.

[2] E. Perthame, F. Forbes, and A. Deleforge. Inverse regression approach to robust non-linear high-to-low dimensional mapping. Submitted, 2016, avalaible on line at \url{https://hal.archives-ouvertes.fr/hal-01347455}.

Converted to R from the Matlab code of the GLLiM toolbox available on: \url{https://team.inria.fr/perception/gllim_toolbox/}
}

\examples{
data(data.xllim)

## Setting 5 components in the model
K = 5

## the model can be initialized by running an EM algorithm for Gaussian Mixtures (EMGM)
r = emgm(data.xllim, init=K); 
## and then the gllim model is estimated
responses = data.xllim[1:2,] # 2 responses in rows and 100 observations in columns
covariates = data.xllim[3:52,] # 50 covariates in rows and 100 observations in columns
mod = gllim(responses,covariates,in_K=K,in_r=r);

# Prediction on a test dataset
data(data.xllim.test)
pred = gllim_inverse_map(data.xllim.test,mod)
## Predicted responses
print(pred$x_exp)

}
\author{Emeline Perthame (emeline.perthame@inria.fr), Florence Forbes (florence.forbes@inria.fr), Antoine Deleforge (antoine.deleforge@inria.fr)
}
\seealso{\code{\link{xLLiM-package}},\code{\link{gllim}} 
}

