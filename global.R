##########################################
##########################################
#GLOBAL FILE FOR SETTING UP THE SHINY APP#
##########################################
##########################################

###################################
#PACKAGES NEEDED FOR THE SHINY APP#
###################################
library(shiny)
library(JASPgraphs)
library(ggplot2)
library(shinyWidgets)

####################################
####################################
#FUNCTIONS NEEDED FOR THE SHINY APP#
####################################
####################################

##############
#OUTPUT TABLE#
##############

# works with and without dataset
.hypothesesTable <- function(hypothesis_table, dataset = NULL, theta0 = NULL, categorical = TRUE){

  # convert hypotheses to factor
  hypothesis_table$label <- factor(hypothesis_table$label,
                                   levels = hypothesis_table$label,
                                   labels = hypothesis_table$label)
  prior_model_probability <- hypothesis_table$prior_model_probability

  # categorical case
  if(categorical){

    theta0 <- hypothesis_table$rate

    t <- data.frame(label                   = hypothesis_table$label,
                    value                   = theta0,
                    prior_model_probability = prior_model_probability/sum(prior_model_probability) # renormalize prior model probabilities (needs to sum to 1)
    )

    # if user has provided the dataset
    if(!is.null(dataset)){

      k <- dataset$k
      n <- dataset$n

      # convert to logscale for numerical stability
      t$log_likelihood                <- dbinom(k, n, prob = theta0, log = TRUE)           # p(data | theta0); binomial distribution
      log_priorTimesLikelihood      <- log(t$prior_model_probability) + t$log_likelihood # unnormalized posterior model probability
      log_ml                        <- log(sum(exp(log_priorTimesLikelihood)))         # normalizing constant; needed to compute posterior
      # see Kruschke p. 108, section 5.3
      t$posterior_model_probability   <- exp(log_priorTimesLikelihood - log_ml)        # posterior model probabilities

    }

  } else {

    # continous case
    H0_prior_model_probability <- theta0$prior_model_probability
    theta0                     <- theta0$rate
    n_hypotheses               <- nrow(hypothesis_table)

    # rescale each alternative hypothesis with respect to the null hypothesis
    pH0 <- H0_prior_model_probability/(H0_prior_model_probability + prior_model_probability)
    pH1 <- prior_model_probability/(H0_prior_model_probability + prior_model_probability)

    # everything is interpreted with respect to H1
    t <- data.frame(label                       = hypothesis_table$label,
                    prior_model_probability     = pH1,              # p(H1)
                    prior_model_odds            = pH1/pH0          # p(H1)/p(H0)
    )

    # if user has provided the dataset
    if(!is.null(dataset)){

      k <- dataset$k
      n <- dataset$n

      # compute log_likelihood
      log_likelihood <- dbinom(k, n, prob = theta0, log = TRUE)

      # compute log marginal likelihood and logBF10
      log_ml <- NULL
      log_bf <- NULL
      for(i in 1:n_hypotheses){

        a         <- hypotheses_continuous$a[i]
        b         <- hypotheses_continuous$b[i]
        log_ml[i] <- lchoose(n, k) + lbeta(k + a, n - k + b) -  lbeta(a, b)
        log_bf[i] <- (lbeta(k + a, n - k + b) -  lbeta(a, b) - k*log(theta0) - (n - k)*log(1 - theta0))

      }
      posterior_model_odds <- (pH1/pH0) * exp(log_bf)

      t$log_likelihood              <- log_likelihood    # p(data | theta0)
      log_ml                        <- log_ml            # p(data | H1)
      t$bayes_factor                <- exp(log_bf)       # p(data | H0)/p(data | H1)
      t$posterior_model_probability <- posterior_model_odds/(1 + posterior_model_odds) # p(H1 | data)
      t$posterior_model_odds        <- posterior_model_odds                            # p(H0 | data)/p(H1 | data)

    }
  }

  # round all numeric values
  t[,-1] <- round(t[,-1], 3)

  colnames(t) <- c("Label", "Value", "Prior Model Probability", "Log-likelihood", "Posterior Model Probability")
  
  return(t)
}


################################################
# Plot Prior and Posterior Model Probabilities #
################################################

.priorPosteriorModelProbabilityPlot <- function(results_table, posterior_is_included = TRUE) {

  # Note: in continous case p(H0) is not plotted because it changes for with each alternative hypothesis
  prior_model_probability     <- results_table$"Prior Model Probability"
  posterior_model_probability <- results_table$"Posterior Model Probability"
  n_hypotheses                <- nrow(results_table)
  x_axis_label                <- results_table$"Label"
  y_axis_label                <- expression(P(theta))

  pd             <- ggplot2::position_dodge(width = 0.2)

  prob_summary   <- data.frame(label = x_axis_label, prob = 'Prior', value = prior_model_probability)
  line_colour    <-  '#909090'

  if(posterior_is_included){

    post_df      <- data.frame(label = x_axis_label, prob = 'Posterior', value = posterior_model_probability)
    prob_summary <- rbind(prob_summary, post_df)
    
    #Specifying how many hypotheses to include in a plot:
    line_colour  <- rep(c('#000000', '#909090'), n_hypotheses)
    y_axis_label <- 'Model Probabilities'

  }

  hypothesis_plot <- ggplot2::ggplot(prob_summary, ggplot2::aes(x = label,
                                                                y = value,
                                                                fill = prob)) +
    ggplot2::geom_linerange(aes(ymin = rep(0, nrow(prob_summary)),
                                ymax = prob_summary$value),
                            position = pd,
                            size = 1.5,
                            colour = line_colour) +
    ggplot2::geom_point(size = 4, shape = 21, position = pd) +
    ggplot2::ylab(y_axis_label) +
    ggplot2::xlab(NULL) +
    ggplot2::scale_fill_manual(name = "", values = c('#909090', '#000000')) +
    ggplot2::ylim(min = 0, max = 1) +
    ggplot2::ggtitle("Model Probability Plot") +
    ggplot2::theme(plot.title = element_text(face = "bold"))


  if(posterior_is_included){

    hypothesis_plot <- JASPgraphs::themeJasp(hypothesis_plot, legend.position = c(.1, .95), xyMargin = c(100, 0.5))

  } else {

    hypothesis_plot <- JASPgraphs::themeJasp(hypothesis_plot, legend.position = "none")

  }

  return(hypothesis_plot)

}

