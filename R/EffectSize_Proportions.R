#' Determining required effect size with two proportions of equal n
#' 
#' This function runs power calculations with two proportions of equal n, determining what effect size is necessary to see a statistically significant effect. Results are printed and can also be assigned.
#' @param n The per-group sample size. Can be a single integer \code{n} or a combination of integers \code{c(n1, n2, etc)}.
#' @param baserate The base rate for the population and probability in question. Required for all analyses.
#' @param alternative The type of hypothesis. Can be \code{"greater"} (p1 > p2), \code{"less"} (p1 < p2), or \code{"two.sided"} (p1 != p2).
#' @param treatmentrate The proportion of those in the treatment group who are expected to complete treatment; see details. 
#' @param sig.level The significance level of the hypothetical test. Defaults to \code{0.05}.
#' @param power The power of the hypothetical test. Defaults to \code{0.8}.
#' @details 
#' The effect size input is a "practical" effect size in that it is the
#' difference in observed probabilities, unlike Cohen's h. The calculations
#' adjust for treatment rate, so consider the effect size the mean effect among
#' those who successfully receive treatment.
#' 
#' The treatment rate is used to (crudely) calculate the effect size. Assuming
#' that those who do not receive treatment will be identical to the control
#' group, the required effect size will need to be inversely proportional to the
#' treatment rate. That is, if an effect of .1 is needed to see a significant
#' effect, and only 90% of the treatment group receives treatment, the required
#' effect size among those 90% treated becomes .1/.9 = .11.
#' 
#' "h" refers to Cohen's h, the formal score for effect size in tests of two
#' means. In the output, Cohen's h is calculated after taking treatment rate
#' into effect - it is the h for the full treatment group, including those who
#' do not receive treatment. If the treatment rate is lower than 1, h will be
#' lower.
#' 
#' Printed value is rounded; for an unrounded value, assign output to an object.
#' 
#' @examples
#' Please refer to vignette for detailed examples.
#' 
#' #Determining effect size given n, etc
#' EffectSize_Proportions(n = 100, baserate = .5, alternative = "greater", treatmentrate = .8)
#' 
#' #Determining required effect size across multiple n's
#' EffectSize_Proportions(n = c(100, 200, 300), baserate = .5, alternative = "greater", treatmentrate = .8)
#'  
#' #Assigning (unrounded) required effect size
#' e <- EffectSize_Proportions(n = 100, baserate = .5, alternative = "greater", treatmentrate = .8)
#' e
#' @export

EffectSize_Proportions <- function(n = NULL, baserate = NULL, alternative = c("two.sided","less","greater"), treatmentrate = 1, sig.level = .05, power = .8) {
  
  #Do not run the function if requirements are not met
  if(is.null(n) | is.null(baserate)) {
    stop("To calculate required effect size, must supply n and baserate.")
  }
  if(treatmentrate > 1) {
    stop("Treatment rate cannot be greater than 1")
  }
  if((length(alternative) != 1) | (alternative != "two.sided" & alternative != "less" & alternative != "greater")) {
    stop("Must select one alternative for hypothesis test. Can be 'two.sided', 'less', or 'greater'")
  }
  
  ReturnValue = "" #Initialize ReturnValue, which will be used to store the output when it is assigned to an object
  
    #Calculating required effect sizes for one or more sample sizes
    for(i in 1:length(n)) {
      
      result <- pwr::pwr.2p.test(h = NULL, n = n[i], sig.level = sig.level, power = power, alternative = alternative) #Run the pwr function to determine the effect size h needed for significance
      h <- as.numeric(result["h"]) #Select from this output only the effect size value h
      realdiff <- (sin(.5*(2*(asin(sqrt(baserate)))+h))^2 - baserate) * (1 / treatmentrate) #Translate d into practical terms by solving h = 2arcsin(sqrt(baserate)) - 2arcsin(sqrt(baserate + realdiff))
      
      #Print output
      if(realdiff > 0) {
        print(paste0("Required effect size with per-group n = ", n[i], ", treatment rate = ", treatmentrate, ": ", sprintf("%.2f", round(realdiff,2)), " (", round(100 * realdiff / baserate, 0), "% increase)   [h = ", round(h, 3), "]"))
      } else if(realdiff < 0) {
        print(paste0("Required Effect Size with per-group n = ", n[i], ", treatment rate = ", treatmentrate, ": ", sprintf("%.2f", round(realdiff,2)), " (", round(100 * -1 * realdiff / baserate, 0), "% decrease)   [h = ", round(h, 3), "]"))
      }
      
      ReturnValue[i] <- as.numeric(realdiff, 2) #Assign printed value to ReturnValue so it can also be assigned
      
    }
  
  
  invisible(as.numeric(ReturnValue)) #Return ReturnValue for assignment without printing it
  
}