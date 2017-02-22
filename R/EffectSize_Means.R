#' Determining required effect size with two means of equal n
#' 
#' This function runs power calculations with two means of equal n, determining what effect size (in practical terms) is necessary to see a statistically significant effect. Results are printed and can also be assigned.
#' @param n The per-group sample size. Can be a single integer \code{n} or a combination of integers \code{c(n1, n2, etc)}.
#' @param populationmean The population mean for the statistic in question. Required for all analyses.
#' @param populationdev The population standard deviation for the statistic in question. Required for all analyses. 
#' @param alternative The type of hypothesis. Can be \code{"greater"} (treatment mean > control mean), \code{"less"} (treatment mean < control mean), or \code{"two.sided"} (treatment mean != control mean).
#' @param treatmentrate The proportion of those in the treatment group who are expected to complete treatment; see details. 
#' @param sig.level The significance level of the hypothetical test. Defaults to \code{0.05}.
#' @param power The power of the hypothetical test. Defaults to \code{0.8}.
#' @details 
#' The effect size input is a "practical" effect size in that it is the
#' difference in means in observed units, unlike Cohen's d. The calculations
#' adjust for treatment rate, so consider the effect size the mean effect among
#' those who successfully receive treatment.
#' 
#' The treatment rate is used to (crudely) calculate the effect size. Assuming
#' that those who do not receive treatment will be identical to the control
#' group, the required effect size will need to be inversely proportional to the
#' treatment rate. That is, if an effect of .1 is needed to see a significant
#' effect, and only .9 of the treatment group receives treatment, the required
#' effect size among those .9 treated becomes .1/.9 = .11.
#' 
#' "d" refers to Cohen's d, the formal score for effect size in tests of two
#' means. In the output, Cohen's d is calculated after taking treatment rate
#' into effect - it is the d for the full treatment group, including those who
#' do not receive treatment. If the treatment rate is lower than 1, d will be
#' lower.
#' 
#' Printed value is rounded; for an unrounded value, assign output to an object.
#' 
#' @examples
#' Please refer to vignette for detailed examples.
#' 
#' #Determining required effect size given n, etc
#' EffectSize_Means(n = 100, populationmean = 50, populationdev = 10, alternative = "greater", treatmentrate = .8)
#' 
#' #Determining required effect size across multiple n's
#' EffectSize_Means(n = c(100, 200, 300), populationmean = 50, populationdev = 10, alternative = "greater", treatmentrate = .8)
#' 
#' #Assigning (unrounded) required effect size
#' e <- EffectSize_Means(n = 100, populationmean = 50, populationdev = 10, alternative = "greater", treatmentrate = .8)
#' e
#' @export

EffectSize_Means <- function(n = NULL, populationmean = NULL, populationdev = NULL, alternative = c("two.sided","less","greater"), treatmentrate = 1, sig.level = .05, power = .8) {
  
  #Do not run the function if requirements are not met
  if(is.null(n) | is.null(populationmean) | is.null(populationdev)) {
    stop("To calculate required effect size, must supply per-group sample size, population mean, and population standard deviation")
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
      
      result <- pwr::pwr.t.test(d = NULL, n = n[i], sig.level = sig.level, power = power, alternative = alternative, type = "two.sample") #Run the pwr function to determine the effect size d needed for significance
      d = as.numeric(result["d"]) #Select from this output only the effect size value d
      realdiff = (d * populationdev) * (1 / treatmentrate) #Translate d into practical terms by solving d = abs(trtmean-controlmean)/sd
      
      #Print output
      if(alternative == "greater") {
        print(paste0("Required effect size with per-group sample size = ", n[i], ", treatment rate = ", treatmentrate, ": ", sprintf("%.2f", round(realdiff,2)), " (", round(100 * realdiff / populationmean, 0), "% increase)   [d = ", round(d, 3), "]"))
      } else if(alternative == "less") {
        print(paste0("Required effect size with per-group sample size = ", n[i], ", treatment rate = ", treatmentrate, ": ", sprintf("%.2f", round(realdiff,2)), " (", round(100 * -1 * realdiff / populationmean, 0), "% decrease)   [d = ", round(d, 3), "]"))
      } else if(alternative == "two.sided") {
        print(paste0("Required effect size with per-group sample size = ", n[i], ", treatment rate = ", treatmentrate, ": ", sprintf("%.2f", round(realdiff,2)), " (", round(100 * realdiff / populationmean, 0), "% change)   [d = ", round(d, 3), "]"))
      }
      
      ReturnValue[i] <- as.numeric(realdiff) #Assign printed value to ReturnValue so it can also be assigned
    
    }
  
  invisible(as.numeric(ReturnValue)) #Return ReturnValue for assignment without printing it
  
}