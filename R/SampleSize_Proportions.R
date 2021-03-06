#' Determining required sample size with two proportions of equal n
#' 
#' This function runs power calculations with two proportions of equal n, determining what sample size is necessary to see a statistically significant effect. Results are printed and can also be assigned.
#' @param baserate The base rate for the population and probability in question. Required for all analyses.
#' @param effectsize The difference in probability between the treatment group and the base rate. Can be a single integer \code{x} or a combination of integers \code{c(x1, x2, etc)}. This is a "practical" effect size; see details.
#' @param alternative The type of hypothesis. Can be \code{"greater"} (treatment probability > control probability), \code{"less"} (treatment probability < control probability), or \code{"two.sided"} (treatment probability != control probability).
#' @param treatmentrate The proportion of those in the treatment group who are expected to complete treatment; see details. 
#' @param sig.level The significance level of the hypothetical test. Defaults to \code{0.05}.
#' @param power The power of the hypothetical test. Defaults to \code{0.8}.
#' @param h Refers to Cohen's h, the formal score for effect size in tests of probabilities. You may supply a Cohen's h score instead of a practical effect size if you prefer.
#' @details 
#' The effect size input is a "practical" effect size in that it is the
#' difference in observed probabilities, unlike Cohen's h. The calculations
#' adjust for treatment rate, so consider the effect size the mean effect among
#' those who successfully receive treatment.
#' 
#' The treatment rate is used to (crudely) calculate the sample size. Assuming
#' that those who do not receive treatment will be identical to the control
#' group, the required sample size will need to be inversely proportional to the
#' treatment rate. That is, a completed treatment is worth an effect size of 1,
#' but only 90% of the treatment group receives treatment, the mean effect size
#' for the treatment group will only be .9. This means that the n will need to
#' be higher than it would if the treatment rate was 1.
#' 
#' In the output, Cohen's h is calculated after taking treatment rate into
#' effect. If the treatment rate given to the function is lower than 1, the h
#' supplied will also be lower, even if all other numbers are constant.
#' 
#' Printed value is rounded; for an unrounded value, assign output to an object.
#' 
#' @examples
#' Please refer to vignette for detailed examples.
#' 
#' #Determining required per-group sample given effect size, etc
#' SampleSize_Proportions(baserate = .5, effectsize = -.2, alternative = "less", treatmentrate = .9)
#'  
#' #Determining required per-group sample size across multiple effect sizes
#' SampleSize_Proportions(baserate = .5, effectsize = c(-.1, -.2, -.3), alternative = "less", treatmentrate = .9)
#' 
#' #Assigning (unrounded) required sample size
#' n <- SampleSize_Proportions(baserate = .5, effectsize = c(-.1, -.2, -.3), alternative = "less", treatmentrate = .9)
#' n
#' @export

SampleSize_Proportions <- function(baserate = NULL, effectsize = NULL, alternative = c("two.sided","less","greater"), treatmentrate = 1, sig.level = .05, power = .8, h = NULL) {
  
  #Do not run the function if requirements are not met
  if((is.null(effectsize) & is.null(h)) | is.null(baserate)) {
    stop("To calculate required sample size, must supply desired effect size, as well as baserate")
  }
  if(!is.null(effectsize) & !is.null(h)) {
    stop("Both practical effect size and h were supplied. Only one type of effect size can be used at a time; supply only one and leave the other NULL")
  }
  if(treatmentrate > 1) {
    stop("Treatment rate cannot be greater than 1")
  }
  if((length(alternative) != 1) | (alternative != "two.sided" & alternative != "less" & alternative != "greater")) {
    stop("Must select one alternative for hypothesis test. Can be 'two.sided', 'less', or 'greater'")
  }
  
  ReturnValue = "" #Initialize ReturnValue, which will be used to store the output when it is assigned to an object
  
    #Calculating required sample sizes for one or more effect sizes
    for(i in 1:length(effectsize)) {
      
      if(is.null(h)) {
        h_temp <- pwr::ES.h(baserate + (effectsize[i] * treatmentrate), baserate) #Calculate the effect size h, only if it is not supplied
      }
      result <- pwr::pwr.2p.test(h = h_temp, n = NULL, sig.level = sig.level, power = power, alternative = alternative) #Run the pwr function to determine per-group n needed for significance
      n <- as.numeric(result["n"]) #Select from this output only the per-group sample size n
      
      #Print output
      print(paste0("Required per-group sample size with effect size = ", effectsize[i], ", treatment rate = ", treatmentrate, ": ", round(ceiling(n)), "   [h = ", round(h_temp, 3), "]"))
      
      ReturnValue[i] <- as.numeric(ceiling(n)) #Assign printed value to ReturnValue so it can also be assigned

    }
  
  invisible(as.numeric(ReturnValue)) #Return ReturnValue for assignment without printing it
  
}