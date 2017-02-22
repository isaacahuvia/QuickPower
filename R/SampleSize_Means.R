#' Determining required sample size with two means of equal n
#' 
#' This function runs power calculations with two means of equal n, determining what sample size is necessary to see a statistically significant effect. Results are printed and can also be assigned.
#' @param populationmean The population mean for the statistic in question. Required for all analyses.
#' @param populationdev The population standard deviation for the statistic in question. Required for all analyses. 
#' @param effectsize The difference in means between the treatment group and the base rate. Can be a single integer \code{x} or a combination of integers \code{c(x1, x2, etc)}. This is a "practical" effect size; see details.
#' @param alternative The type of hypothesis. Can be \code{"greater"} (treatment mean > control mean), \code{"less"} (treatment mean < control mean), or \code{"two.sided"} (treatment mean != control mean).
#' @param treatmentrate The proportion of those in the treatment group who are expected to complete treatment; see details. 
#' @param sig.level The significance level of the hypothetical test. Defaults to \code{0.05}.
#' @param power The power of the hypothetical test. Defaults to \code{0.8}.
#' @param d Refers to Cohen's d, the formal score for effect size in tests of means. You may supply a Cohen's d score instead of a practical effect size if you prefer. 
#' @details 
#' The effect size input is a "practical" effect size in that it is the
#' difference in means in observed units, unlike Cohen's d. The calculations
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
#' In the output, Cohen's d is calculated after taking treatment rate into
#' effect. If the treatment rate given to the function is lower than 1, the d
#' supplied will also be lower, even if all other numbers are constant.
#' 
#' Printed value is rounded; for an unrounded value, assign output to an object.
#' 
#' @examples
#' Please refer to vignette for detailed examples.
#' 
#' #Determining required per-group sample given effect size, etc
#' SampleSize_Means(populationmean = 100, populationdev = 30, effectsize = -20, treatmentrate = .9)
#'  
#' #Determining required per-group sample size across multiple effect sizes
#' SampleSize_Means(populationmean = 100, populationdev = 30, effectsize = c(-20, -10, -5), treatmentrate = .9)
#' 
#' #Assigning (unrounded) required sample size
#' n <- SampleSize_Means(populationmean = 100, populationdev = 30, effectsize = c(-20, -10, -5), treatmentrate = .9)
#' n
#' @export

SampleSize_Means <- function(populationmean = NULL, populationdev = NULL, effectsize = NULL, alternative = c("two.sided","less","greater"), treatmentrate = 1, sig.level = .05, power = .8, d = NULL) {
  
  #Do not run the function if requirements are not met
  if((is.null(effectsize) & is.null(d)) | is.null(populationmean) | is.null(populationdev)) {
    stop("To calculate required sample size, must supply desired effect size, as well as population mean and standard deviation")
  }
  if(!is.null(effectsize) & !is.null(d)) {
    stop("Both practical effect size and d were supplied. Only one type of effect size can be used at a time; supply only one and leave the other NULL")
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
      
      if(is.null(d)) {
        d_temp <- (effectsize[i] * treatmentrate) / populationdev #Calculate effect size d, only if it is not supplied
      }
      result <- pwr::pwr.t.test(d = d_temp, n = NULL, sig.level = sig.level, power = power, alternative = alternative, type = "two.sample") #Run the pwr function to determine per-group n needed for significance
      n <- as.numeric(result["n"]) #Select from this output only the per-group sample size n
      
      #Print output
      print(paste0("Required per-group sample size with effect size = ", effectsize[i], ", treatment rate = ", treatmentrate, ": ", round(ceiling(n)), "   [d = ", round(d_temp, 3), "]"))
      
      ReturnValue[i] <- as.numeric(ceiling(n)) #Assign printed value to ReturnValue so it can also be assigned
      
    }
  
  invisible(as.numeric(ReturnValue)) #Return ReturnValue for assignment without printing it
  
}