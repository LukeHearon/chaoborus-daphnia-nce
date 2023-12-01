library(ggplot2)
library(survival)
library(data.table)

controlBlue <- "#7768FF"
metschOrange <- "#F5A047"
kairoPink <- "#F382FF"
troubleRed <- "#FF5040"

plot.survival <- function(treatment, day_fate, status, title_in){
  df <- data.frame(treatment, day_fate, status)
  
  #this function seems like a kludge to fix my ignorance of how to input arguments to lapply
  makemod <- function(treatment_in){
    sub <- subset(df, treatment == treatment_in)
    survfit(Surv(day_fate, status) ~ 1, data = sub) 
  }
  
  models <- data.frame(treatment = unique(df$treatment))
  models$model <- lapply(X = models$treatment, FUN = makemod)
  
  mod.to.df <- function(model, treatment){
    data.frame(
      treatment = treatment,
      day = model$time,
      survival = model$surv,
      CI_low = model$lower,
      CI_hi = model$upper
    )
  }
  
  fullSurvival <- mapply(FUN = mod.to.df, models$model, models$treatment, SIMPLIFY = F) %>%
    rbindlist
  
  
  ggplot(fullSurvival, aes(x = day, y = survival, color = treatment)) +
    geom_step() +
#    geom_ribbon(aes(ymin = CI_low, ymax = CI_hi, fill = treatment), alpha = 0.2, linetype = 0) +
    scale_color_manual("legend", values=c("k-m-" = controlBlue,
                                          "k-m+" = metschOrange,
                                          "k+m-" = kairoPink,
                                          "k+m+" = troubleRed)) +
    scale_fill_manual("legend", values=c("k-m-" = controlBlue,
                                         "k-m+" = metschOrange,
                                         "k+m-" = kairoPink,
                                         "k+m+" = troubleRed)) +
    ylim(0, 1) +
    xlim(0, 70) + # x limits hard coded to reflect maximum survival seen in the experiment
    ylab("Proportion Population") +
    ggtitle(title_in) +
    theme(plot.title = element_text(size = 18, family = "Charter")) +
    theme(axis.title.y = element_text(family = "Charter"))
  
}

# expects a column titled "status" that indicates if death ocurred (1?) or did not (0?)
plot.survival_clone <- function(dataset){
  
  #this function seems like a kludge to fix my ignorance of how to input arguments to lapply
  makemod <- function(treatment_in){
    sub <- subset(df, treatment == treatment_in)
    survfit(Surv(day_fate, status) ~ 1, data = sub) 
  }
  
  models <- data.frame(treatment = unique(df[{{var_category}}]))
  models$model <- lapply(X = models$treatment, FUN = makemod)
  
  mod.to.df <- function(model, treatment){
    data.frame(
      treatment = treatment,
      day = model$time,
      survival = model$surv,
      CI_low = model$lower,
      CI_hi = model$upper
    )
  }
  
  fullSurvival <- mapply(FUN = mod.to.df, models$model, models$treatment, SIMPLIFY = F) %>%
    rbindlist
  
  
  ggplot(fullSurvival, aes(x = day, y = survival, color = treatment)) +
    geom_step() +
    #    geom_ribbon(aes(ymin = CI_low, ymax = CI_hi, fill = treatment), alpha = 0.2, linetype = 0) +
    scale_color_manual("legend", values=c("k-m-" = controlBlue,
                                          "k-m+" = metschOrange,
                                          "k+m-" = kairoPink,
                                          "k+m+" = troubleRed)) +
    scale_fill_manual("legend", values=c("k-m-" = controlBlue,
                                         "k-m+" = metschOrange,
                                         "k+m-" = kairoPink,
                                         "k+m+" = troubleRed)) +
    ylim(0, 1) +
    xlim(0, 70) + # x limits hard coded to reflect maximum survival seen in the experiment
    ylab("Proportion Population") +
    ggtitle(title_in) +
    theme(plot.title = element_text(size = 18, family = "Charter")) +
    theme(axis.title.y = element_text(family = "Charter"))
  
}
