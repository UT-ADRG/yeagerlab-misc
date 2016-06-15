
### Program to create a balance table


  cond_diffs_cont <- function(condition, outcome){
    library(broom)
    library(MAd)
    mean<- tapply(outcome , condition, mean, na.rm=T)
    sd<- tapply(outcome , condition, sd, na.rm=T)
    t <- tidy(t.test(outcome~condition)) ### getting the statistical test
    es <- t_to_d(t$statistic,t$parameter/2,t$parameter/2) ## Effect size - t to d formula
    x <- round(cbind(mean[1], sd[1], mean[2], sd[2], t$statistic, es[1]),3)
    x <- cbind(x,  format_pval(t$p.value))
  }
  
  
  cond_diffs_categ <- function(condition, outcome){
    library(broom)
    library(MAd)
    mean<- tapply(outcome , condition, mean, na.rm=T)
    t <- tidy(chisq.test(condition, outcome)) ### getting the statistical test
    n <- sum(tidy(table(data$cond, data$s1_mindset_dichot))[,3])
    es <- r_to_d(r_from_chi(t$statistic,n),n) ## Effect size - chisq to r to d  
    x <- cbind(
      paste(round(mean[1], 3)*100, "%", sep="")
      , NA, 
      paste(round(mean[2], 3)*100, "%", sep=""),
      NA, 
      round(t$statistic, 3), 
      round(es[1], 3)
      )
    x <- cbind(x,  format_pval(t$p.value))
  }
  

balancetable <- function(cond, contvars, catvars, rownames){ ## Takes as input columns from data frame
  library(dplyr)
  x<- c()
  for (a in c(1:length(contvars))){
    x<- rbind(x,
              (cond_diffs_cont(cond, contvars[,a]))
    )
  }
  for (b in c(1:length(catvars))){
    x<- rbind(x,
              (cond_diffs_categ(cond, catvars[,b]))
    )
  }
  rownames(x)<- rownames
  colnames(x)<- c("M", "SD", "M", "SD",
                  "Test Statistic", "d", "p value") 
  return(x)
}

# Example: balancetable <- balancetable(cond, contvars, catvars, rows)
