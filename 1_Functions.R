#### Packages and functions required to run replication

#                 Table of Contents                  #
######################################################
# 0.  Load and install packages                      #
# #.  Mean effects and PCA calculators               #
# #.  Variable standardizer                          #
# #.  Table functions                                #
# #.  Plot functions                                 #

######################################################
# 0.  Load and install packages                      #
######################################################

packages <- c("Hmisc",
              "xtable",
              "foreign",
              "magrittr",
              "sandwich",
               "parallel",
              "plyr"
              )


if (FALSE%in%(packages%in%rownames(installed.packages())))
     {stop(c("Please install the following packages: ",
             paste(packages[packages%in%rownames(installed.packages())==FALSE],
                   collapse = ", ")),
             ".")}

sapply(packages,require,character.only=T)

rm(packages)

######################################################
# #.  Mean effects and PCA calculators               #
######################################################

## Mean effects function with no aggregation
m.eff = function(data,vars) {
     a = data
     Ccenter = apply(a[a$treat==0,vars],2,mean,na.rm=TRUE)
     Csd = apply(a[a$treat==0,vars],2,sd,na.rm=TRUE)
     a[,vars] = scale(a[,vars],center=Ccenter,scale=Csd)
     meaneff = apply(a[,vars],1,sum,na.rm=TRUE)/apply(a[,vars],1,function(x) sum(!is.na(x)))
     meaneff/sd(meaneff[a$treat==0], na.rm=TRUE)
}

## Mean effects function that first aggregates data to community level averages
m.eff2 = function(data,vars) {
     if(is.null(data$treat)) print("Treatment var (treat) has to be included in dataframe")
     if(is.null(data$irc)) print("IRC code var (irc) has to be included in dataframe")
     a = data.frame(apply(data[,vars],2,function(x) tapply(x,data$irc,mean,na.rm=TRUE)))
     a$irc = as.numeric(rownames(a))
     a$treat = data$treat[match(a$irc,data$irc)]
     if (is.numeric(vars)) {avars = names(data)[vars]} else {avars = vars}
     Ccenter = apply(a[a$treat==0,avars],2,mean,na.rm=TRUE)
     Csd = apply(a[a$treat==0,avars],2,sd,na.rm=TRUE)
     a[,avars] = scale(a[,avars],center=Ccenter,scale=Csd)
     meaneff = apply(a[,avars],1,sum,na.rm=TRUE)/apply(a[,avars],1,function(x) sum(!is.na(x)))
     meaneff/sd(meaneff[a$treat==0], na.rm=TRUE)
}

## Same as m.eff2 but for mixed/women's treatment
m.effw = function(data,vars) {
     if(is.null(data$W)) print("Treatment var (W) has to be included in dataframe")
     if(is.null(data$irc)) print("IRC code var (irc) has to be included in dataframe")
     a = data.frame(apply(data[,vars],2,function(x) tapply(x,data$irc,mean,na.rm=TRUE)))
     a$irc = as.numeric(rownames(a))
     a$W = data$W[match(a$irc,data$irc)]
     if (is.numeric(vars)) {avars = names(data)[vars]} else {avars = vars}
     Ccenter = apply(a[a$W==0,avars],2,mean,na.rm=TRUE)
     Csd = apply(a[a$W==0,avars],2,sd,na.rm=TRUE)
     a[,avars] = scale(a[,avars],center=Ccenter,scale=Csd)
     meaneff = apply(a[,avars],1,sum,na.rm=TRUE)/apply(a[,avars],1,function(x) sum(!is.na(x)))
     meaneff/sd(meaneff[a$W==0], na.rm=TRUE)
}
# 
# ## Function to produce principal components estimates instead of mean effects
# # Note: imputes missing values based on average for treatment or control set
pca = function(data,vars) {
     a = data.frame(apply(data[,vars],2,function(x) tapply(x,data$irc,mean,na.rm=T)))
     a$irc = as.numeric(rownames(a))
     a$treat = data$treat[match(a$irc,data$irc)]
     a[a$treat==0,] = apply(a[a$treat==0,],2,function(x) replace(x,is.na(x),mean(x,na.rm=T)))
     a[a$treat==1,] = apply(a[a$treat==1,],2,function(x) replace(x,is.na(x),mean(x,na.rm=T)))
     if (is.numeric(vars)) {avars = names(data)[vars]} else {avars = vars}
     t = prcomp(a[,avars],scale.=T)$x[,1]
     (t - mean(t[a$treat==0]))/sd(t[a$treat==0])
}

######################################################
# #.  Variable standardizer                          #
######################################################

## Function to standardize and make composite measures.
comp = function(data,vars) {t = scale(data[,vars])
                            scale(apply(t,1,sum,na.rm=TRUE))}





#' Generates nice output after cluster robust
#'
output_function <- function(X, stars = TRUE, round = 2, alpha = 0.05, coefrows = c(2:4)){
  ncoefs <- length(coefrows)
  out <- names <- matrix(NA, 2*ncoefs+1)
  
  out[2*(1:ncoefs)-1] <-  round(X[[1]][coefrows,1],2)
  out[2*(1:ncoefs)] <-  paste("(", round(X[[1]][coefrows,2],2), ")", sep ="")
  out[2*ncoefs+1] <- X[[2]]
  
  if(stars) out[2*(1:ncoefs)][X[[1]][coefrows,4] <= alpha] <-
    paste(out[2*(1:ncoefs)][X[[1]][coefrows,4] <= 0.05], "**", sep ="")
  
  names[2*(1:ncoefs)-1] <- rownames(X[[1]])[coefrows]
  names[2*(1:ncoefs)] <- paste("sd_", rownames(X[[1]])[coefrows], sep = "")
  names[2*ncoefs+1] <- "N"
  rownames(out) <- names
  
  out
}


# Cluster Robust ------------------------------------------------------------------------------





# RI functions
##############################################################################

## Prepare Y and T data of same dimension; Y averaged over units
Prep.Data = function(y, id_Y, Z, id_Z, subseti=TRUE){
  Y=y
  Y[Y==-66 | Y==-77 | Y==-88 | Y== -99] = NA    # Tricky for variables that have negative numbers
  Y[subseti!=TRUE] = NA
  y = tapply(Y,id_Y, mean, na.rm=TRUE)            # average y
  y = data.frame(id = as.numeric(names(y)),Y=y)   # average y
  Z = data.frame(id=id_Z, treatment=Z)								# Treatment Data
  data.frame(y, Z=Z$treatment[match(y$id,Z$id)])
  }				




