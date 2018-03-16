# Sort data
hh             <- hh[with(hh,order(irccode,hhnum)),]

# Generate the 'mixed' variable in pvill
pvill$mixed <- as.numeric(pvill$gender=="m")

# Match surveys to treatment assignment
hh$treat   <- hh$treatment <- pvill$treatment[match(hh$irccode,pvill$irc_code)]
hh$quarter <- pvill$quarter[match(hh$irccode,pvill$irc_code)]
hh$mixed   <- pvill$mixed[match(hh$irccode,pvill$irc_code)]

f <- function(x) {colnames(x)[2] <- "surveyid"
		c <- match(x[,2],List$surveyid)
		x$irc = List$irccodeall[c]
		c2 = match(x$irc,pvill$irc_code)
		x$treat = pvill$treatment[c2]
		x$quarter = pvill$quarter[c2]
		x$mixed = pvill$mixed[c2]
		x}
a <- lapply(list(
                 survey1,
                 survey2),f)
survey1     <- a[[1]]
survey2     <- a[[2]]
rm(a,f)


# fix coding error
hh$hhnum[hh$hhnum==10 & hh$irccode==51 & hh$enumid==11]  <-  1



