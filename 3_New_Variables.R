#### Generates variables used in the analysis

######################################################
#          0.  Recode treatment variables            #
######################################################

# generate new treatment variables:
pvill$W <- 1- pvill$mixed 
pvill$Q <- pvill$quarter
hh$gencomp <- hh$W <- 1-hh$gencomp # 1 = all women, 0 = mixed 

# identifiers
pvill$Voinjama <- 1*(pvill$district=="Voinjama")
hh$town <- pvill$place_name[match(hh$irccode,pvill$irc_code)]
hh$quarter <- pvill$quarter[match(hh$irccode,pvill$irc_code)]
indiv$quarter <- pvill$quarter[match(indiv$irc,pvill$irc_code)]

# cycle of game
e = pvill$dateofgame
pvill$CYCLE1 = 1*(e=="14-Jul" | e=="15-Jul" | e=="16-Jul" | e=="17-Jul" | e=="18-Jul" | e=="19-Jul" | e=="20-Jul" )

# new Variables for hh data
hh$secret_NM <- hh$secret
hh$secret_NM[hh$secret < 0] <- NA
hh$anon <- 1 - hh$secret_NM
hh$pubopconcern <- hh$pubopwt == 1
hh$pubopconcern[hh$pubopwt < 0] <- NA

######################################################
#        1.  Remove guesses above tolerance          #
######################################################
tolerance <- 30 

# remove non-sensical guesses about how many people kept money
hh$kept300_NM <- hh$kept300 
hh$kept300_NM[hh$kept300 < 0] <- NA

hh$kept0_NM <- hh$kept0 
hh$kept0_NM[hh$kept0 < 0] <- NA 

# drop cases for whom guesses singly or jointly more than "tolerance"
hh$TOO_HIGH <- (hh$kept300_NM + hh$kept0_NM > tolerance) | (hh$kept300_NM>tolerance) | (hh$kept0_NM>tolerance)

# impose 23-person ceiling on other responses
hh$kept300_NM[(hh$kept300+hh$kept0)>23] <- (23*(hh$kept300/(hh$kept300+hh$kept0)))[(hh$kept300+hh$kept0)>23]
hh$kept300_NM[hh$kept300_NM>23] <- 23
hh$kept300_NM[hh$TOO_HIGH==TRUE] <- NA

hh$kept0_NM[(hh$kept300+hh$kept0)>23] <- (23*(hh$kept0/(hh$kept300+hh$kept0)))[(hh$kept300+hh$kept0)>23]
hh$kept0_NM[hh$kept0_NM>23] <- 23
hh$kept0_NM[hh$TOO_HIGH==TRUE] <- NA

hh$kept300_POS <- -hh$kept300_NM

hh$lowhi_NM <- hh$lowhi
hh$lowhi_NM[hh$lowhi < 0] <- NA

######################################################
#        2.  Generate Mean Effects and PCA           #
######################################################

# Mean effects variable at individual level
hh$notanon <- m.eff(hh,c('secret_NM','pubopconcern'))

# Datasets containing IRC code level mean effects for use in analysis
gamemeff2 <- data.frame(irc = 1:83,notanon = m.eff2(hh,c('secret_NM','pubopconcern'))) 

# For use in principle components analysis  
game.pca <- data.frame(irc = 1:83,notanon = pca(hh,c('secret_NM','pubopconcern'))) 


t                  <- tapply(hh$contrib==0,hh$irccode,sum,na.rm=T)
pvill$True_Kept300 <- t[match(pvill$irc_code,rownames(t))]
t                  <- tapply(hh$contrib==300,hh$irccode,sum,na.rm=T)
pvill$True_Kept0   <- t[match(pvill$irc_code,rownames(t))]

hh$True_Kept300 <- pvill$True_Kept300[match(hh$irccode,pvill$irc_code)]
hh$True_Kept0   <- pvill$True_Kept0[match(hh$irccode,pvill$irc_code)]


######################################################
#         3.  Generate New pvill variables           #
######################################################


pvill$meancontrib <- sapply(1:83, function(x) mean(hh$contrib[hh$irccode==x],  na.rm=TRUE))
pvill$irccode     <- pvill$irc_code

######################################################
######################################################

rm(t,e,tolerance)


hh %<>% dplyr::mutate(gen3      = mixed*10 + gender, # check order
                      Z = plyr::mapvalues(gen3, from = c(0,10,11), to = c(1,2,3)),
                      mfgiving2 = (replace(mfgiving, (mfgiving<0), NA))  -  1  ,
                      mfgiving3 = replace(mfgiving2, mfgiving2 == 2, .5),
                      notanon2  = (notanon - min(notanon, na.rm = TRUE))/diff(range(notanon, na.rm = TRUE)),
                      kept300_2 = ifelse(kept300 == 24, 23, ifelse(kept300 < 0, NA, kept300)),
                      kept0_2   = ifelse(kept0 %in% c(24,44), 23, ifelse(kept0 < 0, NA, kept0)),
                      others    = (kept300_2 * 0 + kept0_2 * 300 + (23 - kept0_2 - kept300_2) * 150)/23,
                      others    = replace(others, !is.na(enumid) & (enumid %in% c(32,43,44)), NA),   # Bad enumerators on expectations var
                      others_missing = is.na(others),
                      others_village_av = ave(others, irccode, FUN = function(x) mean(x, na.rm = TRUE)),
                      others_impute = ifelse(others_missing, others_village_av, others), 
                      contrib = 100*round(ifelse(contrib > 300, 300,contrib)/100))







conditions <- c("Women in all women", "Women in mixed", "Men in mixed")




# create data set for gender effects table and permutation matrix
g.eff_df <- Prep.Data(y = hh$contrib, id_Y = hh$irccode, Z = pvill$W, id_Z = pvill$irc_code, subseti = hh$gender==0)


#END