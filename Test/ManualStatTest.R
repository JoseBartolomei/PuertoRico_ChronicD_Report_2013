## This scrip is for anyone to load the data into R and check mannually the conducted
## procedures and veryfy for themself the result of the report

# Remove everithing from memory
rm(list = ls())
gc()

# Load BRFSS 2013 data.frame --------------------------------------------------------

load(file = "./Data/BRFSS_2013.RData")

# Turn data.frame to a .csv file ----------------------------------------------------

# write.csv(BRFSS_ADS,
# 					 file = "/home/surfprjab/Dropbox/Puerto_Rico_Chronic_Report/Data/BRFSS_2013.csv",
# 					 sep = ",", na = "NA", row.names = TRUE, col.names = TRUE)


# Re-reference ----------------------------------------------------------------------
################# Bellow you can reference a level in case is nedded #############
################## The levels need to be relevel here to be taken into account in the
################## survey design
#BRFSS_ADS$ <- relevel(BRFSS_ADS$, ref = "Yes")
BRFSS_ADS$cvdstrk <- relevel(BRFSS_ADS$cvdstrk, ref = "No")
BRFSS_ADS$cvdinfr <- relevel(BRFSS_ADS$cvdinfr, ref = "No")
BRFSS_ADS$cvdcrhd <- relevel(BRFSS_ADS$cvdcrhd, ref = "No")


# Load Survey Library ---------------------------------------------------------------
library(survey)

# Set survey Design -----------------------------------------------------------------
brfss.sd <- svydesign(ids = ~0, strata = ~iyear + ststr, weights = ~finalwt, 
                      nest = TRUE, data = BRFSS_ADS)

brfss.sd <- as.svydesign2(brfss.sd)

# Dependent Var ---------------------------------------------------------------------
DP <- ~diabetes
# ~currasth, ~diabetes, ~cvdcrhd, ~cvdinfr, ~cvdstrk
if(DP == "~currasth"){
	brfss.sd <- subset(brfss.sd, (!asthmst  ==  "Former"))
}
# Below the name of the colums of the five diseases in the report
# For following procedures interchange in DP de variable name


# Calculate prevalence --------------------------------------------------------------
Prevalence <- svyby(DP, ~ageg2, FUN= svymean, brfss.sd, na.rm=TRUE)[, 3] * 100
Prevalence
# Calculate frequency ---------------------------------------------------------------
Cases <- svyby(DP, ~ageg2, FUN= svytotal, brfss.sd, na.rm=TRUE)[,3]
Cases

# Calculate logistic regression model -----------------------------------------------
# "currasth", "diabetes", "cvdcrhd", "cvdinfr", "cvdstrk"
glm <- 
 svyglm(currasth ~ sex + incomg + ageg2 + educag3 +  marital2 + emplrec2, 
        design = brfss.sd , family = quasibinomial)

# Adjusted Odds Ratio
AdjOR <- exp(glm$coefficients)
AdjOR

