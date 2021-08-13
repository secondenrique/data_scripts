library(readr)
library(multicon)

dat <- read_csv("grsmtrx.csv")


#Function to normalize the data on a min-max scale
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}


#Function to invert normalized data--negative correlations become positive correlations for easier compositing of variables
invert <- function(x){
  return(((x-max(x))*(-1)+min(x)))
}


#INSCR composite variable
dat$corrnorm <- normalize(dat$`1a_corrperc`)
dat$bribenorm <- normalize(dat$`1b_bribery`)
dat$bribenorm <- invert(dat$bribenorm)

inscr_comp <- data.frame(dat$corrnorm, dat$bribenorm)
inscr_comp <- as.data.frame(composite(inscr_comp))
inscr_comp <- invert(inscr_comp$`composite(inscr_comp)`)

write.csv(inscr_comp, file="comp.csv")


#Sanctions composite variable
dat$us_norm <- normalize(dat$`2a_us_sanc`)
dat$eu_norm <- normalize(dat$`2b_eu_sanc`)
dat$fatf_norm <- normalize(dat$`2c_fatf_list`)

sanc_comp <- data.frame(dat$us_norm, dat$eu_norm, dat$fatf_norm)
sanc_comp <- as.data.frame(composite(sanc_comp))

write.csv(sanc_comp, file="comp.csv")


#Enforcement composite variable
dat$lawenfnorm <- normalize(dat$`4a_law_enf`)
dat$assetforf <- normalize(dat$`4b_asst_forf`)

enf_comp <- data.frame(dat$lawenfnorm, dat$assetforf)
enf_comp <- as.data.frame(composite(enf_comp))

write.csv(sanc_comp, file="comp.csv")


#Peace index variable
dat$peacenorm <- normalize(dat$`5a_peace_indx`)

stb_comp <- data.frame(stb_comp$peacenorm, stb_comp$rulelawnorm)
stb_comp <- as.data.frame(composite(stb_comp))

write.csv(stb_comp, file="comp.csv")
write.csv(dat, file="risk matrix.csv")
