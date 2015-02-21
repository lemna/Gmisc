library(tableone)

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Make categorical variables factors
varsToFactor <- c("status","ascites","hepato","spiders","edema","stage")
pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)

## Create a variable list
dput(names(pbc))
vars <- c("time","status","age","sex","ascites","hepato",
          "spiders","edema","bili","chol","albumin",
          "copper","alk.phos","ast","trig","platelet",
          "protime","stage")


pbc$trt <- factor(pbc$trt,
                  labels = c("D-penicillmain", "Placebo"))
## Create Table 1 stratified by trt
tableOne <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc)

htmlTable(tableOne,
          caption="A descriptive table with the Mayo Clinic trial
          in primary biliary cirrhosis (PBC) of the liver",
          col.rgroup = c("#FFF", "#F9F9F0"))
