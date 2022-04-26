coronadata <- read.csv(file.choose())
names(coronadata)
head(coronadata)

coronadata <- coronadata[, -1]

# descriptives

coronadescritptives <- summary(coronadata)

# missing data 

coronadata_numeric <- coronadata[, 2:16]

#list_na <- colnames(coronadata_numeric)[ apply(coronadata_numeric, 2, anyNA)]
#list_na

coronadatafine <- data.frame(
  sapply(
    coronadata_numeric,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))

sum(is.na(coronadatafine))

coronadatascaled <- scale(coronadatafine)

library(lavaan)

# pandemic =~ coronadata[, 2] + coronadata[, 3] + coronadata[, 4] + coronadata[, 5] + coronadata[, 6]
# healthcap =~ coronadata[, 7] + coronadata[, 8] + coronadata[, 9] + coronadata[, 10] + coronadata[, 11] + coronadata[, 12] + coronadata[, 13] + coronadata[, 14] + coronadata[, 15] + coronadata[, 16]

# pandemic =~ Active.Cases + Cured.Discharged.Migrated + Deaths + Total.Confirmed.cases
# healthcap =~ NumPrimaryHealthCenters_HMIS + NumCommunityHealthCenters_HMIS + NumSubDistrictHospitals_HMIS + NumDistrictHospitals_HMIS + TotalPublicHealthFacilities_HMIS + NumPublicBeds_HMIS + NumRuralHospitals_NHP18 + NumRuralBeds_NHP18 + NumUrbanHospitals_NHP18 +NumUrbanBeds_NHP18


coronamodel <- '

    pandemic =~ Active.Cases + Cured.Discharged.Migrated + Deaths 
    healthcap =~ NumPrimaryHealthCenters_HMIS + NumCommunityHealthCenters_HMIS + NumSubDistrictHospitals_HMIS + NumDistrictHospitals_HMIS + NumPublicBeds_HMIS + NumRuralHospitals_NHP18 + NumRuralBeds_NHP18 + NumUrbanHospitals_NHP18 + NumUrbanBeds_NHP18
    
    # direct
    pandemic =~ c*healthcap
    
    # mediator
    
    Received.all.8.basic.vaccinations ~ a*healthcap
    pandemic ~ b*Received.all.8.basic.vaccinations
    
    # indirect effect (a*b)
    ab := a*b
    
    # total effect
    total := c + (a*b) 
    
'

coronafit <- lavaan::sem(coronamodel, data = coronadatascaled)
summary(coronafit, fit.measures= TRUE)

library(semPlot)

par(mfrow=c(2, 1))
par()
semPaths(coronafit, rotation=4)
semPaths(coronafit, "std", edge.label.cex = 0.5, curvePivot = TRUE, rotation = 4)


# reports

setwd("E:/Research/covid19/")

coronadescritptives <- summary(coronadata)
write.csv(data.frame(coronadescritptives), "coronadesc.csv")

write.csv(data.frame(coronarel[1]), "alphas.csv")
write.csv(data.frame(coronarel[2]), "alphas_drop.csv")
write.csv(data.frame(coronarel[3]), "rel_item_statistics.csv")

write.csv(coronafit, "coronafit.csv")
coronasummary <- summary(coronafit, fit.measures = TRUE)
write.csv(coronasummary$FIT, "coronafit.csv")
write.csv(coronasummary$PE, "coronafit_estimates.csv")


#detach("package:lavaan")

library(psych)


coronafafit  <- fa(cor(coronadatafine), 2)

semmodel <- structure.sem(coronafafit)
semmodel

library(sem)
detach("package:sem")
library(lavaan)

coronacfa <- sem(semmodel, data = coronadatafine)
coronacfa                   


# sobel test
# ab/se (for ab)
-0.00771/0.040684
# pvalue 
2*(pt(0.1895094, 1)) - 1