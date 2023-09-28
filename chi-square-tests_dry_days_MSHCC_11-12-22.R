# Chi-square test for # dry days per site (canopy type) and stream: data in chi.csv and chiDRY.csv
#

str(chi)

chisq.test(chi, correct=FALSE)


str(chiDRY)

chisq.test(chiDRY, correct=FALSE)