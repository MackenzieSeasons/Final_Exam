#PART 2
library(tidyverse)
library(apaTables)

raw_data <- read_csv(file="exam_data_f16.csv")
str(raw_data)
raw_data <- read_csv(file = "exam_data_f16.csv", na=c("", "NA", "-999", "-888"))

categorical_variables <- select(raw_data, education, gender, age)

categorical_variables$education <- as.factor(categorical_variables$education)
levels(categorical_variables$education) <- list("HS"=1, "Finished HS"=2, "Some College"=3, "College Grad"=4, "Graduate Degree"=5)

categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)

str(categorical_variables)

#out of range
agreeableness_items <- select(raw_data, A1, A2, A3, A4, A5)
conscientiousness_items <- select(raw_data, C1, C2, C3, C4, C5)
performance_items <- select(raw_data, JP1, JP2, JP3, JP4, JP5)
age <- select(raw_data, age)


psych::describe(agreeableness_items) #out of range
psych::describe(conscientiousness_items) 
psych::describe(performance_items)

is_bad_value <- agreeableness_items<1 | agreeableness_items>6
agreeableness_items[is_bad_value] <- NA

#reverse keying
agreeableness_items <- mutate(agreeableness_items, A1=7-A1)
conscientiousness_items <- mutate(conscientiousness_items, C4=7-C4)
conscientiousness_items <- mutate(conscientiousness_items, C5=7-C5)
performance_items <- mutate(performance_items, JP1=7-JP1)
performance_items <- mutate(performance_items, JP2=7-JP2)

#scale scores
agreeableness <- psych::alpha(as.data.frame(agreeableness_items), check.keys = FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscientiousness_items), check.keys = FALSE)$scores
performance <- psych::alpha(as.data.frame(performance_items), check.keys = FALSE)$scores

#combining into analytic data
analytic_data <- cbind(categorical_variables, agreeableness, conscientiousness, performance)

save(analytic_data, file = "analytic_data.RData")

#APA cor table (reliabilities??)*********
apa.cor.table(analytic_data, filename = "Table1_APA.doc", table.number = 1)


#cor_check
psych::pairs.panels(as.data.frame(analytic_data))


#MULTIPLE REGRESSION
#to what extent does agreeableness predict job performance above/beyond conscientiousness?
#IV = performance
#DV1 = agreeeableness
#DV2 = conscientiousness
regression.1 <- lm(performance ~ conscientiousness, data = analytic_data)
apa.reg.table(regression.1)
#sr2 for conscientiousness is .07, total R2 is .07, 95% CI[.05, .09], 95% CI[.05, .09]

regression.2 <- lm(performance ~ conscientiousness + agreeableness, data=analytic_data)
apa.reg.table(regression.2)
apa.reg.table(regression.2, filename="Table2_APA.doc", table.number=2)
#total R2 is .235, p<.01, 95% CI[.21, .26]
#sr2 for agreeableness is .17, 95% CI [.14, .19], p<.01, SEE TABLE 2

#GENDER REGRESSION
analytic_data_nosex <- select(analytic_data, -gender)
apa.cor.table(analytic_data_nosex)

analytic_data_male <- filter(analytic_data, gender=="Male")
analytic_data_female <- filter(analytic_data, gender=="Female")

#MALE REGRESSION
regression.male <- lm(performance ~ conscientiousness, data = analytic_data_male)
apa.reg.table(regression.male)
#sr2 for conscientiousness is .09, total R2 is .09, 95% CI[.23, .36], 95% CI[.23, .36]

regression.male.2 <- lm(performance ~ conscientiousness + agreeableness, data=analytic_data_male)
apa.reg.table(regression.male.2)
apa.reg.table(regression.male.2, filename="Table3_APA.doc", table.number=3)
#agreeableness for men sr2 is .18, 95% CI [.14, .23], predicts 18% of the variance above and beyond con., SEE TABLE 3

#FEMALE REGRESSION
regression.female <- lm(performance ~ conscientiousness, data = analytic_data_female)
apa.reg.table(regression.female)
#sr2 for conscientiousness is .06, total R2 is .06, 95% CI[.04, .08]

regression.female.2 <- lm(performance ~ conscientiousness + agreeableness, data=analytic_data_female)
apa.reg.table(regression.female.2)
apa.reg.table(regression.female.2, filename="Table4_APA.doc", table.number=4)
#agreeableness sr2 is .15, 95% CI [.12, .18], predicts 15% of the variance above and beyond con., SEE TABLE 4

