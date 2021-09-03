install.packages("tidyverse")
library("tidyverse")
inter = read.csv("Interventional_cleaned_final.csv")


####DATA EXPLORATION
ggplot(data = inter) +
  geom_bar(mapping = aes(x = inter$overall_status))
table(inter$overall_status)

table(inter$lead_Sponsor)

inter %>%
  count(inter$lead_Sponsor) %>%
  arrange(desc(n))

inter %>%
  count(inter$country_name) %>%
  arrange(desc(n))

inter %>%
  count(inter$Agency_Class_Lead) %>%
  arrange(desc(n))

inter %>%
  count(inter$condition_name) %>%
  arrange(desc(n))

table(inter$year)

usa <- subset(inter,country_name == "United States")

china <- subset(inter,country_name == "China")

glaxo <- subset(inter,lead_Sponsor == "GlaxoSmithKline")

glaxo2 <- subset(glaxo,country_name != "not_provided")

tweny <- subset(inter,year == "2020")

#changing bin names
senior <- subset(inter,minimum_age_bins == "Older Adults")
adults <- subset(inter,minimum_age_bins == "Adults")
child <- subset(inter, maximum_age_bins == "Children ")
str(inter$maximum_age_bins)

adults %>%
  count(adults$condition_name) %>%
  arrange(desc(n))

child %>%
  count(child$condition_name) %>%
  arrange(desc(n))

senior %>%
  count(senior$condition_name) %>%
  arrange(desc(n))


tweny %>%
  count(tweny$lead_Sponsor) %>%
  arrange(desc(n))

tweny %>%
  count(tweny$condition_name) %>%
  arrange(desc(n))

cairo <- subset(tweny,lead_Sponsor == "Cairo University")

cairo%>%
  count(cairo$condition_name) %>%
  arrange(desc(n))

glaxo2 %>%
  count(glaxo2$country_name) %>%
  arrange(desc(n))






##### RANDOM FOREST CLASSIFICATION OF PHASE
set.seed(123)
index <- sample(1:nrow(inter), 8000)
intermodel <- inter[index, ]
intermodel = intermodel[,-c(1,2,4,5,8,9,10,11,13,14,15,17,19,20,23,25,27,28,26,30,31,32,33)]
intermodel <- droplevels(intermodel)
N=nrow(intermodel)
train = sample(1:N,size =0.5*N)
val= sample(setdiff(1:N, train),size =0.5*N )
droplevels(intermodel$phase)
str(intermodel)

#random forest
install.packages("randomForest")
library(randomForest)
fitrf = randomForest(intermodel$phase ~ ., data = intermodel, subset = train,importance =TRUE, maxit = 500)
#variable importance 
varImpPlot(fitrf, type = 1, main = "Variable Importance Plot")
#predict 
pred4 = predict(fitrf, type = "class", newdata = intermodel[val,]) 
tab4 = table(intermodel$phase[val], pred4)
acc2= sum(diag(tab4))/sum(tab4)
table(intermodel$Agency_Class_Lead)







