#### version 6.0, accuracy .956, sensitivity .943 , specificity .969
#### results #0 276; #1 236
### run time about 2 hours on macos 13.0 with 16G memory


### changed the 0/1 criteria: if more than 30% give a 1 then 1.


# data preparation
library(tidyverse)
library(lubridate)
# data exploration
library(ggmap) # for plotting data on a map
# for meta-ml
library(tidymodels)
library(data.table)
library(GGally)
library(themis)
library(modeldata)
library(parsnip)
library(ROSE)
library(ggcorrplot)
library(mice)

# let's set some global options
options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) # select a lightweight ggplot theme for cleaner plotting
set.seed(2022) # in the AC, you'll be required to set a fixed random seed to make your work reproducible

rm(list=ls())
getwd()



#### load data ---------------------------------------------------------------

##
customers <- read_csv('Desktop/ACup/customers.csv')
sales_orders <- read_csv('Desktop/ACup/sales_orders.csv')
sales_orders_header <- read_csv('Desktop/ACup/sales_orders_header.csv')
service_map <- read_csv('Desktop/ACup/service_map.csv')
business_units <- read_csv('Desktop/ACup/business_units.csv')
classification <- read_csv('Desktop/ACup/classification.csv')
####


#### clean data --------------------------------------------------------------
### clean customers

customers$Type <- as.factor(customers$Type)
customers$Item_Position <- as.numeric(customers$Item_Position)
###

### clean sales_orders

sum(is.na(sales_orders)) ##check na
which(is.na(sales_orders))
sales_orders <- sales_orders[complete.cases(sales_orders),] ## remove na
sales_orders <- rename(sales_orders, Item_Value = Net_Value)
###

### clean sales_orders_header

sum(is.na(sales_orders_header))
sales_orders_header$Creation_Date <- as_date(sales_orders_header$Creation_Date)
sales_orders_header$Release_Date <- as_date(sales_orders_header$Release_Date)

sales_orders_header$Sales_Organization <- as.factor(sales_orders_header$Sales_Organization)
sales_orders_header$Document_Type <- as.factor(sales_orders_header$Document_Type)
sales_orders_header$Delivery <- as.factor(sales_orders_header$Delivery)
sales_orders_header$Net_Value <- as.numeric(sales_orders_header$Net_Value)
sales_orders_header <- rename(sales_orders_header, Total_Value = Net_Value)

glimpse(sales_orders_header %>% mutate_all(is.na) %>% summarize_all(sum))
###

### clean business_units
## need to combine with sales_orders via col cost_center

business_units$Business_Unit <- gsub('BU_', '',business_units$Business_Unit)
business_units$Business_Unit <- as.factor(business_units$Business_Unit) 
business_units <- business_units %>% select(-YHKOKRS)

glimpse(business_units %>% mutate_all(is.na) %>% summarize_all(sum))
###

### clean service_map
## judge whether sales_orders exist service or not#

service_map <- service_map %>% mutate(isService = 1) %>% rename(Material_Class = MATKL_service)
glimpse(service_map %>% mutate_all(is.na) %>% summarize_all(sum))

## join service_map to sales_orders
sales_orders <- left_join(sales_orders, service_map, by = "Material_Class" )
sales_orders <- mutate_at(sales_orders, c('isService'), ~replace(., is.na(.), 0))

glimpse(sales_orders %>% mutate_all(is.na) %>% summarize_all(sum))
###

### clean classification

glimpse(classification %>% mutate_all(is.na) %>% summarize_all(sum)) # 512 entries to be test
### 


#### join tables -------------------------------------------------------------
### join customers to classification
df1 <- left_join(classification, customers, by = "Customer_ID")

glimpse(df1 %>% mutate_all(is.na) %>% summarize_all(sum))

### join other tables to sales_order
df2 <- left_join(sales_orders, sales_orders_header, by = "Sales_Order")
df2 <- left_join(df2, business_units, by = "Cost_Center")


##check na
glimpse(df2 %>% mutate_all(is.na) %>% summarize_all(sum))

### find unmatched Item_Position
unmatched <- anti_join(df2, df1, by = "Item_Position") 
replaced = mutate_at(unmatched, .vars = vars(Item_Position), .funs = funs(.*0))
matched <- semi_join(df2, df1, by = "Item_Position")
df3 <- bind_rows(matched, replaced)

glimpse(df3 %>% mutate_all(is.na) %>% summarize_all(sum))
###

### re-match customers and sales_orders

df4 <- left_join(df1, df3, by = c('Sales_Order', 'Item_Position'))

glimpse(df4 %>% mutate_all(is.na) %>% summarize_all(sum)) # one row in df1 does not match in df3
###

#### further data processing --------------------------------------------------

### delete some meaningless columns
#df4 <- df4 %>% select(-Material_Code, -Cost_Center, -Creator)
df4$Material_Code <- as.factor(df4$Material_Code)
df4$Cost_Center <- as.factor(df4$Cost_Center)
df4$Creator <- as.factor(df4$Creator)
###

### add a overall primary key for df4: each row is a unique transaction
df4$Case_id <- c(1: nrow(df4))
###


### add some engineering features
df4$OrderDuration <- difftime(df4$Release_Date,df4$Creation_Date,units="days")


ggplot(df4, aes(x=OrderDuration)) +
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlab("Order Duration (days)") +
  ylab("Counts")

df4$OrderDuration <- ifelse(df4$OrderDuration == 0, "atonece",
                            ifelse(df4$OrderDuration < 50, "short",
                                   ifelse(df4$OrderDuration < 150, "medium", "long")))



glimpse(df4 %>% mutate_all(is.na) %>% summarize_all(sum))
df4 <- df4 %>% select(-Creation_Date, -Release_Date)
###
df4$Material_Class <- as.factor(df4$Material_Class)
df4$Item_Position <- as.factor(df4$Item_Position)
summary(df4$Item_Position)


### re-order the columns
df4 <- df4[c(   "Case_id", "Test_set_id" , "Reseller" , "Customer_ID",              
                "Sales_Order"    ,    "Item_Position"  , 
                "Type",   "Material_Class" ,"Material_Code","Cost_Center",
                "isService"  ,
                "Sales_Organization", "OrderDuration"   ,  
                "Document_Type"    ,    "Delivery" ,
                "Business_Unit" , "Num_Items"   , "Item_Value"  ,"Total_Value","Creator"      )]
###

# convert string id to numeric data
df4$Customer_ID <- as.numeric(df4$Customer_ID)
df4$Sales_Order <- as.numeric(df4$Sales_Order)
###

# covert Reseller to factor
df4$Reseller <- as.factor(df4$Reseller)
df4$isService <- as.factor(df4$isService)
###



### split train set and test set
df4 <- as.data.table(df4)
df4 <- df4 %>% mutate(OrderDuration = as.factor(OrderDuration))

df4 <- df4 %>% mutate(Material_Class = as.factor(Material_Class))

df4 <- df4 %>% mutate(Material_Code = as.factor(Material_Code))

df4 <- df4 %>% mutate(Cost_Center = as.factor(Cost_Center))

df4 <- df4 %>% mutate(Delivery = as.factor(Delivery))

df4 <- df4 %>% mutate(Creator = as.factor(Creator))

df4 <- df4 %>% mutate(isService = as.factor(isService))

df4 <- df4 %>% mutate(Sales_Organization = as.factor(Sales_Organization))

df4 <- df4 %>% mutate(Document_Type = as.factor(Document_Type))

df4 <- df4 %>% mutate(Business_Unit = as.factor(Business_Unit))

calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(na.omit(x))
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

df4 <- mutate_at(df4, c('Material_Class'), ~replace(., is.na(.), calc_mode(df4$Material_Class)))
df4 <- mutate_at(df4, c('Material_Code'), ~replace(., is.na(.), calc_mode(df4$Material_Code)))
df4 <- mutate_at(df4, c('Cost_Center'), ~replace(., is.na(.), calc_mode(df4$Cost_Center)))
df4 <- mutate_at(df4, c('isService'), ~replace(., is.na(.), calc_mode(df4$isService)))
df4 <- mutate_at(df4, c('Creator'), ~replace(., is.na(.), calc_mode(df4$Creator)))
df4 <- mutate_at(df4, c('Sales_Organization'), ~replace(., is.na(.), calc_mode(df4$Sales_Organization)))
df4 <- mutate_at(df4, c('Document_Type'), ~replace(., is.na(.), calc_mode(df4$Document_Type)))
df4 <- mutate_at(df4, c('Delivery'), ~replace(., is.na(.), calc_mode(df4$Delivery)))
df4 <- mutate_at(df4, c('Business_Unit'), ~replace(., is.na(.), calc_mode(df4$Business_Unit)))
df4 <- mutate_at(df4, c('Item_Position'), ~replace(., is.na(.), calc_mode(df4$Item_Position)))
df4 <- mutate_at(df4, c('OrderDuration'), ~replace(., is.na(.), calc_mode(df4$OrderDuration)))

mice_model <- mice(df4[, c("Item_Value", "Total_Value", "Num_Items")], method = "pmm", seed = 2022)

# Run the imputation
imputed_df <- complete(mice_model)
df4[, c("Item_Value", "Total_Value", "Num_Items")] <- imputed_df[, c("Item_Value", "Total_Value", "Num_Items")]



glimpse(df4 %>% mutate_all(is.na) %>% summarize_all(sum))



trainset <- df4[!is.na(Reseller)]
testset <- df4[is.na(Reseller)]

unique(testset$Test_set_id) %>% length() # count testset id
###

#### template checking --------------------------------------------------------
submission_template <- read_csv('Desktop/ACup/pub_K8PzhiD.csv')

### check whether your test set matches the submission template.
template_ids <- submission_template %>% arrange(id) %>% pull(id)
test_ids <- testset %>% arrange(Test_set_id) %>% pull(Test_set_id) %>% unique()
all(template_ids == test_ids) #true if all the same, otherwise fails
###

#### training set processing --------------------------------------------------

### delete training entries if most of the columns are NAs
trainset <- trainset[!(is.na(trainset$Total_Value)&is.na(trainset$Num_Items)&is.na(trainset$Sales_Organization)),]
trainset <- trainset %>% select(-Test_set_id) # delete test set id in trainset


glimpse(trainset %>% mutate_all(is.na) %>% summarize_all(sum)) # check NAs
###

### balance data
prop.table(table(trainset$Reseller)) # 70%/30% big difference should be balanced
trainset <- ovun.sample(Reseller ~ ., data = trainset, method = "both", N = nrow(trainset) )$data
prop.table(table(trainset$Reseller)) # now 50%/50% -> good
###

#### training set split for validation -----------------------------------------

train_validation_split <- initial_split(trainset, prop = 0.80, strata=Reseller)
train <- training(train_validation_split)
validation <- testing(train_validation_split)
###

#### recipe -------------------------------------------------------------------

rec <- recipe(
  # specify predictors, target and data. Necessary to enable code auto-completion
  Reseller ~ ., data = train) %>% 
  # tell tidymodels that Case_id is an ID and should not be used in any model
  update_role(c(Case_id,Customer_ID,Sales_Order,Item_Position), new_role = "ID") %>% 
  # impute all numerics with mean
  step_impute_mean(all_numeric()) %>% 
  # impute all other nominal (character + factor) columns with the value "none"
  step_impute_mode(all_nominal(),-Reseller) %>% 
  # convert all strings to factors
  step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
  # remove constant columns
  step_zv(all_predictors())

rec
# preparing the recipe via prep():
rec %>% prep()
# bake
rec %>% prep() %>% bake(new_data=train)
###

#### Model specification -------------------------------------------------------
model <- rand_forest(
  mode = 'classification',mtry = 8 ,trees = 600, min_n = 5)%>%
  # you could set additional hyperparameters here if desired
  set_engine("ranger",
             # you could set additional ranger-specific params here
             importance = "impurity" #needed for feature importance plot below
  ) 

model
###

#### Training workflow --------------------------------------------------------

training_workflow <- 
  # start with a blank workflow() object
  workflow() %>% 
  # add our model specification
  add_model(model)%>%
  # add our preprocessing recipe
  add_recipe(rec) 


training_workflow
###


#### cross validation ----------------------------------------------------------

folds <- train %>% vfold_cv(v=5)

cv_fits <- 
  training_workflow %>% 
  fit_resamples(folds,
                metrics = metric_set(accuracy, sens, spec)
  )

cv_fits %>% collect_metrics()
###



#### evaluating and tuning ----------------------------------------------------

best_config <- cv_fits %>%
  # find the best tried configuration for a certain criterion
  select_best('accuracy')

final_workflow <- training_workflow %>% finalize_workflow(best_config)
final_workflow

## Training the final model and validating it:
trained_model <- final_workflow %>% fit(data=train)
trained_model

## look at calculated feature importance:
trained_model %>% extract_fit_parsnip() %>% vip::vip()
##ggcorrplot::ggcorrplot(cor(trained_model %>% extract_fit_parsnip()))

## apply the model to the train set itself:
train_set_with_predictions <-
  bind_cols(
    train,
    trained_model %>% predict(train)
  )
train_set_with_predictions

## compare the new column to the true labels:
train_set_with_predictions %>% ggplot(aes(x=Reseller, y=.pred_class)) +
  geom_point() +
  stat_function(fun=identity)
###

#### accuracy test -------------------------------------------------------------

validation_predictions <-
  predict(trained_model, validation)%>%
  bind_cols(predict(trained_model, validation, type = "prob")) %>%
  bind_cols(validation %>% select(Reseller))

# confusion matrix
validation_predictions %>% conf_mat(Reseller, .pred_class)

# accuracy, sensitivity, specificity
validation_predictions %>% accuracy(Reseller, .pred_class)
validation_predictions %>% sensitivity(Reseller, .pred_class)
validation_predictions %>% specificity(Reseller, .pred_class)

###




#### Prediction on test set ---------------------------------------------------

submission <- bind_cols(
  testset %>% select(Test_set_id),
  trained_model %>% predict(testset)
) %>%
  # column names must be the same as in submission template!
  rename(prediction = .pred_class, id = Test_set_id ) %>%
  # order by id
  arrange(id)

# calculate result based on criteria: for each test cases if #1 greater than #0 then 1, vise versa
submission <- submission %>% group_by(id) %>% summarise(n = n(), pred1 = sum(as.numeric(as.character(prediction))))
#submission$prediction <- ifelse(submission$pred1 >= (submission$n - submission$pred1), 1, 0)
submission$prediction <- ifelse(submission$pred1 >= (submission$n * 0.3), 1, 0)
submission <- submission %>% select(-n, -pred1)

###

#### submission file creation -------------------------------------------------

write_csv(submission, "predictions_Cupupup_07.csv")

###

#### check results ------------------------------------------------------------

table(submission$prediction)
prop.table(table(submission$prediction))

###









