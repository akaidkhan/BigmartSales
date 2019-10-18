library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining 
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling 
library(corrplot)   # used for making correlation plot 
library(xgboost)    # used for building XGBoost model 
library(cowplot)    # used for combining multiple plots

train = fread("Train_UWu5bXk.csv") 
test = fread("Test_u94Q5KV.csv") 
submission = fread("SampleSubmission_TmnO39y.csv")

dim(train)
dim(test)
names(train)
names(test)
str(train)
str(test)
test[,Item_Outlet_Sales := NA] 
combi = rbind(train, test) # combining train and test datasets dim(combi)


ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), 
                               binwidth = 100,
                               fill = "darkgreen") +  xlab("Item_Outlet_Sales")



#Bivariate Analysis

train = combi[1:nrow(train)] # extracting train data from the combined data

# Item_Weight vs Item_Outlet_Sales 
p9 = ggplot(train) +      geom_point(aes(Item_Weight, Item_Outlet_Sales), 
                                     colour = "violet",
                                     alpha = 0.3) +     theme(axis.title = element_text(size = 8.5))
# Item_Visibility vs Item_Outlet_Sales 
p10 = ggplot(train) +       geom_point(aes(Item_Visibility, Item_Outlet_Sales), 
                                       colour = "violet",
                                       alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))
# Item_MRP vs Item_Outlet_Sales 
p11 = ggplot(train) +       geom_point(aes(Item_MRP, Item_Outlet_Sales),
                                       colour = "violet",
                                       alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2) 
plot_grid(p9, second_row_2, nrow = 2)


#--------------------------------------------------

# Item_Type vs Item_Outlet_Sales 
p12 = ggplot(train) +       geom_violin(aes(Item_Type, Item_Outlet_Sales),fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                                                                                                         axis.text = element_text(size = 6),    
                                                                                                         axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) +       geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales),fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                                                                                                                axis.text = element_text(size = 8),
                                                                                                                axis.title = element_text(size = 8.5))
# Outlet_Identifier vs Item_Outlet_Sales 
p14 = ggplot(train) +       geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales),
                                        fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                                                                       axis.text = element_text(size = 8),   
                                                                       axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)


#-----------------
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales),
                            fill = "magenta")

#-------------------------------
p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") 
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")
plot_grid(p15, p16, ncol = 1)


#Missing Value Treatment

sum(is.na(combi$Item_Weight))
missing_index = which(is.na(combi$Item_Weight)) 
for(i in missing_index)
{    item = combi$Item_Identifier[i] 
combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T) }
sum(is.na(combi$Item_Weight))

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)


zero_index = which(combi$Item_Visibility == 0) for(i in zero_index)
{    item = combi$Item_Identifier[i]  
combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  }
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#Feature Engineering 1
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")


# create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable",ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]
table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]



combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 
# years of operation for outlets 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 
# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]


# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",
                                   ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",   
                                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]


# Feature Engineering 2-------------------------------------------------

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
# create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2)) 

combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 
# years of operation for outlets 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 
# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]


# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",  
                                   ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",    
                                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]


combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,                                 ifelse(Outlet_Size == "Medium", 1, 2))] 

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 

# removing categorical variables after label encoding 

combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]


ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 
combi = cbind(combi[,"Item_Identifier"], ohe_df)



#PreProcessing Data

combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]
num_vars = which(sapply(combi, is.numeric)) # index of numeric features 
num_vars_names = names(num_vars) 
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables combi = cbind(combi, combi_numeric_norm)

train = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1):nrow(combi)] 
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset

cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)


#Building MOdel Linear Regration

linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
# preparing dataframe for submission and writing it in a csv file 
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")])
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)

#Regularized Linear Regression
set.seed(1235) my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002)) 
lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales, 
                             method='glmnet',
                             trControl= my_control, tuneGrid = Grid)


set.seed(1236) my_control = trainControl(method="cv", number=5) Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002)) 
ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,      
                             method='glmnet', trControl= my_control, tuneGrid = Grid)



#Random Forest


set.seed(1237) 
my_control = trainControl(method="cv", number=5) # 5-fold CV 
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")


plot(rf_mod)
plot(varImp(rf_mod))

# XGBoost

param_list = list(                objective = "reg:linear",
                                  eta=0.01,      
                                  gamma = 1,    
                                  max_depth=6,   
                                  subsample=0.8,  
                                  colsample_bytree=0.5        )

dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]),
                     label= train$Item_Outlet_Sales) 
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))


set.seed(112) xgbcv = xgb.cv(params = param_list,
                             data = dtrain,    
                             nrounds = 1000,  
                             nfold = 5, 
                             print_every_n = 10,
                             early_stopping_rounds = 30,  
                             maximize = F)

xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 430)

var_imp = xgb.importance(feature_names = setdiff(names(train),
                                                 c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgb_model) xgb.plot.importance(var_imp)




