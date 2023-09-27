#install.packages("betareg")
#install.packages('caret')
library(betareg)
library(caret)

credit <- read.csv("CREDIT_CLUSTER.csv")
credit1 <- read.csv('CREDIT_CLASS_1_CLUSTER.csv')

credit[credit['경영_위기_비율']==0, '경영_위기_비율'] = rep(0.000001, length(credit[credit['경영_위기_비율']==0, '경영_위기_비율']))
credit[credit['경영_위기_비율']==1, '경영_위기_비율'] = rep(0.999999, length(credit[credit['경영_위기_비율']==1, '경영_위기_비율']))

c0 = credit[credit['클러스터']==0,]
model0 <- betareg(경영_위기_비율 ~ total_stores_count + existing_customers_count
                 + average.age. + average.duration. + average.is_franchise. 
                 + average.business_square_size. + average.monthly_rental_fee.
                 + average.regular_employees_count. + average.rental_deposit.
                 + average.sum_new_customer_cnt. + average.sum_purchase_card.
                 + average.sum_purchase_cash. + average.sum_purchase_invoice.
                 + average.sum_weekend_sales_delivery. + average.sum_weekend_sales_card.
                 + average.sum_sales_card. + average.sum_sales_delivery.
                 + average.sum_sales_invoice.
                 , data= c0)

summary(model0)


c1 = credit[credit['클러스터']==1,]
model1 <- betareg(경영_위기_비율 ~ total_stores_count + existing_customers_count
                  + average.age. + average.duration. + average.is_franchise. 
                  + average.business_square_size. + average.monthly_rental_fee.
                  + average.regular_employees_count. + average.rental_deposit.
                  + average.sum_new_customer_cnt. + average.sum_purchase_card.
                  + average.sum_purchase_cash. + average.sum_purchase_invoice.
                  + average.sum_weekend_sales_delivery. + average.sum_weekend_sales_card.
                  + average.sum_sales_card. + average.sum_sales_delivery.
                  + average.sum_sales_invoice.
                  , data= c1)

summary(model1)

c2 = credit[credit['클러스터']==2,]
model2 <- betareg(경영_위기_비율 ~ total_stores_count + existing_customers_count
                  + average.age. + average.duration. + average.is_franchise. 
                  + average.business_square_size. + average.monthly_rental_fee.
                  + average.regular_employees_count. + average.rental_deposit.
                  + average.sum_new_customer_cnt. + average.sum_purchase_card.
                  + average.sum_purchase_cash. + average.sum_purchase_invoice.
                  + average.sum_weekend_sales_delivery. + average.sum_weekend_sales_card.
                  + average.sum_sales_card. + average.sum_sales_delivery.
                  + average.sum_sales_invoice.
                  , data= c2)

summary(model2)

c3 = credit[credit['클러스터']==3,]
model3 <- betareg(경영_위기_비율 ~ total_stores_count + existing_customers_count
                  + average.age. + average.duration. + average.is_franchise. 
                  + average.business_square_size. + average.monthly_rental_fee.
                  + average.regular_employees_count. + average.rental_deposit.
                  + average.sum_new_customer_cnt. + average.sum_purchase_card.
                  + average.sum_purchase_cash. + average.sum_purchase_invoice.
                  + average.sum_weekend_sales_delivery. + average.sum_weekend_sales_card.
                  + average.sum_sales_card. + average.sum_sales_delivery.
                  + average.sum_sales_invoice.
                  , data= c3)

summary(model3)

####################################################

colnames(credit1)

par(mfrow = c(3, 2))
hist(x=credit1$average.age)
hist(x=credit1$average.duration.)
hist(x=credit1$average.is_franchise.)
hist(x=credit1$average.business_square_size)
hist(x=credit1$average.monthly_rental_fee.)
hist(x=credit1$average.regular_employees_count.)
par(mfrow = c(3, 2))
hist(x=credit1$average.rental_deposit.)
hist(x=credit1$average.sum_customer_cnt.)
hist(x=credit1$average.sum_new_customer_cnt.)
hist(x=credit1$average.sum_purchase_card.)
hist(x=credit1$average.sum_purchase_cash.)
hist(x=credit1$average.sum_purchase_invoice.)
par(mfrow = c(3, 2))
hist(x=credit1$average.sum_sales_card.)
hist(x=credit1$average.sum_sales_delivery.)
hist(x=credit1$average.sum_sales_invoice.)
hist(x=credit1$average.sum_weekend_sales_card.)
hist(x=credit1$average.sum_weekend_sales_delivery.)

credit1[credit1['average.is_risky.']==0, 'average.is_risky.'] = rep(0.000001, length(credit1[credit1['average.is_risky.']==0, 'average.is_risky.']))
credit1[credit1['average.is_risky.']==1, 'average.is_risky.'] = rep(0.999999, length(credit1[credit1['average.is_risky.']==1, 'average.is_risky.']))

c0_s_ = credit1[(credit1['cluster']==0) & (credit1['class_1_name']=='서비스업'),]
c0_s_sub = c0_s_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 
minmax_scaler <- preProcess(c0_s_sub, method='range')
c0_s = predict(minmax_scaler, c0_s_sub)
c0_s['average.is_risky.'] = c0_s_$average.is_risky.

standard_scaler <- preProcess(c0_s_sub, method=c('center', 'scale'))
c0_s_s = predict(standard_scaler, c0_s_sub)
c0_s_s['average.is_risky.'] = c0_s_$average.is_risky.

par(mfrow = c(3, 2))
hist(x=c0_s$average.age)
hist(x=c0_s$average.duration.)
hist(x=c0_s$average.is_franchise.)
hist(x=c0_s$average.business_square_size)
hist(x=c0_s$average.monthly_rental_fee.)
hist(x=c0_s$average.regular_employees_count.)
par(mfrow = c(3, 2))
hist(x=c0_s$average.rental_deposit.)
hist(x=c0_s$average.sum_customer_cnt.)
hist(x=c0_s$average.sum_new_customer_cnt.)
hist(x=c0_s$average.sum_purchase_card.)
hist(x=c0_s$average.sum_purchase_cash.)
hist(x=c0_s$average.sum_purchase_invoice.)
par(mfrow = c(3, 2))
hist(x=c0_s$average.sum_sales_card.)
hist(x=c0_s$average.sum_sales_delivery.)
hist(x=c0_s$average.sum_sales_invoice.)
hist(x=c0_s$average.sum_weekend_sales_card.)
hist(x=c0_s$average.sum_weekend_sales_delivery.)

par(mfrow = c(3, 2))
hist(x=c0_s_s$average.age)
hist(x=c0_s_s$average.duration.)
hist(x=c0_s_s$average.is_franchise.)
hist(x=c0_s_s$average.business_square_size)
hist(x=c0_s_s$average.monthly_rental_fee.)
hist(x=c0_s_s$average.regular_employees_count.)
par(mfrow = c(3, 2))
hist(x=c0_s_s$average.rental_deposit.)
hist(x=c0_s_s$average.sum_customer_cnt.)
hist(x=c0_s_s$average.sum_new_customer_cnt.)
hist(x=c0_s_s$average.sum_purchase_card.)
hist(x=c0_s_s$average.sum_purchase_cash.)
hist(x=c0_s_s$average.sum_purchase_invoice.)
par(mfrow = c(3, 2))
hist(x=c0_s_s$average.sum_sales_card.)
hist(x=c0_s_s$average.sum_sales_delivery.)
hist(x=c0_s_s$average.sum_sales_invoice.)
hist(x=c0_s_s$average.sum_weekend_sales_card.)
hist(x=c0_s_s$average.sum_weekend_sales_delivery.)

c0_u_ = credit1[(credit1['cluster']==0) & (credit1['class_1_name']=='유통업'),]
c0_u_sub = c0_u_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_u0 <- preProcess(c0_u_sub, method='range')
c0_u = predict(minmax_scaler_u0, c0_u_sub)
c0_u['average.is_risky.'] = c0_u_$average.is_risky.

standard_scaler_u0 <- preProcess(c0_u_sub, method=c('center', 'scale'))
c0_u_s = predict(standard_scaler_u0, c0_u_sub)
c0_u_s['average.is_risky.'] = c0_u_$average.is_risky.

c0_e_ = credit1[(credit1['cluster']==0) & (credit1['class_1_name']=='외식업'),]
c0_e_sub = c0_e_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_e0 <- preProcess(c0_e_sub, method='range')
c0_e = predict(minmax_scaler_e0, c0_e_sub)
c0_e['average.is_risky.'] = c0_e_$average.is_risky.

standard_scaler_e0 <- preProcess(c0_e_sub, method=c('center', 'scale'))
c0_e_s = predict(standard_scaler_e0, c0_e_sub)
c0_e_s['average.is_risky.'] = c0_e_$average.is_risky.

model0_s <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                  , data= c0_s)

summary(model0_s)

model0_s_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c0_s_s)

summary(model0_s_)


model0_u <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c0_u)

summary(model0_u)

model0_u_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c0_u_s)

summary(model0_u_)


model0_e <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c0_e)

summary(model0_e)

model0_e_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c0_e_s)

summary(model0_e_)

##

c1_s_ = credit1[(credit1['cluster']==1) & (credit1['class_1_name']=='서비스업'),]
c1_s_sub = c1_s_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_s1 <- preProcess(c1_s_sub, method='range')
c1_s = predict(minmax_scaler_s1, c1_s_sub)
c1_s['average.is_risky.'] = c1_s_$average.is_risky.

standard_scaler_s1 <- preProcess(c1_s_sub, method=c('center', 'scale'))
c1_s_s = predict(standard_scaler_s1, c1_s_sub)
c1_s_s['average.is_risky.'] = c1_s_$average.is_risky.

c1_u_ = credit1[(credit1['cluster']==1) & (credit1['class_1_name']=='유통업'),]
c1_u_sub = c1_u_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_u1 <- preProcess(c1_u_sub, method='range')
c1_u = predict(minmax_scaler_u1, c1_u_sub)
c1_u['average.is_risky.'] = c1_u_$average.is_risky.

standard_scaler_u1 <- preProcess(c1_u_sub, method=c('center', 'scale'))
c1_u_s = predict(standard_scaler_u1, c1_u_sub)
c1_u_s['average.is_risky.'] = c1_u_$average.is_risky.

c1_e_ = credit1[(credit1['cluster']==1) & (credit1['class_1_name']=='외식업'),]
c1_e_sub = c1_e_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_e1 <- preProcess(c1_e_sub, method='range')
c1_e = predict(minmax_scaler_e1, c1_e_sub)
c1_e['average.is_risky.'] = c1_e_$average.is_risky.

standard_scaler_e1 <- preProcess(c1_e_sub, method=c('center', 'scale'))
c1_e_s = predict(standard_scaler_e1, c1_e_sub)
c1_e_s['average.is_risky.'] = c1_e_$average.is_risky.

model1_s <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c1_s)

summary(model1_s)

model1_s_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c1_s_s)

summary(model1_s_)

model1_u <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c1_u)

summary(model1_u)

# 최적화가 안돼서 franchise 빼고 돌림
model1_u_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c1_u_s)

summary(model1_u_)


model1_e <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c1_e)

summary(model1_e)

model1_e_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c1_e_s)

summary(model1_e_)


par(mfrow = c(3, 2))
hist(x=c1_s_s$average.age)
hist(x=c1_s_s$average.duration.)
hist(x=c1_s_s$average.is_franchise.)
hist(x=c1_s_s$average.business_square_size)
hist(x=c1_s_s$average.monthly_rental_fee.)
hist(x=c1_s_s$average.regular_employees_count.)
par(mfrow = c(3, 2))
hist(x=c1_s_s$average.rental_deposit.)
hist(x=c1_s_s$average.sum_customer_cnt.)
hist(x=c1_s_s$average.sum_new_customer_cnt.)
hist(x=c1_s_s$average.sum_purchase_card.)
hist(x=c1_s_s$average.sum_purchase_cash.)
hist(x=c1_s_s$average.sum_purchase_invoice.)
par(mfrow = c(3, 2))
hist(x=c1_s_s$average.sum_sales_card.)
hist(x=c1_s_s$average.sum_sales_delivery.)
hist(x=c1_s_s$average.sum_sales_invoice.)
hist(x=c1_s_s$average.sum_weekend_sales_card.)
hist(x=c1_s_s$average.sum_weekend_sales_delivery.)

##

c2_s_ = credit1[(credit1['cluster']==2) & (credit1['class_1_name']=='서비스업'),]
c2_s_sub = c2_s_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_s2 <- preProcess(c2_s_sub, method='range')
c2_s = predict(minmax_scaler_s2, c2_s_sub)
c2_s['average.is_risky.'] = c2_s_$average.is_risky.

standard_scaler_s2 <- preProcess(c2_s_sub, method=c('center', 'scale'))
c2_s_s = predict(standard_scaler_s2, c2_s_sub)
c2_s_s['average.is_risky.'] = c2_s_$average.is_risky.

c2_u_ = credit1[(credit1['cluster']==2) & (credit1['class_1_name']=='유통업'),]
c2_u_sub = c2_u_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_u2 <- preProcess(c2_u_sub, method='range')
c2_u = predict(minmax_scaler_u2, c2_u_sub)
c2_u['average.is_risky.'] = c2_u_$average.is_risky.

standard_scaler_u2 <- preProcess(c2_u_sub, method=c('center', 'scale'))
c2_u_s = predict(standard_scaler_u2, c2_u_sub)
c2_u_s['average.is_risky.'] = c2_u_$average.is_risky.

c2_e_ = credit1[(credit1['cluster']==2) & (credit1['class_1_name']=='외식업'),]
c2_e_sub = c2_e_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_e2 <- preProcess(c2_e_sub, method='range')
c2_e = predict(minmax_scaler_e2, c2_e_sub)
c2_e['average.is_risky.'] = c2_e_$average.is_risky.

standard_scaler_e2 <- preProcess(c2_e_sub, method=c('center', 'scale'))
c2_e_s = predict(standard_scaler_e2, c2_e_sub)
c2_e_s['average.is_risky.'] = c2_e_$average.is_risky.

model2_s <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c2_s)

summary(model2_s)

model2_s_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c2_s_s)

summary(model2_s_)

model2_u <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c2_u)

summary(model2_u)

model2_u_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c2_u_s)

summary(model2_u_)


model2_e <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c2_e)

summary(model2_e)

model2_e_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c2_e_s)

summary(model2_e_)

##

c3_s_ = credit1[(credit1['cluster']==3) & (credit1['class_1_name']=='서비스업'),]
c3_s_sub = c3_s_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_s3 <- preProcess(c3_s_sub, method='range')
c3_s = predict(minmax_scaler_s3, c3_s_sub)
c3_s['average.is_risky.'] = c3_s_$average.is_risky.

standard_scaler_s3 <- preProcess(c3_s_sub, method=c('center', 'scale'))
c3_s_s = predict(standard_scaler_s3, c3_s_sub)
c3_s_s['average.is_risky.'] = c3_s_$average.is_risky.

c3_u_ = credit1[(credit1['cluster']==3) & (credit1['class_1_name']=='유통업'),]
c3_u_sub = c3_u_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_u3 <- preProcess(c3_u_sub, method='range')
c3_u = predict(minmax_scaler_u3, c3_u_sub)
c3_u['average.is_risky.'] = c3_u_$average.is_risky.

standard_scaler_u3 <- preProcess(c3_u_sub, method=c('center', 'scale'))
c3_u_s = predict(standard_scaler_u3, c3_u_sub)
c3_u_s['average.is_risky.'] = c3_u_$average.is_risky.

c3_e_ = credit1[(credit1['cluster']==3) & (credit1['class_1_name']=='외식업'),]
c3_e_sub = c3_e_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_e3 <- preProcess(c3_e_sub, method='range')
c3_e = predict(minmax_scaler_e3, c3_e_sub)
c3_e['average.is_risky.'] = c3_e_$average.is_risky.

standard_scaler_e3 <- preProcess(c3_e_sub, method=c('center', 'scale'))
c3_e_s = predict(standard_scaler_e3, c3_e_sub)
c3_e_s['average.is_risky.'] = c3_e_$average.is_risky.

model3_s <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c3_s)

summary(model3_s)

model3_s_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c3_s_s)

summary(model3_s_)

model3_u <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c3_u)

summary(model3_u)

model3_u_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c3_u_s)

summary(model3_u_)


model3_e <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c3_e)

summary(model3_e)

model3_e_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c3_e_s)

summary(model3_e_)


par(mfrow = c(3, 2))
hist(x=credit1$average.age, breaks=100)
hist(x=credit1$average.duration., breaks=100)
hist(x=credit1$average.is_franchise., breaks=100)
hist(x=credit1$average.business_square_size, breaks=100)
hist(x=credit1$average.monthly_rental_fee., breaks=100)
hist(x=credit1$average.regular_employees_count., breaks=100)
par(mfrow = c(3, 2))
hist(x=credit1$average.rental_deposit., breaks=100)
hist(x=credit1$average.sum_customer_cnt., breaks=100)
hist(x=credit1$average.sum_new_customer_cnt., breaks=100)
hist(x=credit1$average.sum_purchase_card., breaks=100)
hist(x=credit1$average.sum_purchase_cash., breaks=100)
hist(x=credit1$average.sum_purchase_invoice., breaks=100)
par(mfrow = c(3, 2))
hist(x=credit1$average.sum_sales_card., breaks=100)
hist(x=credit1$average.sum_sales_delivery., breaks=100)
hist(x=credit1$average.sum_sales_invoice., breaks=100)
hist(x=credit1$average.sum_weekend_sales_card., breaks=100)
hist(x=credit1$average.sum_weekend_sales_delivery., breaks=100)
hist(x=credit1$average.is_risky., breaks=100)

hist(x=log1p(credit1$average.is_franchise.), breaks=100)
hist(x=log1p(credit1$average.business_square_size), breaks=100)
hist(x=credit1$average.business_square_size, breaks=100)
hist(x=(standard_scaler(credit1$average.business_square_size)), breaks=100)
hist(x=(standard_scaler(log1p(credit1$average.business_square_size))), breaks=100)
hist(x=(minmax(log1p(credit1$average.business_square_size))), breaks=100)

standard_scaler = function(x){
  return (x-mean(x))/sqrt(var(x))
}


## final transformations
hist(x=(standard_scaler(credit1$average.age)), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.duration))), breaks=20)
hist(x=(standard_scaler(credit1$average.is_franchise.)), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.business_square_size))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.monthly_rental_fee.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.regular_employees_count.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.rental_deposit.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_customer_cnt.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_new_customer_cnt.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_purchase_card.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_purchase_cash.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_purchase_invoice.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_sales_card.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_sales_delivery.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_sales_invoice.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_weekend_sales_card.))), breaks=20)
hist(x=(standard_scaler(log1p(credit1$average.sum_weekend_sales_delivery.))), breaks=20)

##re-fitting

c0_s_ = credit1[(credit1['cluster']==0) & (credit1['class_1_name']=='서비스업'),]
c0_s_sub = c0_s_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 
c0_s_sub_log = c0_s_sub
c0_s_sub_log[,c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,17)] <- log1p(c0_s_sub_log[,c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,17)])
c0_s_sub_log

par(mfrow = c(3, 2))
hist(x=standard_scaler(log1p(c0_s_$average.age)))
hist(x=standard_scaler(log1p(c0_s_$average.duration.)))
hist(x=standard_scaler(log1p(c0_s_$average.is_franchise.)))
hist(x=standard_scaler(log1p(c0_s_$average.business_square_size)))
hist(x=standard_scaler(log1p(c0_s_$average.monthly_rental_fee.)))
hist(x=standard_scaler(log1p(c0_s_$average.regular_employees_count.)))
par(mfrow = c(3, 2))
hist(x=c0_s_$average.rental_deposit.)
hist(x=c0_s_$average.sum_customer_cnt.)
hist(x=c0_s_$average.sum_new_customer_cnt.)
hist(x=c0_s_$average.sum_purchase_card.)
hist(x=c0_s_$average.sum_purchase_cash.)
hist(x=c0_s_$average.sum_purchase_invoice.)
par(mfrow = c(3, 2))
hist(x=c0_s_$average.sum_sales_card.)
hist(x=c0_s_$average.sum_sales_delivery.)
hist(x=c0_s_$average.sum_sales_invoice.)
hist(x=c0_s_$average.sum_weekend_sales_card.)
hist(x=c0_s_$average.sum_weekend_sales_delivery.)

minmax_scaler_s0 <- preProcess(c0_s_sub_log, method='range')
c0_s = predict(minmax_scaler_s0, c0_s_sub_log)
c0_s['average.is_risky.'] = c0_s_$average.is_risky.

standard_scaler_s0 <- preProcess(c0_s_sub_log, method=c('center', 'scale'))
c0_s_s = predict(standard_scaler_s0, c0_s_sub_log)
c0_s_s['average.is_risky.'] = c0_s_$average.is_risky.


model0_s <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c0_s)

summary(model0_s)

model0_s_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c0_s_s)

summary(model0_s_)

## 전체 단위 스케일링

credit_sub = credit1[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)]
credit_sub_log = credit_sub
credit_sub_log[,c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,17)] <- log1p(credit_sub_log[,c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,17)])
credit_sub_log

standard_scaler_all <- preProcess(credit_sub_log, method=c('center', 'scale'))
credit_scaled = predict(standard_scaler_all, credit_sub_log)
credit_scaled['average.is_risky.'] = credit1$average.is_risky.
credit_scaled['cluster'] = credit1$cluster
credit_scaled['class_1_name'] = credit1$class_1_name

minmax_scaler_all <- preProcess(credit_sub_log, method='range')
credit_scaled = predict(minmax_scaler_all, credit_sub_log)
credit_scaled['average.is_risky.'] = credit1$average.is_risky.
credit_scaled['cluster'] = credit1$cluster
credit_scaled['class_1_name'] = credit1$class_1_name

c0_s_scaled = credit_scaled[(credit_scaled['cluster']==0) & (credit_scaled['class_1_name']=='서비스업'),]
model0_s <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_weekend_sales_card. 
                    , data= c0_s_scaled)

summary(model0_s)

c2_e_scaled = credit_scaled[(credit_scaled['cluster']==2) & (credit_scaled['class_1_name']=='외식업'),]
model2_e <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_weekend_sales_card. 
                    , data= c2_e_scaled)

summary(model2_e)

par(mfrow = c(3, 2))
hist(x=credit_scaled$average.age, breaks=100)
hist(x=credit_scaled$average.duration., breaks=100)
hist(x=credit_scaled$average.is_franchise., breaks=100)
hist(x=credit_scaled$average.business_square_size, breaks=100)
hist(x=credit_scaled$average.monthly_rental_fee., breaks=100)
hist(x=credit_scaled$average.regular_employees_count., breaks=100)
par(mfrow = c(3, 2))
hist(x=credit_scaled$average.rental_deposit., breaks=100)
hist(x=credit_scaled$average.sum_customer_cnt., breaks=100)
hist(x=credit_scaled$average.sum_new_customer_cnt., breaks=100)
hist(x=credit_scaled$average.sum_purchase_card., breaks=100)
hist(x=credit_scaled$average.sum_purchase_cash., breaks=100)
hist(x=credit_scaled$average.sum_purchase_invoice., breaks=100)
par(mfrow = c(3, 2))
hist(x=credit_scaled$average.sum_sales_card., breaks=100)
hist(x=credit_scaled$average.sum_sales_delivery., breaks=100)
hist(x=credit_scaled$average.sum_sales_invoice., breaks=100)
hist(x=credit_scaled$average.sum_weekend_sales_card., breaks=100)
hist(x=credit_scaled$average.sum_weekend_sales_delivery., breaks=100)
hist(x=credit_scaled$average.is_risky., breaks=100)

## cluster별 전체

c0_ = credit1[(credit1['cluster']==0),]
c0_sub = c0_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_0 <- preProcess(c0_sub, method='range')
c0 = predict(minmax_scaler_0, c0_sub)
c0['average.is_risky.'] = c0_$average.is_risky.

standard_scaler_0 <- preProcess(c0_sub, method=c('center', 'scale'))
c0_s = predict(standard_scaler_0, c0_sub)
c0_s['average.is_risky.'] = c0_$average.is_risky.

model0 <- betareg(average.is_risky. ~ average.age. + average.duration.
                    + average.is_franchise. + average.business_square_size.
                    + average.monthly_rental_fee.
                    + average.regular_employees_count. + average.rental_deposit.
                    + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                    + average.sum_purchase_card. + average.sum_purchase_cash.
                    + average.sum_purchase_invoice. + average.sum_sales_card.
                    + average.sum_sales_delivery. + average.sum_sales_invoice.
                    + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                    , data= c0)

summary(model0)

model0_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                     + average.is_franchise. + average.business_square_size.
                     + average.monthly_rental_fee.
                     + average.regular_employees_count. + average.rental_deposit.
                     + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                     + average.sum_purchase_card. + average.sum_purchase_cash.
                     + average.sum_purchase_invoice. + average.sum_sales_card.
                     + average.sum_sales_delivery. + average.sum_sales_invoice.
                     + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                     , data= c0_s)

summary(model0_)

c1_ = credit1[(credit1['cluster']==1),]
c1_sub = c1_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_1 <- preProcess(c1_sub, method='range')
c1 = predict(minmax_scaler_1, c1_sub)
c1['average.is_risky.'] = c1_$average.is_risky.

standard_scaler_1 <- preProcess(c1_sub, method=c('center', 'scale'))
c1_s = predict(standard_scaler_1, c1_sub)
c1_s['average.is_risky.'] = c1_$average.is_risky.

model1 <- betareg(average.is_risky. ~ average.age. + average.duration.
                  + average.is_franchise. + average.business_square_size.
                  + average.monthly_rental_fee.
                  + average.regular_employees_count. + average.rental_deposit.
                  + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                  + average.sum_purchase_card. + average.sum_purchase_cash.
                  + average.sum_purchase_invoice. + average.sum_sales_card.
                  + average.sum_sales_delivery. + average.sum_sales_invoice.
                  + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                  , data= c1)

summary(model1)

model1_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                   + average.is_franchise. + average.business_square_size.
                   + average.monthly_rental_fee.
                   + average.regular_employees_count. + average.rental_deposit.
                   + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                   + average.sum_purchase_card. + average.sum_purchase_cash.
                   + average.sum_purchase_invoice. + average.sum_sales_card.
                   + average.sum_sales_delivery. + average.sum_sales_invoice.
                   + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                   , data= c1_s)

summary(model1_)

c2_ = credit1[(credit1['cluster']==2),]
c2_sub = c2_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_2 <- preProcess(c2_sub, method='range')
c2 = predict(minmax_scaler_2, c2_sub)
c2['average.is_risky.'] = c2_$average.is_risky.

standard_scaler_2 <- preProcess(c2_sub, method=c('center', 'scale'))
c2_s = predict(standard_scaler_2, c2_sub)
c2_s['average.is_risky.'] = c2_$average.is_risky.

model2 <- betareg(average.is_risky. ~ average.age. + average.duration.
                  + average.is_franchise. + average.business_square_size.
                  + average.monthly_rental_fee.
                  + average.regular_employees_count. + average.rental_deposit.
                  + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                  + average.sum_purchase_card. + average.sum_purchase_cash.
                  + average.sum_purchase_invoice. + average.sum_sales_card.
                  + average.sum_sales_delivery. + average.sum_sales_invoice.
                  + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                  , data= c2)

summary(model2)

model2_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                   + average.is_franchise. + average.business_square_size.
                   + average.monthly_rental_fee.
                   + average.regular_employees_count. + average.rental_deposit.
                   + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                   + average.sum_purchase_card. + average.sum_purchase_cash.
                   + average.sum_purchase_invoice. + average.sum_sales_card.
                   + average.sum_sales_delivery. + average.sum_sales_invoice.
                   + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                   , data= c2_s)

summary(model2_)

c3_ = credit1[(credit1['cluster']==3),]
c3_sub = c3_[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22)] 

minmax_scaler_3 <- preProcess(c3_sub, method='range')
c3 = predict(minmax_scaler_3, c3_sub)
c3['average.is_risky.'] = c3_$average.is_risky.

standard_scaler_3 <- preProcess(c3_sub, method=c('center', 'scale'))
c3_s = predict(standard_scaler_3, c3_sub)
c3_s['average.is_risky.'] = c3_$average.is_risky.

model3 <- betareg(average.is_risky. ~ average.age. + average.duration.
                  + average.is_franchise. + average.business_square_size.
                  + average.monthly_rental_fee.
                  + average.regular_employees_count. + average.rental_deposit.
                  + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                  + average.sum_purchase_card. + average.sum_purchase_cash.
                  + average.sum_purchase_invoice. + average.sum_sales_card.
                  + average.sum_sales_delivery. + average.sum_sales_invoice.
                  + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                  , data= c3)

summary(model3)

model3_ <- betareg(average.is_risky. ~ average.age. + average.duration.
                   + average.is_franchise. + average.business_square_size.
                   + average.monthly_rental_fee.
                   + average.regular_employees_count. + average.rental_deposit.
                   + average.sum_customer_cnt. + average.sum_new_customer_cnt.
                   + average.sum_purchase_card. + average.sum_purchase_cash.
                   + average.sum_purchase_invoice. + average.sum_sales_card.
                   + average.sum_sales_delivery. + average.sum_sales_invoice.
                   + average.sum_weekend_sales_card. + average.sum_weekend_sales_delivery.
                   , data= c3_s)

summary(model3_)