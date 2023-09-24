#install.packages("betareg")
library(betareg)

seoul <- read.csv("SEOUL_CLUSTER.csv")
credit <- read.csv("CREDIT_CLUSTER.csv")
credit1 <- read.csv('CREDIT_CLASS_1_CLUSTER.csv')

credit1

# beta regression ????�� ��?? dependent variable�� 0?? 1 ???̷? -> 0?? ?ֵ??? 1?? ?ֵ? ó??
#seoul[seoul['?濵_��??_??��']==0, '?濵_��??_??��'] = rep(0.000001, length(seoul[seoul['?濵_��??_??��']==0, '?濵_��??_??��']))
#seoul[seoul['?濵_��??_??��']==1, '?濵_��??_??��'] = rep(0.999999, length(seoul[seoul['?濵_��??_??��']==1, '?濵_��??_??��']))

#s0 = seoul[seoul['Ŭ??????']==0,]

#model <- betareg(?濵_��??_??�� ~ ????_�� + ???ɴ?_3040_????_??�� + ???ɴ?_5060_????_??�� + ?????ü?_?? 
#                 + ????_???��? + ??_????_?α?_?? + ??_??Ȱ?α?_?? + ??_????_?ҵ?_?ݾ? 
#                 , data = s0)

#summary(model)

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

credit1[credit1['average.is_risky.']==0, 'average.is_risky'] = rep(0.000001, length(credit1[credit1['average.is_risky.']==0, 'average.is_risky.']))
credit1[credit1['average.is_risky.']==1, 'average.is_risky'] = rep(0.999999, length(credit1[credit1['average.is_risky.']==1, 'average.is_risky.']))

c0 = credit[credit['클러스터']==0,]
model0 <- betareg(average.is_risky. ~ total_stores_count + existing_customers_count
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








