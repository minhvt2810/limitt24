library(dplyr)
library(sqldf)
library(readxl)
library(openxlsx)
library(tidyr)
library(stringr)
library(stringi)


sodu_quayvong_khongquayvong <- function(sktd,thauchi,thauchiquahan,card,baolanh,lc,delimiter,limit,output_save) {
  #nhập sao kê
  options(scipen = 999)

  data_sktd2 <- read.delim(sktd,sep =delimiter) %>% clean_names()
  data_thauchi2 <- read.delim(thauchi,sep =delimiter) %>% clean_names()
  data_thauchi_quahan2 <- read.delim(thauchiquahan,sep =delimiter) %>% clean_names()
  data_card2 <- read.delim(card,sep =delimiter) %>% clean_names()
  data_md2 <- read.delim(baolanh,sep =delimiter) %>%
    clean_names()##để lại delimiter theo $
  data_lc2 <- read.delim(lc,sep =delimiter) %>% clean_names()

  #tính sao kê tín dụng

  data_sktd_20260429_2 <- data_sktd2 %>%
    select(txn_date,customer,co_code
           #,vn_full_name
           ,limit_refference,ld_id,amount_lcy) %>% distinct() %>%
    mutate(
      limit_refference2= case_when(nchar(limit_refference) == 7 ~ substr(limit_refference,1,4)
                                   ,nchar(limit_refference) == 9 ~ substr(limit_refference,3,6)
      )
      ,loaiduno = case_when(limit_refference2 %in% c("100000","1000","1100","1200"
                                                     ,"1300","1400","1500","1600"
                                                     ,"1700","100","3000","3500"
                                                     ,"4000","4200","4600","4700"
                                                     ,"4800","660000","6000","6100"
                                                     ,"6200","100","6300","6400"
                                                     ,"6500","6600","6700","6800"
                                                     ,"6900","7000","7100","7200"
                                                     ,"7300","7400","7500") ~ "quayvong"
                            ,limit_refference2 %in% c('200000','2000','2100'
                                                      ,'2200','2300','2400','2500'
                                                      ,'2600','2700','3100','3600'
                                                      ,'5000','5100','5200'
                                                      ,'8800000','8000','8100','8200'
                                                      ,'8300','8400','8500'
                                                      ,'8600','8700','8800'
                                                      ,'8900','9000','9100'
                                                      ,'9200','9300','9400','9500',"9700"
                            ) ~ "khongquayvong"
                            ,TRUE ~ "khac"
      )
      ,cate ="vay"
      ,amount_lcy = as.numeric(amount_lcy)
      ,customer = as.character(customer)
    ) %>%
    group_by(cate,customer
             #,vn_full_name
             ,loaiduno) %>%
    summarise(duno = sum(amount_lcy))

  #tính sao kê thấu chi
  data_thauchi_20260429_2 <- data_thauchi2 %>%
    select(mov_date,customer,co_code
           #,account_title_1
           ,so_taikhoan,working_balance) %>%
    mutate(loaiduno = "quayvong"
           # ,working_balance = case_when(working_balance == "null" ~ "0",TRUE ~ working_balance)
           ,duno = as.numeric(working_balance)*-1
           ,cate="thauchi"
           ,customer = as.character(customer)
    ) %>% filter(duno >0) %>%
    group_by(cate,customer
             #,account_title_1
             ,loaiduno) %>%
    summarise(duno=sum(duno))

  # tính sao kê thấu chi quá hạn
  data_thauchi_quahan_20260429_2 <- data_thauchi_quahan2 %>%
    select(txn_date,customer,co_code
           #,vn_full_name
           ,limit_refference,ld_id,amount_lcy) %>%
    mutate(cate ="thauchi"
           # ,amount_lcy = case_when(amount_lcy == "null" ~"0",TRUE ~ amount_lcy)
           ,amount_lcy = as.numeric(amount_lcy)
           ,loaiduno = "quayvong"
           ,customer = as.character(customer)
    ) %>%
    group_by(cate,customer
             #,vn_full_name
             ,loaiduno) %>%
    summarise(duno = sum(amount_lcy))

  #tính sao kê thẻ visa
  data_visa_20260429_2 <- data_card2 %>%
    select(txn_date,cif,branch_code_t24
           #,contract_name
           ,visa_contract,current_bal) %>%
    mutate(cate="visa"
           ,loaiduno = "quayvong"
           # ,current_bal = case_when(current_bal == "null" ~"0",TRUE ~ current_bal)
           ,current_bal = as.numeric(current_bal)*(-1)
           ,cif = as.character(cif)
    ) %>% filter(current_bal>0) %>%
    group_by(cate,cif
             #,contract_name
             ,loaiduno) %>%
    summarise(duno = sum(current_bal))

  #tổng hợp cho vay
  data_vay_quayvong_khongquayvong <- sqldf(
    "with a as (select cate,customer
                                       --,vn_full_name
                                       ,loaiduno,duno from data_sktd_20260429_2
                    union all
                    select cate,customer
                                    ---,account_title_1
                                    ,loaiduno,duno from data_thauchi_20260429_2
                    union all
                    select cate,customer
                                  --,vn_full_name
                                  ,loaiduno,duno from data_thauchi_quahan_20260429_2
                    union all
                    select cate,cif
                            ---,contract_name
                            ,loaiduno,duno from data_visa_20260429_2
                    )
                    select customer
                          --,vn_full_name
                          ,loaiduno
                    ,sum(duno)duno
                    from a group by customer
                        --,vn_full_name
                        ,loaiduno
                    ")

  data_vay_quayvong_khongquayvong_2 <- data_vay_quayvong_khongquayvong %>% filter(customer != "0") %>%
    pivot_wider(names_from = loaiduno,values_from = duno)

  #tính sao kê bảo lãnh
  data_md_20260429_2 <- data_md2 %>% select(txn_date,customer,principal_amount=giatri_con_lai_quydoi
                                            ,currency,limit_reference) %>%
    mutate(customer = as.character(customer)
           # ,principal_amount = case_when(principal_amount == "null" ~"0",TRUE ~ principal_amount)
           ,principal_amount = as.numeric(principal_amount)
           # ,principal_amount = case_when(currency == "USD" ~ principal_amount*26000
           #                   ,currency == "EUR" ~ principal_amount*31000
           #                   ,TRUE ~ principal_amount)
           ,cate="baolanh"
           ,limit_reference= as.character(limit_reference)
           ,limit_refference2= case_when(nchar(limit_reference) == 7 ~ substr(limit_reference,1,4)
                                         ,nchar(limit_reference) == 9 ~ substr(limit_reference,3,6))
           ,loaiduno = case_when(limit_refference2 %in% c("100000","1000","1100","1200"
                                                          ,"1300","1400","1500","1600"
                                                          ,"1700","100","3000","3500"
                                                          ,"4000","4200","4600","4700"
                                                          ,"4800","660000","6000","6100"
                                                          ,"6200","100","6300","6400"
                                                          ,"6500","6600","6700","6800"
                                                          ,"6900","7000","7100","7200"
                                                          ,"7300","7400","7500") ~ "quayvong"
                                 ,limit_refference2 %in% c('200000','2000','2100'
                                                           ,'2200','2300','2400','2500'
                                                           ,'2600','2700','3100','3600'
                                                           ,'5000','5100','5200'
                                                           ,'8800000','8000','8100','8200'
                                                           ,'8300','8400','8500'
                                                           ,'8600','8700','8800'
                                                           ,'8900','9000','9100'
                                                           ,'9200','9300','9400','9500',"9700"
                                 ) ~ "khongquayvong"
                                 ,TRUE ~ "quayvong")
    ) %>%
    group_by(cate,customer,loaiduno) %>%
    summarise(dubaolanh = sum(principal_amount))


  data_baolanh_quayvong_khongquayvong <- data_md_20260429_2 %>%
    pivot_wider(names_from = loaiduno,values_from = dubaolanh)

  #tính sao kê lc
  data_lc_20260429_2 <- data_lc2 %>%
    mutate(customer = as.character(cif_id)
           # ,principal_amount = case_when(principal_amount == "null" ~"0",TRUE ~ principal_amount)
           ,os_lc_amount = as.numeric(os_lc_amount)
           ,os_lc_amount = case_when(contract_ccy == "USD" ~ os_lc_amount*26000
                                     ,contract_ccy == "EUR" ~ os_lc_amount*31000
                                     ,TRUE ~ os_lc_amount)
           ,cate="lc"
           ,credit_line= as.character(credit_line)
           ,limit_refference2= case_when(nchar(credit_line) == 7 ~ substr(credit_line,1,4)
                                         ,nchar(credit_line) == 9 ~ substr(credit_line,3,6))
           ,loaiduno = case_when(limit_refference2 %in% c("100000","1000","1100","1200"
                                                          ,"1300","1400","1500","1600"
                                                          ,"1700","100","3000","3500"
                                                          ,"4000","4200","4600","4700"
                                                          ,"4800","660000","6000","6100"
                                                          ,"6200","100","6300","6400"
                                                          ,"6500","6600","6700","6800"
                                                          ,"6900","7000","7100","7200"
                                                          ,"7300","7400","7500") ~ "quayvong"
                                 ,limit_refference2 %in% c('200000','2000','2100'
                                                           ,'2200','2300','2400','2500'
                                                           ,'2600','2700','3100','3600'
                                                           ,'5000','5100','5200'
                                                           ,'8800000','8000','8100','8200'
                                                           ,'8300','8400','8500'
                                                           ,'8600','8700','8800'
                                                           ,'8900','9000','9100'
                                                           ,'9200','9300','9400','9500',"9700"
                                 ) ~ "khongquayvong"
                                 ,TRUE ~ "quayvong")
    ) %>%
    group_by(cate,customer,loaiduno) %>%
    summarise(dulc = sum(os_lc_amount))

  data_lc_quayvong_khongquayvong <- data_lc_20260429_2 %>% filter(customer !="null") %>%
    pivot_wider(names_from = loaiduno,values_from = dulc)


  #gộp với hạn mức

  gophanmuc <- limit %>% left_join(data_vay_quayvong_khongquayvong_2 %>%
                                          select(customer,duno_khongquayvong=khongquayvong
                                                 ,duno_quayvong=quayvong)
                                        ,by = c("LIABILITY_NUMBER"="customer")) %>%
    left_join(data_baolanh_quayvong_khongquayvong %>%
                select(customer,baolanh_khongquayvong=khongquayvong
                       ,dbaolanh_quayvong=quayvong)
              ,by = c("LIABILITY_NUMBER"="customer")
    ) %>%
    left_join(data_lc_quayvong_khongquayvong %>%
                select(customer,lc_khongquayvong=khongquayvong
                       ,lc_quayvong=quayvong)
              ,by = c("LIABILITY_NUMBER"="customer")) %>%
    select(everything(),-`cate.y`,-`cate.x`,-total_hm_quayvong_conlai,-total_hm_khongquayvong_conlai
           ,-total_hm_conlai)

  # in ra excel
  write.xlsx(gophanmuc,output_save,colNames = TRUE)
}
