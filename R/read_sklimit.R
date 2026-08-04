
## đọc sao kê limit
read_sklimit <- function(filesaoke,delim) {
  options(scipen = 999)
  data_limit <- read.delim(filesaoke,sep = delim) %>% 
    
    mutate(TXN_DATE = format(as.Date(TXN_DATE,format = "%m/%d/%Y"),"%Y%m%d"))
  
  
  y <- data_limit %>% mutate(hm_cha = str_sub(as.character(LIMIT_ID),12,-8)
                             ,hm_con = str_sub(as.character(LIMIT_ID),15,-4)
                             ,ma_hm = str_sub(as.character(LIMIT_ID),20,-1)) %>% 
    select(LIMIT_ID,hm_cha,hm_con,ma_hm,everything())
  ## tạo cus cha
  cus_cha <- y %>% filter(!(hm_cha == "000") & hm_con == "0000") %>% 
    mutate(
      # INTERNAL_AMOUNT = case_when(INTERNAL_AMOUNT == "null" ~ "0",TRUE ~ INTERNAL_AMOUNT),
      INTERNAL_AMOUNT = as.numeric(INTERNAL_AMOUNT),
      # AVAIL_AMT = case_when(AVAIL_AMT == "null" ~ "0",TRUE ~ AVAIL_AMT),
      AVAIL_AMT = as.numeric(AVAIL_AMT)
    ) %>% filter(INTERNAL_AMOUNT >0 & EXPIRY_DATE >= TXN_DATE)
  
  
  cus_cha_fix <- sqldf("select * from (
                     select *,row_number()over(partition by liability_number,hm_cha order by ma_hm desc)stt
                     from cus_cha)x
                     where stt=1
                     ")
  # tạo cus con
  cus_con <- y %>% filter(!(hm_con %in% c("0000","0100","4200"
                                          ,"8000","9700"
                                          ,"5900","5999"))
                          & hm_cha == "000" 
                          # & REDUCING_LIMIT == "Y"
                          # & NOTE != "null"
                          # & APPROVAL_DATE !="20250901"################################## để ý chỗ này
                          & DATE_TIME !="20250830"
  ) %>%  
    mutate(
      # INTERNAL_AMOUNT = case_when(INTERNAL_AMOUNT == "null" ~ "0",TRUE ~ INTERNAL_AMOUNT),
      INTERNAL_AMOUNT = as.numeric(INTERNAL_AMOUNT),
      # ,AVAIL_AMT = case_when(AVAIL_AMT == "null" ~ "0",TRUE ~ AVAIL_AMT),
      AVAIL_AMT = as.numeric(AVAIL_AMT)
    ) %>% filter(INTERNAL_AMOUNT >0 & EXPIRY_DATE >= TXN_DATE)
  
  
  cus_con_fix <- sqldf("select * from (
                     select *,row_number()over(partition by liability_number,hm_con order by ma_hm desc)stt
                     from cus_con)x
                     where stt=1
                     ")
  
  # tạo cus con thẻ
  cus_con_the <- y %>% filter(hm_con %in% c("0100","4200")
                              & !(paste0(LIABILITY_NUMBER,".",hm_cha) %in% 
                                    paste0(as.vector(cus_cha$LIABILITY_NUMBER),".",hm_cha))
                              & (hm_cha == "000")
  ) %>% 
    mutate(
      # INTERNAL_AMOUNT = case_when(INTERNAL_AMOUNT == "null" ~ "0",TRUE ~ INTERNAL_AMOUNT),
      INTERNAL_AMOUNT = as.numeric(INTERNAL_AMOUNT),
      # AVAIL_AMT = case_when(AVAIL_AMT == "null" ~ "0",TRUE ~ AVAIL_AMT),
      AVAIL_AMT = as.numeric(AVAIL_AMT)) %>% 
    filter(INTERNAL_AMOUNT >0 & EXPIRY_DATE >= TXN_DATE)
  
  cus_con_the_fix <- sqldf("select * from (
                     select *,row_number()over(partition by liability_number,hm_con order by ma_hm desc)stt
                     from cus_con_the)x
                     where stt=1
                     ")
  
  # tổng hợp lại các limit
  sum_cus_con_the <- cus_con_the_fix %>% group_by(LIABILITY_NUMBER) %>% 
    summarise(total_hm_quayvong = sum(INTERNAL_AMOUNT)
              ,total_hm_quayvong_conlai = sum(AVAIL_AMT)
    ) %>% 
    mutate(total_hm_khong_quayvong = 0
           ,total_hm_khongquayvong_conlai = 0
    ) %>% 
    select(LIABILITY_NUMBER,total_hm_quayvong,total_hm_khong_quayvong
           ,total_hm_quayvong_conlai,total_hm_khongquayvong_conlai
    )
  
  
  sum_cus_cha <- cus_cha_fix %>% group_by(LIABILITY_NUMBER,hm_cha) %>% 
    summarise(total_hm_quayvong = max(INTERNAL_AMOUNT[REDUCING_LIMIT == "N"],na.rm = TRUE)
              ,total_hm_khong_quayvong = sum(INTERNAL_AMOUNT[REDUCING_LIMIT == "Y"])
              ,total_hm_quayvong_conlai = max(AVAIL_AMT[REDUCING_LIMIT == "N"],na.rm = TRUE)
              ,total_hm_khongquayvong_conlai = sum(AVAIL_AMT[REDUCING_LIMIT == "Y"],na.rm = TRUE)
    ) %>% 
    select(everything()) %>% 
    mutate(total_hm_quayvong 
           = replace(total_hm_quayvong,total_hm_quayvong <0,0)
           ,total_hm_khong_quayvong 
           = replace(total_hm_khong_quayvong,total_hm_khong_quayvong <0,0)
    ) %>% 
    group_by(LIABILITY_NUMBER) %>% 
    summarise(total_hm_quayvong=sum(total_hm_quayvong,na.rm = TRUE)
              ,total_hm_khong_quayvong =sum(total_hm_khong_quayvong,na.rm = TRUE)
              ,total_hm_quayvong_conlai=sum(total_hm_quayvong_conlai,na.rm = TRUE)
              ,total_hm_khongquayvong_conlai=sum(total_hm_khongquayvong_conlai,na.rm = TRUE)
    ) %>% 
    mutate(total_hm_quayvong_conlai = replace(total_hm_quayvong_conlai,total_hm_quayvong_conlai <0,0)
           ,total_hm_khongquayvong_conlai = replace(total_hm_khongquayvong_conlai,total_hm_khongquayvong_conlai <0,0)
    ) %>%
    select(LIABILITY_NUMBER,total_hm_quayvong,total_hm_khong_quayvong
           ,total_hm_quayvong_conlai,total_hm_khongquayvong_conlai
    )
  
  
  
  sum_cus_con <- cus_con_fix %>% group_by(LIABILITY_NUMBER) %>% 
    summarise(total_hm_khong_quayvong = sum(INTERNAL_AMOUNT[REDUCING_LIMIT == "Y"])
              ,total_hm_quayvong = sum(INTERNAL_AMOUNT[REDUCING_LIMIT == "N"])
              ,total_hm_quayvong_conlai = sum(AVAIL_AMT[REDUCING_LIMIT == "N"],na.rm = TRUE)
              ,total_hm_khongquayvong_conlai = sum(AVAIL_AMT[REDUCING_LIMIT == "Y"],na.rm = TRUE)
    ) %>% 
    mutate(total_hm_khongquayvong_conlai = replace(total_hm_khongquayvong_conlai,total_hm_khongquayvong_conlai <0,0)
    ) %>%
    select(LIABILITY_NUMBER,total_hm_quayvong,total_hm_khong_quayvong
           ,total_hm_quayvong_conlai,total_hm_khongquayvong_conlai
    )
  
  
  
  sum_all_kh <- sqldf("select * from sum_cus_cha
                     union all
                     select * from sum_cus_con
                     union all
                     select * from sum_cus_con_the
                    ") %>% group_by(LIABILITY_NUMBER) %>% 
    summarise(total_hm_khong_quayvong = sum(total_hm_khong_quayvong,na.rm = TRUE)
              ,total_hm_quayvong = sum(total_hm_quayvong, na.rm = TRUE)
              ,total_hm_quayvong_conlai=sum(total_hm_quayvong_conlai,na.rm = TRUE)
              ,total_hm_khongquayvong_conlai=sum(total_hm_khongquayvong_conlai,na.rm = TRUE)
    ) %>% mutate(total_hm = total_hm_quayvong+total_hm_khong_quayvong
                 ,total_hm_conlai=total_hm_quayvong_conlai+total_hm_khongquayvong_conlai
    ) %>%
    select(LIABILITY_NUMBER,total_hm_quayvong,total_hm_khong_quayvong,total_hm
           ,total_hm_quayvong_conlai,total_hm_khongquayvong_conlai,total_hm_conlai
    )
  # gọi data limit đã tổng hợp
  sum_all_kh
}


