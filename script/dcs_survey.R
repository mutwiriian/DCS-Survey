library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(showtext)
library(glue)
# 
# font_add_google('Open Sans','open_sans')
showtext_auto(enable = T)
update_geom_defaults('text',list(family='open_sans'))

#Run this if you want to produce higher quality images
showtext_opts(dpi=320)

pct_change <- function(x){
  pct=((x-lag(x))/abs(lag(x)))*100
  return(pct)
}

yoy_pct_change <- function(x){
  yoy=((x-lag(x,12))/abs(lag(x,12)))*100
  return(yoy)
}

date=Sys.Date()
change_type <- 'yoy'#or 'yoy'
change_label <- if_else(change_type=='mom','%MoM','%YoY')

latest_month<- foreign_assets %>% 
  slice_tail(n=1) %>% 
  pull(Date) %>% 
  month(label = T,abbr = F)

year <- foreign_assets %>% 
  slice_tail(n=1) %>% 
  pull(Date) %>% 
  year()


# Foreign Assets ----------------------------------------------------------
foreign_assets<- read_xlsx('data/dcs_survey_main.xlsx',sheet='foreign_assets')

foreign_assets<- foreign_assets %>% 
  unite(Date,Year,Month,sep = '-') %>% 
  mutate(Date=as_date(paste0(Date,'-01')),
         across(matches('nfa'),.fns = ~./(120*1e3)))

nfa_change <- foreign_assets %>% 
  mutate_at(vars(matches('nfa')),
            .funs = list(mom_pct=pct_change,yoy_pct=yoy_pct_change)) %>% 
  select(-(2:4)) %>% 
  pivot_longer(cols = -Date,names_to = 'var',values_to = 'value')

foreign_assets_long<- foreign_assets%>% 
  pivot_longer(cols = -Date,names_to = 'var',values_to = 'value') %>% 
  mutate(colors=case_when(var=='nfa_nbfi'~'#048A18FC',
                          var=='nfa_cbk_gov'~'#118AB2',
                          TRUE~'#910E07')) 

nfa_in_labels=tibble(Date=as_date(c('2014-11-28','2016-2-28','2013-10-28')),
                value=c(0,3.1,5.4),
                label=c('Bank and Non-bank Financial Institutions',
                        'Net Foreign Assets',
                        'Central Bank/Government'),
                colors=c('#048A18FC','#910E07','#118AB2'))

nfa_end_labels<- foreign_assets_long %>% 
  slice_tail(n=3) %>% 
  mutate(label=scales::dollar(value,accuracy = .1,suffix = 'b')) %>% 
  bind_cols(
  nfa_change %>% 
    mutate(var=str_remove(var,'nfa_')) %>% 
    filter(str_detect(var,change_type)) %>% 
    slice_tail(n=3) %>% 
    mutate(label_pct=percent(value,scale = 1))%>% 
    select(-c(Date,var,value))) %>% 
    mutate(label=paste0(label,',\n',label_pct))

foreign_assets_plot<- foreign_assets_long%>% 
  ggplot(aes(Date,value,group=var)) +
  geom_line(aes(color=colors),size=1.5,show.legend = F)+
  geom_text(data = nfa_in_labels,aes(Date,value,label=label,color=colors),
            show.legend = F,inherit.aes = F,size=5)+
  geom_text(data = nfa_end_labels,
            aes(Date+months(3),value,label=label,color=colors),
            size=5,show.legend = F)+
  scale_y_continuous(labels = label_dollar(accuracy = .1,
                                                   suffix = 'b'))+
  scale_color_manual(values=c('#048A18FC','#910E07','#118AB2'))+
  labs(x='',y='',
       title = 'Composition of foreign currency holdings',
       subtitle = glue('Monthly totals to {latest_month},{year} and {change_label}'),
       caption = glue('Source: Central Bank of Kenya,{date}. Chart and calculations by @mutwiriian'))+
  theme_minimal(base_family = 'open_sans')+
  theme(
    axis.text = element_text(size=12),
    plot.caption = element_text(size=12,hjust = -.05,vjust = 2),
    plot.title = element_text(face = 'bold',colour = 'black'),
    plot.subtitle = element_text(size = 12,color = 'grey50'),
    plot.title.position = 'panel'
  )
foreign_assets_plot

ggsave('foreign_assets.jpeg',width = 4000,height = 2248,units = 'px',
       bg = 'white')

# Domestic Credit ---------------------------------------------------------
credit_data <- readxl::read_xlsx('data/dcs_survey_main.xlsx',sheet = 'credit')
credit_data<- credit_data %>% 
  unite(Date,Year,Month,sep ='-') %>% 
  mutate(Date=as_date(paste0(Date,'-01')),
         across(.cols = -Date,.fns=~./(1e3*120)),
         claims_on_public_sector=claims_on_central_govt+claims_on_other_public_sector) %>% 
  select(-c(claims_on_central_govt,claims_on_other_public_sector)) %>% 
  mutate(across(-Date,.fns = list(mom_pct=pct_change,
                                  yoy_pct=yoy_pct_change),.names = '{col}_{.fn}'))

credit_data_raw<- credit_data %>% 
  pivot_longer(cols = -Date) %>% 
  filter(!str_detect(name,'pct'))

credit_data_pct<- credit_data %>% 
  select(Date,matches('pct'))

credit_in_labels <- tibble(Date=c(as_date('2015-01-25')),
                     value=c(7,13,24),
                     name=c('Public sector credit',
                             'Private sector credit',
                             'Total credit'))

credit_end_labels<- credit_data_raw %>% 
  slice_tail(n=3) %>% 
  mutate(label=scales::dollar(round(value,1),
                              accuracy = .1,suffix = 'b')) %>% 
  bind_cols(credit_data_pct %>% 
               pivot_longer(-Date) %>% 
               mutate(name=str_remove(name,pattern = 'claims_on_')) %>%
               filter(str_detect(name,change_type)) %>% 
               slice_tail(n=3) %>% 
               mutate(label_pct=scales::percent(value,scale = 1))  %>% 
               select(-c(Date,value,name))) %>% 
  mutate(label=paste0(label,',\n',label_pct))
         # color=c('#048A18FC','#910E07','#118AB2'))

credit_plot<- credit_data_raw%>% 
  ggplot(aes(Date,value,color=name))+
  geom_line(show.legend = F,size=1.2)+
  geom_text(data = credit_in_labels,
            aes(Date,value,label=name),
            size=5,show.legend = F,inherit.aes = T)+
  geom_text(data = credit_end_labels,
            aes(Date+months(3),value,label=label),
            size=4,show.legend = F,inherit.aes = T)+
   scale_y_continuous(labels = scales::dollar_format(
    accuracy = 1,suffix = 'b'),limits = c(0,45))+
  scale_color_manual(values = rep(c('#048A18FC','#910E07','#118AB2'),2))+
  labs(x='',y='',
       title = 'Domestic credit expansion is primarily driven by public borrowing',
       subtitle = glue('Monthly totals to {latest_month},{year} and {change_label}'),
       caption = glue('Source: Central Bank of Kenya, {date}. Chart and calculations by @mutwiriian'))+
  theme_minimal(base_family = 'open_sans')+
  theme(
    axis.text = element_text(size=14),
    plot.caption = element_text(hjust = -.05,size = 14,vjust = 2),
    plot.title = element_text(face = 'bold',size = 25,colour = 'black',
                              vjust = .5),
    plot.subtitle = element_text(color = 'grey50',size = 14),
    plot.title.position = 'panel'
  )
credit_plot

ggsave('credit.jpeg',width = 4000,height = 2248,units = 'px',bg='white')

# Money Supply ------------------------------------------------------------
money <- readxl::read_xlsx('data/dcs_survey_main.xlsx',sheet = 'money')

money <- money %>% 
  unite(Date,Year,Month,sep = '-') %>% 
  mutate(Date=as_date(paste0(Date,'-01')),
         across(.cols = where(is.numeric),.fns = ~./(1e3*120))) %>% 
  pivot_longer(cols = -Date)

guide_data<- tibble(Date=as_date('2011-08-01'),
       value=seq(36,54,4),name=money$name[1:5],
       label=c('Overall Liquidity, L = M3 + Non-bank holdings of government securities',
               'Extended broad money, M3 = M2 + Resident foreign currency holdings',
               'Broad money, M2 = M1 + Long term money deposits',
               'Narrow money, M1 = M0 + Demand deposits',
               'MO = Currency outside the banking system'))

money_plot<- money %>% 
  ggplot(aes(Date,value,group=name))+
  geom_line(aes(color=name),size=1.2,show.legend = F)+
  geom_text(data = money %>% slice_head(n=5),
            aes(Date-months(3),value,label=name,color=name),size=5,
            show.legend = F)+
  geom_text(data = money %>% slice_tail(n=5) %>% 
              mutate(label=dollar(value,accuracy = .1,suffix = 'b')),
            aes(Date+months(5),value,label=label,color=name),size=5,
            show.legend = F)+
  geom_text(data = guide_data,
            aes(Date,value,label=rev(label),color=rev(name)),
            inherit.aes = F,show.legend = F,hjust=0,size=5)+
  labs(x='',y='',
       title = 'Money supply components',
       subtitle = glue('Monthly totals to {latest_month},{year}'),
       caption = glue('Source: Central Bank of Kenya, {date}. Chart by @mutwiriian'))+
  scale_y_continuous(labels = scales::label_dollar(suffix = 'b'))+
  see::scale_colour_okabeito(reverse = T)+
  theme_bw(base_family = 'open_sans')+
  theme(
    axis.text = element_text(size=14),
    plot.caption = element_text(hjust = .05,size = 14,vjust = 2),
    plot.title = element_text(face = 'bold',size = 25,colour = 'black',
                              vjust = .5),
    plot.subtitle = element_text(color = 'grey50',size = 14),
    plot.title.position = 'panel'
  )
money_plot
  
ggsave('money.jpeg',width = 4000,height = 2248,units = 'px',bg='white')

# Net Domestic Assets -----------------------------------------------------
nda <- read_xlsx('data/dcs_survey_main.xlsx',sheet = 'nda')

nda<- nda %>% 
  unite(Date,Year,Month,sep = '-') %>% 
  mutate(Date=as_date(paste0(Date,'-01')),
    nda=nda/(1e3*120))

nda_plot<- nda %>% 
  #filter(year(Date)>2018) %>% 
  ggplot(aes(Date,nda)) +
  geom_line(color="#048A18FC",size=1.2)+
  geom_text(data = nda %>% slice_tail(n=1),
            aes(Date+months(4),nda,
                label=dollar(nda,accuracy = .1,suffix = 'b')),
                size=5)+
  scale_y_continuous(labels = label_dollar(suffix = 'b'),limits=c(0,40))+
  labs(x='',y='',
       title = 'Net Domestic Assets',
       subtitle = glue('Monthly data to {latest_month},{year}'),
       caption = glue('Source: Central Bank of Kenya, {date}. Chart by @mutwiriian'))+
  theme_minimal(base_family = 'open_sans')+
  theme(
    axis.text = element_text(size=12),
    plot.caption = element_text(hjust = .01,size = 12,vjust = 2),
    plot.title = element_text(face = 'bold',size = 14,vjust = -.1,
                              colour = 'black'),
    plot.subtitle = element_text(color = 'grey50',size = 12,vjust = .1),
    plot.title.position = 'panel'
  )
nda_plot

ggsave('nda.jpeg',width = 4000,height = 2248,units = 'px',bg='white')


# Inflation ---------------------------------------------------------------
cpi_17_21<- read_xlsx('data/cpi_indices_17-21.xlsx')

cpi_17_21<- cpi_17_21 %>% 
  filter(row_number()!=n()) %>% 
  pivot_longer(-Month) %>% 
  unite(Date,Month,name,sep=' ') %>% 
  mutate(Date=my(Date)+days(26)) %>% 
  rename('CPI_index'=value) %>% 
  arrange(Date) 

cpi_21_22 <- read_xlsx('cpi_indices_21-22.xlsx')

cpi_21_22<- cpi_21_22 %>% 
  mutate(Date=my(Month)+days(26),
         CPI_index=`Overall CPI`) %>% 
  select(-Month,-`Overall CPI`) %>% 
  relocate(Date,.before = CPI_index)

cpi_hist <- read_xlsx('cpi_hist.xlsx')

cpi_hist<- cpi_hist %>% 
  mutate(Year=as.numeric(Year)) %>% 
  rename('CPI_index'=Index) %>% 
  filter(Year>2007) %>% 
  unite(Date,Year,Month,sep=' ') %>% 
  mutate(Date=ym(Date)+days(26))

cpi_index <- cpi_hist %>% 
  bind_rows(
    cpi_17_21 %>% 
      filter(Date>'2020-09-27')
  ) %>% 
  bind_rows(
    cpi_21_22 %>% 
      filter(Date>'2021-12-27')
  )

inflation <- cpi_index %>% 
  mutate(mom=pct_change(CPI_index),
         yoy=yoy_pct_change(CPI_index)) %>% 
  filter(year(Date)>2008)

cpi_series <- inflation %>% 
  pull(mom) %>% 
  ts(start = c(2009,1),frequency=12)

seas_adj <- seasonal::seas(cpi_series,
                           transform='none',
                           regression.variables='td',
                           outlier.types=c('ao','tc'),
                           outlier.method='addone')

inflation<- inflation %>% 
  mutate(mom_adj=seasonal::final(seas_adj),
         ma=slider::slide_dbl(.x = mom,.f = mean,
                              .before = 2,.after = 0,
                              .complete = T)) %>% 
  pivot_longer(-Date) %>% 
  mutate(name=factor(name)) %>% 
  filter(year(Date)>2018,name%in%c('mom','mom_adj','ma')) %>% 
  na.omit()

inflation_labels <- tibble(Date=as_date(c('2022-10-14','2022-10-1',
                                          '2022-10-6')),
                           value=c(.4,.95,.63),
                           label_text=c('%MoM','Seasonally\nAdjusted','3-MA'),
                           inflation %>% distinct(name),
                           actual_value=inflation %>% 
                             tail(n=3) %>% 
                             pull(value) %>% 
                             scales::percent(scale = 1,accuracy = .01),
                           label=glue('{label_text},\n{actual_value}'))


inflation_plot <- inflation %>% 
  ggplot(aes(Date,value,color=name))+
  #geom_point(aes(color=name),size=5)+
  geom_line(size=1.5)+
  geom_point(size=4)+
  geom_text(data = inflation_labels,
            aes(Date,value,label=label),size=4.5)+
  scale_x_date(date_breaks = 'year',
               labels = scales::label_date_short())+
  scale_y_continuous(labels = scales::label_percent(scale = 1))+
  see::scale_color_okabeito()+
  labs(x='',y='',
       title = 'Inflation trending lower but inflationary pressures remain high ',
       subtitle = 'Monthly CPI inflation to August, 2022',
       caption = 'Source KNBS. Chart by @mutwiriian              \nSeasonal adjustment by X-13ARIMA-SEATS')+
  theme_minimal(base_family = 'open_sans')+
  theme(
    legend.position = 'none',
    axis.text = element_text(size = 12),
    plot.title = element_text(face = 'bold',size = 16),
    plot.subtitle = element_text(color = 'gray50',size=11),
    plot.caption = element_text(hjust=.05,vjust=5,size=10)
  )
inflation_plot

ggsave('inflation.jpeg',width = 4000,height = 2248,units = 'px',bg='white')






















