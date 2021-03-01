library(tidyverse)
library(geobr)

obitos_totais_anuais <- read_csv("Untitled Explorer - 2_27_21, 10_02 PM_Tab 1_Bar chart.csv")


#populating dataset with  covid cases for municipalities 
casos_covid<- read_delim(gzfile("caso_full.csv.gz"), 
           ",", escape_double = FALSE,  
           trim_ws = TRUE)

glimpse(casos_covid)

#generating dataframe summarized since the first covid case in Brazil
#source: https://agenciabrasil.ebc.com.br/saude/noticia/2021-02/primeiro-caso-de-covid-19-no-brasil-completa-um-ano

casos_covid_trabalho<-
  casos_covid %>%
  filter(place_type == "city",
         date<= "2021-02-26") %>%
  select(date, city_ibge_code, city,state,last_available_deaths,estimated_population ) %>%
  arrange(desc(last_available_deaths)) 




  
#populating dataset with death average for respiratory disesases 
respiratory_deaths <- read_csv("bq-results-20210227-210732-vwu2crfgsqnr.csv") %>%
                       rename(city_ibge_code = id_municipio)

#populating dataset with death average for all kind of causa mortis
all_deaths <- read_csv("bq-results-20210228-100158-b5mov1aikmv7.csv") %>%
  rename(city_ibge_code = id_municipio)


glimpse(respiratory_deaths)

df_trabalho_respiratory<-
casos_covid_trabalho %>%
  inner_join(respiratory_deaths) %>%
  mutate(perc_casos = last_available_deaths/media_anual)
  

df_trabalho_todas<-
  casos_covid_trabalho %>%
  inner_join(all_deaths) %>%
  mutate(perc_casos = last_available_deaths/media_anual_todos_casos)

library(viridis)


####Gráfico que mostra os óbitos totais anuais (melhorar depois a marcação da linha da covid)
obitos_totais_anuais %>%
  ggplot() +
  geom_col(aes(x= ano, y= numero_totais_obitos), fill= "white")+
  geom_line(aes(x= ano,y=249691))+
  theme_light() +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(249691,500000, 1000000))  +
  theme(
    panel.background = element_rect(fill = "black"),
    #panel.grid = element_blank()
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color="black")
  ) +
  labs(
   
    x= "year",
    y = "Number of dead"
    
    
  )


###Jitter da distribuição em relação ao percentual de covid sobre a média dos casos

df_trabalho_todas %>%
  mutate(causa_mortis = "All Deaths",
         perc_casos = perc_casos*100) %>%
  filter(date== "2021-02-26") %>%
  ggplot(aes(x=causa_mortis, y= perc_casos)) +
  geom_jitter(color= "white")  +
  theme_light() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )+
  labs(
    y = "(%) of dead"
  )
  
mid_point <- 
  249691/1282770.6


###ranking dos estados com número de municípios maior ou igual ao do brasil como um todo
df_trabalho_todas %>%
  filter(date== "2021-02-26",
         perc_casos >= mid_point) %>%
  mutate(perc_casos = perc_casos*100,
         state= fct_infreq(state)) %>%
  ggplot(aes(y = fct_rev(state))) +
  geom_bar(fill= "white")+
  theme_light() +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.title.y =  element_blank()
  )



###ranking dos municípios com percentual maior ou igual ao do brasil como um todo
df_trabalho_todas %>%
  
  filter(date== "2021-02-26",
         perc_casos >= mid_point) %>%
  mutate(perc_casos = perc_casos*100) %>%
  slice_max(order_by= perc_casos, n= 10) %>%
  mutate( municipality = str_c(city, " - ", state), 
          municipality= reorder(municipality, perc_casos)) %>% 
  ggplot(aes(x=perc_casos, y = municipality)) +
  geom_col(fill= "white")+
  theme_light() +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )

#evolução da série temporal de casos acumulados. Pegar os picos para fazer análises (colocar as datas chaves)

fab<-
  casos_covid %>%
  filter(place_type == "city",
         date<= "2021-02-26") %>%
  group_by(date) %>%
  summarise(
    total_dia = sum(new_deaths)
  )

fab1<-
  casos_covid%>%
  filter(place_type == "city",
         date== "2020-11-20")



fab2 <-
  casos_covid %>%
  filter(place_type == "city") %>%
  group_by(date) %>%
  group_by(date) %>%
  summarise(
    total_dia = sum(new_deaths)
  )

fab3<- 
  casos_covid %>%
  filter(place_type == "city",
         date<= "2021-02-26" &
           date != "2020-11-20" ) %>%
  group_by(date) %>%
  summarise(
    total_dia = sum(new_deaths)
  )

casos_covid %>%
  filter(place_type == "city",
         date<= "2021-02-26" &
          date != "2020-11-20" ) %>%
  group_by(date) %>%
  summarise(
    total_dia = sum(new_deaths)
  ) %>%
  ggplot()+
  geom_line(aes(x= date,y = total_dia ), color = "white")+
  theme_light() +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )+
  annotate("text", x= as.Date("2020-06-04"), y= 1600, label= "06/04/2020", color = "white")+
  annotate("text", x= as.Date("2020-08-06"), y= 1600, label= "08/06/2020", color = "white")+
  annotate("text", x= as.Date("2021-01-07"), y= 1600, label= "07/01/2021", color = "white") +
  labs(
    y = "Number of dead"
  )
  



library(colorspace)



muni<- read_municipal_seat()

muni_pol<- read_municipality()
states <- read_state(year=2014)

# Baseline 2020-03-17


df_mapa<-
  muni_pol %>%
  left_join(df_trabalho_todas %>%
              rename(code_muni = city_ibge_code) %>%
              filter(date== "2020-03-17")) %>%
  mutate(perc_casos= ifelse(is.na(perc_casos),0,perc_casos),
         perc_casos = perc_casos *100)

df_mapa %>%
  ggplot()+
  #geom_sf(  aes(color=perc_casos), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(  aes(fill=perc_casos), color=NA,  show.legend = TRUE,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=mid_point*100, rev = TRUE)+
  #scale_fill_gradient2( )+
  #scale_fill_gradient2( low= "blue", high= "red", midpoint =mid_point*100)+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( fill="(%) of dead", size=8) +
  theme_minimal()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )




#Gráficos para datas chaves 

#2020-06-04


df_mapa<-
muni_pol %>%
  left_join(df_trabalho_todas %>%
              rename(code_muni = city_ibge_code) %>%
              filter(date== "2020-06-04")) %>%
  mutate(perc_casos= ifelse(is.na(perc_casos),0,perc_casos),
         perc_casos = perc_casos *100)

df_mapa %>%
  ggplot()+
  #geom_sf(  aes(color=perc_casos), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(  aes(fill=perc_casos), color=NA,  show.legend = TRUE,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=mid_point*100, rev = TRUE)+
  #scale_fill_gradient2( )+
  #scale_fill_gradient2( low= "blue", high= "red", midpoint =mid_point*100)+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( fill="(%) of dead", size=8) +
  theme_minimal()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )


###ranking dos municípios com percentual maior ou igual ao do brasil como um todo
df_trabalho_todas %>%
  
  filter(date== "2020-06-04",
         perc_casos >= mid_point) %>%
  mutate(perc_casos = perc_casos*100) %>%
  slice_max(order_by= perc_casos, n= 10) %>%
  mutate( municipality = str_c(city, " - ", state), 
          municipality= reorder(municipality, perc_casos)) %>% 
  ggplot(aes(x=perc_casos, y = municipality)) +
  geom_col(fill= "white")+
  theme_light() +
  theme(
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  ) +
  labs(
    x= "(%) of dead"
  )



#2020-08-06


df_mapa<-
  muni_pol %>%
  left_join(df_trabalho_todas %>%
              rename(code_muni = city_ibge_code) %>%
              filter(date== "2020-08-06")) %>%
  mutate(perc_casos= ifelse(is.na(perc_casos),0,perc_casos),
         perc_casos = perc_casos *100)

df_mapa %>%
  ggplot()+
  #geom_sf(  aes(color=perc_casos), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(  aes(fill=perc_casos), color=NA,  show.legend = TRUE,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=mid_point*100, rev = TRUE)+
  #scale_fill_gradient2( )+
  #scale_fill_gradient2( low= "blue", high= "red", midpoint =mid_point*100)+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( fill="(%) of dead", size=8) +
  theme_minimal()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )


###ranking dos municípios com percentual maior ou igual ao do brasil como um todo
df_trabalho_todas %>%
  
  filter(date== "2020-08-06",
         perc_casos >= mid_point) %>%
  mutate(perc_casos = perc_casos*100) %>%
  slice_max(order_by= perc_casos, n= 10) %>%
  mutate( municipality = str_c(city, " - ", state), 
          municipality= reorder(municipality, perc_casos)) %>% 
  ggplot(aes(x=perc_casos, y = municipality)) +
  geom_col(fill= "white")+
  theme_light() +
  theme(
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )+
  labs(
    x= "(%) of dead"
  )



#2021-01-07


df_mapa<-
  muni_pol %>%
  left_join(df_trabalho_todas %>%
              rename(code_muni = city_ibge_code) %>%
              filter(date== "2021-01-07")) %>%
  mutate(perc_casos= ifelse(is.na(perc_casos),0,perc_casos),
         perc_casos = perc_casos *100)

df_mapa %>%
  ggplot()+
  #geom_sf(  aes(color=perc_casos), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(  aes(fill=perc_casos), color=NA,  show.legend = TRUE,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=mid_point*100, rev = TRUE)+
  #scale_fill_gradient2( )+
  #scale_fill_gradient2( low= "blue", high= "red", midpoint =mid_point*100)+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( fill="(%) of deads", size=8) +
  theme_minimal()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )


###ranking dos municípios com percentual maior ou igual ao do brasil como um todo
df_trabalho_todas %>%
  
  filter(date== "2021-01-07",
         perc_casos >= mid_point) %>%
  mutate(perc_casos = perc_casos*100) %>%
  slice_max(order_by= perc_casos, n= 10) %>%
  mutate( municipality = str_c(city, " - ", state), 
          municipality= reorder(municipality, perc_casos)) %>% 
  ggplot(aes(x=perc_casos, y = municipality)) +
  geom_col(fill= "white")+
  theme_light() +
  theme(
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )+
  labs(
    x= "(%) of dead"
  )



#2021-02-26


df_mapa<-
  muni_pol %>%
  left_join(df_trabalho_todas %>%
              rename(code_muni = city_ibge_code) %>%
              filter(date== "2021-02-26")) %>%
  mutate(perc_casos= ifelse(is.na(perc_casos),0,perc_casos),
         perc_casos = perc_casos *100)

df_mapa %>%
  ggplot()+
  #geom_sf(  aes(color=perc_casos), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(  aes(fill=perc_casos), color=NA,  show.legend = TRUE,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=mid_point*100, rev = TRUE)+
  #scale_fill_gradient2( )+
  #scale_fill_gradient2( low= "blue", high= "red", midpoint =mid_point*100)+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( fill="(%) of dead", size=8) +
  theme_minimal()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )


###ranking dos municípios com percentual maior ou igual ao do brasil como um todo
df_trabalho_todas %>%
  
  filter(date== "2021-02-26",
         perc_casos >= mid_point) %>%
  mutate(perc_casos = perc_casos*100) %>%
  slice_max(order_by= perc_casos, n= 10) %>%
  mutate( municipality = str_c(city, " - ", state), 
          municipality= reorder(municipality, perc_casos)) %>% 
  ggplot(aes(x=perc_casos, y = municipality)) +
  geom_col(fill= "white")+
  theme_light() +
  theme(
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  ) +
  labs(
    x= "(%) of dead"
  )


#### animação

df_trabalho_todas<-
df_trabalho_todas %>%
  mutate(date= as.Date(date))

p<-
  muni %>%
  inner_join(df_trabalho_todas %>%
              rename(code_muni = city_ibge_code) %>%
              filter(date>= "2021-02-20")) %>%
  mutate(perc_casos= ifelse(is.na(perc_casos),0,perc_casos),
         perc_casos = perc_casos *100)%>%
  ggplot()+
  geom_sf(  aes(color=perc_casos), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  #geom_sf(  aes(fill=perc_casos), color=NA,  show.legend = TRUE,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=mid_point*100, rev = TRUE)+
  #scale_fill_gradient2( )+
  #scale_fill_gradient2( low= "blue", high= "red", midpoint =mid_point*100)+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( fill="(%) de mortes", size=8) +
  theme_minimal()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )

library(gganimate)
library(transformr)
library(gifski)
library(png)

anim<- 
  p + transition_time(date)  

n_frames<- NROW(df_mean[df_mean$date>= "2020-03-17",])

anim<-
  anim+
  labs(title = paste("Date: {format(frame_time, '%d/%m/%Y')}"))  

anim_gif<- animate(anim, renderer = gifski_renderer())




anim_save("covid_cities.gif",animation = anim_gif)