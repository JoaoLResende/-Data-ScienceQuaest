library(tidyverse)
library(scales)
library(openxlsx)
library(janitor)
library(broom)
library(ggfx)
library(gt)
library(ggthemes)
library(formattable)
library(grid)
library(gridExtra)



surveyquaest <- read.xlsx("https://github.com/quaest-pesquisa/avaliacao_tecnica/raw/main/datascientistjr/bd_surveyquaest.xlsx") 

##Primeira coisa a se fazer é checar se o dataframe não possui elementos repetidos, para isso iremos utilizar a função duplicated.

surveyquaest %>%
  duplicated()%>%
  sum()
##Dado o resultado que não contém nenhuma coluna verdadeira, temos que não existe repetição na base de dados. A seguir será realizado o sumário da base de dados, com isso temremos a estatística descritiva de toda a base. 

surveyquaest%>%
  summary()

##De acordo com o sumário as idades são divididas em 4 quantis de mesmo tamanho, até 30 anos, de 30 até 43, de 44 até 58 e maior que 58. Logo, para facilitar a criação de uma tabela de contingência entre idade e voto, será criada uma nova coluna com os 4 diferentes quantis.

analise_surveyquaest_contigencia<- surveyquaest%>%
  mutate(idade_agrupada = ifelse(idade <= 30, "<30", ifelse(idade <= 43,"entre 31 e 43", ifelse(idade <= 58 , "entre 43 e 58",ifelse(idade > 58, "Maior que 58", NA)))))%>% 
  select(-idade) %>% 
  rename("Candidato" = voto1,
         "Renda_Familiar" = rendaf,
         "Escolaridade" = esc,
         "Avaliacao_governo" = aval_gov,
  )
##Agora iremos realizar a função para gerar automaticamente as Tabelas de contingência.

resultado = vector('list', 5)
auxiliar_loop<- colnames((analise_surveyquaest_contigencia%>%
                            select(-Candidato,-sbjnum )))


for(i in 1:as.numeric(length(auxiliar_loop))){
  resultado[[i]] <- 
    analise_surveyquaest_contigencia%>%
    tabyl(Candidato, !!as.symbol(auxiliar_loop[i])) %>% 
    adorn_totals(c("col", "row")) %>% 
    adorn_title()
  
}



for (i in 1:length(resultado)) {
  assign(paste0("Tabela_Contigencia_", i ), as.data.frame(resultado[[i]] )%>% row_to_names(row_number = 1) %>% 
           formattable(list(`Indicator Name` = formatter(
             "span", style = ~ style(color = "grey",font.weight = "bold")) 
           )))
}

## código para exportas as tabelas em pdf...
##pdf(file = "Tabelas.pdf")
##lapply(resultado, function(x){
##  grid.newpage()
##  grid.table(x)})

##A seguir veremos como está a distribuição de votos, para entender se existe uma concentração de votos em algum candidato.

analise_surveyquaest_contigencia%>%
  count(Candidato)%>%
  filter(n < 25) %>% 
  count()

##Temos que do total de 16 possíveis grupos de votos, 10 possuem menos que 25 votos, ou 2.5% dos votos totais. Para uma melhor análise gráfica, esses candidatos serão unificados em uma categoria chamada de outros, onde os votos serão somados.

analise_surveyquaest_grafico <- analise_surveyquaest_contigencia%>%
  mutate(Candidato= fct_lump(Candidato,6, other_level = "Outro"))

 analise_surveyquaest_grafico%>%
  group_by(Candidato)%>%
  summarise(count = n())%>%
  mutate(prop = count/sum(count))%>%
  ggplot(aes(x= prop, y = Candidato, fill=Candidato))+
  geom_col()+
  scale_x_continuous(label = percent)+
  scale_y_discrete(limits = c( "Outro","NS/NR","Ninguém/Branco/Nulo","Candidato 8","Candidato 5","Candidato 1",  "Candidato 2" ))+
  labs(title = "Porcentagem de votos de cada candidato",
       subtitle = "",
       x= "Porcentagem de Votos",
       y= "Candidaotos",
       fill = "")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), legend.position = "none")+
  scale_fill_brewer(palette = "Set3")+ geom_vline(xintercept=0.5, linetype="dotted")

##O gráfico mostra a intensão de votos dos 4 candidatos com mais votos, além da proporção de votos brancos e nulos e Não souberam ou não responderam. Além disso, os candidatos que não obtiveram pelo menos 2.5% dos votos totais foram adicionados na categoria Outro.

analise_surveyquaest_grafico_binario <- analise_surveyquaest_grafico%>%
  select(Candidato, Avaliacao_governo)%>%
  mutate(Avaliacao_governo = ifelse(Avaliacao_governo == "Regular positiva", "Positiva", ifelse(Avaliacao_governo == "Boa", "Positiva", ifelse(Avaliacao_governo == "	Ótima", "Positiva","Negativa"))))


analise_surveyquaest_grafico_binario %>%
  count(Avaliacao_governo, Candidato) %>% 
  group_by(Avaliacao_governo) %>% 
  mutate(pct = n/sum(n)) %>% 
  mutate(Candidato = fct_reorder(Candidato, n))%>% 
  ggplot(aes (x = Avaliacao_governo, y = n, fill =Candidato ))+
  geom_col(position = "dodge")+
  scale_y_continuous(breaks=seq(0, 500, 25), labels=abs(seq(0, 500, 25)))+
  coord_flip()+
  labs( title = "Como os eleitores votam em cada candidato?",
        subtitle = "De acordo com a avaliação que fazem do governo",
        x = "Avaliação do governo", 
        y = "Total de votos")+
  guides(fill=guide_legend(title="Candidatos:"))+ 
  geom_text(aes(label = percent(pct)), position = position_dodge(0.9),hjust = -0.1, size =3 )+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  scale_fill_brewer(palette = "Set3")



## O gráfico mostra a quantidade total de votos de cada candidato divididos em dois grupos de acordo como os eleitores avaliam o governo (positivo ou negativo). Além disso, as porcentagens mostram qual a proporção de votos que cada candidato teve em cada um dos grupos. Ou seja, dos eleitores que avaliam o governo de forma positiva, qual a proporção de votos de cada candidato nesse grupo?