## LEITURA DOS DADOS

library(tidyverse)
dados <- readxl::read_xlsx("Inclusão e Educação Financeira(1-216).xlsx")
dados <- dados[,-(1:6)]
colnames(dados)



## LIMPEZA DOS DADOS

# junção de perguntas iguais
dados <- dados %>% replace(is.na(.), "")
dados$forma_renda <- apply(dados[,11:12], 1, paste, collapse = "")
dados$invest <- apply(dados[,22:23], 1, paste, collapse = "")
dados$razao_poupa <- apply(dados[,c(24,33)], 1, paste, collapse = "")
dados$razao_npoupa <- apply(dados[,c(30,36)], 1, paste, collapse = "")
dados$razao_invest <- apply(dados[,c(25,29)], 1, paste, collapse = "")
dados$razao_ninvest <- apply(dados[,c(34,37)], 1, paste, collapse = "")
dados$valor_poupa <- apply(dados[,c(26,35)], 1, paste, collapse = "")
dados$valor_invest <- apply(dados[,c(28,32)], 1, paste, collapse = "")
dados$perfil_invest <- apply(dados[,c(27,31)], 1, paste, collapse = "")

# transformação de variáveis
dados <- dados %>%
  mutate(idade=as.integer(`Qual a sua idade?`)) %>% 
  mutate(sexo=`Qual o seu sexo?`) %>% 
  mutate(cor=`Qual a sua cor/raça?`) %>% 
  mutate(civil=`Qual o seu estado civil?`) %>% 
  mutate(instrucao=`Qual o seu grau de instrução atual?`) %>% 
  mutate(uf=`Em que estado você reside?`) %>% 
  mutate(municipio=`Em que município você reside?`) %>% 
  mutate(area=`Qual situação melhor descreve o local onde você reside?`) %>% 
  mutate(num_moradores=dados[,9] %>% unlist() %>% as.integer()) %>% 
  mutate(sit_trabalho=`Qual a sua situação de trabalho atual?`) %>%
  mutate(renda=`Qual o seu rendimento mensal médio pessoal?`) %>% 
  mutate(forma_pag=`Qual forma de pagamento você mais utiliza?`) %>% 
  mutate(cartao=`Você possui cartão de crédito em seu nome?`) %>% 
  mutate(cb=`Você possui conta bancária?`) %>% 
  mutate(razao_ncb=`Qual a sua principal razão para não possuir conta bancária?`) %>% 
  mutate(razao_cb=`Qual a sua principal razão para possuir conta bancária?`) %>%
  mutate(uso_cb=`Quantos dias você usou sua conta bancária nos últimos 30 dias?`) %>% 
  mutate(tempo_cb=`Há quanto tempo você criou sua primeira conta bancária?`) %>% 
  mutate(poupa=`Você guarda dinheiro na poupança?`) %>% 
  mutate(tipos_invest=`Quais dos investimentos abaixo você faz?`) %>% 
  mutate(reserva=`Se tivesse que usar sua reserva financeira de emergência a partir de hoje, quanto tempo ela duraria?`) %>% 
  select(idade, sexo, cor, civil, instrucao, uf, municipio, area, num_moradores, sit_trabalho, forma_renda,
         renda, forma_pag, cartao, cb, razao_cb, razao_ncb, uso_cb, tempo_cb, poupa, invest, razao_poupa, razao_npoupa, razao_invest, razao_ninvest, valor_poupa, valor_invest, perfil_invest, tipos_invest,
         reserva)

# remoção de duplicadas
dados <- dados[!duplicated(dados), ]
cat('Quantidade de duplicatas:', nrow(dados[duplicated(dados), ]))

# write.csv2(dados, "base_semi_limpa.csv")



## PLANO DE CRÍTICA

dados <- dados %>%
  # ajuste de strings
  mutate(municipio = recode(municipio, 'rio de janeiri' = 'rio de janeiro'),
         municipio = recode(municipio, 'rio' = 'rio de janeiro'),
         municipio = recode(municipio, 'rj' = 'rio de janeiro'),
         municipio = recode(municipio, 'guapimirin' = 'guapimirim'),
         municipio = recode(municipio, 'sp' = 'sao paulo'),
  ) %>%
  # plano de crítica
  mutate(idade = ifelse(idade >= 120, 'ignorado', idade),
         num_moradores = ifelse(num_moradores >= 25, 'ignorado', num_moradores),
         sit_trabalho = ifelse(renda == 'Sem trabalho remunerado' &
                                 forma_renda == 'Não recebo rendimentos' &
                                 renda != 'Até R$ 660,00',
                               'ignorado', renda),
         sit_trabalho = ifelse(sit_trabalho == 'Estagiário' &
                                 renda %in% c('De R$ 6.600,01 a R$ 13.200,00', 'De R$ 660,01 a R$ 1.320,00', 'Mais de R$ 13.200,01'),
                               'ignorado', sit_trabalho),
         sit_trabalho = ifelse(sit_trabalho == 'Empregado do setor privado, com carteira assinada' &
                                 renda == 'Até R$ 660,00',
                               'ignorado', sit_trabalho),
         forma_renda = ifelse(forma_renda == 'Não recebo rendimentos além do trabalho' &
                                renda != 'Até R$ 660,00',
                              'ignorado', forma_renda),
         razao_ncb = ifelse(razao_ncb == 'Não tenho dinheiro' &
                              renda %in% c('De R$ 6.600,01 a R$ 13.200,00', 'De R$ 660,01 a R$ 1.320,00', 'Mais de R$ 13.200,01'),
                            'ignorado', razao_ncb),
         forma_pag = ifelse(forma_pag == 'Pix' &
                              cb == 'Não',
                            'ignorado', forma_pag),
         tipos_invest = ifelse(tipos_invest == 'Não faço investimentos' &
                                 invest == 'Sim',
                         'ignorado', tipos_invest))

cat('Quantidade de imputações:',
    dados %>%
      filter(idade == 'ignorado' | num_moradores == 'ignorado' |
           sit_trabalho == 'ignorado' | forma_renda == 'ignorado' |
           razao_ncb == 'ignorado' | forma_pag == 'ignorado' |
           tipos_invest  == 'ignorado') %>%
      count() %>%
      unlist()
)

# write.csv2(dados, "base_limpa.csv")


# FALTA FAZER
## revisar fluxo de questionário e conferir respostas ignoradas
## verificar casos suspeitos


