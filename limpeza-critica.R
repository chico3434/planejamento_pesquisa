## PACOTES E FUNÇÕES NECESSÁRIAS
library(tidyverse)
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}



## LEITURA DOS DADOS

data <- readxl::read_xlsx("Inclusão e Educação Financeira(1-216).xlsx")
dados <- data[,-(1:6)]
colnames(dados)



## LIMPEZA DOS DADOS

# fluxo de questionário
for(i in c(13,14,15,16,18,19,20,38,39)) {
  dados[,i] <- dados %>% select(i) %>% mutate_all(coalesce, 'Não informado')
}

for (i in nrow(dados)) {
  if (dados[i,21] == 'Sim' & dados[i,22] == 'Sim') {
    dados[i,26] <- ifelse(is.na(dados[i,26]), 'Não informado', dados[i,26])
    dados[i,28] <- ifelse(is.na(dados[i,28]), 'Não informado', dados[i,28])
  }
  if (dados[i,21] == 'Não' & dados[i,22] == 'Sim') {
    dados[i,32] <- ifelse(is.na(dados[i,32]), 'Não informado', dados[i,32])
  }
  if (dados[i,21] == 'Sim' & dados[i,22] == 'Não') {
    dados[i,35] <- ifelse(is.na(dados[i,35]), 'Não informado', dados[i,35])
  }
}

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
  mutate(municipio=tolower(rm_accent(`Em que município você reside?`))) %>% 
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

# write.csv2(dados, "base_semi_limpa.csv")



## PLANO DE CRÍTICA

dados <- dados %>%
  # ajuste de strings
  mutate(municipio = recode(municipio, 'rio de janeiri' = 'rio de janeiro'),
         municipio = recode(municipio, 'río de janeiro' = 'rio de janeiro'),
         municipio = recode(municipio, 'rio' = 'rio de janeiro'),
         municipio = recode(municipio, 'rj' = 'rio de janeiro'),
         municipio = recode(municipio, 'río de janeiro' = 'rio de janeiro'),
         municipio = recode(municipio, 'guapimirin' = 'guapimirim'),
         municipio = recode(municipio, 'sp' = 'sao paulo'),
  ) %>%
  # plano de crítica
  mutate(idade = ifelse(idade >= 120, 'Não informado', idade),
         num_moradores = ifelse(num_moradores >= 25, 'Não informado', num_moradores),
         sit_trabalho = ifelse(renda == 'Sem trabalho remunerado' &
                                 forma_renda == 'Não recebo rendimentos' &
                                 renda != 'Até R$ 660,00',
                               'Não informado', sit_trabalho),
         sit_trabalho = ifelse(sit_trabalho == 'Estagiário' &
                                 renda %in% c('De R$ 6.600,01 a R$ 13.200,00', 'De R$ 660,01 a R$ 1.320,00', 'Mais de R$ 13.200,01'),
                               'Não informado', sit_trabalho),
         sit_trabalho = ifelse(sit_trabalho == 'Empregado do setor privado, com carteira assinada' &
                                 renda == 'Até R$ 660,00',
                               'Não informado', sit_trabalho),
         forma_renda = ifelse(forma_renda == 'Não recebo rendimentos além do trabalho' &
                                renda != 'Até R$ 660,00',
                              'Não informado', forma_renda),
         razao_ncb = ifelse(razao_ncb == 'Não tenho dinheiro' &
                              renda %in% c('De R$ 6.600,01 a R$ 13.200,00', 'De R$ 660,01 a R$ 1.320,00', 'Mais de R$ 13.200,01'),
                            'Não informado', razao_ncb),
         forma_pag = ifelse(forma_pag == 'Pix' &
                              cb == 'Não',
                            'Não informado', forma_pag),
         tipos_invest = ifelse(tipos_invest == 'Não faço investimentos' &
                                 invest == 'Sim',
                               'Não informado', tipos_invest),
         municipio = ifelse(municipio == 'canpinas' &
                              uf != 'São Paulo (SP)',
                            'Não informado', municipio),
         tipos_invest = ifelse(tipos_invest == 'criptomoedas',
                               'Criptomoedas', tipos_invest))

# remoção de duplicadas
cat('Quantidade de duplicatas:', nrow(dados[duplicated(dados), ]))
dados <- dados[!duplicated(dados),]

# remoção de casos suspeitos
dados[23,]
dados <- dados[-23,]

# write.csv2(dados, "base_limpa.csv")

