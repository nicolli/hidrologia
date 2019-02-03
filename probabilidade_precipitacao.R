#Geração de uma curva de probabilidade de precipitação

#Autora: Nicolli Albuquerque
#Contato: albuquerquenicolli@hotmail.com

#Fonte de dados: Agência Nacional de Águas - ANA

#0.Instalar e ativar pacotes
    install.packages("ggplot2")
    library(ggplot2)

#1.Aquisição
    #Arquivos da ANA possuem informações dispensáveis
    dados<-read.csv("precipitacao.csv", sep=";", dec = ",", skip= x) #(excluir x linhas do inicio)

#2.Tratamento
    #Colunas
    View(as.data.frame(colnames(dados))) #Interesse: Data e TotalAnual
    #Verificar estrutura geral 
    summary(dados) # x NA para TotalAnual
    str(dados)
    #Quantidade de dados
    nrow(dados) #nrow(dados) - x NA Total Anual = Y dados válidos
    #Criar coluna com Ano
    var <- as.Date(dados$Data,"%d/%m/%Y")
    dados$Ano <- format(var,'%Y')
    #Remover dados duplicados
    dadosp_ano<-dados[!duplicated(dados$Ano), ]
    #Dados de interesse
    dados_int<-as.data.frame(cbind(dadosp_ano$Ano,dadosp_ano$TotalAnual))
    colnames(dados_int)<-c("Ano", "TotalAnual")
    str(dados_int)
    dados_int$Ano<-as.numeric(as.character(dados_int$Ano))
    dados_int$TotalAnual<-as.numeric(as.character(dados_int$TotalAnual))
    #Que anos não há dados para precipitação anual?
    anos_sem<-dados_int[is.na(dados_int$TotalAnual), ]
    anos_sem<-as.data.frame(anos_sem$Ano)
    View(anos_sem)
    #Dados finais
    sdd<-dados_int[dados_int$Ano>= 1900 & dados_int$Ano<= 2000, ] #Inserir a e b anos de interesse
    nrow(sdd)
    #Ordenar pela coluna de Total Anual
    sdd<-sdd[order(sdd$TotalAnual, decreasing=TRUE),]
    #Gerar id 
    sdd$id<-1:nrow(sdd)
    #Probabilidade de ocorrência
    sdd$Probabilidade<-(sdd$id/nrow(sdd))*100
    #Exportar dados finais
    write.table(sdd, file='prob_prec.csv', sep=';', dec='.', row.names=FALSE)
    
#3.Visualização
    graph_prob<-ggplot(sdd, aes(x=Probabilidade, y=TotalAnual)) + geom_line(color="black") + 
      xlab("Probabilidade (%)") + ylab("Precipitação (mm)") + ggtitle("Probabilidade de precipitação - Posto 0000000") +
      theme(plot.title = element_text(color="black", size=14, hjust=0.5),
            axis.title.x = element_text(color="#993333", size=8, face="bold"),
            axis.title.y = element_text(color="#993333", size=8, face="bold"))+
      scale_x_continuous(breaks=seq(0, 100, 5),limits = c(0, 100)) + scale_y_continuous(breaks=seq(0, 1050, 50), limits = c(0, 1050))
    graph_prob
