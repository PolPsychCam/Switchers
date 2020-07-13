#create a barplot with the parties that 2010 voters voted in 2015

library(dplyr)
library(ggplot2)
library(gridExtra)


# ggplot(LabourVotersAndSwitchers2015, aes(as.factor(generalElectionVoteW6))) + 
#   geom_bar(aes(y = (..count..)/sum(..count..)), fill='red', col='black') +
#   scale_y_continuous(labels=scales::percent) +
#   ggtitle("Labour switchers destinations") + 
#   xlab('Party voted in 2015') + ylab('Voters')+
#   theme_classic()

LabBar = ggplot(LabourVotersAndSwitchers2015,
                aes(as.factor(generalElectionVoteW6))) + 
                geom_bar(aes(y = (..count..)/sum(..count..)), fill='red', col='black') +
                scale_y_continuous(labels=scales::percent) +
                scale_x_discrete(labels = c("Con", "Lab", "Libdem", "SNP", "PC", "UKIP", 'Green', 'BNP', 'Other')) +
                ggtitle("Labour switchers destinations") + 
                xlab('Party voted in 2015') + ylab('Voters')+
                theme_classic() 

ConBar = ggplot(ConservativeVotersAndSwitchers2015,
                aes(as.factor(generalElectionVoteW6))) + 
                geom_bar(aes(y = (..count..)/sum(..count..)), fill='blue', col='black') +
                scale_y_continuous(labels=scales::percent) +
                scale_x_discrete(labels = c("Con", "Lab", "Libdem", "SNP", "PC", "UKIP", 'Green', 'BNP', 'Other')) +
                ggtitle("Conservative switchers destinations") + 
                xlab('Party voted in 2015') + ylab('Voters')+
                theme_classic() 

LibBar = ggplot(LibdemVotersAndSwitchers2015,
                aes(as.factor(generalElectionVoteW6))) + 
                geom_bar(aes(y = (..count..)/sum(..count..)), fill='Yellow', col='black') +
                scale_y_continuous(labels=scales::percent) +
                scale_x_discrete(labels = c("Con", "Lab", "Libdem", "SNP", "PC", "UKIP", 'Green', 'BNP', 'Other')) +
                ggtitle("Libdem switchers destinations") + 
                xlab('Party voted in 2015') + ylab('Voters')+
                theme_classic()

SNPBar = ggplot(SNPVotersAndSwitchers2015,
                aes(as.factor(generalElectionVoteW6))) + 
                geom_bar(aes(y = (..count..)/sum(..count..)), fill='Orange', col='black') +
                scale_y_continuous(labels=scales::percent) +
                scale_x_discrete(labels = c("Con", "Lab", "Libdem", "SNP", "PC", "UKIP", 'Green', 'BNP', 'Other')) +
                ggtitle("SNP switchers destinations") + 
                xlab('Party voted in 2015') + ylab('Voters')+
                theme_classic()

UKIPBar = ggplot(UKIPVotersAndSwitchers2015,
                 aes(as.factor(generalElectionVoteW6))) + 
                  geom_bar(aes(y = (..count..)/sum(..count..)), fill='Purple', col='black') +
                  scale_y_continuous(labels=scales::percent) + 
                  scale_x_discrete(labels = c("Con", "Lab", "Libdem", "SNP", "PC", "UKIP", 'Green', 'BNP', 'Other')) +
                  ggtitle("UKIP switchers destinations") + 
                  xlab('Party voted in 2015') + ylab('Voters')+
                  theme_classic()

BNPBar = ggplot(BNPVotersAndSwitchers2015,
                aes(as.factor(generalElectionVoteW6))) + 
                geom_bar(aes(y = (..count..)/sum(..count..)), fill='Black', col='black') +
                scale_y_continuous(labels=scales::percent) +
                scale_x_discrete(labels = c("Con", "Lab", "Libdem", "SNP", "UKIP", 'Green', 'BNP', 'Other')) +
                ggtitle("BNP switchers destinations") + 
                xlab('Party voted in 2015') + ylab('Voters')+
                theme_classic()

png(filename="../Results/All voters and switchers.png", width=2400, height=3000,units = "px", pointsize=12, res=300)

grid.arrange(LabBar, ConBar, LibBar, SNPBar, UKIPBar, BNPBar, nrow = 3,   top = " ")

dev.off()

#OLD template with absolute values:

# SNPBar = ggplot(SNPVotersAndSwitchers2015,
              #   aes(as.factor(generalElectionVoteW6))) + 
              #   geom_bar(fill='orange', col='black') + 
              #   scale_x_discrete(labels = c("Con", "Lab", "Libdem", "SNP", "UKIP", "Other")) +
              #   ggtitle("SNP switchers destinations") + 
              #   xlab('Party voted in 2015') + ylab('Voters')+
              #   theme_classic()