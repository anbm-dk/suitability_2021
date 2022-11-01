# Variable importance violin plot

library(tidyverse)
library(extrafont)
# font_import()
loadfonts(device = "win", quiet = TRUE)

wd <- getwd()
setwd('..')
dir <- getwd()
setwd(wd)

imp <- read.table(file = paste0(dir, '/Excel/varimp_figure.csv')
                  , sep = ';'
                  , header = TRUE
                  )

imp$mean <- apply(imp[, 4:ncol(imp)], 1, mean)

i1 <- order(imp$mean)

imp$explanation <- factor(imp$explanation
                          , levels = imp$explanation[i1])

imp <- imp %>% tibble() %>% arrange(desc(mean))

imp <- imp[1:20, ]

imp <- imp %>% pivot_longer(Tablepotatoes:Cucumber
                            , names_to = "crop"
                            , values_to = "imp")

plotf <- imp %>% ggplot(aes(x = explanation
                            , y = imp
                            , fill = Type)) + 
  geom_violin() +
  coord_flip() +
  stat_summary(fun = mean
               , geom = "point"
               , stroke = 1
               , shape = 21
               , size = 1.5
               , color = "black") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman")
        , axis.text.y = element_text(color = 'black')
        , axis.text.x = element_text(color = 'black')) +
  xlab(NULL) +
  ylab('Importance')


tiff(filename = paste0(dir, '/Figures/Figure_imp.tiff')
     , width = 16
     , height = 10
     , units = 'cm'
     , res = 600)
plotf
dev.off()
dev.off()

ggsave(plotf
       , filename = paste0(dir, '/Figures/Figure_imp.pdf')
       , device = cairo_pdf
       , width = 16
       , height = 10
       , units = "cm")

# END