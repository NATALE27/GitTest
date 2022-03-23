library(tidyverse)
library(readxl)

setwd("/home/arku/Documentos/R/RETC")

# # anotar 2004 y 2005
# read_xlsx("../RETC/retc2004_original.xlsx", sheet = 1, skip = 9, col_names = TRUE) %>%
#   select(NRA, Estado, `Delegación\\Municipio`) %>%
#   filter(Estado == "PUEBLA" | Estado == "TLAXCALA") %>%
#   mutate(Estado = fct_recode(as.factor(Estado), `21` = "PUEBLA", `29` = "TLAXCALA")) %>%
#   right_join(Mpios, by = c("Estado" = "CVE_ENT", "Delegación\\Municipio" = "NOM_MUN")) %>%
#   inner_join(read_xlsx("../RETC/retc2004_original.xlsx", sheet = 2, skip = 9, col_names = TRUE), by = "NRA") %>% 
#   write_csv("../Discard.csv")
#  

# emisiones totales en la cuenca en 2010 ----
# temp <- read_csv("../RETC/retc2010.csv") %>%
#   mutate(Categoria = factor(SUSTANCIA,
#                             levels = c("1,1-Dicloro-1-Fluoroetano (HCFC-141b)", "1,2-Diclorobenceno", "1,4-Diclorobenceno",                   
#                                        "Acetaldehido", "Acroleina",  "Arsénico",                             
#                                        "Arsénico (compuestos)", "Asbesto",  "Benceno",                              
#                                        "Bifenilos policlorados", "Bióxido de carbono",  "Bióxido de nitrógeno",                 
#                                        "Cadmio", "Cadmio (compuestos)",  "Cianuro inorgánico/orgánico",          
#                                        "Cloruro de vinilo", "Cromo (compuestos)",  "Dioxinas",              
#                                        "Estireno (fenil etileno)", "Fenol",  "Formaldehido",
#                                        "Furanos", "Hidrofluorocarbonos",  "Mercurio",
#                                        "Mercurio (compuestos)", "Metano",  "Níquel (compuestos)",
#                                        "Plomo (compuestos)", "Toluen diisocianato"),
#                             labels = c("CFC", "COV", "COV",
#                                        "COV", "COV", "Arsénico",
#                                        "Arsénico", "COP", "COV",
#                                        "COP", "GEI", "GEI",
#                                        "MP", "MP", "Cianuros",
#                                        "COV", "MP", "COP",
#                                        "COV", "COV", "COV",
#                                        "COV", "CFC", "MP",
#                                        "MP", "GEI", "MP",
#                                        "MP", "COV")))
# temp$ESTADO <- str_to_sentence(temp$ESTADO)
# 
Mpios <- read_csv("../Microcuencas/Mun_Microcuencas.csv", col_names = TRUE, col_types = "cccd")
Mpios$CVE_MUN <- str_pad(Mpios$CVE_MUN, width = 3, side = "left", pad = "0")
# 
# {tempG <- Mpios %>%
#   inner_join(temp, by = c("NOM_MUN" = "MUNICIPIO")) %>%
#   select(CVE_ENT:Microcuenca, SUSTANCIA, Categoria, UNIDAD, AIRE, AGUA, SUELO) %>% 
#   filter(UNIDAD == "g") %>% 
#   mutate(UNIDAD = "kg",
#          AIRE = AIRE / 1000,
#          AGUA = AGUA / 1000,
#          SUELO = SUELO / 1000)
# tempTON <- Mpios %>%
#   inner_join(temp, by = c("NOM_MUN" = "MUNICIPIO")) %>%
#   select(CVE_ENT:Microcuenca, SUSTANCIA, Categoria, UNIDAD, AIRE, AGUA, SUELO) %>% 
#   filter(UNIDAD == "ton") %>% 
#   mutate(UNIDAD = "kg",
#          AIRE = AIRE * 1000,
#          AGUA = AGUA * 1000,
#          SUELO = SUELO / 1000)
# tempKG <- Mpios %>%
#   inner_join(temp, by = c("NOM_MUN" = "MUNICIPIO")) %>% 
#   select(CVE_ENT:Microcuenca, SUSTANCIA, Categoria, UNIDAD, AIRE, AGUA, SUELO) %>% 
#   filter(UNIDAD == "kg")
# }
# temp1 <- bind_rows(tempG, tempKG, tempTON)
# 
# # emisiones totales en la cuenca en 2019 ----
# temp <- read_csv("../RETC/retc2019.csv") 
# temp$ESTADO <- str_to_sentence(temp$ESTADO)
# 
# Mpios <- read_csv("../Microcuencas/Mun_Microcuencas.csv") 
# Mpios$CVE_MUN <- str_pad(Mpios$CVE_MUN, width = 3, side = "left", pad = "0")
# 
# #tempG <- 
# Mpios %>% 
#     inner_join(temp, by = c("NOM_MUN" = "MUNICIPIO")) %>%
#     select(Microcuenca, SUSTANCIA, UNIDAD, AIRE, AGUA, SUELO) %>%
#     group_by(Microcuenca, SUSTANCIA) %>%
#     summarise(AIRE = sum(AIRE),
#               AGUA = sum(AGUA),
#               SUELO = sum(SUELO)) %>% 
# 
# write_csv("../Discard.csv")

# emisiones totales en la cuenca en 2006 ----
# temp <- read_csv("../RETC/retc2006.csv") 
# temp$ESTADO <- str_to_sentence(temp$ESTADO)
# 
# Mpios <- read_csv("../Microcuencas/Mun_Microcuencas.csv") 
# Mpios$CVE_MUN <- str_pad(Mpios$CVE_MUN, width = 3, side = "left", pad = "0")

# Ubicación de industrias en la cuenca ----
tempUbicacion <- tibble()
guia <- c("04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")

for(v in guia){
  temp <- read_xlsx("../RETC/retc2015.xlsx", sheet = 1, skip = 9, col_names = TRUE) %>% 
    filter(ESTADO == "PUEBLA" | ESTADO == "TLAXCALA") %>%
    mutate(ESTADO = fct_recode(ESTADO, "21" = "PUEBLA", "29" = "TLAXCALA")) %>% 
    inner_join(Mpios, by = c("ESTADO" = "CVE_ENT", "MUNICIPIO" = "NOM_MUN")) %>%
    mutate(Anio = paste0("20",v))
  tempUbicacion <- bind_rows(tempUbicacion, temp)
}

tempUbicacion %>%
  write_csv("../RETC/IndUbicaciones.csv")


# Emisiones totales en la cuenca ----
Mpios <- read_csv("../Microcuencas/Mun_Microcuencas.csv", col_names = TRUE, col_types = "cccc") 
Mpios$CVE_MUN <- str_pad(Mpios$CVE_MUN, width = 3, side = "left", pad = "0")

guia <- c("04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")
tempETC <- tibble("CVE_ENT" = character(), "CVE_MUN" = character(), "NOM_MUN" = character(), 
                  "Microcuenca" = character(), "NRA" = character(), "SECTOR" = character(), "SUSTANCIA" = character(), 
                  "UNIDAD" = character(), "AIRE" = double(), "AGUA" = double(), "SUELO" = double(), 
                  "DISPOSICIÓN FINAL" = double(), "ALCANTARILLADO" = double(), "INCINERACIÓN" = double())

for(v in guia) {
  temp <- read_xlsx(paste0("./retc20",v,".xlsx"), sheet = 2, col_names = TRUE, skip = 9, na = "NA") %>%
    filter(ESTADO == "PUEBLA" | ESTADO == "TLAXCALA") %>%
    mutate(ESTADO = fct_recode(as.factor(ESTADO), `21` = "PUEBLA", `29` = "TLAXCALA"))
  
  tempG <- Mpios %>%
    inner_join(temp, by = c("CVE_ENT" = "ESTADO", "NOM_MUN" = "MUNICIPIO")) %>%
    select(CVE_ENT:NRA, SECTOR, SUSTANCIA, UNIDAD, AIRE, AGUA, SUELO, `DISPOSICIÓN FINAL`, ALCANTARILLADO, INCINERACIÓN) %>% 
    filter(UNIDAD == "g" | UNIDAD == "g/año") %>% 
    mutate(UNIDAD = "kg",
           AIRE = AIRE / 1000,
           AGUA = AGUA / 1000,
           SUELO = SUELO / 1000)
  tempTON <- Mpios %>%
    inner_join(temp, by = c("CVE_ENT" = "ESTADO", "NOM_MUN" = "MUNICIPIO")) %>%
    select(CVE_ENT:NRA, SECTOR, SUSTANCIA, UNIDAD, AIRE, AGUA, SUELO, `DISPOSICIÓN FINAL`, ALCANTARILLADO, INCINERACIÓN) %>% 
    filter(UNIDAD == "ton" | UNIDAD == "ton/año") %>% 
    mutate(UNIDAD = "kg",
           AIRE = AIRE * 1000,
           AGUA = AGUA * 1000,
           SUELO = SUELO * 1000)
  tempKG <- Mpios %>%
    inner_join(temp, by = c("CVE_ENT" = "ESTADO", "NOM_MUN" = "MUNICIPIO")) %>%
    select(CVE_ENT:NRA, SECTOR, SUSTANCIA, UNIDAD, AIRE, AGUA, SUELO, `DISPOSICIÓN FINAL`, ALCANTARILLADO, INCINERACIÓN) %>% 
    filter(UNIDAD == "kg" | UNIDAD == "Kg" | UNIDAD == "kg/año") %>%
    mutate(UNIDAD = "kg")
  
  temp <- bind_rows(tempG, tempKG, tempTON) %>%
    mutate(Anio = paste0("20",v)) %>%
    replace_na(list(AGUA = 0, AIRE = 0, SUELO = 0, `DISPOSICIÓN FINAL` = 0, ALCANTARILLADO = 0, INCINERACIÓN = 0))
    
  tempETC <- bind_rows(tempETC, temp)
  
}

tempETC %>%
  mutate(SUSTANCIA = factor(SUSTANCIA, 
                            levels = c("1,1-Dicloro-1-Fluoroetano (HCFC-141b)",        "1,2-Diclorobenceno"                          ,
                                        "1,4-Diclorobenceno",                           "Acetaldehido"                                ,
                                        "Acido 2,4 Diclorofenoxiacetico",               "Acido sulfhídrico"                           ,
                                        "Acrilonitrilo",                                "Acroleina"                                   ,
                                        "Arsénico",                                     "Arsénico (compuestos)"                       ,
                                        "Asbesto",                                      "Benceno"                                     ,
                                        "Bifenilo",                                     "Bifenilos policlorados"                      ,
                                        "Bióxido de carbono",                           "Bióxido de nitrógeno"                        ,
                                        "Cadmio",                                       "Cadmio (compuestos)"                         ,
                                        "Cianuro inorgánico/orgánico",                  "Clorodifluorometano (HCFC-22)"               ,
                                        "Cloruro de metileno",                          "Cloruro de vinilo"                           ,
                                        "Cromo (compuestos)",                           "Cromo (polvos respirables, humos o vapores)" ,
                                        "Dioxinas",                                     "Estireno (fenil etileno)"                    ,
                                        "Fenol",                                        "Formaldehido"                                ,
                                        "Furanos",                                      "Hidrofluorocarbonos"                         ,
                                        "Mercurio",                                     "Mercurio (compuestos)"                       ,
                                        "Metano",                                       "Metileno bis(fenilisocianato)"               ,
                                        "Níquel (compuestos)",                          "Níquel (polvos respirables, humos o vapores)",
                                        "Oxido nitroso",                                "Piridina"                                    ,
                                        "Plomo (compuestos)",                           "Plomo (polvos respirables, humos o vapores)" ,
                                        "Toluen diisocianato",                          "Tolueno"                                     ,
                                        "Xileno (mezcla de isómeros)"  ),
                            labels = c("1,1-Dicloro-1-Fluoroetano (HCFC-141b)",        "1,2-Diclorobenceno"                          ,
                                       "1,4-Diclorobenceno",                           "Acetaldehido"                                ,
                                       "Acido 2,4 Diclorofenoxiacetico",               "Acido sulfhídrico"                           ,
                                       "Acrilonitrilo",                                "Acroleina"                                   ,
                                       "Arsénico",                                     "Arsénico"                                    ,
                                       "Asbesto",                                      "Benceno"                                     ,
                                       "Bifenilo",                                     "Bifenilos policlorados"                      ,
                                       "Bióxido de carbono",                           "Bióxido de nitrógeno"                        ,
                                       "Cadmio",                                       "Cadmio"                                      ,
                                       "Cianuro inorgánico/orgánico",                  "Clorodifluorometano (HCFC-22)"               ,
                                       "Cloruro de metileno",                          "Cloruro de vinilo"                           ,
                                       "Cromo",                                        "Cromo"                                       ,
                                       "Dioxinas",                                     "Estireno (fenil etileno)"                    ,
                                       "Fenol",                                        "Formaldehido"                                ,
                                       "Furanos",                                      "Hidrofluorocarbonos"                         ,
                                       "Mercurio",                                     "Mercurio"                                    ,
                                       "Metano",                                       "Metileno bis(fenilisocianato)"               ,
                                       "Níquel",                                       "Níquel"                                      , 
                                       "Oxido nitroso",                                "Piridina"                                    ,
                                       "Plomo",                                        "Plomo"                                       ,
                                       "Toluen diisocianato",                          "Tolueno"                                     ,
                                       "Xileno (mezcla de isómeros)")),
         Categoria = factor(SUSTANCIA,
                            levels = c("1,1-Dicloro-1-Fluoroetano (HCFC-141b)" ,"1,2-Diclorobenceno"                    ,"1,4-Diclorobenceno"                   ,
                                       "Acetaldehido"                          ,"Acido 2,4 Diclorofenoxiacetico"        ,"Acido sulfhídrico"                    ,
                                       "Acrilonitrilo"                         ,"Acroleina"                             ,"Arsénico"                             ,
                                       "Asbesto"                               ,"Benceno"                               ,"Bifenilo"                             ,
                                       "Bifenilos policlorados"                ,"Bióxido de carbono"                    ,"Bióxido de nitrógeno"                 ,
                                       "Cadmio"                                ,"Cianuro inorgánico/orgánico"           ,"Clorodifluorometano (HCFC-22)"        ,
                                       "Cloruro de metileno"                   ,"Cloruro de vinilo"                     ,"Cromo"                                ,
                                       "Dioxinas"                              ,"Estireno (fenil etileno)"              ,"Fenol"                                ,
                                       "Formaldehido"                          ,"Furanos"                               ,"Hidrofluorocarbonos"                  ,
                                       "Mercurio"                              ,"Metano"                                ,"Metileno bis(fenilisocianato)"        ,
                                       "Níquel"                                ,"Oxido nitroso"                         ,"Piridina"                             ,
                                       "Plomo"                                 ,"Toluen diisocianato"                   ,"Tolueno"                              ,
                                       "Xileno (mezcla de isómeros)"),
                            labels = c("CFC", "COV", "COV",
                                       "COV", "Ácido", "Ácido",
                                       "COV", "COV", "Arsénico",
                                       "COP", "COV", "COP",
                                       "COP", "GEI", "GEI",
                                       "MP", "Cianuro", "CFC",
                                       "COV", "COV", "MP",
                                       "COP", "COV", "COV",
                                       "COV", "COP", "CFC",
                                       "MP", "GEI", "COV",
                                       "MP", "GEI", "COV",
                                       "MP", "COV", "COV",
                                       "COV")),
         Anio = as.numeric(Anio)) %>%
  select(Anio, CVE_ENT:NRA, SECTOR, Categoria, SUSTANCIA:INCINERACIÓN) %>% 
  # group_by(Anio, CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, SECTOR, Categoria, SUSTANCIA, UNIDAD) %>%
  # summarise(AIRE = sum(AIRE),
  #           AGUA = sum(AGUA),
  #           SUELO = sum(SUELO),
  #           `DISPOSICIÓN FINAL` = sum(`DISPOSICIÓN FINAL`),
  #           ALCANTARILLADO = sum(ALCANTARILLADO),
  #           INCINERACIÓN = sum(INCINERACIÓN))
  write_csv("../RETC/retc0419.csv")

# preparación de tablas de datos para diferentes compartimentos ----

read_csv("../RETC/retc0419.csv", col_names = TRUE, col_types = "dcccccccccdddddd") %>% 
  group_by(CVE_ENT, CVE_MUN, NOM_MUN, SUSTANCIA) %>%
  summarise(AIRE = median(AIRE),
            AGUA = median(AGUA),
            SUELO = median(SUELO),
            DISPOFINAL = median(`DISPOSICIÓN FINAL`),
            ALCANTARILLADO = median(ALCANTARILLADO),
            INCINERACIÓN = median(INCINERACIÓN)) %>%
  pivot_wider(names_from = SUSTANCIA, 
              names_sort = TRUE, 
              values_from = c(AIRE, AGUA, SUELO, DISPOFINAL, ALCANTARILLADO, INCINERACIÓN), 
              values_fill = 0) %>%
  ungroup() %>%
  right_join(Mpios) %>% 
  arrange(NOM_MUN) %>%
  write_csv("../RETC/EmisionesMun.csv")

read_csv("../RETC/EmisionesMun.csv")
# preparación de tablas de datos para diferentes compartimentos por grupo de contaminantes ----

read_csv("../RETC/retc0419.csv", col_names = TRUE, col_types = "dcccccccccdddddd") %>% 
  group_by(CVE_ENT, CVE_MUN, NOM_MUN, Categoria, SUSTANCIA) %>%
  summarise(AIRE = median(AIRE),
            AGUA = median(AGUA),
            SUELO = median(SUELO),
            DISPOFINAL = median(`DISPOSICIÓN FINAL`),
            ALCANTARILLADO = median(ALCANTARILLADO),
            INCINERACIÓN = median(INCINERACIÓN)) %>%
  group_by(CVE_ENT, CVE_MUN, NOM_MUN, Categoria) %>%
  summarise(AIRE = sum(AIRE),
            AGUA = sum(AGUA),
            SUELO = sum(SUELO),
            DISPOFINAL = sum(DISPOFINAL),
            ALCANTARILLADO = sum(ALCANTARILLADO),
            INCINERACIÓN = sum(INCINERACIÓN)) %>%
  pivot_wider(names_from = Categoria, 
              names_sort = TRUE, 
              values_from = c(AIRE, AGUA, SUELO, DISPOFINAL, ALCANTARILLADO, INCINERACIÓN), 
              values_fill = 0) %>%
  ungroup() %>%
  right_join(Mpios) %>% 
  arrange(NOM_MUN) %>%
  write_csv("../RETC/EmisionesCatMun.csv")

# # 
# temp <- read_csv("../RETC/retc0419.csv", col_names = TRUE, col_types = "dccccffccddd") %>%
#   mutate(SECTOR = fct_recode(SECTOR,`Vidrio` = "vidrio"),
#          SECTOR = fct_recode(SECTOR, 
#                              `Generación de energía eléctrica` = "Generacion de energia electrica", 
#                              `Generación de energía eléctrica` = "Generación de energia electrica"))
# levels(temp$SECTOR)
# temp %>%
#   filter(SUSTANCIA == "1,2-Diclorobenceno" | SUSTANCIA == "1,4-Diclorobenceno" | SUSTANCIA == "Benceno" | 
#            SUSTANCIA == "Bióxido de nitrógeno" | SUSTANCIA == "Cadmio" | SUSTANCIA == "Cadmio (compuestos)" | 
#            SUSTANCIA == "Cromo" | SUSTANCIA == "Cromo (polvos respirables, humos o vapores)" | SUSTANCIA == "Mercurio" | 
#            SUSTANCIA == "Mercurio (compuestos)" | SUSTANCIA == "Níquel (compuestos)" | SUSTANCIA == "Níquel (polvos respirables, humos o vapores)" | 
#            SUSTANCIA == "Oxido nitroso" | SUSTANCIA == "Plomo (compuestos)" | SUSTANCIA == "Plomo (polvos respirables, humos o vapores)") %>%
#   select(1:10) %>% 
#   pivot_wider(names_from = SUSTANCIA, values_from = AIRE, values_fill = 0)


# determinación de contaminantes por tipo de industria ----
# 
# read_xlsx(paste0("./retc20",v,".xlsx"), sheet = 1, col_names = TRUE, skip = 9) %>%
#   filter(ESTADO == "PUEBLA" | ESTADO == "TLAXCALA") %>%
#   select(1, 3, 4) %>%
#   right_join(read_xlsx(paste0("./retc20",v,".xlsx"), sheet = 2, col_names = TRUE, skip = 9) %>%
#                filter(ESTADO == "PUEBLA" | ESTADO == "TLAXCALA")) %>%
#   group_by(SCIAN, `DESCRIPCION SCIAN`, SUSTANCIA) %>% 
#   summarise(AireMn = mean(AIRE),
#             AireMd = median(AIRE)) %>% 
#   mutate(AireLg = as.logical(AireMn)) 

Mpios <- read_csv("../Microcuencas/Mun_Microcuencas.csv", col_names = TRUE, col_types = "cccc") 
Mpios$CVE_MUN <- str_pad(Mpios$CVE_MUN, width = 3, side = "left", pad = "0")

guia <- c("06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")
tempETC <- tibble("SCIAN" = character(), "DESCRIPCION SCIAN" = character(), "SUSTANCIA" = character(),
                  "AireMn" = double(), "AguaMn" = double(), "SueloMn" = double(), "DispoMn" = double(), 
                  "AlcanMn" = double(), "IncinMn" = double(), "Anio" = character())

for(v in guia) {
  temp <- read_xlsx(paste0("./retc20",v,".xlsx"), sheet = 1, col_names = TRUE, skip = 9) %>% 
    mutate(SCIAN = as.character(SCIAN)) %>%
    filter(ESTADO == "PUEBLA" | ESTADO == "TLAXCALA") %>%
    select(1, 3, 4) %>%
    right_join(read_xlsx(paste0("./retc20",v,".xlsx"), sheet = 2, col_names = TRUE, skip = 9) %>%
                 filter(ESTADO == "PUEBLA" | ESTADO == "TLAXCALA")) %>%
    mutate(ESTADO = fct_recode(as.factor(ESTADO), `21` = "PUEBLA", `29` = "TLAXCALA"))
  
  tempG <- Mpios %>%
    inner_join(temp, by = c("CVE_ENT" = "ESTADO", "NOM_MUN" = "MUNICIPIO")) %>%
    filter(UNIDAD == "g" | UNIDAD == "g/año") %>% 
    mutate(UNIDAD = "kg",
           AIRE = AIRE / 1000,
           AGUA = AGUA / 1000,
           SUELO = SUELO / 1000,
           `DISPOSICIÓN FINAL` = `DISPOSICIÓN FINAL` / 1000,
           ALCANTARILLADO = ALCANTARILLADO / 1000,
           INCINERACIÓN = INCINERACIÓN / 1000) #%>%
    # group_by(SCIAN, `DESCRIPCION SCIAN`, SUSTANCIA) %>% 
    # summarise(AireMn = mean(AIRE),
    #           AguaMn = mean(AGUA),
    #           SueloMn = mean(SUELO),
    #           DispoMn = mean(`DISPOSICIÓN FINAL`),
    #           AlcanMn = mean(ALCANTARILLADO),
    #           IncinMn = mean(INCINERACIÓN))
  tempTON <- Mpios %>%
    inner_join(temp, by = c("CVE_ENT" = "ESTADO", "NOM_MUN" = "MUNICIPIO")) %>%
    filter(UNIDAD == "ton" | UNIDAD == "ton/año") %>% 
    mutate(UNIDAD = "kg",
           AIRE = AIRE * 1000,
           AGUA = AGUA * 1000,
           SUELO = SUELO * 1000,
           `DISPOSICIÓN FINAL` = `DISPOSICIÓN FINAL` * 1000,
           ALCANTARILLADO = ALCANTARILLADO * 1000,
           INCINERACIÓN = INCINERACIÓN * 1000) #%>%
    # group_by(SCIAN, `DESCRIPCION SCIAN`, SUSTANCIA) %>% 
    # summarise(AireMn = mean(AIRE),
    #           AguaMn = mean(AGUA),
    #           SueloMn = mean(SUELO),
    #           DispoMn = mean(`DISPOSICIÓN FINAL`),
    #           AlcanMn = mean(ALCANTARILLADO),
    #           IncinMn = mean(INCINERACIÓN))
  tempKG <- Mpios %>%
    inner_join(temp, by = c("CVE_ENT" = "ESTADO", "NOM_MUN" = "MUNICIPIO")) %>%
    filter(UNIDAD == "kg" | UNIDAD == "Kg" | UNIDAD == "kg/año") %>%
    mutate(UNIDAD = "kg") #%>%
    # group_by(SCIAN, `DESCRIPCION SCIAN`, SUSTANCIA) %>% 
    # summarise(AireMn = mean(AIRE),
    #           AguaMn = mean(AGUA),
    #           SueloMn = mean(SUELO),
    #           DispoMn = mean(`DISPOSICIÓN FINAL`),
    #           AlcanMn = mean(ALCANTARILLADO),
    #           IncinMn = mean(INCINERACIÓN))
  
  temp <- bind_rows(tempG, tempKG, tempTON) %>%
    mutate(Anio = paste0("20",v)) %>%
    replace_na(list(AGUA = 0, AIRE = 0, SUELO = 0, `DISPOSICIÓN FINAL` = 0, ALCANTARILLADO = 0, INCINERACIÓN = 0)) 

  tempETC <- bind_rows(tempETC, temp)
  
}

tempETC %>%
  mutate(SUSTANCIA = factor(SUSTANCIA, 
                            levels = c("1,1-Dicloro-1-Fluoroetano (HCFC-141b)",        "1,2-Diclorobenceno"                          ,
                                       "1,4-Diclorobenceno",                           "Acetaldehido"                                ,
                                       "Acido 2,4 Diclorofenoxiacetico",               "Acido sulfhídrico"                           ,
                                       "Acrilonitrilo",                                "Acroleina"                                   ,
                                       "Arsénico",                                     "Arsénico (compuestos)"                       ,
                                       "Asbesto",                                      "Benceno"                                     ,
                                       "Bifenilo",                                     "Bifenilos policlorados"                      ,
                                       "Bióxido de carbono",                           "Bióxido de nitrógeno"                        ,
                                       "Cadmio",                                       "Cadmio (compuestos)"                         ,
                                       "Cianuro inorgánico/orgánico",                  "Clorodifluorometano (HCFC-22)"               ,
                                       "Cloruro de metileno",                          "Cloruro de vinilo"                           ,
                                       "Cromo (compuestos)",                           "Cromo (polvos respirables, humos o vapores)" ,
                                       "Dioxinas",                                     "Estireno (fenil etileno)"                    ,
                                       "Fenol",                                        "Formaldehido"                                ,
                                       "Furanos",                                      "Hidrofluorocarbonos"                         ,
                                       "Mercurio",                                     "Mercurio (compuestos)"                       ,
                                       "Metano",                                       "Metileno bis(fenilisocianato)"               ,
                                       "Níquel (compuestos)",                          "Níquel (polvos respirables, humos o vapores)",
                                       "Oxido nitroso",                                "Piridina"                                    ,
                                       "Plomo (compuestos)",                           "Plomo (polvos respirables, humos o vapores)" ,
                                       "Toluen diisocianato",                          "Tolueno"                                     ,
                                       "Xileno (mezcla de isómeros)"  ),
                            labels = c("1,1-Dicloro-1-Fluoroetano (HCFC-141b)",        "1,2-Diclorobenceno"                          ,
                                       "1,4-Diclorobenceno",                           "Acetaldehido"                                ,
                                       "Acido 2,4 Diclorofenoxiacetico",               "Acido sulfhídrico"                           ,
                                       "Acrilonitrilo",                                "Acroleina"                                   ,
                                       "Arsénico",                                     "Arsénico"                                    ,
                                       "Asbesto",                                      "Benceno"                                     ,
                                       "Bifenilo",                                     "Bifenilos policlorados"                      ,
                                       "Bióxido de carbono",                           "Bióxido de nitrógeno"                        ,
                                       "Cadmio",                                       "Cadmio"                                      ,
                                       "Cianuro inorgánico/orgánico",                  "Clorodifluorometano (HCFC-22)"               ,
                                       "Cloruro de metileno",                          "Cloruro de vinilo"                           ,
                                       "Cromo",                                        "Cromo"                                       ,
                                       "Dioxinas",                                     "Estireno (fenil etileno)"                    ,
                                       "Fenol",                                        "Formaldehido"                                ,
                                       "Furanos",                                      "Hidrofluorocarbonos"                         ,
                                       "Mercurio",                                     "Mercurio"                                    ,
                                       "Metano",                                       "Metileno bis(fenilisocianato)"               ,
                                       "Níquel",                                       "Níquel"                                      , 
                                       "Oxido nitroso",                                "Piridina"                                    ,
                                       "Plomo",                                        "Plomo"                                       ,
                                       "Toluen diisocianato",                          "Tolueno"                                     ,
                                       "Xileno (mezcla de isómeros)")),
         Categoria = factor(SUSTANCIA,
                            levels = c("1,1-Dicloro-1-Fluoroetano (HCFC-141b)" ,"1,2-Diclorobenceno"                    ,"1,4-Diclorobenceno"                   ,
                                       "Acetaldehido"                          ,"Acido 2,4 Diclorofenoxiacetico"        ,"Acido sulfhídrico"                    ,
                                       "Acrilonitrilo"                         ,"Acroleina"                             ,"Arsénico"                             ,
                                       "Asbesto"                               ,"Benceno"                               ,"Bifenilo"                             ,
                                       "Bifenilos policlorados"                ,"Bióxido de carbono"                    ,"Bióxido de nitrógeno"                 ,
                                       "Cadmio"                                ,"Cianuro inorgánico/orgánico"           ,"Clorodifluorometano (HCFC-22)"        ,
                                       "Cloruro de metileno"                   ,"Cloruro de vinilo"                     ,"Cromo"                                ,
                                       "Dioxinas"                              ,"Estireno (fenil etileno)"              ,"Fenol"                                ,
                                       "Formaldehido"                          ,"Furanos"                               ,"Hidrofluorocarbonos"                  ,
                                       "Mercurio"                              ,"Metano"                                ,"Metileno bis(fenilisocianato)"        ,
                                       "Níquel"                                ,"Oxido nitroso"                         ,"Piridina"                             ,
                                       "Plomo"                                 ,"Toluen diisocianato"                   ,"Tolueno"                              ,
                                       "Xileno (mezcla de isómeros)"),
                            labels = c("CFC", "COV", "COV",
                                       "COV", "Ácido", "Ácido",
                                       "COV", "COV", "Arsénico",
                                       "COP", "COV", "COP",
                                       "COP", "GEI", "GEI",
                                       "MP", "Cianuro", "CFC",
                                       "COV", "COV", "MP",
                                       "COP", "COV", "COV",
                                       "COV", "COP", "CFC",
                                       "MP", "GEI", "COV",
                                       "MP", "GEI", "COV",
                                       "MP", "COV", "COV",
                                       "COV"))) %>%
  group_by(SCIAN, SUSTANCIA) %>%
  summarise(Aire = median(AIRE),
            Agua = median(AGUA),
            Suelo = median(SUELO),
            Dispo = median(`DISPOSICIÓN FINAL`),
            Alca = median(ALCANTARILLADO),
            Inci = median(INCINERACIÓN)) %>%
  pivot_wider(names_from = SUSTANCIA, values_from = c(Aire, Agua, Suelo, Dispo, Alca, Inci), values_fill = 0) %>%
  write_csv("../Discard.csv")
# tempETC %>%
#   group_by(SCIAN, `DESCRIPCION SCIAN`) %>%
#   summarise(cuenta = n()) %>%
#   pivot_wider(names_from = `DESCRIPCION SCIAN`, values_from = cuenta) %>%
#   write_csv("../Discard.csv")
# 
# tempsc <- read_xlsx(paste0("./retc2019.xlsx"), sheet = 1, col_names = TRUE, skip = 9) %>% 
#   mutate(SCIAN = as.character(SCIAN)) %>%
#   select(3,4) %>%
#   mutate(SCIAN = as.factor(SCIAN),
#          DescripcionSCIAN = as.factor(`DESCRIPCION SCIAN`)) %>%
#   group_by(SCIAN, DescripcionSCIAN) %>%
#   summarise(n())
# CodSCIAN <- tibble("SCIAN" = character(), "DescripcionSCIAN" = character()) %>%
#   bind_rows(tempsc %>%
#               select(1,2))


tempETC %>%
  replace_na(list(AireMn = 0, AguaMn = 0, SueloMn = 0, DispoMn = 0, AlcanMn = 0, IncinMn = 0)) %>%
  group_by(SCIAN, SUSTANCIA) %>%
  summarise(Aire = mean(AireMn),
            Agua = mean(AguaMn),
            Suelo = mean(SueloMn),
            DisposicionF = mean(DispoMn),
            Alcantarillado = mean(AlcanMn),
            Incineracion = mean(IncinMn)) %>%
  mutate(Aire = as.logical(Aire),
         Agua = as.logical(Agua),
         Suelo = as.logical(Suelo),
         DisposicionF = as.logical(DisposicionF),
         Alcantarillado = as.logical(Alcantarillado),
         Incineracion = as.logical(Incineracion)) %>%
  mutate(Aire = as.numeric(Aire),
         Agua = as.numeric(Agua),
         Suelo = as.numeric(Suelo),
         DisposicionF = as.numeric(DisposicionF),
         Alcantarillado = as.numeric(Alcantarillado),
         Incineracion = as.numeric(Incineracion)) %>%
   pivot_wider(names_from = SUSTANCIA, values_from = c(Aire, Agua, Suelo, DisposicionF, Alcantarillado, Incineracion),
               values_fill = 0, names_sort = FALSE) %>%
  left_join(read_csv("../RETC/CodSCIAN.csv", col_names = TRUE, col_types = "cc")) %>%
  select(1,248, 2:247) %>% 
  write_csv("../Discard.csv")
  

summary(tempETC)
view(tempETC)

# boxplots para medianas ----
read_xlsx("../RETC/2022_03_03 ContaminacionCompartimentosMediana.xlsx", sheet = 1) %>% 
  filter(Benceno != 0) %>%
  mutate(CVE_ENT = as.character(CVE_ENT),
         CVE_MUN = as.character(CVE_MUN),
         BencenoLog = log10((Benceno)+1)) %>% 
  left_join(Mpios, by = "NOM_MUN") %>%
  mutate(Entidad = fct_recode(CVE_ENT.x, "Puebla" = "21", "Tlaxcala" = "29"),
         NOM_MUN = paste0("(", Microcuenca, ") ", NOM_MUN)) %>%
  ggplot(aes(x = NOM_MUN, y = BencenoLog)) +
  geom_col(aes(fill = Entidad))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title = element_blank())

tempSust <- levels(as.factor(read_csv("../RETC/retc0419.csv", col_names = TRUE, col_types = "dfccccccccddddd")$SUSTANCIA)) %>%
  str_to_title(.) %>%
  str_replace_all(., "/", "") %>%
  str_replace_all(., " ", "")
tempPlot <- read_csv("../RETC/retc0419.csv", col_names = TRUE, col_types = "dfccccccccddddd") %>%
  mutate(Entidad = fct_recode(CVE_ENT, "Puebla" = "21", "Tlaxcala" = "29"),
         NOM_MUN = paste0("(", Microcuenca, ") ", NOM_MUN),
         SUSTANCIA = str_replace_all(str_to_title(SUSTANCIA), " ", ""),
         SUSTANCIA = str_replace_all(SUSTANCIA, "/", ""))

for (v in tempSust) {
  tempPlot %>% 
    filter(SUSTANCIA == v & AIRE > 0) %>%  
    ggplot(aes(x = NOM_MUN, y = AIRE)) +
    geom_boxplot(aes(color = Entidad), fill = "grey80") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title = element_blank())
  ggsave(paste0("../RETC/Graficos/BxPlt_","AIRE_",v,".png"),
         device = "png",
         width = 900, height = 600,
         units = "px",
         dpi = 100)
}

for (v in tempSust) {
  tempPlot %>% 
    filter(SUSTANCIA == v & AGUA > 0) %>%  
    ggplot(aes(x = NOM_MUN, y = AGUA)) +
    geom_boxplot(aes(color = Entidad), fill = "grey80") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title = element_blank())
  ggsave(paste0("../RETC/Graficos/BxPlt_","AGUA_",v,".png"),
         device = "png",
         width = 900, height = 600,
         units = "px",
         dpi = 100)
}

for (v in tempSust) {
  tempPlot %>% 
    filter(SUSTANCIA == v & SUELO > 0) %>%  
    ggplot(aes(x = NOM_MUN, y = SUELO)) +
    geom_boxplot(aes(color = Entidad), fill = "grey80") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title = element_blank())
  ggsave(paste0("../RETC/Graficos/BxPlt_","SUELO_",v,".png"),
         device = "png",
         width = 900, height = 600,
         units = "px",
         dpi = 100)
}

for (v in tempSust) {
  tempPlot %>% 
    filter(SUSTANCIA == v & `DISPOSICIÓN FINAL` > 0) %>%  
    ggplot(aes(x = NOM_MUN, y = `DISPOSICIÓN FINAL`)) +
    geom_boxplot(aes(color = Entidad), fill = "grey80") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title = element_blank())
  ggsave(paste0("../RETC/Graficos/BxPlt_","DISPOFINAL_",v,".png"),
         device = "png",
         width = 900, height = 600,
         units = "px",
         dpi = 100)
}

for (v in tempSust) {
  tempPlot %>% 
    filter(SUSTANCIA == v & ALCANTARILLADO > 0) %>%  
    ggplot(aes(x = NOM_MUN, y = ALCANTARILLADO)) +
    geom_boxplot(aes(color = Entidad), fill = "grey80") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title = element_blank())
  ggsave(paste0("../RETC/Graficos/BxPlt_","ALCANTARILLADO_",v,".png"),
         device = "png",
         width = 900, height = 600,
         units = "px",
         dpi = 100)
}

for (v in tempSust) {
  tempPlot %>% 
    filter(SUSTANCIA == v & INCINERACIÓN > 0) %>%  
    ggplot(aes(x = NOM_MUN, y = INCINERACIÓN)) +
    geom_boxplot(aes(color = Entidad), fill = "grey80") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title = element_blank())
  ggsave(paste0("../RETC/Graficos/BxPlt_","INCINERACIÓN_",v,".png"),
         device = "png",
         width = 900, height = 600,
         units = "px",
         dpi = 100)
}

# gráficas sin valores extremos ----
# Aire: Bióxido de carbono
v = "BióxidoDeCarbono"
tempPlot %>% 
  filter(SUSTANCIA == v & AIRE > 0) %>% 
  filter(AIRE < 6500000000) %>%
  ggplot(aes(x = NOM_MUN, y = AIRE)) +
  geom_boxplot(aes(color = Entidad), fill = "grey80") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title = element_blank())
ggsave(paste0("../RETC/Graficos/BxPlt_","AIRE_",v,"_3.png"),
       device = "png",
       width = 900, height = 600,
       units = "px",
       dpi = 100)

# Agua: arsénico, cadmio, cianuro, cromo, mercurio, níquel, plomo
v = "Cadmio"
tempPlot %>% 
    filter(SUSTANCIA == v & AGUA > 0) %>%  
    filter(AGUA < 600) %>%
    ggplot(aes(x = NOM_MUN, y = AGUA)) +
    geom_boxplot(aes(color = Entidad), fill = "grey80") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title = element_blank())
  ggsave(paste0("../RETC/Graficos/BxPlt_","AGUA_",v,"_3.png"),
         device = "png",
         width = 900, height = 600,
         units = "px",
         dpi = 100)

# Suelo: arsénico
v = "Arsénico"
tempPlot %>% 
    filter(SUSTANCIA == v & SUELO > 0) %>%  
    filter(SUELO < 200) %>%
    ggplot(aes(x = NOM_MUN, y = SUELO)) +
    geom_boxplot(aes(color = Entidad), fill = "grey80") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title = element_blank())
# ggsave(paste0("../RETC/Graficos/BxPlt_","SUELO_",v,"_2.png"),
#        device = "png",
#        width = 900, height = 600,
#        units = "px",
#        dpi = 100)
  
# Boxplot con facet para hacer caracterización ambiental ----
## metales pesados ----
p1 <- tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(Categoria == "Arsénico") %>% 
  filter(SUELO < 200) 
p2 <- tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(SUSTANCIA == "Cadmio") %>% 
  filter(AGUA < 1500) 
p3 <- tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(SUSTANCIA == "Cromo" | SUSTANCIA == "Níquel" | SUSTANCIA == "Plomo") %>% 
  filter(AGUA < 4000) 
p4 <- tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(SUSTANCIA == "Mercurio") %>% 
  filter(AGUA < 75) 

p <- bind_rows(p1, p2, p3, p4)

p %>%
  ggplot(aes(x = NOM_MUN, y = AGUA)) +
  geom_boxplot(aes(color = Entidad), fill = "grey80") +
  facet_wrap(vars(SUSTANCIA), 
             ncol = 1, 
             strip.position = "left",
             scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 13),
        axis.title = element_blank())
ggsave(paste0("../RETC/Graficos/BxPlt_MP_Agua.png"),
       device = "png",
       width = 1024, height = 768,
       units = "px",
       dpi = 100)

## cov's----
tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(Categoria == "COV") %>% 
  mutate(SUSTANCIA = factor(SUSTANCIA,
                            levels = c("1,2-Diclorobenceno", "1,4-Diclorobenceno",
                                       "Acetaldehido", "Acrilonitrilo",
                                       "Acroleina", "Benceno",
                                       "CloruroDeMetileno", "CloruroDeVinilo",
                                       "Estireno(FenilEtileno)", "Fenol",
                                       "Formaldehido", "MetilenoBis(Fenilisocianato)",
                                       "Piridina", "ToluenDiisocianato",
                                       "Tolueno", "Xileno(MezclaDeIsómeros)"),
                            labels = c("1,2-Diclorobenceno", "1,4-Diclorobenceno",
                                       "Acetaldehido", "Acrilonitrilo",
                                       "Acroleina", "Benceno",
                                       "Cloruro de metileno", "Cloruro de vinilo",
                                       "Estireno (Fenil etileno)", "Fenol",
                                       "Formaldehido", "Metileno Bis (Fenilisocianato)",
                                       "Piridina", "Toluen Diisocianato",
                                       "Tolueno", "Xileno (Mezcla de isómeros)"))) %>%
  ggplot(aes(x = NOM_MUN, y = `DISPOSICIÓN FINAL`)) +
  geom_boxplot(aes(color = Entidad), fill = "grey80") +
  facet_wrap(vars(SUSTANCIA), 
             ncol = 4, 
             strip.position = "top",
             scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 11),
        axis.title = element_blank())
ggsave(paste0("../RETC/Graficos/BxPlt_COV_","DispoFinal",".png"),
       device = "png",
       width = 1200, height = 768,
       units = "px",
       dpi = 100)

## cop's----
tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(Categoria == "COP") %>%
  mutate(SUSTANCIA = str_replace_all(SUSTANCIA, "BifenilosPoliclorados", "Bifenilos\nPoliclorados")) %>% 
  ggplot(aes(x = NOM_MUN, y = `DISPOSICIÓN FINAL`)) +
  geom_boxplot(aes(color = Entidad), fill = "grey80") +
  facet_wrap(vars(SUSTANCIA), 
             ncol = 1, 
             strip.position = "left",
             scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 11),
        axis.title = element_blank())
ggsave(paste0("../RETC/Graficos/BxPlt_COP_","DispoFinal",".png"),
       device = "png",
       width = 1024, height = 768,
       units = "px",
       dpi = 100)

## gei ----
p1 <- tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(SUSTANCIA == "BióxidoDeCarbono") %>%
  filter(AIRE < 6500000000)
p2 <- tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(Categoria == "GEI") %>%
  filter(SUSTANCIA != "BióxidoDeCarbono")

p <- bind_rows(p1,p2) %>%
  mutate(SUSTANCIA = str_replace_all(SUSTANCIA, "BióxidoDeCarbono", "Bióxido de\nCarbono")) %>%
  mutate(SUSTANCIA = str_replace_all(SUSTANCIA, "BióxidoDeNitrógeno", "Bióxido de\nNitrógeno")) %>%
  mutate(SUSTANCIA = str_replace_all(SUSTANCIA, "OxidoNitroso", "Oxido Nitroso"))
  
p %>%
  ggplot(aes(x = NOM_MUN, y = AIRE)) +
  geom_boxplot(aes(color = Entidad), fill = "grey80") +
  facet_wrap(vars(SUSTANCIA), 
             ncol = 1, 
             strip.position = "left",
             scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 13),
        axis.title = element_blank())
ggsave(paste0("../RETC/Graficos/BxPlt_GEI_","Aire",".png"),
       device = "png",
       width = 1024, height = 768,
       units = "px",
       dpi = 100)

## cfc's----
tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(Categoria == "CFC") %>% 
  mutate(SUSTANCIA = factor(SUSTANCIA, 
                            levels = c("1,1-Dicloro-1-Fluoroetano(Hcfc-141b)",
                                       "Clorodifluorometano(Hcfc-22)",
                                       "Hidrofluorocarbonos"),
                            labels = c("1,1-Dicloro-1-\nFluoroetano (Hcfc-141b)",
                                       "Clorodifluorometano\n(Hcfc-22)",
                                       "Hidrofluorocarbonos"))) %>% 
  ggplot(aes(x = NOM_MUN, y = AIRE)) +
  geom_boxplot(aes(color = Entidad), fill = "grey80") +
  facet_wrap(vars(SUSTANCIA), 
             ncol = 1, 
             strip.position = "left",
             scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 11),
        axis.title = element_blank())
ggsave(paste0("../RETC/Graficos/BxPlt_CFC_","Aire",".png"),
       device = "png",
       width = 1024, height = 768,
       units = "px",
       dpi = 100)

## cianuro's----
tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(Categoria == "Cianuro") %>% 
  mutate(SUSTANCIA = str_replace_all(SUSTANCIA, "CianuroInorgánicoOrgánico", "Cianuro\nInorgánico/\nOrgánico")) %>% 
  pivot_longer(cols = 11:16, names_to = "Compartimento", values_to = "Concentracion") %>%
  mutate(Compartimento = str_to_sentence(Compartimento),
         Compartimento = str_wrap(Compartimento, width = 15)) %>%
  filter(Compartimento != "Aire" & Compartimento != "Incineración") %>%
  ggplot(aes(x = NOM_MUN, y = Concentracion)) +
  geom_boxplot(aes(color = Entidad), fill = "grey80") +
  facet_wrap(vars(Compartimento), 
             ncol = 1, 
             strip.position = "left",
             scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 11),
        axis.title = element_blank())
ggsave(paste0("../RETC/Graficos/BxPlt_Cianuro_","General",".png"),
       device = "png",
       width = 1024, height = 768,
       units = "px",
       dpi = 100)

## acidos's----
tempPlot %>%
  mutate(NOM_MUN = str_wrap(NOM_MUN, width = 30)) %>%
  filter(Categoria == "Ácido") %>% 
  mutate(SUSTANCIA = factor(SUSTANCIA, 
                            levels = c("Acido2,4Diclorofenoxiacetico", "AcidoSulfhídrico"),
                            labels = c("Acido 2,4\nDiclorofenoxiacetico", "Acido Sulfhídrico"))) %>% 
  pivot_longer(cols = 11:16, names_to = "Compartimento", values_to = "Concentracion") %>%
  mutate(Compartimento = str_to_sentence(Compartimento),
         Compartimento = str_wrap(Compartimento, width = 15)) %>%
  filter(Compartimento == "Aire" | Compartimento == "Agua") %>%
  ggplot(aes(x = NOM_MUN, y = Concentracion)) +
  geom_boxplot(aes(color = Entidad), fill = "grey80") +
  facet_wrap(vars(Compartimento, SUSTANCIA), 
             ncol = 2, 
             strip.position = "left",
             scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 11),
        axis.title = element_blank())
ggsave(paste0("../RETC/Graficos/BxPlt_Acido_","General",".png"),
       device = "png",
       width = 1024, height = 768,
       units = "px",
       dpi = 100)
