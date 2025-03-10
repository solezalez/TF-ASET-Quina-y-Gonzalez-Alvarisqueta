
library(tidyverse)
library(openxlsx)
library(eph)
library(scales)
library(dplyr)
library(knitr)
library(kableExtra)
library(htmltools)
library(writexl)
library(ggplot2)
library(flexdashboard)
library(plotly)
library(janitor)

variables <- c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE", "REGION", "AGLOMERADO", "CH03","CH04","CH06","ESTADO","CAT_OCUP","CAT_INAC", "PP03C", "PP03I", "PP04A", "PP04B_COD", "PP05H", "PP07H","P21","P47T","PONDERA","PP04D_COD","PP04C", "PP06E", "PP07A","PP07C","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","PONDIIO","PP04C","PP03G","PP3E_TOT")

eph_2024_t3 <-eph::get_microdata(year = 2024, period = 3, vars = variables)

eph_2024_t3_grupos_edad <- eph_2024_t3 %>% 
  filter(CH06 >= 10)  %>%
  mutate(Grupos_Etarios = case_when(CH06  %in%  10:17   ~ "Menores", 
                                    CH06  %in%  18:25   ~ "Adultos Jóvenes",
                                    CH04 == 1 & CH06 %in% 26:65 ~ "Adultos mediana edad" ,
                                    CH04 == 2 & CH06 %in% 26:60 ~ "Adultos mediana edad", 
                                    CH04 == 1 & CH06 > 65 ~ "Adultos Mayores", 
                                    CH04 == 2 & CH06 > 60 ~ "Adultos Mayores",
                                    is.na(CH06) ~ "No responde edad"))


eph_2024_t3_grupos_edad <- eph_2024_t3_grupos_edad %>% 
  mutate(Sexo = case_when(CH04  == 1   ~ "Varón", CH04  == 2   ~ "Mujer"))


eph_2024_t3_grupos_edad <- eph_2024_t3_grupos_edad %>% 
  mutate(Nivel_Educativo = case_when(NIVEL_ED  == 1   ~ "Primaria Incompleta",
                                     NIVEL_ED  == 2   ~ "Primaria Completa",
                                     NIVEL_ED  == 3   ~ "Secundaria Incompleta",
                                     NIVEL_ED  == 4   ~ "Secundaria Completa",
                                     NIVEL_ED  == 5   ~ "Superior Univ Incompleta",                                     NIVEL_ED  == 6   ~ "Superior Univ Completa",
                                     NIVEL_ED  == 7   ~ "Sin Instrucción"))


eph_2024_t3_grupos_edad <- eph_2024_t3_grupos_edad %>% 
  mutate(Categ_ocup = case_when(CAT_OCUP  == 1   ~ "Patrón",
                                CAT_OCUP  == 2   ~ "Cuenta propia",
                                CAT_OCUP  == 3   ~ "Obrero o empleado",
                                CAT_OCUP  == 4   ~ "Trab fam sin remun"))


Base_por_grupos_edad_y_sexo <- calculate_tabulates(base = eph_2024_t3_grupos_edad,x = "Grupos_Etarios",y = "Sexo",weights = "PONDERA",add.totals = "both")

Base_por_cat_ocup <- calculate_tabulates(base = eph_2024_t3_grupos_edad,x = "Categ_ocup",y = "Nivel_Educativo",weights = "PONDERA",add.totals = "both")

## convertimos esta última a proporciones en porcentaje:

Base_por_cat_ocup <- data.frame(
  Categ_Ocup = c("Cuenta propia", "Obrero o empleado", "Patrón", "Trab fam sin remun"),
  Primaria_Completa = c(445360, 942935, 37863, 3769),
  Primaria_Incompleta = c(140498, 216581, 11312, 3940),
  Secundaria_Completa = c(944297, 3192672, 127558, 18454),
  Secundaria_Incompleta = c(619775, 1558282, 50325, 10139),
  Sin_Instruccion = c(13904, 14338, 303, 297),
  Superior_Univ_Completa = c(648006, 2833458, 158771, 822),
  Superior_Univ_Incompleta = c(474154, 1580701, 58776, 15344),
  Total = c(3285994, 10338967, 444908, 52765))

Base_por_cat_ocup <- Base_por_cat_ocup %>%
  mutate(across(Primaria_Completa:Superior_Univ_Incompleta, 
                ~ round((. / Total) * 100, 1)))

print(Base_por_cat_ocup)

Tabla_edad_y_sexo <- kable(Base_por_grupos_edad_y_sexo, format = "html", caption = "Tabla 1: Base EPH 3er trimestre 2024 por edad según sexo", align = 'c')

Tabla_edad_y_sexo

##TABLA 2

Tabla_Cat_Ocup_Niv_Ed <- kable(Base_por_cat_ocup, format = "html", caption = "Tabla 2: Base EPH 3er trimestre 2024 por Categoría Ocupacional según nivel educativo", align = 'c')

Tabla_Cat_Ocup_Niv_Ed


##Exploramos las cantidades de perceptores de ingresos según sexo. Para ello, creamos la variable perceptor de ingresos (s_p) y la convertimos a factor. Luego la cruzaremos con las regiones de pertenencia.

eph_2024_t3_grupos_edad <- eph_2024_t3_grupos_edad %>% 
  mutate(s_p = case_when(
    CH04 == 1 & P47T > 0 ~ "Varón perceptor", 
    CH04 == 1 & P47T == 0 ~ "Varón no perceptor", 
    CH04 == 2 & P47T > 0 ~ "Mujer perceptora", 
    CH04 == 2 & P47T == 0 ~ "Mujer no perceptora", 
    P47T== -9 ~ "No responde ingresos"))

s_p_niveles <- c("Varón perceptor", "Varón no perceptor", "Mujer perceptora", "Mujer no perceptora", "No responde ingresos")

# Convertir a factor con niveles específicos
eph_2024_t3_grupos_edad$s_p <- factor(eph_2024_t3_grupos_edad$s_p, levels = s_p_niveles, labels = c("Varón perceptor", "Varón no perceptor", "Mujer perceptora", "Mujer no perceptora", "No responde ingresos"))


# Mostramos el factor

tabulation <- calculate_tabulates(base = eph_2024_t3_grupos_edad, x = "s_p",
                                  weights = "PONDERA")


tabulationcheck <- calculate_tabulates(base = eph_2024_t3_grupos_edad, x =
                                         "CH04", weights = "PONDERA")


REGION_niveles <- c("1", "40", "41", "42", "43", "44")


# Convertimos a factor con niveles específicos
eph_2024_t3_grupos_edad$REGION <- factor(eph_2024_t3_grupos_edad$REGION, levels = REGION_niveles, labels=c("1", "40", "41", "42", "43", "44"))

Perceptores_segun_region <- calculate_tabulates(base = eph_2024_t3_grupos_edad, x = "s_p", y = "REGION", weights = "PONDERA", add.totals="both", add.percentage="col")


Perceptores_segun_region <- Perceptores_segun_region %>% 
  rename ( "Gran Buenos Aires" = "1",
           "NOA" = "40",
           "NEA" = "41", 
           "Cuyo" = "42", 
           "Pampeana" = "43",
           "Patagonia" = "44" )

eph_2024_t3_grupos_edad

##Creamos una tabla de perceptores de ingresos según sexo y región
##TABLA 3


Tabla_perceptores_region <- kable(Perceptores_segun_region, format = "html", caption = "Tabla 3:
Distribución Cruzada por Regiones y Sexo Perceptor", align = 'c')

Tabla_perceptores_region

##Creamos una tabla para explorar percepción de ingresos según grupo etario
##TABLA 4

Tabla_perceptores_grupo_etario <- calculate_tabulates(base = eph_2024_t3_grupos_edad, x = "s_p", y = "Grupos_Etarios", weights = "PONDERA", add.totals="row", add.percentage="col")

Tabla_perceptores_edad <- kable(Tabla_perceptores_grupo_etario, format = "html", caption = "Tabla 4: Distribución cruzada por Grupos Etarios y Sexo Perceptor", align = 'c')

Tabla_perceptores_edad

save_html(Tabla_edad_y_sexo, file = "Tabla_edad_y_sexo.html")
save_html(Tabla_Cat_Ocup_Niv_Ed, file = "Tabla_Cat_Ocup_Niv_Ed.html")
save_html(Tabla_perceptores_region, file = "Tabla_perceptores_region.html")
save_html(Tabla_perceptores_edad, file = "Tabla_perceptores_edad.html")

writeLines(as.character(Tabla_edad_y_sexo), "tabla_clasificacion1.html")
writeLines(as.character(Tabla_Cat_Ocup_Niv_Ed), "tabla_clasificacion2.html")
writeLines(as.character(Tabla_perceptores_region), "tabla_clasificacion3.html")
writeLines(as.character(Tabla_perceptores_edad), "tabla_clasificacion4.html")

Mercado_trabajo <- eph_2024_t3_grupos_edad %>% group_by(REGION) %>% 
  summarize( 
    población = sum(PONDERA, na.rm = TRUE), 
    Ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE),
    desempleo_total = sum(PONDERA[ESTADO == 2], na.rm = TRUE),
    PEA = Ocupados + desempleo_total,
    PEI = población - PEA,
    Tasa_actividad = round((PEA / población) * 100,1), 
    Tasa_empleo = round((Ocupados / población)* 100,1),
    Tasa_desocupados = round((desempleo_total / PEA) * 100,1) )


Mercado_trabajo_sexo <- eph_2024_t3_grupos_edad %>% group_by(Sexo) %>% 
  summarize( población = sum(PONDERA, na.rm = TRUE), 
             Ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE), 
             desempleo_total = sum(PONDERA[ESTADO == 2], na.rm = TRUE), 
             PEA = Ocupados + desempleo_total, 
             PEI = población - PEA,
             Tasa_actividad = round((PEA / población) * 100,1), 
             Tasa_empleo = round((Ocupados / población)* 100,1),
             Tasa_desocupados = round((desempleo_total / PEA) * 100,1) )%>% 
  ungroup()

Mercado_trabajo_sexo

Mercado_trabajo_edades <- eph_2024_t3_grupos_edad %>% group_by(Grupos_Etarios) %>%
  summarize( población = sum(PONDERA, na.rm = TRUE), 
             Ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE), 
             desempleo_total = sum(PONDERA[ESTADO == 2], na.rm = TRUE), 
             PEA = Ocupados + desempleo_total, 
             PEI = población - PEA, 
             Tasa_actividad = round((PEA / población) * 100,1), 
             Tasa_empleo = round((Ocupados / población)* 100,1),
             Tasa_desocupados = round((desempleo_total / PEA) * 100,1) ) %>% ungroup()


Mercado_trabajo_Region <- eph_2024_t3_grupos_edad %>%
  group_by(REGION) %>% 
  summarize(población = sum(PONDERA, na.rm = TRUE), 
            Ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE), 
            desempleo_total = sum(PONDERA[ESTADO == 2], na.rm =TRUE), 
            PEA = Ocupados + desempleo_total,
            PEI = población - PEA,
            Tasa_actividad = round((PEA / población) * 100,1), 
            Tasa_empleo = round((Ocupados / población)* 100,1),
            Tasa_desocupados = round((desempleo_total / PEA) * 100,1) )%>%  
  ungroup()

Mercado_trabajo_Region <- Mercado_trabajo_Region %>%
  mutate(REGION = case_when(
    REGION == 1   ~ "Gran Buenos Aires",
    REGION == 40  ~ "Noroeste",
    REGION == 41  ~ "Nordeste",
    REGION == 42  ~ "Cuyo",
    REGION == 43  ~ "Pampeana",  
    REGION == 44  ~ "Patagonica"
  ))

Mercado_trabajo_Region

##Generamos y guardamos las tablas en kable

Tabla_Mercado_Trabajo <- kable(Mercado_trabajo, format = "html", caption = "Tabla 5: Mercado de Trabajo", align = 'c')

Tabla_Mercado_trabajo_sexo <- kable(Mercado_trabajo_sexo, format = "html", caption = "Tabla 6: Mercado de Trabajo según sexo", align = 'c')

Tabla_Mercado_trabajo_edades <- kable(Mercado_trabajo_edades, format = "html", caption = "Tabla 7: Mercado de Trabajo según edades", align = 'c')

Tabla_Mercado_trabajo_regiones <- kable(Mercado_trabajo_Region, format = "html", caption = "Tabla 8: Mercado de Trabajo según regiones", align = 'c')

save_html(Tabla_Mercado_Trabajo, file = "Tabla_Mercado_Trabajo.html")
save_html(Tabla_Mercado_trabajo_sexo, file = "Tabla_Mercado_trabajo_sexo.html")
save_html(Tabla_Mercado_trabajo_edades, file = "Tabla_Mercado_trabajo_edades.html")
save_html(Tabla_Mercado_trabajo_regiones, file = "Tabla_Mercado_trabajo_regiones.html")

writeLines(as.character(Tabla_Mercado_Trabajo), "tabla_clasificacion5.html")
writeLines(as.character(Tabla_Mercado_trabajo_sexo), "tabla_clasificacion6.html")
writeLines(as.character(Tabla_Mercado_trabajo_edades), "tabla_clasificacion7.html")
writeLines(as.character(Tabla_Mercado_trabajo_regiones), "tabla_clasificacion8.html")

eph_2024_t3 <- eph_2024_t3 %>% filter(CH06 >= 10)  %>%
  mutate(Grupos_Etarios = case_when(CH06  %in%  10:17   ~ "Menores", 
                                    CH06  %in%  18:25   ~ "Adultos Jóvenes",
                                    CH06 %in% 26:39 ~ "Adultos mediana edad 1" ,
                                    CH06 %in% 40:49 ~ "Adultos mediana edad 2",
                                    CH04 == 1 & CH06 %in% 50:65 ~ "Adultos mediana edad 3",
                                    CH04 == 2 & CH06 %in% 50:60 ~ "Adultos mediana edad 3", 
                                    CH04 == 1 & CH06 > 65 ~ "Adultos Mayores", 
                                    CH04 == 2 & CH06 > 60 ~ "Adultos Mayores",
                                    is.na(CH06) ~ "No responde edad"))

eph_2024_t3 <- eph_2024_t3 %>% 
  mutate(Sexo = case_when(CH04  == 1   ~ "Varón", CH04  == 2   ~ "Mujer"))


eph_2024_t3 <- eph_2024_t3 %>% 
  mutate(Nivel_Educativo = case_when(NIVEL_ED  == 1   ~ "Primaria Incompleta",
                                     NIVEL_ED  == 2   ~ "Primaria Completa",
                                     NIVEL_ED  == 3   ~ "Secundaria Incompleta",
                                     NIVEL_ED  == 4   ~ "Secundaria Completa",
                                     NIVEL_ED  == 5   ~ "Superior Univ Incompleta",                                     NIVEL_ED  == 6   ~ "Superior Univ Completa",
                                     NIVEL_ED  == 7   ~ "Sin Instrucción"))


eph_2024_t3 <- eph_2024_t3 %>% 
  mutate(Categ_ocup = case_when(CAT_OCUP  == 1   ~ "Patrón",
                                CAT_OCUP  == 2   ~ "Cuenta propia",
                                CAT_OCUP  == 3   ~ "Obrero o empleado",
                                CAT_OCUP  == 4   ~ "Trab fam sin remun"))

## filtramos ocupados y renombramos
ocupados_2024_t3 <- eph_2024_t3 %>%
  filter(ESTADO == 1, CAT_OCUP == 3 | 2)

ocupados_2024_t3_indicadores_prec <-ocupados_2024_t3 %>% mutate(
  multiempleo = case_when(PP03C == 1 ~ "Unica ocupacion",
                          PP03C == 2 ~ "Multiempleo",
                          TRUE ~ "Ns/Nr"),
  tamanio.establec = factor(
    case_when(PP04C %in% 1:6  ~ "Pequeño",
              PP04C %in% 7:8  ~ "Mediano",
              PP04C %in% 9:12 ~ "Grande",
              PP04C %in% 99   ~ "Ns/Nr"),
    levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
  descuento_jubil = case_when(PP07H == 1 ~ "Si",
                              PP07H == 2 ~ "No"),
  indepte_informal = case_when(PP06E == 3 ~ "Si",
                               PP06E == 2 | PP06E == 1 ~ "No"),
  part.time.inv = case_when(PP3E_TOT < 35 & PP03G == 1  | PP03I == 1 ~ "Si",
                            TRUE ~ "No"),
  tiempo.determinado = case_when(PP07C ==  1 ~ "Si",
                                 TRUE ~ "No"),
  informales = case_when(PP06E == 3 | PP07H == 2 ~ "Si", PP06E < 3 | PP07H == 1 ~ "No"))

ocupados_2024_t3_indicadores_prec_filtrada <- ocupados_2024_t3_indicadores_prec %>%
  filter(!multiempleo %in% c("Ns/Nr"))
ocupados_2024_t3_indicadores_prec_filtrada

ocupados_2024_t3_indicadores_prec_SIGNOS <- ocupados_2024_t3_indicadores_prec %>% 
  mutate(signos.precariedad = ifelse(is.na(informales), 0, ifelse(informales == "Si", 1, 0))+
           ifelse(is.na(part.time.inv), 0, ifelse(part.time.inv == "Si", 1 , 0))+
           ifelse(is.na(tiempo.determinado), 0, ifelse(tiempo.determinado == "Si", 1, 0)))

ocupados_2024_t3_precarios <- ocupados_2024_t3_indicadores_prec_SIGNOS %>% 
  group_by(signos.precariedad) %>% 
  summarise(Poblacion = sum(PONDERA))

ocupados_2024_t3_precarios

## calculamos ahora las proporciones de esto:

proporciones_precariedad_totales <- ocupados_2024_t3_precarios %>% 
  summarise(proporcion_precarios = sum(Poblacion [signos.precariedad > 0]) / sum(Poblacion),
            precariedad_leve = sum(Poblacion[signos.precariedad == 1]) / sum(Poblacion),
            precariedad_media = sum(Poblacion[signos.precariedad == 2]) / sum(Poblacion),
            precariedad_severa = sum(Poblacion[signos.precariedad == 3]) / sum(Poblacion))


##para expresarlo en porcentajes 
proporciones_precariedad_totales <- proporciones_precariedad_totales %>%
  mutate(proporcion_precarios = round (proporcion_precarios * 100, 1), precariedad_leve = round (precariedad_leve * 100,1), precariedad_media = round(precariedad_media * 100, 1), precariedad_severa = round (precariedad_severa * 100, 1))

proporciones_precariedad_totales

Tabla_precariedad_totales <- kable(proporciones_precariedad_totales, format = "html", caption = "Tabla 9: Precariedad total", align = 'c')

save_html(Tabla_precariedad_totales, file = "Tabla_precariedad_total.html")

writeLines(as.character(Tabla_precariedad_totales), "tabla_clasificacion9.html")



## calculamos ahora las proporciones por región:
ocupados_2024_t3_precarios_region <- ocupados_2024_t3_indicadores_prec_SIGNOS %>% 
  group_by(REGION, signos.precariedad) %>% 
  summarise(Poblacion = sum(PONDERA))

ocupados_2024_t3_precarios_region

## calculamos ahora las proporciones de esto:

proporciones_precariedad_region <- ocupados_2024_t3_precarios_region %>% 
  summarise(proporcion_precarios = sum(Poblacion [signos.precariedad > 0]) / sum(Poblacion),
            precariedad_leve = sum(Poblacion[signos.precariedad == 1]) / sum(Poblacion),
            precariedad_media = sum(Poblacion[signos.precariedad == 2]) / sum(Poblacion),
            precariedad_severa = sum(Poblacion[signos.precariedad == 3]) / sum(Poblacion))  %>% mutate(REGION = case_when(REGION  == 1   ~ "Gran Buenos Aires",
                                                                                                                          REGION  == 40   ~ "Noroeste",
                                                                                                                          REGION  == 41   ~ "Nordeste",
                                                                                                                          REGION  == 42   ~ "Cuyo",
                                                                                                                          REGION  == 43   ~ "Pampeana",  
                                                                                                                          REGION  == 44   ~ "Patagonica"))


##para expresarlo en porcentajes 
proporciones_precariedad_region <- proporciones_precariedad_region %>%
  mutate(proporcion_precarios = round (proporcion_precarios * 100, 1), precariedad_leve = round (precariedad_leve * 100,1), precariedad_media = round(precariedad_media * 100, 1), precariedad_severa = round (precariedad_severa * 100, 1))


##Generamos y guardamos la tabla en kable

proporciones_precariedad_region

Tabla_precariedad_regiones <- kable(proporciones_precariedad_region, format = "html", caption = "Tabla 10: Precariedad regional", align = 'c')

save_html(Tabla_precariedad_regiones, file = "Tabla_precariedad_regiones.html")

writeLines(as.character(Tabla_precariedad_regiones), "tabla_clasificacion10.html")

##Ahora hacemos el gráfico de precaridad por región
##primero pasamos la tabla a modo largo con pivot longer

proporciones_precariedad_region_long <- proporciones_precariedad_region %>%
  pivot_longer(cols = starts_with("preca"),
               names_to = "variable",
               values_to = "valor")

print(proporciones_precariedad_region_long)

##luego generamos con ello un gráfico de barras
graf_preca_regiones <- ggplot(proporciones_precariedad_region_long, aes(x = REGION, y = valor, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(valor, 1)), vjust = -0.5) +
  labs(title = "Indices de precariedad según regiones. Total de ocupados, Argentina, 3er trimestre 2024", 
       x = "Región", 
       y = "% precariedad") +
  theme_minimal()

graf_preca_regiones

ggsave("graf_preca_regiones.png", width = 8, height = 6)

ocupados_2024_t3_no_preca <- ocupados_2024_t3_indicadores_prec_SIGNOS %>% 
  filter (signos.precariedad == 0 ) %>% mutate(REGION = case_when(REGION  == 1   ~ "Gran Buenos Aires",
                                                                  REGION  == 40   ~ "Noroeste",
                                                                  REGION  == 41   ~ "Nordeste",
                                                                  REGION  == 42   ~ "Cuyo",
                                                                  REGION  == 43   ~ "Pampeana",  
                                                                  REGION  == 44   ~ "Patagonica"))

ocupados_2024_t3_no_preca <- ocupados_2024_t3_no_preca %>% 
  mutate(riesgo_precariedad = ifelse (tamanio.establec == "Pequeño" & multiempleo== "Multiempleo", "sí", "no"))


riesgo_precariedad <- calculate_tabulates(base = ocupados_2024_t3_no_preca, x = "riesgo_precariedad", y=
                                            "REGION", weights = "PONDERA", add.totals="col")

riesgo_precariedad

Riesgo_prec <- data.frame(
  Cuyo = c(408866, 12140),
  Gran_Buenos_Aires = c(3981282, 125384),
  Nordeste = c(342257, 7360),
  Noroeste = c(613179, 18766),
  Pampeana = c(1598820, 52004),
  Patagonica = c(346859, 3667),
  Total = c(7291263, 219321)
)

tasa_riesgo <- round(Riesgo_prec[2, ] / (Riesgo_prec[1, ] + Riesgo_prec[2, ]) * 100, 2)

Riesgo_prec <- rbind(Riesgo_prec, tasa_riesgo)

rownames(Riesgo_prec) <- c("Sin riesgo de precariedad", "Con riesgo de precariedad", "Tasa riesgo de precariedad")

Riesgo_prec_regional <- Riesgo_prec %>% slice(3)

Riesgo_prec_regional


## Guardamos la tabla con kable


Tabla_riesgo_precariedad_regiones <- kable(Riesgo_prec_regional, format = "html", caption = "Tabla 11: Riesgo de precariedad regional", align = 'c')

save_html(Tabla_riesgo_precariedad_regiones, file = "Tabla_riesgo_precariedad_regiones.html")

writeLines(as.character(Tabla_riesgo_precariedad_regiones), "tabla_clasificacion11.html")

Tabla_riesgo_precariedad_regiones

##Graficamos con Ggplot

##Primero, creamos data frame limpio desde valores de la tabla
valores_riesgo_prec_region <- data.frame(
  Region = c("Cuyo", "Gran Buenos Aires", "Nordeste", "Noroeste", "Pampeana", "Patagonica", "Total"),
  Riesgo_Precariedad = c(2.88, 3.05, 2.11, 2.97, 3.15, 1.05, 2.92)
)

##Segundo, generamos el gráfico:

ggplot(valores_riesgo_prec_region, aes(x = reorder(Region, Riesgo_Precariedad), y = Riesgo_Precariedad, fill = Region)) +
  geom_col(show.legend = FALSE) + 
  geom_text(aes(label = Riesgo_Precariedad), 
            hjust = -0.2,  
            size = 4) +  
  coord_flip() +
  labs(title = "Tasa de Riesgo de Precariedad por Región",
       x = "Región",
       y = "Tasa de Riesgo de Precariedad") +
  theme_minimal()

ggsave("graf_riesgo_prec_regiones.png", width = 8, height = 6)


ocupados_2024_t3_preca_agrav <- ocupados_2024_t3_indicadores_prec_SIGNOS %>% 
  filter (signos.precariedad > 0 ) %>% mutate(REGION = case_when(REGION  == 1   ~ "Gran Buenos Aires",
                                                                 REGION  == 40   ~ "Noroeste",
                                                                 REGION  == 41   ~ "Nordeste",
                                                                 REGION  == 42   ~ "Cuyo",
                                                                 REGION  == 43   ~ "Pampeana",  
                                                                 REGION  == 44   ~ "Patagonica"))

ocupados_2024_t3_preca_agrav <- ocupados_2024_t3_preca_agrav %>% 
  mutate(precariedad_agravada = ifelse (tamanio.establec == "Pequeño" | multiempleo== "Multiempleo", "sí", "no"))


precariedad_agravada <- calculate_tabulates(base = ocupados_2024_t3_preca_agrav, x = "precariedad_agravada", y=
                                              "REGION", weights = "PONDERA", add.totals="col")


Preca_agrav <- data.frame(
  Cuyo = c(103470, 274395),
  Gran_Buenos_Aires = c(942950, 1996493),
  Nordeste = c(62987, 168537),
  Noroeste = c(159116, 382322),
  Pampeana = c(298545, 841982),
  Patagonica = c(29524, 80194),
  Total = c(1596592, 3743923)
)

tasa_agrav <- round(Preca_agrav[2, ] / (Preca_agrav[1, ] + Preca_agrav[2, ]) * 100, 2)


tasa_agrav

Preca_agrav <- rbind(Preca_agrav, tasa_agrav)

rownames(Preca_agrav) <- c("Sin precariedad agravada", "Con precariedad agravada", "Tasa de precariedad agravada")

Preca_agrav_regional <- Preca_agrav %>% slice(3)

Preca_agrav_regional


## Guardamos la tabla con kable


Tabla_precariedad_agrav_regiones <- kable(Preca_agrav_regional, format = "html", caption = "Tabla 12: Precariedad agravada regional", align = 'c')

save_html(Tabla_precariedad_agrav_regiones, file = "Tabla_precariedad_agravada_regiones.html")

writeLines(as.character(Tabla_precariedad_agrav_regiones), "tabla_clasificacion12.html")

Tabla_precariedad_agrav_regiones

ocupados_2024_t3_precarios_sexo <- ocupados_2024_t3_indicadores_prec_SIGNOS %>% 
  group_by(Sexo, signos.precariedad) %>% 
  summarise(Poblacion = sum(PONDERA)) %>% ungroup()

ocupados_2024_t3_precarios_sexo

proporciones_precariedad_sexo <- ocupados_2024_t3_precarios_sexo %>% group_by(Sexo)  %>% 
  summarise(proporcion_precarios = sum(Poblacion [signos.precariedad > 0]) / sum(Poblacion),
            precariedad_leve = sum(Poblacion[signos.precariedad == 1]) / sum(Poblacion),
            precariedad_media = sum(Poblacion[signos.precariedad == 2]) / sum(Poblacion),
            precariedad_severa = sum(Poblacion[signos.precariedad == 3]) / sum(Poblacion))

proporciones_precariedad_sexo

##para expresarlo en porcentajes 
proporciones_precariedad_sexo_x100 <- proporciones_precariedad_sexo %>%
  mutate(proporcion_precarios = round (proporcion_precarios * 100, 1), precariedad_leve = round (precariedad_leve * 100,1), precariedad_media = round(precariedad_media * 100, 1), precariedad_severa = round (precariedad_severa * 100, 1))

proporciones_precariedad_sexo_x100

##guardamos tabla con kabel

Tabla_prop_precariedad_sexo <- kable(proporciones_precariedad_sexo_x100, format = "html", caption = "Tabla 13: Precariedad según sexo", align = 'c')

save_html(Tabla_prop_precariedad_sexo, file = "Tabla_precariedad_sexo.html")

writeLines(as.character(Tabla_prop_precariedad_sexo), "tabla_clasificacion13.html")

Tabla_prop_precariedad_sexo

##primero pasamos la tabla a modo largo con pivot longer

proporciones_precariedad_sexo_x100_long <- proporciones_precariedad_sexo_x100 %>%  pivot_longer(cols = starts_with("pr"),
                                                                                                names_to = "variable",
                                                                                                values_to = "valor")

print(proporciones_precariedad_sexo_x100_long)


graf_preca_sexo <- ggplot(proporciones_precariedad_sexo_x100_long, aes(x = Sexo, y = valor, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(valor, 1)), vjust = -0.5) +
  labs(title = "Indices de precariedad según sexo. Total de ocupados, Argentina, 3er trimestre 2024", 
       x = "Sexo", 
       y = "% precariedad") +
  theme_minimal()

graf_preca_sexo

ggsave("graf_preca_sexo.png", width = 8, height = 6)

##SEGUN GRUPOS ETARIOS


ocupados_2024_t3_precarios_grupoet <- ocupados_2024_t3_indicadores_prec_SIGNOS %>% 
  group_by(Grupos_Etarios, signos.precariedad) %>%
  summarise(Poblacion = sum(PONDERA)) %>% ungroup()

save(ocupados_2024_t3_indicadores_prec_filtrada,"ocupados_2024_t3_indicadores_prec_filtrada.RData")




## calculamos ahora las proporciones de esto:

proporciones_precariedad_grupoet <- ocupados_2024_t3_precarios_grupoet %>% group_by(Grupos_Etarios)  %>% 
  summarise(proporcion_precarios = sum(Poblacion [signos.precariedad > 0]) / sum(Poblacion),
            precariedad_leve = sum(Poblacion[signos.precariedad == 1]) / sum(Poblacion),
            precariedad_media = sum(Poblacion[signos.precariedad == 2]) / sum(Poblacion),
            precariedad_severa = sum(Poblacion[signos.precariedad == 3]) / sum(Poblacion))

proporciones_precariedad_grupoet

##para expresarlo en porcentajes 
proporciones_precariedad_grupoet_x100 <- proporciones_precariedad_grupoet %>%
  mutate(proporcion_precarios = round (proporcion_precarios * 100, 1), precariedad_leve = round (precariedad_leve * 100,1), precariedad_media = round(precariedad_media * 100, 1), precariedad_severa = round (precariedad_severa * 100, 1))

proporciones_precariedad_grupoet_x100

##guardamos tabla con kabel


Tabla_prop_precariedad_edad <- kable(proporciones_precariedad_grupoet_x100, format = "html", caption = "Tabla 14: Precariedad según grupos etarios", align = 'c')

save_html(Tabla_prop_precariedad_edad, file = "Tabla_precariedad_edad.html")
writeLines(as.character(Tabla_prop_precariedad_edad), "tabla_clasificacion14.html")

Tabla_prop_precariedad_edad

##primero pasamos la tabla a modo largo con pivot longer

proporciones_precariedad_GE_100_long <- proporciones_precariedad_grupoet_x100 %>%  pivot_longer(cols = starts_with("pr"),
                                                                                                names_to = "variable",
                                                                                                values_to = "valor")

print(proporciones_precariedad_GE_100_long)


graf_preca_edades <- ggplot(proporciones_precariedad_GE_100_long, 
                            aes(x = Grupos_Etarios, y = valor, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(valor, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.3, 
            size = 3.5, 
            angle = 30) +
  ggtitle("Índices de precariedad según grupo etario\n",
          "Total de ocupados, Argentina, 3er trimestre 2024") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(graf_preca_edades)



graf_preca_edades

ggsave("graf_preca_edades.png", width = 8, height = 6)

ocupados_2024_t3_precarios_NE <- ocupados_2024_t3_indicadores_prec_SIGNOS %>% 
  group_by(Nivel_Educativo, signos.precariedad) %>% 
  summarise(Poblacion = sum(PONDERA)) %>% ungroup()

ocupados_2024_t3_precarios_NE

## calculamos ahora las proporciones de esto:

proporciones_precariedad_NE <- ocupados_2024_t3_precarios_NE %>% group_by(Nivel_Educativo)  %>% 
  summarise(proporcion_precarios = sum(Poblacion [signos.precariedad > 0]) / sum(Poblacion),
            precariedad_leve = sum(Poblacion[signos.precariedad == 1]) / sum(Poblacion),
            precariedad_media = sum(Poblacion[signos.precariedad == 2]) / sum(Poblacion),
            precariedad_severa = sum(Poblacion[signos.precariedad == 3]) / sum(Poblacion))

proporciones_precariedad_NE

##para expresarlo en porcentajes 
proporciones_precariedad_NE_x100 <- proporciones_precariedad_NE %>%
  mutate(proporcion_precarios = round (proporcion_precarios * 100, 1), precariedad_leve = round (precariedad_leve * 100,1), precariedad_media = round(precariedad_media * 100, 1), precariedad_severa = round (precariedad_severa * 100, 1))

proporciones_precariedad_NE_x100

##guardamos tabla con kabel


Tabla_prop_precariedad_NE <- kable(proporciones_precariedad_NE_x100, format = "html", caption = "Tabla 15: Precariedad según nivel educativo", align = 'c')

save_html(Tabla_prop_precariedad_NE, file = "Tabla_precariedad_NE.html")

writeLines(as.character(Tabla_prop_precariedad_NE), "tabla_clasificacion15.html")

Tabla_prop_precariedad_NE

##primero pasamos la tabla a modo largo con pivot longer

proporciones_precariedad_NE_x100_long <- proporciones_precariedad_NE_x100 %>%  pivot_longer(cols = starts_with("pr"),
                                                                                            names_to = "variable",
                                                                                            values_to = "valor")

print(proporciones_precariedad_NE_x100_long)


graf_preca_NE <- ggplot(proporciones_precariedad_NE_x100_long, aes(x = Nivel_Educativo, y = valor, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(valor, 1)), vjust = -0.5) +
  labs(title = "Indices de precariedad según nivel educativo. Total de ocupados, Argentina, 3er trimestre 2024", 
       x = "Nivel educativo", 
       y = "% precariedad") +
  theme_minimal()

graf_preca_NE

ggsave("graf_preca_NE.png", width = 8, height = 6)

save(Mercado_trabajo_edades, file = "Mercado_trabajo_edades.RData")
getwd()

save(ocupados_2024_t3_indicadores_prec_SIGNOS, file = "ocupadosyprecariedad.RData")
getwd()

ocupadosyprecariedad <- ocupadosyprecariedad.RData
