library(readxl);library(tidyverse);library(cowplot);theme_set(theme_cowplot());library(leaflet)
setwd("C:/Users/walkerro/Desktop/r scripts/terras/terras2")

#indigenous deaths https://caci.cimi.org.br/#!/caso/8695/?loc=-3.008869978848142,-58.97460937500001,5&init=true
#brasil map of municipios https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html
#brasilian homicides (all) https://www.ipea.gov.br/atlasviolencia/filtros-series

df <- read.csv("casos.csv") #1470 raw data of indigenous homicides thru 2023
colSums(is.na(df)) #check for missing data
hist(df$idade,20) #age at death

df |> 
  group_by(year) |>
  summarize(count = n()) |>
  ggplot(aes(x = year, y = count)) + geom_bar(stat = "identity") 

leaflet(df) |>
  addTiles() |>
  setView(lng = -60, lat = 0, zoom = 5) |>
  addAwesomeMarkers(
    lat = ~lat,
    lng = ~long,
    icon=~makeAwesomeIcon(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'orange'),
    label=~as.character(povo),
    popup = ~paste("Name: ", nome),
    clusterOptions = markerClusterOptions() )

#leaflet() %>%
#  addTiles() %>%
#  setView(lng = -60, lat = 0, zoom = 5) %>%
#  addMarkers(data = data.frame(lng = c(-48.15252304		), lat = c(-2.416904681)))
 
#convert points to sf
library(sf)
df_sf <- sf::st_as_sf(df, coords = c("long","lat"))

#plot shapefile of brazilian municipios
my_sf <- st_read(file.path("C:/Users/walkerro/Desktop/R scripts/terras/terras2/BR_Municipios_2022", "BR_Municipios_2022.shp"))
#plot(st_geometry(my_sf))
colSums(is.na(my_sf))

#combine points and municipios
st_crs(my_sf)
df_sf <- sf::st_set_crs(df_sf, 4674)
(my_sf$pt_count <- lengths(st_intersects(my_sf, df_sf)))
plot(my_sf[5145, 1], reset = FALSE, col = "grey")
plot(df_sf, add = TRUE)

#bring in all homicides
hs <- read.csv("homicidios.csv", sep=';')
min(df_sf$year) #hs <- hs[hs$período >= 1985,]
colSums(is.na(hs))
hs_yr <- hs |> group_by(cod) |> summarise(homs = sum(valor,na.rm=T))
hist(log(hs_yr$homs+1))
table(hs_yr$homs)

hs_yr$cod %in% my_sf$CD_MUN
d <- merge(my_sf, hs_yr, by.x ="CD_MUN", by.y="cod")
nrow(d[d$pt_count > 0,])

#table 3 from https://indigenas.ibge.gov.br/estudos-especiais-3.html (click on ODS)
t3 <- read.csv("table3_2010.csv")
d2 <- merge(d, t3, by.x ="CD_MUN", by.y="cod")

d3 <- d2 |> dplyr::select(CD_MUN, pt_count, homs, SIGLA_UF, nonindpop2010, Total, AREA_KM2, Rural, Urbana) |> filter(Total > 0, homs > 0) #this kicks out ~1k municipios (no indigenous pop)
nrow(d3[d3$pt_count > 0,])
table(d3$pt_count)
hist(log(d3$Total),100)

#merge in deforestation from https://basedosdados.org/dataset/e5c87240-ecce-4856-97c5-e6b84984bf42?table=d7a76d45-c363-4494-826d-1580e997ebf0
t4 <- read.csv("br_inpe_prodes_municipio_bioma.csv")
t4 <- t4 |> arrange(id_municipio, desc(ano)) |>
 group_by(id_municipio) |>
 summarize(deforest = last(desmatado),
           biome = last(bioma))
d3 <- merge(d3, t4, by.x ="CD_MUN", by.y="id_municipio")

#get rural info https://www.ibge.gov.br/geociencias/organizacao-do-territorio/tipologias-do-territorio/15790-classificacao-rural-e-urbana.html?=&t=downloads
my_sf2 <- st_read(file.path("C:/Users/walkerro/Desktop/R scripts/terras/terras2/RurUrb", "RurUrb.shp"))
#plot(st_geometry(my_sf2))
colSums(is.na(my_sf2))
#remotidao_	Distância em minutos (via terrestre ou aquática) para a Metrópole mais próxima
#Pop_Urb_Mu	População Urbana do município
#Pop_Rur_Mu	População Rural do município
#Pop_Tot_Mu	População Total do município
#Perc_Urb_U	Percentual da população urbana na Unidade Populacional 
#Tipologia_	Classificação Rural-Urbano
my_sf2_df <- data.frame(my_sf2)
d3b <- data.frame(d3)
d3c <- merge(d3b, my_sf2_df, by.x ="CD_MUN", by.y="BaseRurU_1", all.x=T)
names(d3c)
cor(d3c$Pop_Tot_Mu, d3c$nonindpop2010)
#ggplot(d3c, aes(y=pt_count+1, x=remotidao_)) + geom_point() + geom_smooth() + scale_y_log10()

#how to interpet typology of munis?
d3c |> #arrange(id_municipio, desc(ano)) |>
  group_by(Tipologia_) |>
  summarize(remotidao_ = median(remotidao_, na.rm=T),
            remotidao1 = median(remotidao1, na.rm=T),
            remotida_1 = median(remotida_1, na.rm=T),
            remotida_2 = median(remotida_2, na.rm=T),
            remotida_3 = median(remotida_3, na.rm=T),
            remotida_4 = median(remotida_4, na.rm=T),
            remotida_5 = median(remotida_5, na.rm=T),
            remotida_6 = median(remotida_6, na.rm=T),
            remotida_7 = median(remotida_7, na.rm=T)
            )

#add in pib https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html (2021 pib municipios)
pib <- read_xlsx("pib.xlsx")
pib <- pib |> #arrange(id_municipio, desc(ano)) |>
  group_by(muni) |>
  summarize(pibbruto = median(pibbruto, na.rm=T),
            pibbrutopc = median(pibbrutopc, na.rm=T)  )
d3c <- merge(d3c, pib, by.x ="CD_MUN", by.y="muni", all.x=T)

#add in unemployment rates (indigenous) https://datasus.saude.gov.br/informacoes-de-saude-tabnet/ 
un <- read_xlsx("desempreg_ind.xlsx")
un$muni <- substr(un$muni, 0, 6)
hist(un$desempreg_indigena)
d3c$muni6 <- substr(d3c$CD_MUN, 0, 6) 
d3c <- merge(d3c, un, by.x ="muni6", by.y="muni", all.x=T)

#http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/obt10br.def
#add in data sus indigenous homicides Óbitos p/Residênc segundo Município Causa - CID-BR-10: . 110 Agressões Cor/raça: Indígena Período: 1996-2023
dh <- read_xlsx("datasus_homicide_ind.xlsx")
dh$muni <- substr(dh$muni, 0, 6)
hist(dh$deaths_ind, 50)
d3c$muni6 <- substr(d3c$CD_MUN, 0, 6) 
d3c <- merge(d3c, dh, by.x ="muni6", by.y="muni", all.x=T)
d3c$deaths_ind[is.na(d3c$deaths_ind)] <- 0
colSums(is.na(d3c))

library(MASS)
summary(m <- glm.nb( homs ~ 1 + log(nonindpop2010) + Tipologia_, d3c )) #confint(m)
summary(m <- glm.nb( pt_count ~ 1 + log(homs) + log(nonindpop2010) + log(Total), d3[d3$pt_count > 0,] )) #confint(m)

#check these out
summary(m <- glm.nb( pt_count ~ 0 + Tipologia_ + log(homs) + log(nonindpop2010) + log(Total), d3c )) #SIGLA_UF confint(m)
summary(m <- glm.nb( deaths_ind ~ 0 + Tipologia_ + log(homs) + log(nonindpop2010) + log(Total), d3c )) #SIGLA_UF confint(m)


library(visreg)
visreg(m, gg=T, scale = 'response')
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = m)
plot(simulationOutput) #be careful using base::plot on a glm model

d3$pred <- fitted(m, type = "response")
min(d3$pred)
plot(d3$pred, d3$pt_count)
hist(d3$pred/d3$Total)
ggplot(data = d3) + geom_sf(aes(fill = pred/Total))

#plot map1
d3$deaths_ind <- d3c$deaths_ind
d3c$homicides <- d3c$homs - d3c$deaths_ind
  
library(viridis);library(ggplot2);library(sf);library(dplyr);library(ggspatial);library(maps);library(leaflet);library(rnaturalearth);library(cowplot);theme_set(theme_cowplot())
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = d, color='gray') +
  geom_sf(data = d3[d3$deaths_ind > 0,], aes(fill = deaths_ind/Total*100000/27)) + #estimated indig homicide rate per 100,000 per year (data includes 27 years)
  scale_fill_viridis(name = "Indigenous\nhomicides\nper 100k", trans = "log", breaks = c(10,100,1000), labels = c(10,100,1000)) +
  coord_sf(xlim = c(-82, -33), ylim = c(-36, 13), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  #ggtitle("Map", subtitle = "(with ?)") +
  theme(legend.position = c(.8,.85), 
        #panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue")) +
  annotate("text", x = -56.5, y=-23, label = "2", color="black", size=3) +
  # annotate("text", x = -72, y=-11, label = "2", color="black", size=3) +
  annotate("text", x = -63, y=5, label = "1", color="black", size=3)
#ggsave("map.pdf", height =7, width =8)

#overall indigenous homicide rate
sum(d3$deaths_ind)/sum(d3$Total) *100000 /27 #18.14

#zinb
library(pscl)
m1 <- zeroinfl(pt_count ~ 1 + log(homs) + log(nonindpop2010) + log(Total) | log(Total),
               data = d3, dist = "negbin")
summary(m1)
m1 <- zeroinfl(pt_count ~ 1 + log(homs) + log(nonindpop2010) + log(Total) | log(Total),
               data = d3, dist = "poisson")
summary(m1)

#get centroids (omits ~1k municipios w/out indigenous pop)
centroids_sf <- d3 %>%
  #group_by(site) %>% summarize(geometry = st_union(geometry)) %>% 
  st_centroid

d4 <- centroids_sf %>%
  dplyr::mutate(long = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])
#testSpatialAutocorrelation(simulationOutput, x =  d4$long, y = d4$lat)

d4$Tipologia_ <- d3c$Tipologia_
d4$remotidao_  <- d3c$remotidao_ 
d4$pibbruto <- d3c$pibbruto
d4$pibbrutopc <- d3c$pibbrutopc
d4$biome <- d3c$biome
d4$deforest <- d3c$deforest
d4$desempreg <- d3c$desempreg_indigena
d4$deaths_ind <- d3c$deaths_ind
sum(d4$deaths_ind)
#d4$ratio <- d3c$ratio
d4$homicides <- d3c$homicides

#General additive model (gam)
library(mgcv);library(gamm4)
table(d4$Tipologia_)
d4$Tipologia <- factor(d4$Tipologia_)
d4 <- within(d4, Tipologia <- relevel(Tipologia, ref = 4))
library(forcats)
d4$Tipologia <- fct_collapse(d4$Tipologia, rural = c("RuralRemoto", "IntermediarioAdjacente","IntermediarioRemoto","RuralAdjacente"))
table(d4$Tipologia, d4$Tipologia_)

d4$loghoms <- log(d4$homs)
d4$lognonindpop2010 <- log(d4$nonindpop2010) 
d4$logtotal <- log(d4$Total)

d4$loghoms_c <- d4$loghoms - mean(d4$loghoms, na.rm=T)
d4$lognonindpop2010_c <- d4$lognonindpop2010 - mean(d4$lognonindpop2010, na.rm=T)
d4$logtotal_c <- d4$logtotal - mean(d4$logtotal, na.rm=T)

summary(m.gam1 <- mgcv::gam(homs ~ 0 + log(deaths_ind + 1) + Tipologia + lognonindpop2010_c + logtotal_c + # SIGLA_UF
                      s(lat, long, k = 200), #play with k
                      data = d4, method = 'REML', family = nb() )) #?family.mgcv poisson or nb or tw ziP cnorm scat
summary(m.gam2 <- mgcv::gam(deaths_ind ~ 0 + loghoms_c + Tipologia + lognonindpop2010_c + logtotal_c + # SIGLA_UF
                              s(lat, long, k = 200), #play with k
                            data = d4, method = 'REML', family = nb() )) #?family.mgcv poisson or nb or tw ziP cnorm scat
gam.check(m.gam2)
#plot(m.gam1,pages=1,residuals=TRUE)
vis.gam(m.gam2, view = c("long", "lat"), plot.type = "contour", too.far = .1) #persp or contour #plot(m.gam, scheme = 2)
library(tidymv)
plot_smooths(
  model = m.gam2,
  series = remotidao_, transform = exp, series_length = 10
  #comparison = fac
  ) + theme(legend.position = "top")
#visreg(m.gam1, gg=T, scale = 'response')
simulationOutput <- simulateResiduals(fittedModel = m.gam2)
plot(simulationOutput)
testSpatialAutocorrelation(simulationOutput, x =  d4$long, y = d4$lat)

#write_sf(d4, "d4.shp")
#d4 <- st_read(file.path("C:/Users/walkerro/Desktop/R scripts/terras/terras2", "d4.shp"))

library(brms)
get_prior(deaths_ind ~ 0 + Tipologia + loghoms_c + lognonindpop2010_c + logtotal_c + 
            (0 + Tipologia + loghoms_c + lognonindpop2010_c + logtotal_c | SIGLA_UF) +
             s(lat, long, k=200),  #gp(lat,long),
          data = d4, family = negbinomial())

#fit_a is s(), fit_a2 is gp
fit_a <- brm(deaths_ind ~ 0 + Tipologia + loghoms_c + lognonindpop2010_c + logtotal_c +
               (0 + Tipologia + loghoms_c + lognonindpop2010_c + logtotal_c | SIGLA_UF) +
               s(long, lat, k=200), #gp(long,lat),  
             data = d4, family = negbinomial(),
             prior = c(prior(normal(1, 1), class = b, coef=loghoms_c),
                       prior(normal(-1, 1), class = b, coef=lognonindpop2010_c),
                       prior(normal(1, 1), class = b, coef=logtotal_c),
                       #prior(normal(0, 1), class = b, coef=slatlong_1), prior(normal(0, 1), class = b, coef=slatlong_2),
                       prior(normal(-3, 1), class = b, coef=TipologiaUrbano),
                       prior(normal(-3, 1), class = b, coef=Tipologiarural),
                       prior(lkj(2), class = cor),
                       prior(exponential(1), class = sd),
                       prior(exponential(1), class = sds)), #sds if s() sdgp if gp()
             iter = 1e4, warmup = 5e3, chains = 4, cores = 4,
             sample_prior = T, control = list(adapt_delta = 0.99))
fit_a <- readRDS("fit_a.Rds")
#plot(fit_a)
summary(fit_a)
plot(hypothesis(fit_a, "loghoms_c = 0"))
plot(hypothesis(fit_a, "Tipologiarural = 0"))
plot(hypothesis(fit_a, "lognonindpop2010_c = 0"))
plot(hypothesis(fit_a, "logtotal_c = 0"))
ranef(fit_a)
pp_check(fit_a, nsamples=100) + xlim(c(0,5))
prior_summary(fit_a)
plot(conditional_smooths(fit_a,too_far = .1))
conditional_effects(fit_a, #https://bookdown.org/content/4857/conditional-manatees.html
                    spaghetti = T, 
                    ndraws = 200) %>% 
  plot(points = T,
       point_args = c(alpha = 1/2, size = 1),
       line_args = c(colour = "black"))

library(DHARMa);library(DHARMa.helpers)
simres <- dh_check_brms(fit_a,integer=T)
testSpatialAutocorrelation(simres, x =  d4$long, y = d4$lat)
bayes_R2(fit_a) #.68

library(sjPlot)
tab_model(fit_a)
#saveRDS(fit_a, "fit_a.Rds")

#proportion graph
library(tidybayes);library(modelr);library(RColorBrewer)
sp <- d4 %>%
  data_grid(lognonindpop2010_c = log(1000) - mean(d4$lognonindpop2010, na.rm=T), #seq_range(lognonindpop2010_c, n = 10), 
            logtotal_c = seq_range(logtotal_c, n = 100), 
            #logtotal_c = 0, #max(d4$logtotal_c),
            Tipologia = "rural",
            loghoms_c = 0, #max(d4$loghoms_c),
            lat =  -14.2350,
            long = -51.9253,
            SIGLA_UF = NA,
            re_formula = NA) #%>% add_epred_draws(fit_b, ndraws = 10) %>%

ft <- fitted(fit_a, re_formula =NA, probs = c(0.025, 0.975),
               newdata = sp) %>% 
  data.frame() %>% 
  bind_cols(sp) %>% 
  mutate(nonindpop2010 = exp(lognonindpop2010_c + mean(d4$lognonindpop2010, na.rm=T)),
         total = exp(logtotal_c + mean(d4$logtotal, na.rm=T)))

sp2 <- d4 %>%
  data_grid(lognonindpop2010_c = log(10000) - mean(d4$lognonindpop2010, na.rm=T), #seq_range(lognonindpop2010_c, n = 10), 
            logtotal_c = seq_range(logtotal_c, n = 100), 
            #logtotal_c = 0, #max(d4$logtotal_c),
            Tipologia = "rural",
            loghoms_c = 0, #max(d4$loghoms_c),
            lat =  -14.2350,
            long = -51.9253,
            SIGLA_UF = NA,
            re_formula = NA) #%>% add_epred_draws(fit_b, ndraws = 10) %>%

ft2 <- fitted(fit_a, re_formula =NA, probs = c(0.025, 0.975),
             newdata = sp2) %>% 
  data.frame() %>% 
  bind_cols(sp2) %>% 
  mutate(nonindpop2010 = exp(lognonindpop2010_c + mean(d4$lognonindpop2010, na.rm=T)),
         total = exp(logtotal_c + mean(d4$logtotal, na.rm=T)))

plot1 <- ggplot(ft, aes(x = total)) +
  geom_smooth(aes(y = Estimate/total*100000/27, ymin = Q2.5/total*100000/27, ymax = Q97.5/total*100000/27),
              stat = "identity",
              fill = "blue", color = "blue", alpha = 1/7, linewidth = 1/4) +
  geom_smooth(data=ft2, aes(y = Estimate/total*100000/27, ymin = Q2.5/total*100000/27, ymax = Q97.5/total*100000/27),
              stat = "identity", 
              fill = "black", color = "black", alpha = 1/7, linewidth = 1/4) +
  scale_color_gradientn(colours = colorspace::diverge_hcl(5), name="Age (kya)") +
  coord_cartesian(xlim = c(10,20000), #range(d2$nonindpop2010), 
                  ylim = c(0,250)) + #range(d2$pt_count)) +
  theme_cowplot() +
  theme(panel.grid = element_blank()) +
  scale_x_log10(labels = label_comma()) + #breaks = c(1e3, 2.5e3, 5e3)
  #scale_y_log10(labels = label_comma()) +
  labs(y = "Indigenous\nhomicides per 100k",
       x = "Indigenous population") +
  annotate("text", x = 140, y=90, label = "Nonindigenous population = 1k", color="blue", size=3) +
  annotate("text", x = 50, y=2, label = "Nonindigenous population = 10k", color="black", size=3)
plot1

library(patchwork)

#proportion graph for nonindigenous
library(tidybayes);library(modelr);library(RColorBrewer)
sp3 <- d4 %>%
  data_grid(logtotal_c = log(10) - mean(d4$logtotal, na.rm=T), #seq_range(lognonindpop2010_c, n = 10), 
            lognonindpop2010_c = seq(from=-4, to=7, by=.2), #seq_range(lognonindpop2010_c, n = 100), 
            #logtotal_c = 0, #max(d4$logtotal_c),
            Tipologia = "rural",
            loghoms_c = 0, #max(d4$loghoms_c),
            lat =  -14.2350,
            long = -51.9253,
            SIGLA_UF = NA,
            re_formula = NA) #%>% add_epred_draws(fit_b, ndraws = 10) %>%

sp4 <- d4 %>% data_grid(logtotal_c = log(10000) - mean(d4$logtotal, na.rm=T), #seq_range(lognonindpop2010_c, n = 10), 
                 lognonindpop2010_c = seq(from=-4, to=7, by=.2), #seq_range(lognonindpop2010_c, n = 100), 
                 #logtotal_c = 0, #max(d4$logtotal_c),
                 Tipologia = "rural",
                 loghoms_c = 0, #max(d4$loghoms_c),
                 lat =  -14.2350,
                 long = -51.9253,
                 SIGLA_UF = NA,
                 re_formula = NA) #%>% add_epred_draws(fit_b, ndraws = 10) %>%

ft3 <- fitted(fit_a, re_formula =NA, probs = c(0.025, 0.975),
             newdata = sp3) %>% 
  data.frame() %>% 
  bind_cols(sp3) %>% 
  mutate(nonindpop2010 = exp(lognonindpop2010_c + mean(d4$lognonindpop2010, na.rm=T)),
         total = exp(logtotal_c + mean(d4$logtotal, na.rm=T)))

ft4 <- fitted(fit_a, re_formula =NA, probs = c(0.025, 0.975),
              newdata = sp4) %>% 
  data.frame() %>% 
  bind_cols(sp4) %>% 
  mutate(nonindpop2010 = exp(lognonindpop2010_c + mean(d4$lognonindpop2010, na.rm=T)),
         total = exp(logtotal_c + mean(d4$logtotal, na.rm=T)))

plot <- ggplot(ft3, aes(x = nonindpop2010)) +
  geom_smooth(aes(y = Estimate/total*100000/27, ymin = Q2.5/total*100000/27, ymax = Q97.5/total*100000/27),
              stat = "identity",
              fill = "firebrick", color = "firebrick", alpha = 1/7, linewidth = 1/4) +
  geom_smooth(data=ft4, aes(y = Estimate/total*100000/27, ymin = Q2.5/total*100000/27, ymax = Q97.5/total*100000/27),
              stat = "identity", 
              fill = "black", color = "black", alpha = 1/7, linewidth = 1/4) +
  scale_color_gradientn(colours = colorspace::diverge_hcl(5), name="Age (kya)") +
  coord_cartesian(xlim = c(1000,100000), #range(d2$nonindpop2010), 
                  ylim = c(0,250)) + #range(d2$pt_count)) +
  theme_cowplot() +
  theme(panel.grid = element_blank()) +
  scale_x_log10(breaks = c(1e3, 1e4, 1e5), labels = label_comma()) +
  #scale_y_log10(labels = label_comma()) +
  labs(y = "Indigenous\nhomicides per 100k",
       x = "Nonindigenous population") +
  annotate("text", x = 2000, y=-1, label = "Indigenous population = 10k", color="black", size=3) +
  annotate("text", x = 3100, y=100, label = "Indigenous population = 10", color="firebrick", size=3)
plot
(plot / plot1) + plot_annotation(tag_levels ="A")
ggsave("plot.pdf", height = 6, width=6)


#look at ratio of nonind to indigenous population 
plt1 <- ggplot(d3, aes(x = 100*Total/(Total+nonindpop2010), y = (deaths_ind+1)/Total*100000/27)) +
  geom_point(size = .2) +
  geom_smooth(se=F, method = loess, #span=10,
              #stat = "identity",
              #fill = "blue", 
              linewidth = .5, color = "blue") +
  scale_color_gradientn(colours = colorspace::diverge_hcl(5), name="Age (kya)") +
  coord_cartesian(xlim = c(0,30)) +#, #range(d2$nonindpop2010), 
            #     ylim = c(0,2500)) + #range(d2$pt_count)) +
  theme_cowplot() +
  theme(panel.grid = element_blank()) +
  #scale_x_log10(labels = label_comma(), breaks = c(.01, .1, 1, 10, 100)) +
  scale_y_log10(labels = label_comma()) +
  labs(y = "Indigenous\nhomicides per 100k",
       x = "Percent population indigenous")
plt1
(plot / plot1 / plt1) + plot_annotation(tag_levels ="A")
#ggsave("plot.pdf", height = 9, width=7)




sp <- d4 %>%
  data_grid(lognonindpop2010_c = seq_range(lognonindpop2010_c, n = 100), 
            logtotal_c = seq_range(logtotal_c, n = 100), 
            #logtotal_c = 0, #max(d4$logtotal_c),
            Tipologia = "rural",
            loghoms_c = 0, #max(d4$loghoms_c),
            lat =  -14.2350,
            long = -51.9253,
            SIGLA_UF = NA,
            re_formula = NA) #%>% add_epred_draws(fit_b, ndraws = 10) %>%

ft <- fitted(fit_b, re_formula =NA, probs = c(0.025, 0.975),
             newdata = sp) %>% 
  data.frame() %>% 
  bind_cols(sp) %>% 
  mutate(nonindpop2010 = exp(lognonindpop2010_c + mean(d4$lognonindpop2010, na.rm=T)),
         total = exp(logtotal_c + mean(d4$logtotal, na.rm=T)))

plt1 <- ggplot(ft[ft$total>0,], aes(x = total/(total+nonindpop2010))) +
  geom_smooth(aes(y = Estimate/total*100000/27), se=F, method = loess, span=2,
              #stat = "identity",
              #fill = "blue", 
            color = "blue") +
 scale_color_gradientn(colours = colorspace::diverge_hcl(5), name="Age (kya)") +
 # coord_cartesian(xlim = c(10,30000), #range(d2$nonindpop2010), 
#                  ylim = c(0,250)) + #range(d2$pt_count)) +
  theme_cowplot() +
  theme(panel.grid = element_blank()) +
  #scale_x_log10() + #breaks = c(1e3, 2.5e3, 5e3)
  #scale_y_log10(labels = label_comma()) +
  labs(y = "Indigenous\nhomicides per 100k",
       x = "Fraction of population indigenous")
plt1


#homs effect
sp4 <- d4 %>%
  data_grid(logtotal_c = log(100) - mean(d4$logtotal, na.rm=T), #seq_range(lognonindpop2010_c, n = 10), 
            lognonindpop2010_c = log(1000) - mean(d4$lognonindpop2010, na.rm=T), 
            Tipologia = "intermediate",
            loghoms_c = seq_range(loghoms_c, n = 100), 
            lat =  -14.2350,
            long = -51.9253,
            SIGLA_UF = NA,
            re_formula = NA) #%>% add_epred_draws(fit_b, ndraws = 10) %>%

ft4 <- fitted(fit_b, re_formula =NA, #probs = c(0.25, 0.75),
              newdata = sp4) %>% 
  data.frame() %>% 
  bind_cols(sp4) %>% 
  mutate(nonindpop2010 = exp(lognonindpop2010_c + mean(d4$lognonindpop2010, na.rm=T)),
         total = exp(logtotal_c + mean(d4$logtotal, na.rm=T)),
         homs = exp(loghoms_c + mean(d4$loghoms, na.rm=T)))

plot3 <- ggplot(ft4, aes(x = homs)) +
  geom_smooth(aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "black", color = "black", alpha = 1/7, linewidth = 1/4) +
  scale_color_gradientn(colours = colorspace::diverge_hcl(5), name="Age (kya)") +
  #coord_cartesian(xlim = c(1000,5000)) + #range(d2$nonindpop2010), 
  #                ylim = c(0,22)) + #range(d2$pt_count)) +
  theme_cowplot() +
  theme(panel.grid = element_blank()) +
  scale_x_log10() + #breaks = c(1e3, 2.5e3, 5e3)) +
  scale_y_log10(labels = label_comma()) 
  #labs(y = "Indigenous\nhomicides per capita",
  #     x = "Nonidigenous population") +
  #annotate("text", x = 50, y=.017, label = "Nonindigenous population = 1,000", color="blue", size=3) +
  #annotate("text", x = 1270, y=.012, label = "Indigenous population = 100", color="black", size=3)
plot3
#(plot2 / plot / plot1) + plot_annotation(tag_levels ="A")
#ggsave("plot.pdf", height = 9, width=7)



# interpolation target grid
grid <- tibble(expand.grid(lognonindpop2010_c = seq(from = min(d4$lognonindpop2010_c), to = max(d4$lognonindpop2010_c), length.out = 100),
                                  logtotal_c = seq(from = min(d4$logtotal_c), to = max(d4$logtotal_c), length.out = 100), 
                      Tipologia = "intermediate",
                      loghoms_c = 0, #max(d4$loghoms_c),
                      lat =  -14.2350,
                      long = -51.9253,
                      SIGLA_UF = NA))
fits <- fitted(fit_b, re_formula =NA, probs = c(0.025, 0.975),
               newdata = grid) %>% 
  data.frame() %>% 
  bind_cols(grid) %>% 
  mutate(nonindpop2010 = exp(lognonindpop2010_c + mean(d4$lognonindpop2010, na.rm=T)),
         total = exp(logtotal_c + mean(d4$logtotal, na.rm=T)))

ggplot(fits, aes(logtotal_c, lognonindpop2010_c, fill= Estimate/total)) + 
  geom_tile()


#make fitted map
d4b <- d4
#d4b$loghoms_c <- 0 #set any parameters here
preds <- fitted(fit_a, d4b, type = 'response')
d3$preds <- preds[,1]

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = d, color='gray') +
  geom_sf(data = d3, aes(fill = preds/Total*100000/27)) +
  scale_fill_viridis(name = "Indigenous\nhomicides\nper 100k\n(predicted)", trans = "log", breaks = c(1,10,100), labels = c(1, 10,100)) +
  coord_sf(xlim = c(-82, -33), ylim = c(-36, 13), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  #ggtitle("Map", subtitle = "(with ?)") +
  theme(legend.position = c(.8,.85), 
        #panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue")) +
  annotate("text", x = -56.5, y=-23, label = "2", color="black", size=3) +
 # annotate("text", x = -72, y=-11, label = "2", color="black", size=3) +
  annotate("text", x = -63, y=5, label = "1", color="black", size=3)
#ggsave("map2.pdf", height =7, width =8)





#nonspatial model
get_prior(pt_count ~ 0 + Tipologia + loghoms_c + lognonindpop2010_c * logtotal_c ,
          data = d4, family = negbinomial())
fit_a <- brm(pt_count ~ 0 + Tipologia + loghoms_c + lognonindpop2010_c * logtotal_c ,
               #(0 + Tipologia + loghoms_c + lognonindpop2010_c * logtotal_c | SIGLA_UF) +
               #s(lat, long, k=200), 
             data = d4, family = negbinomial(),
             prior = c(prior(normal(1, 1), class = b, coef=loghoms_c),
                       prior(normal(-1, 1), class = b, coef=lognonindpop2010_c),
                       prior(normal(1, 1), class = b, coef=logtotal_c),
                       prior(normal(0, 1), class = b, coef=lognonindpop2010_c:logtotal_c),
                      # prior(normal(0, 1), class = b, coef=slatlong_1),
                      # prior(normal(0, 1), class = b, coef=slatlong_2),
                       prior(normal(-5, 1), class = b, coef=Tipologiaintermediate),
                       prior(normal(-5, 1), class = b, coef=TipologiaUrbano),
                       prior(normal(-5, 1), class = b, coef=TipologiaRuralRemoto)
                       #prior(exponential(1), class = sd)
                       #prior(exponential(1), class = sds)
                      ), #sdgp if gp()
             iter = 2e3, warmup = 1e3, chains = 2, cores = 4,
             sample_prior = T, control = list(adapt_delta = 0.99))
summary(fit_a)
plot(hypothesis(fit_a, "loghoms_c = 0"))
plot(hypothesis(fit_a, "Tipologiaintermediate = 0"))
plot(hypothesis(fit_a, "lognonindpop2010_c = 0"))
plot(hypothesis(fit_a, "logtotal_c = 0"))
plot(hypothesis(fit_a, "lognonindpop2010_c:logtotal_c = 0"))

pp_check(fit_a, nsamples=100) + xlim(c(0,5))
prior_summary(fit_a)
#plot(conditional_smooths(fit_a,too_far = .1))
conditional_effects(fit_a, #https://bookdown.org/content/4857/conditional-manatees.html
                    spaghetti = T, 
                    ndraws = 200) %>% 
  plot(points = T,
       point_args = c(alpha = 1/2, size = 1),
       line_args = c(colour = "black"))

library(DHARMa);library(DHARMa.helpers)
simres <- dh_check_brms(fit_a,integer=T)
testSpatialAutocorrelation(simres, x =  d4$long, y = d4$lat)
bayes_R2(fit_a) #.47

saveRDS(fit_a, "fit_a.Rds") 
fit_a <- readRDS("fit_a.Rds")

# compare model fit
fit_a <- add_criterion(fit_a, criterion = c("loo"))
fit_b <- add_criterion(fit_b, criterion = c("loo"))
loo_compare(fit_a, fit_b) #bayes_factor(fit_a, fit_b) save_pars = save_pars(all = TRUE)






