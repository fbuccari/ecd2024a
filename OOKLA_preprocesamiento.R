#Script de pre-procesamiento datos OOKLA
rm(list=ls())
#install.packages("remotes")
remotes::install_github("teamookla/ooklaOpenDataR")
library(ooklaOpenDataR)
library(sf)
library(mapview)
library(dplyr)
library(naniar)
library(tmap)
library(tmaptools)
library(ggplot2)
library(tmap)
library(viridis)
#install.packages('sfarrow')
library(sfarrow)
#install.packages("geoarrow")
#library(geoarrow)
sf_use_s2(FALSE)
remove.packages("DBI")
install.packages("parquetize")

library(parquetize)

#Directorios
folder_mapas= "C:/Users/mbuccari/OneDrive - CAF/RED 2025 - C8/Base de datos/Mapas/"
folder_work="C:/Users/mbuccari/OneDrive - CAF/RED 2025 - C8/Base de datos/TICs/"

#Leo los archivos con los shapes de los subnacionales Nivel 1(provincias / estados) y Nivel 2 
#(gobiernos locales/municipios)
lac1 = st_read(paste0(folder_mapas, "Nivel 1/Nivel 1 - v3.gpkg"), promote_to_multi = FALSE)
lac2 = st_read(paste0(folder_mapas, "Nivel 2/Nivel 2 - v3 (fixed).gpkg"), promote_to_multi = FALSE)
#En el caso de argentina y uruguay, tomo el Nivel1, ya que sus municipios no comprenden todo el territorio
#Cambio el coordinate reference system (crs) para poder hacer el rbind
crsl2=st_crs(lac2)
lac1=st_transform(lac1, crs =crsl2 )
lac1$NAME_2=NA
lac2=lac2[,which(colnames(lac2)!="fid")]
cl1=c("Argentina", "Uruguay")
lacsf=rbind(lac1[lac1$COUNTRY %in% cl1,], lac2[!(lac2$COUNTRY %in% cl1),])
d=st_read(paste0(folder_work, "cleandata/comp.gpkg"))

#Hay algunos shps en la base subnacionales (lac1 y lac2) que pueden ser lagunas u otros que no corresponden 
#a niveles de gobierno, los saco. 
lacsf=lacsf %>% filter(es_gob==1)


#Abro archivos con datos de población y superficie de subnacionales
pob = readxl::read_excel("C:/Users/mbuccari/OneDrive - CAF/RED 2025 - C8/Base de datos/TICs/rawdata/SUBNACIONALES - Niveles de gobierno.xlsx")
keep=c("codigo_unico", "id_code", "region", "población", "superficie_km2", "densidad_pobl")
pob=pob[,colnames(pob) %in% keep]
pob=pob %>% rename(codigo_uni=codigo_unico)

#Abro archivo con datos de indicadores ODS
ods = readxl::read_excel("C:/Users/mbuccari/OneDrive - CAF/RED 2025 - C8/Base de datos/TICs/rawdata/Base completa de datos censales (ODS).xlsx")
drop=c("id_pais", "nombre_pais", "anio_censo", "nivel", "nombre_nivel", "es_gobierno", "codigo_subnacional", 
       "codigo_arg_2010", "nombre_subnacional", "codigo_nivel_1", "nombre_nivel_1","codigo_nivel_2", "nombre_nivel_2")
ods=ods[,!(colnames(pob) %in% drop)]
ods=ods %>% rename(codigo_uni=codigo_unico)
ods=ods %>% filter(is.na(codigo_uni)==F)

##Trabajo con datos de OOKLA


for (i in c(1:4)){
#Bajo los datos de OOKLA, para servicio fijo, para cada trimestre de 2023
speed <- get_performance_tiles(service = "fixed", quarter = i, year = 2023, sf=TRUE)
speed$tile_y=as.numeric(speed$tile_y)
speed$tile_x=as.numeric(speed$tile_x)
st_as_sf(speed)
summary(speed$devices)
summary(speed$tests)

#intersect y luego un summarize by subnational y probar mapearlo
#Me quedo con el bounding box de América Latina y el Caribe
lacbbox <- lac1 %>% # use to subset open data
     st_bbox()

speed_lac=speed %>%
  filter(speed$tile_y <= as.numeric(lacbbox['ymax']), speed$tile_y >= as.numeric(lacbbox['ymin']),
         speed$tile_x <= as.numeric(lacbbox['xmax']), speed$tile_x >= as.numeric(lacbbox['xmin'])) %>%
  st_as_sf(wkt = "tile", crs = crsl2)

#Corrijo algunas geometrías
speed_lac2=st_make_valid(speed_lac)

lacsf=lacsf %>% st_make_valid()

#Hago la interseccion entre los grids de velocidad y los de los shapes de los subnacionales
velocidad=st_intersection(lacsf,speed_lac2)


aux=velocidad
#Armo una dummy que indica los grids que solo tienen un dispositivo que realiza el test
velocidad$filter1=ifelse(velocidad$devices==1,0,1)

#Guardo los archivos trimestrales
st_write(velocidad, paste0(folder_work, "rawdata/velocidad_fija_q", i, ".gpkg"))

}

#Comienzo a hacer el append de los distintos trimestres
df=st_read(paste0(folder_work, "rawdata/velocidad_fija_q1.gpkg"))
df$quarter=1
for (i in c(2:4)){
aux=st_read(paste0(folder_work, "rawdata/velocidad_fija_q",i,".gpkg"))
aux$quarter=i
df=rbind(df,aux)
}

df$subnacional=ifelse(is.na(df$NAME_2)==TRUE,df$NAME_1, df$NAME_2)

#Calculo la velocidad y algunas variables sin tener en cuenta los grids que tenían solo un dispositivo que hizo tests
df$avg_d_kbpsf1=ifelse(df$filter1==1,df$avg_d_kbps, NA)
df$avg_u_kbpsf1=ifelse(df$filter1==1,df$avg_u_kbps, NA)
df$testsf1=ifelse(df$filter1==1,df$tests, NA)
df$devicesf1=ifelse(df$filter1==1,df$devices, NA)
df$gridsf1=ifelse(df$filter1==1,df$quadkey, NA)

#Hago un summary con distintos indicadores
sum= df %>% group_by(codigo_uni) %>%  
  summarise(speed_d=mean(avg_d_kbps, na.rm=TRUE),
            speed_u=mean(avg_u_kbps, na.rm=TRUE),
            speed_df1=mean(avg_d_kbpsf1, na.rm=TRUE),
            speed_uf1=mean(avg_u_kbpsf1, na.rm=TRUE),
            tests=sum(tests, na.rm=TRUE),
            testsf1=sum(testsf1, na.rm=TRUE),
            devices=sum(devices, na.rm=TRUE),
            devicesf1=sum(devicesf1, na.rm=TRUE),
            grids=n_distinct(quadkey, na.rm = TRUE),
            gridsf1=n_distinct(gridsf1, na.rm = TRUE),
            mspeed_d=quantile(avg_d_kbps,probs=0.5, na.rm=T),
            mspeed_u=quantile(avg_u_kbps,probs=0.5, na.rm=T),
            mspeed_df1=quantile(avg_d_kbpsf1,probs=0.5, na.rm=T),
            mspeed_uf1=quantile(avg_u_kbpsf1,probs=0.5, na.rm=T))

#Quiero tener las geometrías de los subnacionales, no de la unión de los grids que tienen test
#por lo que hago el left join
df_final=left_join(lacsf, st_drop_geometry(sum), by='codigo_uni')

st_write(df_final, paste0(folder_work, "cleandata/ookla_porlevel_2023.gpkg"),delete_dsn=TRUE)
df_final=st_read(paste0(folder_work, "cleandata/ookla_porlevel_2023.gpkg"))
#Paso las métricas relacionadas con velocidad de kilobites per second a megabites per second 
cols_velocidad = c("speed_d", "speed_u","speed_df1", "speed_uf1", "mspeed_d", "mspeed_u", 
                    "mspeed_df1", "mspeed_uf1")


df_final[cols_velocidad] = lapply(df_final[cols_velocidad], function(x) x / 1000)

#Armo tres categorías de velocidad de internet

df_final$cmspeedd=ifelse(df_final$mspeed_df1<10,1, ifelse(df_final$mspeed_df1<25,2,
                  ifelse(df_final$mspeed_df1<50,3,ifelse(df_final$mspeed_df1<100,4,5))))
#df_final=left_join(df_final, pob, by='codigo_uni')

df_final=left_join(df_final, ods, by='codigo_uni')
df_final=df_final[,which(colnames(df_final)!="es_gob")]

write.csv(st_drop_geometry(df_final), paste0(folder_work, "cleandata/ookla_porlevel_2023.csv"))
st_write(df_final, paste0(folder_work, "cleandata/ookla_porlevel_2023_ods.gpkg"),delete_dsn=TRUE)
df=st_read(paste0(folder_work, "cleandata/ookla_porlevel_2023_ods.gpkg"))
keep= c("GID_0", "COUNTRY", "NAME_1", "NAME_2", "speed_df1", "speed_uf1", "testsf1", "devicesf1", 
        "gridsf1", "poblacion", "hogares", "tics_internet", "tics_celular", "tics_televisor")
keep2= c("codigo_uni")
df=df[, colnames(df) %in% keep2]
st_write_parquet(obj=df, dsn=paste0(folder_work, "cleandata/ookla_geom.parquet"))



#me quedo con la lista de quadkeys de LAC y su correspondencia con niveles subnacionales
df=st_read(paste0(folder_work, "rawdata/velocidad_fija_q4.gpkg"))
keep= c("GID_0", "COUNTRY", "NAME_1", "NAME_2","codigo_uni", "quadkey")
#"speed_df1", "speed_uf1", "testsf1", "devicesf1", 
 #       "gridsf1", "poblacion", "hogares", "tics_internet", "tics_celular", "tics_televisor")

df=df[, colnames(df) %in% keep]
write.csv(st_drop_geometry(df), paste0(folder_work, "cleandata/intersection_ookla_lac.csv"))
