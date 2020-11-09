#' Mean PV
#'
#'Saca la media, desviación estándar y el tamaño del efecto de una comparación de
#'dos medias
#'
#' @param data nombre de la base de datos
#' @param ... grupo de comparación
#'
#' @return medias, desviación estándar y tamaño del efecto
#' @export
#'
#' @examples
#' mesd.pv(bd,Sexo)
#'
mesd.pv <- function(data, ...) {
  group_ <- enquos(...)
  data %>% gather("PV1_100","PV2_100","PV3_100","PV4_100","PV5_100",
                  key = "PV",value = "Media") %>%
    group_by(PV,!!!group_) %>%
    summarise(mean_pv=mean(Media),
              de_pv=sd(Media),
              contar=n()) %>%
    drop_na() %>%
    mutate_if(is.factor,~as.numeric(.x)) %>%
    gather(mean_pv:contar,value = valor,key=med) %>%
    mutate(unico=paste0(med,!!!group_)) %>%
    select(PV,valor,unico) %>%
    spread(unico,valor) %>% rowwise() %>%
    mutate(d=abs(mean_pv1-mean_pv2)/(sqrt((de_pv1^2+de_pv2^2)/2)),
           g=abs(mean_pv1-mean_pv2)/sqrt(((contar1-1)*(de_pv1^2)+(contar2-1)*(de_pv2^2))/(contar1+contar2-2))) %>%
    as_tibble() %>%
    summarise(Media_1=mean(mean_pv1),
              DE_1=mean(de_pv1),
              n1=mean(contar1),
              Media_2=mean(mean_pv2),
              DE_2=mean(de_pv2),
              n2=mean(contar2),
              d=mean(d),
              g=mean(g))
  }
