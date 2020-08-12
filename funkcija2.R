# DOCUMENTATION

# dolžina vplačevanja je v mesecih  
# vplačina in izplačila potekajo na začetku meseca
# starosti so v mesecih

library(dplyr)
library(readxl)

tablice_smrtnosti <- read_excel("tablice smrtnosti.xlsx", col_names = c("starost","stopnja prezivetja"), skip = 2)

pridobi_verjetnosti_za_smrt <- function(tablica, starost){
  aktualna_tablica <- tablica[tablica$starost >= starost,]
  verjetnost_prezivetja <- tablica[tablica$starost == starost,2][[1,1]]
  aktualna_tablica$standardizirane <- aktualna_tablica$`stopnja prezivetja` / verjetnost_prezivetja
  aktualna_tablica$verjetnosti_smrti <- aktualna_tablica$standardizirane[1:length(aktualna_tablica$standardizirane)] - c(aktualna_tablica$standardizirane[2:length(aktualna_tablica$standardizirane)],0)
  aktualna_tablica
}


izracunaj_mesecni_donos <- function(letna_obrestna_mera){
  letna_obrestna_mera ** (1/12)
}


izracunaj_premozenje_pred_izplacevanjem <- function(dolzina_vplacevanja, starost_leta, starost_meseci, starost_ob_sklenitvi_police_leta, starost_ob_sklenitvi_police_meseci, dosedanji_letni_donos, pricakovan_letni_donos,mesecno_vplacilo){
  
  tablica <- pridobi_verjetnosti_za_smrt(tablice_smrtnosti, starost_leta)[,c(1,3)]
  stevilo_preostalih_mesecev_vplacevanja <- dolzina_vplacevanja - (starost_leta *12 + starost_meseci - starost_ob_sklenitvi_police_leta * 12 - starost_ob_sklenitvi_police_meseci + 1)
  starost_ob_koncu_vplacevanja_leto <- (starost_ob_sklenitvi_police_leta * 12 + starost_ob_sklenitvi_police_meseci + dolzina_vplacevanja - 1) %/% 12
  starost_ob_koncu_vplacevanja_mesec <- (starost_ob_sklenitvi_police_meseci + dolzina_vplacevanja - 1) %% 12

  preostali_meseci_varcevanja <- dolzina_vplacevanja - (starost_leta * 12 + starost_meseci - starost_ob_sklenitvi_police_leta * 12 - starost_ob_sklenitvi_police_meseci) - 1
  
  tablica <- tablica[tablica$starost <= starost_ob_koncu_vplacevanja_leto +1,]

  verjetnosti <- NULL
  if (preostali_meseci_varcevanja <= 12 - starost_meseci){
    verjetnosti <- rep(tablica[2,2][[1]], preostali_meseci_varcevanja)
  } else if (preostali_meseci_varcevanja <= (24 - starost_meseci)){
    verjetnosti <- c(rep(tablica[2,2][[1]], 12 - starost_meseci), rep(tablica[3,2][[1]], preostali_meseci_varcevanja - (12-starost_meseci)))
  } else if (starost_ob_koncu_vplacevanja_mesec != 0){
    verjetnosti <- c(rep(tablica[2,2][[1]], 12 - starost_meseci), rep(tablica[3:(length(tablica$starost) - 1),2][[1]], each = 12), rep(tablica[length(tablica$starost), 2][[1]], starost_ob_koncu_vplacevanja_mesec))
  } else {
    verjetnosti <- c(rep(tablica[2,2][[1]], 12 - starost_meseci), rep(tablica[3:(length(tablica$starost) - 1),2][[1]], each = 12), rep(tablica[length(tablica$starost), 2][[1]], starost_ob_koncu_vplacevanja_mesec))
  }
    


  #sredstva do sedaj
  meseci1 <- seq.int(from = 0, to = starost_leta * 12 + starost_meseci - starost_ob_sklenitvi_police_leta * 12 - starost_ob_sklenitvi_police_meseci, by = 1)
  trenutna_sredstva <- mesecno_vplacilo * sum((izracunaj_mesecni_donos(dosedanji_letni_donos) ** meseci1))
  trenutna_obrestovana_sredstva <- trenutna_sredstva * (izracunaj_mesecni_donos(pricakovan_letni_donos) ** (dolzina_vplacevanja - (starost_leta * 12 - starost_ob_sklenitvi_police_leta * 12 + starost_meseci - starost_ob_sklenitvi_police_meseci)))

  #sredstva, ki jih bom naložil (v primeru, da sem živ) do konca varčevanja
  preostali_meseci_varcevanja <- dolzina_vplacevanja - (starost_leta * 12 + starost_meseci - starost_ob_sklenitvi_police_leta * 12 - starost_ob_sklenitvi_police_meseci) - 1
  
  skupna_vsota <- NULL
  if (preostali_meseci_varcevanja == 0) {
    skupna_vsota <- trenutna_obrestovana_sredstva
  } else {
    meseci <- seq.int(from = 1, to = preostali_meseci_varcevanja, by = 1)
    preostala_vlozena_sredstva <- mesecno_vplacilo * sum((izracunaj_mesecni_donos(pricakovan_letni_donos) ** meseci) * verjetnosti) 
    skupna_vsota <- trenutna_obrestovana_sredstva + preostala_vlozena_sredstva
  }

  skupna_vsota
}


izracunaj_pricakovano_rento <- function(dolzina_vplacevanja, mesecno_vplacilo, dosedanji_letni_donos, pricakovan_letni_donos, starost_leta, starost_meseci, starost_ob_sklenitvi_police_leta, starost_ob_sklenitvi_police_meseci, zamik_leta, zamik_meseci, casovna_omejenost_leta, vrsta){
  
  mesecni_donos <- izracunaj_mesecni_donos(pricakovan_letni_donos)
  faktor_zaradi_zamika <- mesecni_donos ** (zamik_leta * 12 + zamik_meseci)
  premozenje_ob_koncu_varcevanja <- izracunaj_premozenje_pred_izplacevanjem(dolzina_vplacevanja, starost_leta, starost_meseci, starost_ob_sklenitvi_police_leta, starost_ob_sklenitvi_police_meseci, dosedanji_letni_donos, pricakovan_letni_donos, mesecno_vplacilo) * faktor_zaradi_zamika 

  znesek <- 0
  
  st_mesecev <- starost_ob_sklenitvi_police_leta * 12 + starost_ob_sklenitvi_police_meseci + dolzina_vplacevanja + zamik_leta * 12 + zamik_meseci
  starost_izplacevanje_leta <-  st_mesecev %/% 12
  starost_izplacevanje_mesec <- st_mesecev %% 12
  
  a_tablica <- pridobi_verjetnosti_za_smrt(tablice_smrtnosti, starost_ob_sklenitvi_police_leta)
  a_tablica <- a_tablica[a_tablica$starost >= starost_izplacevanje_leta + 1, ]
  
  if (vrsta == "vedno"){
    meseci <- c(starost_izplacevanje_mesec : 12, rep(1:12, 119 - starost_izplacevanje_leta ))
    verjetnosti <- c(rep(a_tablica[1, 2][[1]], 12 - starost_izplacevanje_mesec + 1), rep(a_tablica[a_tablica$starost > starost_izplacevanje_leta + 1, 2][[1]], each = 12))

    faktorji <- (1 / mesecni_donos) ** (1 : length(meseci))
    kolicnik <- sum(faktorji * verjetnosti)
    mesecna_anuiteta <-  premozenje_ob_koncu_varcevanja / kolicnik
    znesek <- mesecna_anuiteta

  }else if (vrsta == "enkrat"){
    znesek <- premozenje_ob_koncu_varcevanja
  
  }else if (vrsta == "omejeno"){
    meseci <- rep((1:12), casovna_omejenost_leta)
    smrtnost <- a_tablica[a_tablica$starost <= (starost_izplacevanje_leta + casovna_omejenost_leta + 1), 2][[1]]
    verjetnosti <- NULL
    if (starost_izplacevanje_mesec == 0){
      verjetnosti <- c(rep(head(smrtnost, - 1), each = 12))
    } else { 
      if (casovna_omejenost_leta == 1){
        verjetnosti <- c(rep(smrtnost[1], 12 - starost_izplacevanje_mesec + 1), rep(smrtnost[2], starost_izplacevanje_mesec - 1))
      } else {
        verjetnosti <- c(rep(smrtnost[1], 12 - starost_izplacevanje_mesec + 1), rep(smrtnost[2 : casovna_omejenost_leta], each = 12), rep(smrtnost[casovna_omejenost_leta + 1], starost_izplacevanje_mesec - 1))
      }
    }
    
    faktorji <- (1/mesecni_donos) ** (1:length(meseci))
    kolicnik <- sum(faktorji * verjetnosti)
    mesecna_anuiteta <- premozenje_ob_koncu_varcevanja / kolicnik
    znesek <- mesecna_anuiteta
  }
  znesek
}
