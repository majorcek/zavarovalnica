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
  meseci1 <- seq.int(from = 0, to = starost_leta * 12 + starost_meseci - starost_ob_sklenitvi_police_leta * 12 - starost_ob_sklenitvi_police_meseci, by = 1)
  trenutna_sredstva <- mesecno_vplacilo * sum((izracunaj_mesecni_donos(dosedanji_letni_donos) ** meseci1)) 
  trenutna_obrestovana_sredstva <- trenutna_sredstva * (izracunaj_mesecni_donos(pricakovan_letni_donos) ** (dolzina_vplacevanja - (starost_leta * 12 - starost_ob_sklenitvi_police_leta * 12 + starost_meseci - starost_ob_sklenitvi_police_meseci)))

  #kar bo vloženo v prihodnje
  meseci <- seq.int(from = 1, to = dolzina_vplacevanja - (starost_leta * 12 - starost_ob_sklenitvi_police_leta * 12 + starost_meseci - starost_ob_sklenitvi_police_meseci - 1), by = 1)
  mesecni_donos <- izracunaj_mesecni_donos(pricakovan_letni_donos)
  obrestni_faktorji <- mesecni_donos ** meseci
  prihodnja_sredstva <- sum(obrestni_faktorji) * mesecno_vplacilo
  skupna_vsota <- trenutna_obrestovana_sredstva + prihodnja_sredstva

  skupna_vsota
}


izracunaj_pricakovano_rento <- function(dolzina_vplacevanja, mesecno_vplacilo, dosedanji_letni_donos, pricakovan_letni_donos, starost_leta, starost_meseci, starost_ob_sklenitvi_police_leta, starost_ob_sklenitvi_police_meseci, zamik_leta, zamik_meseci, casovna_omejenost_leta, vrsta){
  a_tablica <- pridobi_verjetnosti_za_smrt(tablice_smrtnosti, starost_ob_sklenitvi_police_leta)
  
  mesecni_donos <- izracunaj_mesecni_donos(pricakovan_letni_donos)
  faktor_zaradi_zamika <- mesecni_donos ** (zamik_leta * 12 + zamik_meseci)
  premozenje_ob_koncu_varcevanja <- izracunaj_premozenje_pred_izplacevanjem(dolzina_vplacevanja, starost_leta, starost_meseci, starost_ob_sklenitvi_police_leta, starost_ob_sklenitvi_police_meseci, dosedanji_letni_donos, pricakovan_letni_donos, mesecno_vplacilo) * faktor_zaradi_zamika 

  znesek = 0
  if (vrsta == "vedno"){
    faktor <- mesecni_donos / (mesecni_donos - 1) * (1 - (1/mesecni_donos) ** 12)
    leta = seq.int(from = ceiling((starost_ob_sklenitvi_police_leta * 12 + starost_ob_sklenitvi_police_meseci + dolzina_vplacevanja + zamik_leta * 12 + zamik_meseci)/12), to = 120, by = 1)
    faktor_izplacil <- faktor * ((1/pricakovan_letni_donos) ** (leta - leta[1]))
    b_tablica <- a_tablica[a_tablica$starost >= leta[1],]
    EX_tabela <- data.frame("leto" = leta, "faktor" = faktor_izplacil, "verjetnost" = b_tablica$standardizirane)
    pricakovan_skupni_faktor <- sum(EX_tabela$faktor * EX_tabela$verjetnost)
    
    mesecna_anuiteta =  premozenje_ob_koncu_varcevanja / pricakovan_skupni_faktor
    znesek = mesecna_anuiteta

  }else if (vrsta == "enkrat"){
    znesek = premozenje_ob_koncu_varcevanja
  
  }else if (vrsta == "omejeno"){
    aux_tabela <- data.frame(c(seq(from = starost_ob_sklenitvi_police_leta, to = last(a_tablica$starost))))
    colnames(aux_tabela) <- c("starost")
    aux_tabela$obrestni_faktor <- (1/(pricakovan_letni_donos)) ** (aux_tabela$starost - starost_ob_sklenitvi_police_leta)
    faktor_za_vplacila <- sum(head(a_tablica$standardizirane * aux_tabela$obrestni_faktor,dolzina_vplacevanja/12))
    
    faktor_za_izplacila <- head(tail(a_tablica$standardizirane * aux_tabela$obrestni_faktor, length(aux_tabela$obrestni_faktor) - dolzina_vplacevanja/12 - casovni_zamik),casovna_omejenost_leta)
    faktor_za_izplacila1 <- sum(faktor_za_izplacila)
    
    znesek  =  mesecno_vplacilo * faktor_za_vplacila / faktor_za_izplacila1
  }
  znesek
}
