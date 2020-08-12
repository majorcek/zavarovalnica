library(shiny)
library(shinyWidgets)
library(shinythemes)
library(readr)
library(knitr)
library(rsconnect)
library(dplyr)
library(readxl)

source("funkcija2.R")

ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("POKOJNINSKA RENTA"),
                sidebarLayout(
                  sidebarPanel(
                    wellPanel("vaša starost",               
                      fluidRow(
                        column(6, numericInput(inputId = "starost_leta", label = "leta", value = 40, min = 15, max = 80)),
                        column(6, numericInput(inputId = "starost_meseci", label = "mesecev", value = 0, min = 0, max = 11))
                      )
                    ),
                    wellPanel("Vaša starost ob sklenitvi pogodbe",
                      fluidRow(
                        column(6, numericInput(inputId = "starost_ob_sklenitvi_police_leta", label = "leta", value = 40, min = 15, max = 80)),
                        column(6, numericInput(inputId = "starost_ob_sklenitvi_police_meseci", label = "meseci", value = 0, min = 0, max = 11))
                      )
                    ),

                    hr(),
                    wellPanel( style = "background: lightgreen",
                              fluidRow("TRAJANJE VARČEVANJA"),
                              fluidRow("",
                                column(6, numericInputIcon(inputId = "dolzina_varcevanja_leta", label = "", icon = list("leta"), value = 10, min = 0, max = 60, step = 1)),
                                column(6, numericInputIcon(inputId = "dolzina_varcevanja_meseci", label = "", icon = list("meseci"), value = 0, min = 0, max = 11, step = 1))
                              ), 
                              fluidRow("ZNESEK MESEČNEGA VPLAČILA"),
                              fluidRow("",
                                column(6,numericInputIcon(inputId = "vplacilo", label = "", icon = list("€"), value = as.numeric(100), min = 0, max = 10000, step = 1))),
                    
                              fluidRow("VRSTA RENTE",
                                selectInput(inputId = "vrsta", label = "", c("doživljenjska renta" = "vedno", "omejena renta" = "omejeno", "enkratno izplačilo" = "enkrat"), selected = "doživljenjska renta")),
                              
                              conditionalPanel("input.vrsta == `omejeno`", 
                                               sliderInput(inputId = "omejenost_cas", "TRAJANJE RENTE", value = 10, min = 1, max = 40)),),
                    hr(),
 
                    hr(),
                    sliderInput("donos", "Kakšen letni donos  pričakujete? (v %)", min = 0, max = 5, value = 1.5, step = 0.1), 
                    fluidRow("ZAMIK"),
                    fluidRow("",
                      column(6, numericInput("zamik_leta", "leta", value = 0, min = 0, max = 30)),
                      column(6, numericInput("zamik_meseci", "meseci", value = 0, min = 0, max = 11))
                    )
                  ),
                  
                  mainPanel(
                    fluidRow("Izračun",
                             wellPanel(verbatimTextOutput("znesek"))
                    ),
                    
                    fluidRow("Dokumentacija",
                             wellPanel( "S prvim drsnikom označite vašo starost. 
                                          V zelenem okvirčku izberete željen čas varčevanja in višino posameznega vplačila. Nato imate še možnost, da izberete vrsto rente. 
                                          Izbirate lahko med doživljenjsko rento, rento z vnaprej določenim omejenim časom izplačevanja ter enkratnim izplačilom ob vaši upokojitvi. 
                                          V primeru, da se odločite za rento z omejenim trajanjem, morate trajanje izplačevanja določiti.
                                          Pod okvirkom je drsnik, ki vam omogoča, da sami nastavite donosnost, ki jo pričakujete. Povsem spodaj pa imate še možnost, 
                                          da po upokojitvi s prejemanjem izplačil še malo počakate (prejemanje odložite) in imate nato v naslednjih letih zato ustrezno višjo dodatno pokojnino. 
                                          Za izračun točne pokojnine bi bilo potrebno upoštevati še olajšave in dajatve, za kar za kar bi bili potrebni dodatni podatki o prihodkih. Posledično je ta kalkulator namenjen zgolj infrormativnemu izračunu.")
                    ),
                    fluidRow("Predpostavke", 
                            wellPanel("V izračunu imate možnost, da donosnost izberete sami. Trenutno v svetu ni veliko dobrih razmer za zagotavljanje visokih donosov, zato je ta parameter prednastavljen na 1,5%. V primeru, da menite, da se bodo razmere izboljšale in se bodo donosnosti povečale, lahko izberete višje pričakovane donosnosti, če pa menite, da se bodo razmere še dodatno zaostrile, pa izberite nižje.  
                                      Nadalje je privzeto, da se izplačevanje denarja začne en mesec po zadnjem vplačilu. Pri izračunu povprečne življenjske dobe upoštevamo aktualne slovenske tablice smrtnosti.
                                      V pričakovani donosnosti so implicitno že vključeni upravljavski stroški ter ostali stroški in provizije, ki nujno nastanejo pri vlaganju v vrednostne papirje.")
                  
                            ),
                    fluidRow("Primer 1",
                             wellPanel("Ana je stara 40 let. Odločila se je, da bo od vključno tega meseca dalje v pokojninski sklad vsak mesec vplačala 80€. To namerava početi 25 let oziroma do svojega 65. leta, ko se bo upokojila. Strinja se, da lahko pričakuje letno donosnost v višini 1,5%. Do upokojitve se bo na računu nabralo 28781,06€. Odloči se, da bo s tem denarjem kupila doživljensko rento, tako da bo do svoje smrti prejemala 121,23€ dodatne pokojnine.
                                       ")
                             ),
                    
                    fluidRow("Primer 2",
                             wellPanel("Ana je stara 50 let. Odločila se je, da bo od vključno tega meseca dalje v pokojninski sklad vsak mesec vplačala 115€. To namerava početi 15 let oziroma do svojega 65. leta, ko se bo upokojila. Zdi se ji, da lahko pričakuje letno donosnost v višini 2%. Predvideva, da bo denar najbolj potrebovala od 75. leta naprej, zato se odloči za 10 letni odlog izplačila. Do upokojitve se bo na računu nabralo 29170,00€. Odloči se, da bo s tem denarjem kupila doživljensko rento, tako da bo do svoje smrti prejemala 193,08€ dodatne pokojnine.
                                       ")
                    )
                  )
                )
                
)




server <- function(input, output, session){
  koncni_znesek <- reactive({
    if (input$starost_ob_sklenitvi_police_leta * 12 + input$starost_ob_sklenitvi_police_meseci > input$starost_leta * 12 + input$starost_meseci) {
      koncni_znesek <- "starost ne sme biti nižja od starosti ob sklenitvi pogodbe"
    } else if (input$starost_ob_sklenitvi_police_leta * 12 + input$dolzina_varcevanja_leta * 12 + input$starost_ob_sklenitvi_police_meseci + input$dolzina_varcevanja_meseci <= input$starost_leta * 12 + input$starost_meseci){
      koncni_znesek <- "Prilagoditi morate dolžino varčevanja, saj je predpostavljeno, da varčevanje še vedno poteka."
    } else if (input$vrsta == "omejeno"){
      koncni_znesek <- paste0("Pri starosti ", (input$starost_ob_sklenitvi_police_leta * 12 + input$starost_ob_sklenitvi_police_meseci + input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci + input$zamik_leta * 12 + input$zamik_meseci) %/% 12, " let in ",(input$starost_ob_sklenitvi_police_leta * 12 + input$starost_ob_sklenitvi_police_meseci + input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci + input$zamik_leta * 12 + input$zamik_meseci) %% 12, " mesecev lahko pričakujete začetek izplačevanja ", input$omejenost_cas,"-letne rente v višini  ", round(izracunaj_pricakovano_rento(input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci, input$vplacilo, 1.03, 1 + input$donos / 100, input$starost_leta, input$starost_meseci,  input$starost_ob_sklenitvi_police_leta, input$starost_ob_sklenitvi_police_meseci, input$zamik_leta, input$zamik_meseci, input$omejenost_cas, input$vrsta),2), "  evrov.")
    } else if (input$vrsta == "enkrat"){
      koncni_znesek <- paste("Pri starosti", (input$starost_ob_sklenitvi_police_leta * 12 + input$starost_ob_sklenitvi_police_meseci + input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci + input$zamik_leta * 12 + input$zamik_meseci) %/% 12, "let in ",(input$starost_ob_sklenitvi_police_leta * 12 + input$starost_ob_sklenitvi_police_meseci + input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci + input$zamik_leta * 12 + input$zamik_meseci) %% 12, "mesecev lahko pričakujete enkratno izplačilo v višini ", round(izracunaj_pricakovano_rento(input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci, input$vplacilo, 1.03, 1 + input$donos / 100, input$starost_leta, input$starost_meseci,  input$starost_ob_sklenitvi_police_leta, input$starost_ob_sklenitvi_police_meseci, input$zamik_leta, input$zamik_meseci, 0, input$vrsta),2), " evrov.")
    } else {
      koncni_znesek <- paste("Pri starosti", (input$starost_ob_sklenitvi_police_leta * 12 + input$starost_ob_sklenitvi_police_meseci + input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci + input$zamik_leta * 12 + input$zamik_meseci) %/% 12, "let in ",(input$starost_ob_sklenitvi_police_leta * 12 + input$starost_ob_sklenitvi_police_meseci + input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci + input$zamik_leta * 12 + input$zamik_meseci) %% 12, "mesecev lahko pričakujete začetek izplačevanja doživljenjske rente v višini ", round(izracunaj_pricakovano_rento(input$dolzina_varcevanja_leta * 12 + input$dolzina_varcevanja_meseci, input$vplacilo, 1.03, 1 + input$donos / 100, input$starost_leta, input$starost_meseci,  input$starost_ob_sklenitvi_police_leta, input$starost_ob_sklenitvi_police_meseci, input$zamik_leta, input$zamik_meseci, input$omejenost_cas, input$vrsta),2), " evrov.")
    }
    
    return(koncni_znesek)
  })
  
  output$znesek <- renderText({koncni_znesek()})
}


shinyApp(ui = ui, server = server)
