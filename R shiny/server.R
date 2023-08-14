#ponemos la funcion server 
server <- function(input, output, session) {
  

 #ponemos un detector que calcula la antiguedad en años del registro
beta_antiguedad <-reactive({ round(as.numeric(as.Date("2023-06-01") - input$fecha)/365 , 2)*coeficientes[10]})

#ponemos un detector el cual cuenta el numero de comas puestas en el texto y por lo tanto el numero de amenidades
beta_amenidades <-reactive({ length(str_split(input$amenidades , "," , simplify = TRUE))*coeficientes[15]})
#ponemos un detector que observa si el usuario es host o super host
beta_super_host <- reactive({if ( input$cuenta == "Super Host"){beta_cuenta<- coeficientes[3]
beta_cuenta}else {beta_coeficientes<- 0 
beta_coeficientes}})

#ponemos un detector para saber si el usuario esta verificado o no 

beta_verificado <- reactive({if ( input$verificado == 1){beta_verif <- coeficientes[2]
beta_verif}else{beta_verif <- 0
beta_verif} })

#ponemos un detector para saber con que tipo de vivienda el usuario describe su inmueble

beta_tipo_de_viv <- reactive({
  
  if(input$Colonia %in% tipo_A){
    beta <- 0
    beta
  } else if(input$Colonia %in% tipo_B){beta<- coeficientes[5] 
  beta
  } else {beta <- coeficientes[6]
  beta
  }
}
)
#ponemos un detector para saber que tipo de reservacion maneja el dueño del inmueble 

beta_reservacion_inst <- reactive({if(input$agenda_inst == 1){beta_inst <- coeficientes[4] 
beta_inst}
  else{beta_inst <- 0  
  beta_inst}})
#ponemos un detector para saber que tipo de cuarto es el inmueble 
beta_tipo_de_cuarto <- reactive({if(input$cuarto == "Entero ( casa o apartamento)"){beta_tipo_de_cuarto_2<- 0
beta_tipo_de_cuarto_2}else if(input$cuarto=="Cuarto privado"){beta_tipo_de_cuarto_2 <- coeficientes[8]
beta_tipo_de_cuarto_2}else if (input$cuarto=="Cuarto de hotel "){beta_tipo_de_cuarto_2<- coeficientes[7]
beta_tipo_de_cuarto_2}else{beta_tipo_de_cuarto_2<- coeficientes[9]
beta_tipo_de_cuarto_2}})

#ponemos un detector para observar el numero de reviews del inmueble

beta_review_numero <- reactive({coeficientes[16]*input$num_de_reviews })

#ponemos un detector para observar el promedio dado por las reviews

beta_review_promedio <- reactive({input$promedio_reviews * coeficientes[17]})

#ponemos un detector para observar el numero de camas 

beta_camas <- reactive({if(1==1){a<-coeficientes[13]*input$num_de_camas 
a}})

#ponemos un detector para observar el numero de baños

beta_baño <- reactive({coeficientes[12]* input$num_de_baños})

#ponemos un detector para observar el numero maximo de residentes permitidos 


beta_num_max_res <-reactive({coeficientes[11]*input$num_de_hab_max})
  
#ponemos un detector para observar el numero de cuartos 


beta_cuartos <- reactive({coeficientes[14]*input$num_de_cuartos})


#finalmente sumamos todos los valores beta (previamente calculados)
precio_estimado_lamda<- reactive({sum(c(coeficientes[1],beta_cuartos(),beta_amenidades()
  
  , beta_num_max_res() ,beta_baño() , beta_camas() ,beta_review_promedio()
  
  ,beta_review_numero(),beta_tipo_de_cuarto(),beta_tipo_de_viv(),beta_super_host()
  
  , beta_verificado() , beta_antiguedad(),beta_reservacion_inst()))})

   
#se da un vector el cual descentraliza el valor obtenido para conocer el valor real 
precio_estimado_real <- reactive({(precio_estimado_lamda()*lamda+1)^(1/lamda)})

#se imprimen unas cuantas recomendaciones al ususario (dependiendiendo de sus respuestas) para 
#mejorar el valor del inmueble
recomendaciones_cuenta<- reactive({if (input$cuenta == "Super Host"){rec_cuenta <- "."
  rec_cuenta }else{rec_cuenta <- "los super hosts pueden obtener un beneficio para aumentar el precio del inmueble pues obtiene un beneficio al brindar una mayor atencion." }})

recomendaciones_verificado <- reactive({if(input$verificado==0){rec_verificado <- "Se recomienda a los host verificar sus cuentas e inmuebles pues asi los inquilinos pueden tener la confianza con el arrendatario pujede ser mayor (ademas esto posibilita el poder cobrar una cantidad mayor por el inmueble"}
  else{rec_verificado <- "." 
    rec_verificado}})

recomendaciones_agenda_inst <- reactive({if(input$agenda_inst == 1){"recuerde que para agendar de manera instantanea se deben solicitar documentos de identidad al inquilino tanto por su seguridad como por la seguridad del inmueble y posibles vecinos"}else{"El poder agendar de manera instantanea es  una ventaja por sobre otro inmuebles la cual se considera importante, se recomienda afectuar esta opcion con la mayor antelacion posible"}})

 
#usando el detector de los años registrados damos un anuncio sobre cuanto tiempo
# lleva registrado el usuario

  output$fecha_1 <- renderText({paste("Usted lleva" , beta_antiguedad(),  "años registrado" )}) 
  
  output$amenidades <- renderText({paste("el numero de amenidades es" , beta_amenidades())})
    
  output$Clasificacion <- renderText({paste("Tu tipo de vivienda es clasificacion" , beta_tipo_de_viv(),
                                            "verificado check", beta_verificado() ,
                                            "cuenta beta check" , beta_super_host()
                                            ,"Reservacion instantanea", beta_reservacion_inst(),
                                            "tipo de cuarto",beta_tipo_de_cuarto() ,
                                            "numero reviews",beta_review_numero(),
                                            "score de reviews",beta_review_promedio(),
                                            "baños",beta_baño(),
                                            "habitaciones",beta_cuartos(),
                                            "capacidad maxima", beta_num_max_res(),
                                            "camas", beta_camas()) })
  
  output$precio <- renderText({paste("El precio que se recomienda es:" , precio_estimado_real())})

#programammos un histograma que calcula el impacto de cada beta para observar su importancia individual
#visible en la pestaña hist
  
 output$hist <- renderPlot({
    barplot(height = abs(c(beta_cuartos(),beta_amenidades()
           
           , beta_num_max_res() ,beta_baño() , beta_camas() ,beta_review_promedio()
           
           ,beta_review_numero(),beta_tipo_de_cuarto(),beta_tipo_de_viv(),beta_super_host()
           
           ,beta_verificado() , beta_antiguedad(),beta_reservacion_inst())),
           names.arg=c("cuartos" , "amenid." , "resid." , "baños" , "camas" , "prom. rev." , 
                       "num rev." , "cuarto tipo" , "colonia" , "host tipo" ,"verificado" 
                       , "antiguedad" ,"reserv. inst." ),
         col = wes_palette("FantasticFox1", 10, type = "continuous"),las=3)  } , height = 500)
  
  #se programa el precio estimado y las recoemendaciones para la pestaña precio 
  output$lamda <- renderText({paste("real" , precio_estimado_real(),"lamda",precio_estimado_lamda())})
  output$recomendacion1 <- renderText({paste(recomendaciones_cuenta())})
  output$recomendacion2 <- renderText({paste(recomendaciones_verificado())})
  output$recomendacion3 <- renderText({paste(recomendaciones_agenda_inst())})
  
}
