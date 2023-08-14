
#se desarrolla la interfaz de usuario

ui <- fluidPage(
  
  #color y titulo
  titlePanel("CALCULADORA DEL PRECIO DEL INMUEBLE"),
  theme = shinythemes::shinytheme('readable'),
  
  #todo lo que desarrolla el panel de personalisacion
  
  sidebarPanel(
  
    #seleccion de la colonia
  selectInput("Colonia"
            ,
            "En que delegacion esta localizado el inmueble?" , 
 "Coyoacán" ,               
                        choices =nom_col[order(nom_col)] ),
  
   #seleccion tipo de cuenta
  
  selectInput("cuenta"
              ,
              "Cual es tu tipo de cuenta?" , 
              choices= c("Host", "Super Host")),
 #seleccion tipo de cuarto
  selectInput("cuarto"
              ,
              "Cual es tu tipo de cuarto?" , 
              choices= c("Entero ( casa o apartamento)" , "Cuarto privado"
                         ,"Cuarto de hotel " , "Cuarto compartido")), 
 
 
 #seleccion de fecha de inicio del registro
 
 
  dateInput("fecha", label = "Fecha de Registro"),
 
 #seleccion num de habitaciones
 
 numericInput("num_de_hab_max", label ="Cuantos residentes como maximo puede tener el inmueble" , value = 1),
 
 #seleccion num de baños
 
 numericInput("num_de_baños", label = "Cuantos baños tiene el inmueble", value = 1),
 
 #seleccion num de cuartos
 
 numericInput("num_de_cuartos", label ="Cuantos cuartos tiene el inmueble" , value = 1),
 
 #seleccion num de camas en el inmueble 
 
 numericInput("num_de_camas", label ="Cuantas camas tiene el inmueble" , value = 1),
 
 #seleccion el promedio  que las reviews han recibido
 
 sliderInput("promedio_reviews", label ="Cual es el promedio de sus reviews" , min = 0 , max=5 ,value = 1,step = .1),
 
 #seleccion del numero de reviews establecidos
 
 numericInput("num_de_reviews", label ="Cuantas reviews contiene su inmueble" , value = 1),
 
 #seleccion sobre de que manera puede ser agendado el inmueble
 
 radioButtons("agenda_inst", label = "Su inmueble puede ser agendado de manera instantanea",
              choices = list("SI" = 1, "NO" = 0), 
              selected = 1),
 
 #seleccionar si el perfil se encuentra verificado o no 
 
 radioButtons("verificado", label = "Se encuentra verificado en nuetra plataforma?",
              choices = list("SI" = 1, "NO" = 0), 
              selected = 0),
 
 #registro de cuantas amenidades "extras" tiene el inmueble
 
  textInput("amenidades", label = "Registre todas las amenidades del inmueble separando por comas",
            value = "")
  
  ),
 
 #el main manel refleja los resultados de la personalizacion
 mainPanel(
 tabsetPanel(
   #aqui se programa lo que aparecera en la pestaña de precio 
   #o sea las recomendaciones y el precio que se recomienda poner al inmueble aproximadamente
   
 tabPanel("precio" ,textOutput("precio"),"Acontinuacion se presentan algunas alternativas para mejorar el costo por su inmueble",
          textOutput("recomendacion1"),
          "_",
          textOutput("recomendacion2"),
          "_",
          textOutput("recomendacion3"),
          "_",
          ("Las amenidades pueden elevar el precio del inmueble, lo cual en principio pareceria ser bueno, sin embargo se recomienda tener estas amenidades en regla y correctamente implementadas, pues los usuarios llegan a molestarse si estas no se encuentran correctamente implementadas o simplemente estas no se encuentran"),

   #Aqui se programa la pesña histograma       
   #aqui se depliega el histograma el cual ayuda a visualizar el impacto de cada beta 
   
   
   
 tabPanel("hist" ,plotOutput("hist") )
 )
))


