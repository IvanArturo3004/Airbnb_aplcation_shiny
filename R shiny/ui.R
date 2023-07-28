

ui <- fluidPage(
  
  titlePanel("CALCULADORA DEL PRECIO DEL INMUEBLE"),
  theme = shinythemes::shinytheme('readable'),
  
  
  sidebarPanel(
  
  selectInput("Colonia"
            ,
            "En que delegacion esta localizado el inmueble?" , 
 "Coyoacán" ,               
                        choices =nom_col[order(nom_col)] ),
  
   
  
  selectInput("cuenta"
              ,
              "Cual es tu tipo de cuenta?" , 
              choices= c("Host", "Super Host")),
 
  selectInput("cuarto"
              ,
              "Cual es tu tipo de cuarto?" , 
              choices= c("Entero ( casa o apartamento)" , "Cuarto privado"
                         ,"Cuarto de hotel " , "Cuarto compartido")), 
 
  dateInput("fecha", label = "Fecha de Registro"),
 
 numericInput("num_de_hab_max", label ="Cuantos residentes como maximo puede tener el inmueble" , value = 1),
 
 numericInput("num_de_baños", label = "Cuantos baños tiene el inmueble", value = 1),
 
 numericInput("num_de_cuartos", label ="Cuantos cuartos tiene el inmueble" , value = 1),
 
 numericInput("num_de_camas", label ="Cuantas camas tiene el inmueble" , value = 1),
 
 sliderInput("promedio_reviews", label ="Cual es el promedio de sus reviews" , min = 0 , max=5 ,value = 1,step = .1),
 
 numericInput("num_de_reviews", label ="Cuantas reviews contiene su inmueble" , value = 1),
 
 
 
 radioButtons("agenda_inst", label = "Su inmueble puede ser agendado de manera instantanea",
              choices = list("SI" = 1, "NO" = 0), 
              selected = 1),
 
 radioButtons("verificado", label = "Se encuentra verificado en nuetra plataforma?",
              choices = list("SI" = 1, "NO" = 0), 
              selected = 0),
  textInput("amenidades", label = "Registre todas las amenidades del inmueble separando por comas",
            value = "")
  
  ),
 mainPanel(
 tabsetPanel(
   
 tabPanel("precio" ,textOutput("precio"),"Acontinuacion se presentan algunas alternativas para mejorar el costo por su inmueble",
          textOutput("recomendacion1"),
          "_",
          textOutput("recomendacion2"),
          "_",
          textOutput("recomendacion3"),
          "_",
          "Las amenidades pueden elevar el precio del inmueble, lo cual en principio pareceria ser bueno, sin embargo se recomienda tener estas amenidades en regla y correctamente implementadas, pues los usuarios llegan a molestarse si estas no se encuentran correctamente implementadas o simplemente estas no se encuentran")
          ,
 tabPanel("hist" ,plotOutput("hist") )
 )
))


