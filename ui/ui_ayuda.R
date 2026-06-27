# ============ UI: Cómo Ayudar ============

ui_ayuda <- function() {
  tagList(
    # Guía de uso humanitario
    div(
      class = "paso-card",
      tags$h4("\u2753 Gu\u00eda de Uso Humanitario"),
      
      # Seguridad
      div(
        class = "alerta-seguridad",
        tags$h5("\u26A0\uFE0F Seguridad de Infraestructura y Privacidad"),
        tags$p(
          "Si vas a reportar a un ciudadano, hazlo con respeto y ",
          "resguardando su integridad f\u00edsica. No expongas direcciones ",
          "exactas de domicilios particulares deshabitados o destruidos ",
          "para evitar robos o saqueos en zonas afectadas por el terremoto."
        )
      ),
      
      # Fuentes oficiales
      div(
        class = "fuentes-oficiales",
        tags$h5("\U0001F4E1 FUENTES DE INFORMACI\u00d3N OFICIALES"),
        tags$p(class = "texto-gris",
               "En caso de terremoto o sismos intensos, recurre \u00fanicamente a ",
               "canales certificados para evitar la propagaci\u00f3n de rumores o ",
               "informaci\u00f3n falsa:"),
        
        div(class = "fuente-item",
          tags$h6("\U0001F310 FUNVISIS (Fundaci\u00f3n Venezolana de Investigaciones Sismol\u00f3gicas)"),
          tags$p("Reportes oficiales de sismos y magnitud en tiempo real en Venezuela."),
          tags$p("X/Twitter: @FUNVISIS | Sitio web: funvisis.gob.ve")
        ),
        div(class = "fuente-item",
          tags$h6("\U0001F310 Protecci\u00f3n Civil y Administraci\u00f3n de Desastres (PC)"),
          tags$p("Organismo de rescate, atenci\u00f3n inmediata y evaluaci\u00f3n de da\u00f1os estructurales a nivel nacional."),
          tags$p("X/Twitter: @PCivil_Ve")
        ),
        div(class = "fuente-item",
          tags$h6("\U0001F310 Cruz Roja Venezolana"),
          tags$p("Apoyo y respuesta en log\u00edstica humanitaria, atenci\u00f3n prehospitalaria y reencuentros."),
          tags$p("X/Twitter: @CruzRojaVe")
        )
      )
    ),
    
    # Código de 4 dígitos
    div(
      class = "paso-card",
      tags$h5("\U0001F511 \u00bfPor qu\u00e9 se usa un c\u00f3digo de 4 d\u00edgitos?"),
      tags$p(
        "Para proteger la privacidad y garantizar la veracidad de los datos. ",
        "Cada video o reporte genera un c\u00f3digo exclusivo. Solo quien subi\u00f3 el ",
        "reporte posee este c\u00f3digo y es la \u00fanica persona facultada para marcar ",
        "un caso como ", tags$strong("reunificado"), " cuando se localice a los familiares, ",
        "evitando borrados accidentales o falsos cierres."
      )
    ),
    
    # Marcar como reunificado
    div(
      class = "paso-card",
      tags$h5("\u2705 Marcar caso como reunificado"),
      textInput("codigo_reunificar", "Ingresa tu c\u00f3digo de 4 d\u00edgitos",
                placeholder = "Ej: 4827"),
      actionButton("btn_reunificar", "Marcar como reunificado",
                   class = "btn-reunificar")
    ),
    
    # Números de emergencia
    div(
      class = "paso-card numeros-emergencia",
      tags$h5("\U0001F4DE N\u00fameros de Respuesta R\u00e1pida"),
      tags$ul(
        tags$li("Bomberos de Caracas: 171 / 0212-5454545"),
        tags$li("Cruz Roja Venezolana: 0212-5714545"),
        tags$li("Defensa Civil Nacional: 0212-6623654")
      )
    ),
    
    # Estadísticas
    div(
      class = "paso-card",
      tags$h5("\U0001F4CA Estad\u00edsticas"),
      uiOutput("stats_ui")
    )
  )
}
