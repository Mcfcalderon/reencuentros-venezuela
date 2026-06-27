# ============ UI: Reportar SOS ============

ui_reportar <- function() {
  tagList(
    # Alerta de seguridad
    div(
      class = "alerta-seguridad",
      tags$h5(
        tags$span(class = "icono-alerta", "\u26A0\uFE0F"),
        tags$span(class = "texto-rojo", " SEGURIDAD PERSONAL PRIMERO")
      ),
      tags$p(
        "Si vas a reportar a un ciudadano, hazlo con respeto y ",
        "resguardando su integridad f\u00edsica. No expongas direcciones ",
        "exactas de domicilios particulares deshabitados o destruidos ",
        "para evitar robos o saqueos."
      )
    ),
    
    # Mensaje informativo
    div(
      class = "info-busqueda",
      tags$h5("\U0001F49A \u00bfEst\u00e1s buscando a tu familia o conoces a alguien que lo est\u00e1 haciendo?"),
      tags$p(
        "Graba un video corto con los datos m\u00e1s importantes como: ",
        "nombre de la persona, d\u00f3nde est\u00e1, d\u00f3nde se puede contactar ",
        "y en qu\u00e9 estado f\u00edsico o de salud se encuentra."
      )
    ),
    
    # Indicador de progreso
    uiOutput("progress_steps_ui"),
    
    # Paso 1: Tipo de reporte
    div(
      class = "paso-card",
      tags$h5("1. \u00bfA QUI\u00c9N VAS A REPORTAR?", tags$span(class = "texto-rojo", " *")),
      div(
        class = "tipo-grid",
        actionButton("tipo_nino", label = tagList(
          tags$div(class = "tipo-emoji", "\U0001F9D2"),
          tags$div("Ni\u00f1o / Ni\u00f1a")
        ), class = "btn-tipo", `aria-label` = "Reportar ni\u00f1o o ni\u00f1a"),
        actionButton("tipo_adulto", label = tagList(
          tags$div(class = "tipo-emoji", "\U0001F9D1"),
          tags$div("Adulto")
        ), class = "btn-tipo", `aria-label` = "Reportar adulto"),
        actionButton("tipo_mayor", label = tagList(
          tags$div(class = "tipo-emoji", "\U0001F9D3"),
          tags$div("Adulto Mayor")
        ), class = "btn-tipo", `aria-label` = "Reportar adulto mayor"),
        actionButton("tipo_mascota", label = tagList(
          tags$div(class = "tipo-emoji", "\U0001F43E"),
          tags$div("Mascota / Animal")
        ), class = "btn-tipo", `aria-label` = "Reportar mascota o animal")
      ),
      uiOutput("tipo_seleccionado_ui")
    ),
    
    # Paso 2: Video
    div(
      class = "paso-card",
      tags$h5("2. GRABA O SUBE UN VIDEO CORTO (PRIORITARIO)"),
      tags$p(class = "texto-gris", "Muestra el rostro de la persona o el lugar para reconocimiento inmediato. (M\u00e1x. 20MB)"),
      fileInput("video_file", label = NULL,
                accept = c("video/mp4", "video/quicktime", "video/webm"),
                placeholder = "Seleccionar video..."),
      uiOutput("video_preview_ui")
    ),
    
    # Paso 3: Fotos
    div(
      class = "paso-card",
      tags$h5("3. SUBIR UNA O VARIAS FOTOS (OPCIONAL)"),
      tags$p(class = "texto-gris",
             "A\u00f1ade fotos de su rostro, cicatrices, tatuajes o marcas ",
             "distintivas para facilitar el match sem\u00e1ntico. (M\u00e1x. 5MB por foto)"),
      fileInput("fotos_file", label = NULL,
                accept = c("image/jpeg", "image/png", "image/webp"),
                multiple = TRUE,
                placeholder = "Seleccionar fotos..."),
      uiOutput("fotos_preview_ui")
    ),
    
    # Paso 4: Información adicional
    div(
      class = "paso-card",
      tags$h5("4. COMPLETAR INFORMACI\u00d3N ADICIONAL"),
      textInput("rep_nombre", "Nombre (si lo conoces)",
                placeholder = "Nombre de la persona o mascota"),
      textInput("rep_ubicacion",
                tags$span("Ubicaci\u00f3n donde fue visto/a", tags$span(class = "texto-rojo", " *")),
                placeholder = "Ej: Altamira, Caracas"),
      selectInput("rep_estado_salud", "Estado de salud",
                  choices = c("Seleccionar..." = "",
                              "Aparentemente bien" = "bien",
                              "Herido/a leve" = "herido_leve",
                              "Herido/a grave" = "herido_grave",
                              "Necesita atenci\u00f3n m\u00e9dica" = "necesita_atencion",
                              "Desconocido" = "desconocido")),
      textInput("rep_contacto", "Contacto de quien reporta",
                placeholder = "Tel\u00e9fono o red social"),
      textAreaInput("rep_descripcion",
                    tags$span("Descripci\u00f3n", tags$span(class = "texto-rojo", " *")),
                    placeholder = "Vestimenta, heridas visibles, puntos de referencia...",
                    rows = 4)
    ),
    
    # Botón publicar
    div(
      class = "text-center",
      style = "margin-top: 20px; margin-bottom: 30px;",
      actionButton("btn_publicar", "PUBLICAR REPORTE",
                   class = "btn-publicar", width = "100%",
                   `aria-label` = "Publicar reporte de persona encontrada")
    )
  )
}
