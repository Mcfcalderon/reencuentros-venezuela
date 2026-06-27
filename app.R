# ============ Reencuentros Venezuela — app.R ============

# ==== LOAD ENV VARS ====
for (.ef in c("app_env", ".Renviron")) {
  if (file.exists(.ef)) {
    for (.ln in readLines(.ef, warn = FALSE)) {
      .ln <- trimws(.ln)
      if (nchar(.ln) > 0 && !startsWith(.ln, "#") && grepl("=", .ln)) {
        .k <- sub("=.*", "", .ln)
        .v <- sub("^[^=]+=", "", .ln)
        .v <- gsub("\r", "", .v)
        do.call(Sys.setenv, setNames(list(.v), .k))
      }
    }
    message("[Reencuentros] Env loaded from: ", .ef)
    break
  }
}
message("[Reencuentros] MONGODB_URI = ", nchar(Sys.getenv("MONGODB_URI")), " chars")

# ---- Load global: libraries + R/ + ui/ ----
source("global.R")

# ---- Initialize DB ----
ensure_init()

# ---- Build UI ----
ui <- ui_main()

# ---- Server ----
server <- function(input, output, session) {

  # Estado de navegación
  nav_actual <- reactiveVal("reportar")

  # Navegación inferior (con scroll al top)
  cambiar_tab <- function(tab) {
    nav_actual(tab)
    for (btn in c("nav_reportar", "nav_buscar", "nav_ayuda")) {
      shinyjs::removeClass(id = btn, class = "nav-btn-activo")
    }
    shinyjs::addClass(id = paste0("nav_", tab), class = "nav-btn-activo")
    shinyjs::runjs("window.scrollTo({top:0, behavior:'smooth'});")
  }
  
  observeEvent(input$nav_reportar, cambiar_tab("reportar"))
  observeEvent(input$nav_buscar, cambiar_tab("buscar"))
  observeEvent(input$nav_ayuda, cambiar_tab("ayuda"))

  # Exportar nav_actual para conditionalPanel
  output$nav_actual_out <- reactive(nav_actual())
  outputOptions(output, "nav_actual_out", suspendWhenHidden = FALSE)

  # ---- Cargar módulos server (con local=TRUE para compartir reactivos) ----
  for (.sf in list.files("server", pattern = "\\.R$", full.names = TRUE)) {
    source(.sf, local = TRUE)
    message("[Reencuentros] Server loaded: ", .sf)
  }
}

shinyApp(ui, server)
