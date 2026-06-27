# ============ Reencuentros Venezuela — global.R ============
# Carga librerías, funciones puras (R/) y módulos UI (ui/).
# Los módulos server/ se cargan en app.R con local=TRUE.

.startup_time <- proc.time()

options(shiny.maxRequestSize = 25 * 1024^2)  # 25 MB para videos

# ---- Librerías ----
library(shiny)
library(bslib)
library(shinyjs)
library(mongolite)
library(jsonlite)
library(jpeg)
library(png)
library(base64enc)

# ---- Funciones puras (carpeta R/) ----
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), function(f) {
  source(f)
  message("[Reencuentros] Loaded: ", f)
})

# ---- UI modules (carpeta ui/) ----
lapply(list.files("ui", pattern = "\\.R$", full.names = TRUE), function(f) {
  source(f)
  message("[Reencuentros] UI loaded: ", f)
})

# ============ APP CONFIG ============
APP_NAME <- "Reencuentro Tras Terremotos"
APP_SUBTITLE <- "Venezuela"
MAX_VIDEO_MB <- 20
MAX_FOTO_MB <- 5

# ============ LAZY INIT (MongoDB) ============
assign(".app_initialized", FALSE, envir = globalenv())

ensure_init <- function() {
  if (isTRUE(get0(".app_initialized", envir = globalenv()))) return(invisible(TRUE))
  tryCatch({
    uri <- Sys.getenv("MONGODB_URI_REENCUENTROS")
    # Fallback al URI general si no hay uno específico
    if (nchar(uri) == 0) uri <- Sys.getenv("MONGODB_URI")
    if (nchar(uri) == 0) {
      message("[Reencuentros] ensure_init: MONGODB_URI not set")
      return(invisible(FALSE))
    }
    test <- mongolite::mongo(collection = "reportes", url = uri)
    test$count()
    assign(".app_initialized", TRUE, envir = globalenv())
    message("[Reencuentros] MongoDB initialized OK")
    test$disconnect()
    invisible(TRUE)
  }, error = function(e) {
    message("[Reencuentros] ensure_init FAILED: ", e$message)
    invisible(FALSE)
  })
}

# ============ STARTUP COMPLETE ============
.elapsed <- (proc.time() - .startup_time)["elapsed"]
message(sprintf("[Reencuentros] global.R loaded in %.1fs - %d R/ + %d ui/ modules",
  .elapsed,
  length(list.files("R", pattern = "\\.R$")),
  length(list.files("ui", pattern = "\\.R$"))))
