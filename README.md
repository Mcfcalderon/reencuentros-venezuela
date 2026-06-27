# Reencuentros Terremotos Venezuela

Plataforma humanitaria para centralizar reportes de personas y animales encontrados tras terremotos en Venezuela, facilitando el reencuentro con sus familias.

**Build4Venezuela Hackathon 2026**

## Funcionalidades

- **Reportar SOS**: Registrar personas/animales encontrados con fotos, video y descripción
- **Búsqueda Tradicional**: Filtrar reportes por nombre, ubicación o tipo
- **Match IA**: Búsqueda semántica con Google Gemini — describe libremente a quien buscas
- **Código de 4 dígitos**: Privacidad y verificación para marcar casos como reunificados
- **Guía Humanitaria**: Fuentes oficiales y números de emergencia

## Stack Tecnológico

| Componente | Tecnología |
|------------|------------|
| Frontend | R Shiny + bslib (Bootstrap 5) |
| Base de datos | MongoDB Atlas (cloud) |
| IA | Google Gemini 2.5 Flash vía ellmer |
| Hosting | shinyapps.io |

## Setup Local

```bash
# 1. Clonar repo
git clone https://github.com/Mcfcalderon/reencuentros-venezuela.git
cd reencuentros-venezuela

# 2. Instalar paquetes
install.packages(c("shiny", "bslib", "shinyjs", "mongolite", "jsonlite", "ellmer", "base64enc"))

# 3. Configurar variables de entorno
cp .Renviron.example .Renviron
# Editar .Renviron con tus credenciales de MongoDB y Gemini

# 4. Ejecutar
shiny::runApp()
```

## Estructura del Proyecto

```
├── app.R              # Punto de entrada + server principal
├── global.R           # Librerías + carga de módulos
├── deploy.R           # Script de deploy a shinyapps.io
├── R/
│   ├── db_mongo.R     # Operaciones MongoDB (CRUD)
│   └── ai_functions.R # Match IA con Google Gemini
├── server/
│   ├── server_reportar.R  # Lógica de reporte
│   ├── server_buscar.R    # Lógica de búsqueda
│   └── server_ayuda.R     # Reunificación y stats
├── ui/
│   ├── ui_main.R      # Layout principal
│   ├── ui_reportar.R  # Vista Reportar SOS
│   ├── ui_buscar.R    # Vista Buscar
│   └── ui_ayuda.R     # Vista Cómo Ayudar
└── www/
    └── styles.css     # Estilos custom
```

## Equipo

- Alejandra Castaño Alzate (Coordinación/Diseño)
- Iván Castillo
- Natalia Gajardo
- Mary Hengy Torres
- Marvin Calderón (Desarrollo web + IA)

## Licencia

MIT
