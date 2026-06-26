library(shiny)
library(bslib)
library(pwr)
library(ggplot2)

# ======================
# THEME
# ======================
my_theme <- bs_theme(
  version = 5,
  bootswatch = "lux"
)

# ======================
# UI
# ======================
ui <- page_navbar(
  title = HTML("SPASI<br><span style='font-size: 15px; font-weight: normal; text-transform: none !important; display: block; line-height: 1.2; letter-spacing: 0.5px;'>Smart Statistical Planning System</span>"),
  theme = my_theme,
  
  header = tags$head(
    tags$style(HTML("
      pre {
        font-family: Consolas, Courier New, monospace;
        font-size: 14px;
        white-space: pre-wrap;
      }
      blockquote {
        border-left: 4px solid #0D47A1;
        padding-left: 15px;
        background-color: #f8f9fa;
        padding-top: 10px;
        padding-bottom: 10px;
        border-radius: 0 4px 4px 0;
      }
      .navbar-brand {
        padding-top: 0px;
        padding-bottom: 0px;
      }
      
      @media (min-width: 992px) {
        nav.navbar .container-fluid {
          display: flex !important;
          justify-content: space-between !important;
        }
        nav.navbar .navbar-collapse {
          flex-grow: 0 !important;
          display: flex !important;
          justify-content: flex-end !important;
        }
        nav.navbar .navbar-nav {
          margin-left: auto !important;
          margin-right: 0px !important;
        }
      }
    "))
  ),
)
  