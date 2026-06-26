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
  # SIDEBAR 
  sidebar = sidebar(
    title = "Configuration",
    
    selectInput(
      "test_family",
      "Test Family",
      choices = c(
        "t tests",
        "Correlation and Regression",
        "F tests"
      )
    )
  ),
  
  uiOutput("stat_test_ui"),
  
  selectInput(
    "power_type",
    "Type of Power Analysis",
    choices = c(
      "A priori: Compute required sample size",
      "Post hoc: Compute achieved power",
      "Sensitivity: Compute effect size"
    )
  ), 
  
  sliderInput(
    "power",
    "Power",
    min = 0.50,
    max = 0.99,
    value = 0.80
  ),
  
  sliderInput(
    "sig_level",
    "Alpha",
    min = 0.01,
    max = 0.10,
    value = 0.05
  ),
  
  numericInput(
    "effect",
    "Effect Size",
    value = 0.5,
    step = 0.1
  ),
  
  actionButton(
    "calc",
    "Calculate",
    class = "btn-primary w-100"
  ), 
  
  
  # ESTIMATOR
  nav_panel(
    "Estimator",
    layout_column_wrap(
      width = 1,
      
      card(
        card_header("Power Analysis Result"),
        verbatimTextOutput("result_text")
      ),
      
      card(
        navset_card_tab(
          nav_panel(
            "Central and noncentral distributions",
            plotOutput("power_plot", height = 500)
          ),
          nav_panel(
            "Protocol of power analyses",
            verbatimTextOutput("protocol_text")
          )
        )
      )
    )
  ),
  
  # SENSITIVITY ANALYSIS
  nav_panel(
    "Sensitivity Analysis",
    layout_column_wrap(
      width = 1/2, 
      
      card(
        card_header("Heatmap Sample Size"),
        plotOutput("heatmap_plot", height = 400)
      ),
      
      card(
        card_header("Grafik Hubungan Effect Size & Sample Size"),
        plotOutput("curve_plot", height = 400)
      )
    ),
    
    card(
      card_header("Tabel Berbagai Effect Size & Power Matrix"),
      tableOutput("sens_table")
    )
  ),
) 