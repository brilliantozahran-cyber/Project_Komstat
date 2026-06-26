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
    ) 
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
  
  # EFFECT SIZE CALCULATOR
  nav_panel(
    "Effect Size Calculator",
    layout_column_wrap(
      width = 1/2,
      
      card(
        card_header("Input Nilai Kelompok"),
        numericInput("mean1", "Mean Kelompok 1 (Kontrol / Pre-test)", value = 50, step = 0.1),
        numericInput("mean2", "Mean Kelompok 2 (Eksperimen / Pasca-test)", value = 55, step = 0.1),
        numericInput("sd_pooled", "Standard Deviation (SD) Pooled", value = 10, min = 0.001, step = 0.1),
        hr(),
        actionButton("send_effect", "Kirim ke Power Analysis", class = "btn-success w-100")
      ),
      
      card(
        card_header("Hasil Perhitungan Cohen's d"),
        uiOutput("cohens_d_result")
      )
    )
  ),
  
  # INTERPRETASI OTOMATIS
  nav_panel(
    "Automatic Interpretation",
    card(
      card_header("Narasi Hasil Analisis (Format Skripsi / Artikel Ilmiah)"),
      p(class = "text-muted", "Catatan: Pastikan Anda sudah menekan tombol 'Calculate' di sidebar agar narasi ini sinkron dengan data terbaru."),
      hr(),
      uiOutput("interpretation_text")
    )
  )
)

# ======================
# SERVER
# ======================
server <- function(input, output, session){
  
  # Dynamic Statistical Test Menu
  output$stat_test_ui <- renderUI({
    if(input$test_family == "t tests"){
      selectInput(
        "stat_test",
        "Statistical Test",
        choices = c(
          "Means: Difference between two independent means" = "two.sample",
          "Means: Difference between paired means" = "paired",
          "Means: Difference from constant" = "one.sample"
        )
      )
    } else if(input$test_family == "Correlation and Regression") {
      selectInput(
        "stat_test",
        "Statistical Test",
        choices = c(
          "Correlation: Bivariate normal model" = "correlation"
        )
      )
    } else {
      selectInput(
        "stat_test",
        "Statistical Test",
        choices = c(
          "ANOVA: Fixed effects, omnibus" = "anova",
          "ANOVA: Repeated measures, within factors" = "anova.rm"
        )
      )
    }
  })
  
  # Power Analysis Calculation
  calc_res <- eventReactive(input$calc, {
    req(input$stat_test)
    if(input$stat_test %in% c("two.sample","paired","one.sample")){
      pwr.t.test(
        d = input$effect,
        power = input$power,
        sig.level = input$sig_level,
        type = input$stat_test
      )
    } else if(input$stat_test == "correlation") {
      pwr.r.test(
        r = input$effect,
        power = input$power,
        sig.level = input$sig_level
      )
    } else if(input$stat_test == "anova") {
      pwr.anova.test(
        k = 3,
        f = input$effect,
        power = input$power,
        sig.level = input$sig_level
      )
    } else if(input$stat_test == "anova.rm") {
      f_adj <- input$effect / sqrt(1 - 0.5)
      pwr.anova.test(
        k = 3,
        f = f_adj,
        power = input$power,
        sig.level = input$sig_level
      )
    }
  })
  
  # Data Generator for Sensitivity Matrix
  sens_data <- eventReactive(input$calc, {
    req(input$stat_test)
    
    if(input$stat_test == "correlation") {
      es_seq <- seq(0.1, 0.9, by = 0.1)
    } else {
      es_seq <- seq(0.1, 1.2, by = 0.1)
    }
    
    pow_seq <- c(0.70, 0.80, 0.85, 0.90, 0.95)
    grid <- expand.grid(EffectSize = es_seq, Power = pow_seq)
    
    grid$Required_N <- sapply(1:nrow(grid), function(i) {
      es <- grid$EffectSize[i]
      p <- grid$Power[i]
      alpha <- input$sig_level
      
      val <- tryCatch({
        if(input$stat_test %in% c("two.sample","paired","one.sample")){
          pwr.t.test(d = es, power = p, sig.level = alpha, type = input$stat_test)$n
        } else if(input$stat_test == "correlation") {
          pwr.r.test(r = es, power = p, sig.level = alpha)$n
        } else if(input$stat_test == "anova") {
          pwr.anova.test(k = 3, f = es, power = p, sig.level = alpha)$n
        } else if(input$stat_test == "anova.rm") {
          f_adj <- es / sqrt(1 - 0.5)
          pwr.anova.test(k = 3, f = f_adj, power = p, sig.level = alpha)$n
        }
      }, error = function(e) return(NA))
      
      return(ceiling(val))
    })
    
    grid
  })
  
  # Tab 1 Outputs
  output$result_text <- renderPrint({
    req(calc_res())
    print(calc_res())
  })
  
  output$protocol_text <- renderText({
    req(calc_res())
    res <- calc_res()
    
    stat_name <- switch(
      input$stat_test,
      "two.sample" = "Means: Difference between two independent means",
      "paired" = "Means: Difference between paired means",
      "one.sample" = "Means: Difference from constant",
      "correlation" = "Correlation: Bivariate normal model",
      "anova" = "ANOVA: Fixed effects, omnibus",
      "anova.rm" = "ANOVA: Repeated measures, within factors"
    )
    
    txt <- paste0(
      "[1] -- ", format(Sys.time(), "%A, %B %d, %Y -- %H:%M:%S"), "\n\n",
      input$test_family, " - ", stat_name, "\n\nAnalysis:\n    ", 
      input$power_type, "\n\nInput:\n    Effect size = ", input$effect,
      "\n    α err prob = ", input$sig_level, "\n    Power (1-β err prob) = ", input$power, "\n\nOutput:\n"
    )
    
    if(!is.null(res$n)){ txt <- paste0(txt, "    Required sample size = ", ceiling(res$n), "\n") }
    if(!is.null(res$power)){ txt <- paste0(txt, "    Actual power = ", round(res$power, 6), "\n") }
    if(!is.null(res$sig.level)){ txt <- paste0(txt, "    Alpha = ", round(res$sig.level, 6)) }
    txt
  })
  
  output$power_plot <- renderPlot({
    req(calc_res())
    
    effect_seq <- seq(max(0.1, input$effect - 0.8), input$effect + 0.8, length.out = 50)
    power_vals <- sapply(effect_seq, function(es){
      if(input$stat_test %in% c("two.sample","paired","one.sample")){
        pwr.t.test(d = es, sig.level = input$sig_level, n = ceiling(calc_res()$n), type = input$stat_test)$power
      } else if(input$stat_test == "correlation") {
        pwr.r.test(r = es, sig.level = input$sig_level, n = ceiling(calc_res()$n))$power
      } else if(input$stat_test == "anova"){
        pwr.anova.test(k = 3, f = es, sig.level = input$sig_level, n = ceiling(calc_res()$n))$power
      } else if(input$stat_test == "anova.rm"){
        f_adj <- es / sqrt(1 - 0.5)
        pwr.anova.test(k = 3, f = f_adj, sig.level = input$sig_level, n = ceiling(calc_res()$n))$power
      }
    })
    
    df <- data.frame(EffectSize = effect_seq, Power = power_vals)
    
    ggplot(df, aes(EffectSize, Power)) +
      geom_line(linewidth = 1.3, colour = "#0073C2FF") +
      geom_point(size = 2, colour = "#EFC000FF") +
      geom_hline(yintercept = input$power, linetype = 2, colour = "red") +
      theme_minimal(base_size = 14) +
      labs(title = "Power Curve", x = "Effect Size", y = "Power")
  })
  
  # Tab 2 Outputs
  output$heatmap_plot <- renderPlot({
    df <- sens_data()
    req(df)
    
    ggplot(df, aes(x = factor(EffectSize), y = factor(Power), fill = Required_N)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient(low = "#E3F2FD", high = "#0D47A1", na.value = "grey90") +
      geom_text(aes(label = Required_N), color = "black", size = 3.5) +
      theme_minimal(base_size = 13) +
      labs(x = "Effect Size", y = "Power (1 - Beta)", fill = "Sample Size (N)") +
      theme(panel.grid = element_blank())
  })
  
  output$curve_plot <- renderPlot({
    df <- sens_data()
    req(df)
    
    ggplot(df, aes(x = EffectSize, y = Required_N, color = factor(Power), group = Power)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal(base_size = 13) +
      labs(x = "Effect Size", y = "Required Sample Size (N)", color = "Power Level")
  })
  
  output$sens_table <- renderTable({
    df <- sens_data()
    req(df)
    
    df_wide <- reshape(df, idvar = "EffectSize", timevar = "Power", direction = "wide")
    pow_seq <- c(0.70, 0.80, 0.85, 0.90, 0.95)
    colnames(df_wide) <- c("Effect Size", paste0("Power = ", pow_seq))
    
    df_wide
  }, digits = 2, align = "c", striped = TRUE, hover = TRUE)
  
  # Tab 3: Effect Size Calculator
  cohens_d_calc <- reactive({
    req(input$sd_pooled)
    if(input$sd_pooled <= 0) return(0)
    abs(input$mean1 - input$mean2) / input$sd_pooled
  })
  
  output$cohens_d_result <- renderUI({
    d_val <- round(cohens_d_calc(), 3)
    magnitude <- if(d_val < 0.2) {
      "Negligible (Efek sangat kecil / dapat diabaikan)"
    } else if(d_val >= 0.2 && d_val < 0.5) {
      "Small Effect (Efek Kecil)"
    } else if(d_val >= 0.5 && d_val < 0.8) {
      "Medium Effect (Efek Sedang)"
    } else {
      "Large Effect (Efek Besar)"
    }
    
    HTML(paste0(
      "<div style='margin-top: 15px;'>",
      "<h5>Nilai Efek Terhitung:</h5>",
      "<h1 style='color: #0D47A1; font-weight: bold;'>", d_val, "</h1>",
      "<h5>Klasifikasi (Cohen's Criteria):</h5>",
      "<span class='badge bg-primary' style='font-size: 14px; padding: 8px 12px;'>", magnitude, "</span>",
      "<hr style='margin-top:25px;'/>",
      "<p class='text-muted'>*Gunakan tombol di bawah kolom input untuk mentransfer nilai ini secara instan ke menu utama di sidebar kiri.</p>",
      "</div>"
    ))
  })
  
  observeEvent(input$send_effect, {
    d_val <- round(cohens_d_calc(), 3)
    updateNumericInput(session, "effect", value = d_val)
    showNotification(
      paste0("Berhasil! Nilai Effect Size (", d_val, ") disalin ke konfigurasi utama. Silakan klik 'Calculate' untuk memperbarui data."),
      type = "message",
      duration = 6
    )
  })
  
  # Tab 4: Automatic Interpretation
  output$interpretation_text <- renderUI({
    req(calc_res())
    res <- calc_res()
    
    stat_name_id <- switch(
      input$stat_test,
      "two.sample" = "Uji t Dua Sampel Independen (Independent Samples t-test)",
      "paired" = "Uji t Sampel Berpasangan (Paired Samples t-test)",
      "one.sample" = "Uji t Satu Sampel (One-Sample t-test)",
      "correlation" = "Analisis Korelasi Bivariat (Bivariate Correlation Model)",
      "anova" = "Uji ANOVA Satu Arah (One-Way ANOVA)",
      "anova.rm" = "Uji ANOVA Pengukuran Berulang (ANOVA Repeated Measures, Within Factors)"
    )
    
    effect_name <- switch(
      input$test_family,
      "t tests" = paste0("Cohen's d sebesar ", input$effect),
      "Correlation and Regression" = paste0("Koefisien Korelasi (r) sebesar ", input$effect),
      "F tests" = paste0("Effect Size f sebesar ", input$effect)
    )
    
    n_per_group <- ceiling(res$n)
    sample_text <- ""
    
    if(input$stat_test == "two.sample") {
      sample_text <- paste0("<strong>", n_per_group, " responden per kelompok</strong> (sehingga total keseluruhan sampel yang dibutuhkan adalah <strong>", n_per_group * 2, " responden</strong>)")
    } else if(input$stat_test == "anova") {
      sample_text <- paste0("<strong>", n_per_group, " responden per kelompok</strong> (sehingga total sampel untuk 3 kelompok adalah <strong>", n_per_group * 3, " responden</strong>)")
    } else if(input$stat_test == "anova.rm") {
      sample_text <- paste0("<strong>", n_per_group, " subjek/responden tunggal</strong> yang diukur secara berulang pada setiap kondisi/faktor waktu penelitian")
    } else {
      sample_text <- paste0("<strong>", n_per_group, " responden</strong>")
    }
    
    HTML(paste0(
      "<h5>1. Draf untuk Bab 3: Metodologi Penelitian (Sub-bab Ukuran Sampel)</h5>",
      "<blockquote>",
      "Dalam penelitian ini, penentuan ukuran sampel minimum dilakukan secara objektif menggunakan analisis kekuatan uji statistik (<em>A priori Power Analysis</em>). ",
      "Prosedur ini diterapkan untuk memastikan bahwa desain penelitian memiliki sensitivitas yang memadai untuk mendeteksi efek yang diteliti serta menekan probabilitas terjadinya Kesalahan Tipe II (<em>Type II Error</em>). ",
      "Berdasarkan jenis analisis data yang akan digunakan, yaitu metode <strong>", stat_name_id, "</strong>, peneliti menetapkan kriteria pengujian berupa tingkat signifikansi (&alpha;) sebesar ", input$sig_level, " dan target kuas uji statistik (<em>statistical power</em>, 1-&beta;) sebesar ", input$power, ". ",
      "Dengan mengasumsikan standar nilai ", effect_name, ", hasil kalkulasi menunjukkan bahwa ukuran sampel minimum yang wajib dipenuhi adalah sebesar ", sample_text, ". ",
      "Dengan jumlah tersebut, pengujian hipotesis dalam penelitian ini diproyeksikan memiliki akurasi dan validitas statistikal yang tinggi.",
      "</blockquote>",
      
      "<br/><h5>2. Draf untuk Jurnal / Artikel Ilmiah (Format Ringkas)</h5>",
      "<blockquote>",
      "Ukuran sampel minimum ditentukan melalui <em>a priori power analysis</em> menggunakan paket statistika <code>pwr</code> untuk prosedur <strong>", stat_name_id, "</strong>. ",
      "Melalui spesifikasi parameter tingkat kekeliruan &alpha; = ", input$sig_level, ", kuas uji (<em>power</em>) = ", input$power, ", serta estimasi ukuran efek sebesar ", input$effect, ", ",
      "diperoleh rekomendasi ukuran sampel minimum sebesar N = ", if(input$stat_test == "two.sample") n_per_group * 2 else if(input$stat_test == "anova") n_per_group * 3 else n_per_group, ". ",
      "Oleh karena itu, ukuran sampel yang dilibatkan dalam penelitian ini telah memenuhi batas ambang kecukupan metodologis guna menjamin keterandalan kesimpulan statistik.",
      "</blockquote>"
    ))
  })
}

shinyApp(ui, server)
