# ============================================================
#  Conjuntura do Crédito no Brasil — Shiny Dashboard
#  Autor: Lucas Viana | Dados: SGS/BCB
# ============================================================

# ----- 0. Pacotes -------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, bslib, plotly, dplyr, tidyr, lubridate,
  GetBCBData, deflateBR, scales, shinycssloaders,
  shinyWidgets, DT, zoo
)

# ----- 1. Constantes ----------------------------------------
SGS_IDS <- c(
  "Concessões de crédito - Total" = 20631,
  "Concessões de crédito - PJ"    = 20632,
  "Concessões de crédito - PF"    = 20633,
  "Taxa média de juros"           = 20714,
  "Inadimplência"                 = 21082
)

COR_PRIMARIA   <- "royalblue"
COR_SECUNDARIA <- "orange"
COR_ACENTO     <- "darkred"
COR_TEAL       <- "darkgreen"
COR_CLARO      <- "white"

# ----- 2. Tema bslib ----------------------------------------
tema <- bs_theme(
  version      = 5,
  bg           = "#F7F9FC",
  fg           = "#1A2533",
  primary      = COR_PRIMARIA,
  secondary    = COR_SECUNDARIA,
  base_font    = font_google("IBM Plex Sans"),
  heading_font = font_google("Playfair Display"),
  code_font    = font_google("IBM Plex Mono"),
  `navbar-bg`  = COR_PRIMARIA
) |>
  bs_add_rules("
    .value-box .value-box-value { font-family: 'Playfair Display', serif !important; }
    .card { border: none !important; box-shadow: 0 2px 12px rgba(27,58,92,0.08) !important; border-radius: 12px !important; }
    .card-header { background: linear-gradient(135deg, #1B3A5C 0%, #243E62 100%) !important; color: white !important; border-radius: 12px 12px 0 0 !important; font-family: 'Playfair Display', serif; letter-spacing: 0.03em; }
    .sidebar { background: #EEF2F7 !important; border-right: 2px solid #D6E0EC !important; }
    .navbar-brand { font-family: 'Playfair Display', serif !important; font-size: 1.2rem !important; letter-spacing: 0.04em; }
    .badge-periodo { background: #E8A838; color: #1A2533; font-family: 'IBM Plex Mono'; font-size: 0.75rem; padding: 3px 8px; border-radius: 4px; }
    hr.divisor { border-color: #D6E0EC; margin: 1rem 0; }
    .insight-box { background: linear-gradient(135deg, #EEF2F7, #E4EBF5); border-left: 4px solid #E8A838; border-radius: 6px; padding: 0.9rem 1.1rem; font-size: 0.87rem; line-height: 1.6; margin-top: 0.8rem; }
    .fonte-tag { font-family: 'IBM Plex Mono'; font-size: 0.72rem; color: #7A8EA5; }
    .nav-tabs .nav-link.active { border-bottom: 3px solid #E8A838 !important; font-weight: 600; color: #1B3A5C !important; }
    .tab-content { padding-top: 1rem; }
  ")

# ----- 3. Funções auxiliares --------------------------------
formatar_bi <- function(x) paste0("R$ ", round(x / 1000, 1), " bi")
formatar_pct <- function(x) paste0(round(x, 2), "%")

# Helper para ícone + texto (movido para antes da UI)
icon_text <- function(ico, txt) tagList(icon(ico), txt)

layout_plotly <- function(p, titulo, ytitle, fonte = "SGS/BCB") {
  p |>
    layout(
      title = list(
        text = paste0("<b>", titulo, "</b>"),
        font = list(family = "Playfair Display", size = 16, color = COR_PRIMARIA),
        x = 0.01
      ),
      xaxis = list(
        title = "", showgrid = FALSE,
        tickfont = list(family = "IBM Plex Sans", size = 11, color = "#5A6E84")
      ),
      yaxis = list(
        title = list(text = ytitle, font = list(family = "IBM Plex Sans", size = 11, color = "#5A6E84")),
        showgrid = TRUE, gridcolor = "#E4EBF5", gridwidth = 1,
        tickfont = list(family = "IBM Plex Mono", size = 11, color = "#5A6E84")
      ),
      legend = list(
        orientation = "h", x = 0.01, y = -0.15,
        font = list(family = "IBM Plex Sans", size = 11)
      ),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin = list(l = 60, r = 30, t = 60, b = 80),
      annotations = list(list(
        text = paste0("Fonte: ", fonte),
        xref = "paper", yref = "paper",
        x = 0, y = -0.22, showarrow = FALSE,
        font = list(family = "IBM Plex Mono", size = 10, color = "#7A8EA5")
      )),
      hovermode = "x unified"
    ) |>
    config(displayModeBar = FALSE, locale = "pt-BR")
}

# ----- 4. UI ------------------------------------------------
ui <- page_navbar(
  title = "Crédito Brasil",
  theme = tema,
  window_title = "Conjuntura do Crédito — BCB",
  
  # ── Painel Principal ──────────────────────────────────────
  nav_panel(
    title = icon_text("chart-line", " Painel"),
    
    layout_sidebar(
      # ── Sidebar ─────────────────────────────────────
      sidebar = sidebar(
        width = 260,
        tags$div(
          class = "mb-3",
          tags$p(
            class = "fonte-tag",
            "Dados: SGS/BCB via GetBCBData"
          )
        ),
        hr(class = "divisor"),
        
        dateRangeInput(
          "periodo",
          label = tags$b("Período de análise"),
          start = "2019-01-01",
          end   = "2026-01-01",
          min   = "2019-01-01",
          max   = Sys.Date(),
          format = "mm/yyyy",
          language = "pt-BR",
          separator = " até "
        ),
        
        hr(class = "divisor"),
        
        tags$b("Deflação"),
        switchInput(
          "deflacionar",
          label   = "Deflacionar pelo IPCA",
          value   = TRUE,
          onLabel = "SIM",
          offLabel = "NÃO",
          onStatus = "success",
          size = "small"
        ),
        
        hr(class = "divisor"),
        
        actionButton(
          "atualizar",
          label = tagList(icon("rotate"), " Atualizar dados"),
          class = "btn-primary w-100"
        ),
        
        hr(class = "divisor"),
        tags$small(
          class = "text-muted",
          "Preços de referência: jan/2026.",
          tags$br(),
          "Período: jan/2019 – jan/2026"
        )
      ),
      
      # ── Corpo principal ──────────────────────────────
      tags$div(
        # KPIs
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          
          value_box(
            title = "Concessões — Jan/2026",
            value = textOutput("kpi_concessoes"),
            theme  = "primary",
            height = "110px"
          ),
          value_box(
            title = "Taxa média de juros",
            value = textOutput("kpi_juros"),
            theme  = "warning",
            height = "110px"
          ),
          value_box(
            title = "Inadimplência",
            value = textOutput("kpi_inadimplencia"),
            theme  = value_box_theme(bg = COR_ACENTO, fg = "white"),
            height = "110px"
          ),
          value_box(
            title = "Participação PF",
            value = textOutput("kpi_pf"),
            theme  = value_box_theme(bg = COR_TEAL, fg = "white"),
            height = "110px"
          )
        ),
        
        tags$br(),
        
        # Gráficos linha 1
        layout_columns(
          col_widths = c(8, 4),
          
          card(
            card_header("Concessões de Crédito — Total"),
            withSpinner(
              plotlyOutput("plot_concessoes", height = "320px"),
              color = COR_PRIMARIA, type = 6
            ),
            tags$div(
              class = "insight-box mx-3 mb-2",
              "📊 O volume real de concessões praticamente dobrou entre 2019 e 2026,
              passando de ~R$ 300 bi para ~R$ 700 bi — crescimento expressivo mesmo
              durante o ciclo de aperto monetário mais intenso desde o Plano Real."
            )
          ),
          
          card(
            card_header("Participação PF vs. PJ"),
            withSpinner(
              plotlyOutput("plot_participacao", height = "320px"),
              color = COR_PRIMARIA, type = 6
            ),
            tags$div(
              class = "insight-box mx-3 mb-2",
              "📈 Composição estável em ~45-55%. Pico PJ em 2020 (~58%) reflete
              linhas emergenciais anticíclicas (PRONAMPE, FGI)."
            )
          )
        ),
        
        tags$br(),
        
        # Gráficos linha 2
        layout_columns(
          col_widths = c(6, 6),
          
          card(
            card_header("Taxa Média de Juros"),
            withSpinner(
              plotlyOutput("plot_juros", height = "300px"),
              color = COR_PRIMARIA, type = 6
            ),
            tags$div(
              class = "insight-box mx-3 mb-2",
              "📉 Dois ciclos distintos: queda até piso histórico ~19% a.a. (2020) e
              alta até ~32% a.a. (2025-26), superando o pico de 2023."
            )
          ),
          
          card(
            card_header("Inadimplência da Carteira"),
            withSpinner(
              plotlyOutput("plot_inadimplencia", height = "300px"),
              color = COR_PRIMARIA, type = 6
            ),
            tags$div(
              class = "insight-box mx-3 mb-2",
              "⚠️ Nível atual (~4,1%) supera o pico pós-pandemia de 2023 e está 37 bps
              acima do pré-pandemia. A queda de 2020 foi artefato de moratórias regulatórias."
            )
          )
        )
      )
    )
  ),
  
  # ── Aba Dados ─────────────────────────────────────────────
  nav_panel(
    title = icon_text("table", " Dados"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        selectInput(
          "serie_tabela",
          "Série",
          choices = c(
            "Concessões — Total"  = "Concessões de crédito - Total",
            "Concessões — PJ"     = "Concessões de crédito - PJ",
            "Concessões — PF"     = "Concessões de crédito - PF",
            "Taxa média de juros" = "Taxa média de juros",
            "Inadimplência"       = "Inadimplência"
          )
        ),
        tags$br(),
        downloadButton("baixar_csv", "Baixar CSV", class = "btn-outline-primary w-100")
      ),
      card(
        card_header("Série histórica"),
        DTOutput("tabela_dados")
      )
    )
  ),
  
  # ── Aba Metodologia ───────────────────────────────────────
  nav_panel(
    title = icon_text("info-circle", " Metodologia"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Fontes e Tratamento"),
        card_body(
          tags$h5("Séries utilizadas (SGS/BCB)", style = "font-family: 'Playfair Display'; color: #1B3A5C;"),
          tags$table(
            class = "table table-sm",
            tags$thead(tags$tr(
              tags$th("Código SGS"), tags$th("Descrição"), tags$th("Unidade")
            )),
            tags$tbody(
              tags$tr(tags$td("20631"), tags$td("Concessões — Total"), tags$td("R$ milhões")),
              tags$tr(tags$td("20632"), tags$td("Concessões — PJ"), tags$td("R$ milhões")),
              tags$tr(tags$td("20633"), tags$td("Concessões — PF"), tags$td("R$ milhões")),
              tags$tr(tags$td("20714"), tags$td("Taxa média de juros"), tags$td("% a.a.")),
              tags$tr(tags$td("21082"), tags$td("Inadimplência"), tags$td("%"))
            )
          ),
          hr(),
          tags$h5("Deflação", style = "font-family: 'Playfair Display'; color: #1B3A5C;"),
          tags$p("Valores nominais deflacionados pelo IPCA a preços de jan/2026 via pacote ",
                 tags$code("deflateBR"), "."),
          tags$h5("Dados via API", style = "font-family: 'Playfair Display'; color: #1B3A5C;"),
          tags$p("Acesso às séries via pacote ", tags$code("GetBCBData"),
                 " (API pública do SGS/BCB). Nenhuma chave de API necessária."),
          tags$h5("Visualizações", style = "font-family: 'Playfair Display'; color: #1B3A5C;"),
          tags$p("Gráficos interativos com ", tags$code("plotly"), ". Dashboard construído com ",
                 tags$code("shiny"), " + ", tags$code("bslib"), ".")
        )
      ),
      card(
        card_header("Sobre o Projeto"),
        card_body(
          tags$p("Análise da dinâmica recente do mercado de crédito brasileiro em termos reais,
                 com foco na transmissão da política monetária via canal de crédito."),
          tags$p(tags$b("Período:"), " jan/2019 – jan/2026"),
          tags$p(tags$b("Autor:"), " Lucas Viana"),
          hr(),
          tags$p(
            tags$a(
              href = "https://github.com/vianalucaz/Macro",
              target = "_blank",
              class = "btn btn-outline-primary btn-sm",
              icon("github"), " Ver no GitHub"
            )
          ),
          tags$p(
            tags$a(
              href = "https://www.bcb.gov.br/",
              target = "_blank",
              class = "btn btn-outline-secondary btn-sm mt-1",
              "🏦 Banco Central do Brasil"
            )
          )
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item(
    tags$span(
      class = "badge-periodo",
      "Jan/2019 – Jan/2026"
    )
  )
)

# ----- 5. Server --------------------------------------------
server <- function(input, output, session) {
  
  # ── 6.1 Buscar dados (reactive com cache) ─────────────────
  dados_raw <- reactive({
    input$atualizar  # dependência para recarregar
    
    withProgress(message = "Buscando dados no SGS/BCB…", value = 0.3, {
      tryCatch({
        df <- GetBCBData::gbcbd_get_series(
          id          = SGS_IDS,
          first.date  = "2019-01-01",
          use.memoise = FALSE
        ) |>
          dplyr::select(
            data   = ref.date,
            valor  = value,
            series = series.name
          )
        setProgress(1)
        df
      }, error = function(e) {
        showNotification(
          paste("Erro ao buscar dados:", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    })
  })
  
  # ── 6.2 Dados filtrados pelo período ──────────────────────
  dados_filtrados <- reactive({
    req(dados_raw())
    dados_raw() |>
      dplyr::filter(
        data >= input$periodo[1],
        data <= input$periodo[2]
      )
  })
  
  # ── 6.3 Série de concessões (com deflação opcional) ────────
  concessoes_total <- reactive({
    req(dados_filtrados())
    df <- dados_filtrados() |>
      dplyr::filter(series == "Concessões de crédito - Total")
    
    if (input$deflacionar) {
      df <- df |>
        dplyr::mutate(
          valor = deflateBR::deflate(valor, data, "01/2026", "ipca")
        )
    }
    df
  })
  
  # ── 6.4 KPIs ──────────────────────────────────────────────
  ultimo_valor <- function(serie) {
    req(dados_raw())
    dados_raw() |>
      dplyr::filter(series == serie) |>
      dplyr::slice_max(data, n = 1) |>
      dplyr::pull(valor)
  }
  
  output$kpi_concessoes <- renderText({
    v <- tryCatch(ultimo_valor("Concessões de crédito - Total"), error = function(e) NA)
    if (is.na(v)) "—" else formatar_bi(v)
  })
  
  output$kpi_juros <- renderText({
    v <- tryCatch(ultimo_valor("Taxa média de juros"), error = function(e) NA)
    if (is.na(v)) "—" else paste0(round(v, 1), "% a.a.")
  })
  
  output$kpi_inadimplencia <- renderText({
    v <- tryCatch(ultimo_valor("Inadimplência"), error = function(e) NA)
    if (is.na(v)) "—" else formatar_pct(v)
  })
  
  output$kpi_pf <- renderText({
    req(dados_raw())
    df <- dados_raw() |>
      dplyr::filter(series %in% c("Concessões de crédito - PF", "Concessões de crédito - Total")) |>
      dplyr::slice_max(data, n = 2) |>
      tidyr::pivot_wider(names_from = series, values_from = valor)
    
    v <- tryCatch(
      df[["Concessões de crédito - PF"]] / df[["Concessões de crédito - Total"]] * 100,
      error = function(e) NA
    )
    if (all(is.na(v))) "—" else formatar_pct(mean(v, na.rm = TRUE))
  })
  
  # ── 6.5 Gráfico: Concessões ───────────────────────────────
  output$plot_concessoes <- renderPlotly({
    req(concessoes_total())
    df <- concessoes_total()
    ytitle <- if (input$deflacionar) "R$ bilhões (preços jan/2026)" else "R$ bilhões"
    
    p <- plot_ly() |>
      add_lines(
        data = df,
        x = ~data, y = ~(valor / 1000),
        name = "Concessões mensais",
        line = list(color = COR_ACENTO, width = 1.5),
        hovertemplate = "R$ %{y:.1f} bi<extra></extra>"
      ) |>
      add_lines(
        data = df |>
          dplyr::mutate(ma12 = zoo::rollmean(valor, k = 12, fill = NA, align = "right") / 1000),
        x = ~data, y = ~ma12,
        name = "Média móvel 12m",
        line = list(color = COR_PRIMARIA, width = 2.5, dash = "solid"),
        hovertemplate = "MM12: R$ %{y:.1f} bi<extra></extra>"
      )
    
    layout_plotly(p, "Concessões de Crédito — Total", ytitle)
  })
  
  # ── 6.6 Gráfico: Participação PF/PJ ──────────────────────
  output$plot_participacao <- renderPlotly({
    req(dados_filtrados())
    
    df_part <- dados_filtrados() |>
      dplyr::filter(series %in% c(
        "Concessões de crédito - Total",
        "Concessões de crédito - PJ",
        "Concessões de crédito - PF"
      )) |>
      tidyr::pivot_wider(id_cols = "data", names_from = "series", values_from = "valor") |>
      dplyr::transmute(
        data            = data,
        `Pessoa Física`   = `Concessões de crédito - PF`    / `Concessões de crédito - Total` * 100,
        `Pessoa Jurídica` = `Concessões de crédito - PJ`    / `Concessões de crédito - Total` * 100
      )
    
    p <- plot_ly(df_part, x = ~data) |>
      add_bars(
        y = ~`Pessoa Física`,
        name = "Pessoa Física",
        marker = list(color = "#FA8072"),
        hovertemplate = "PF: %{y:.1f}%<extra></extra>"
      ) |>
      add_bars(
        y = ~`Pessoa Jurídica`,
        name = "Pessoa Jurídica",
        marker = list(color = COR_TEAL),
        hovertemplate = "PJ: %{y:.1f}%<extra></extra>"
      ) |>
      layout(barmode = "stack")
    
    layout_plotly(p, "Participação nas Concessões", "%")
  })
  
  # ── 6.7 Gráfico: Taxa de juros ────────────────────────────
  output$plot_juros <- renderPlotly({
    req(dados_filtrados())
    
    df_j <- dados_filtrados() |>
      dplyr::filter(series == "Taxa média de juros")
    
    p <- plot_ly(df_j, x = ~data, y = ~valor) |>
      add_lines(
        name = "Taxa média",
        line = list(color = COR_PRIMARIA, width = 2.5),
        hovertemplate = "%{y:.2f}% a.a.<extra></extra>"
      ) |>
      add_ribbons(
        ymin = ~pmin(valor, 25), ymax = ~pmax(valor, 25),
        fillcolor = "rgba(27,58,92,0.08)",
        line = list(color = "transparent"),
        name = "Área ref.",
        showlegend = FALSE
      ) |>
      layout(
        shapes = list(list(
          type = "rect",
          x0 = "2020-02-01", x1 = "2021-10-01",
          y0 = 0, y1 = 40,
          fillcolor = "rgba(41,182,246,0.1)",
          line = list(color = "rgba(0,0,0,0)")
        )),
        annotations = list(list(
          text = "Ciclo pandêmico",
          x = "2020-10-01", y = 33,
          showarrow = FALSE,
          font = list(family = "IBM Plex Sans", size = 10, color = "#7A8EA5")
        ))
      )
    
    layout_plotly(p, "Taxa Média de Juros", "% a.a.")
  })
  
  # ── 6.8 Gráfico: Inadimplência ────────────────────────────
  output$plot_inadimplencia <- renderPlotly({
    req(dados_filtrados())
    
    df_i <- dados_filtrados() |>
      dplyr::filter(series == "Inadimplência")
    
    p <- plot_ly(df_i, x = ~data, y = ~valor) |>
      add_lines(
        name = "Inadimplência",
        line = list(color = COR_ACENTO, width = 2.5),
        hovertemplate = "%{y:.2f}%<extra></extra>"
      ) |>
      add_lines(
        x = ~data, y = rep(3.0, nrow(df_i)),
        name = "Nível pré-pandemia",
        line = list(color = "#888", dash = "dash", width = 1.5),
        hovertemplate = "Ref.: 3,0%<extra></extra>"
      )
    
    layout_plotly(p, "Inadimplência da Carteira de Crédito", "%")
  })
  
  # ── 6.9 Tabela ────────────────────────────────────────────
  output$tabela_dados <- renderDT({
    req(dados_filtrados())
    df <- dados_filtrados() |>
      dplyr::filter(series == input$serie_tabela) |>
      dplyr::arrange(desc(data)) |>
      dplyr::transmute(
        Data  = format(data, "%b/%Y"),
        Valor = round(valor, 4),
        Série = series
      )
    
    datatable(
      df,
      options = list(
        pageLength = 15,
        language   = list(url = "//cdn.datatables.net/plug-ins/1.11.5/i18n/pt-BR.json")
      ),
      rownames = FALSE,
      class = "table-sm table-hover"
    )
  })
  
  output$baixar_csv <- downloadHandler(
    filename = function() paste0("credito_brasil_", input$serie_tabela, ".csv"),
    content  = function(file) {
      req(dados_filtrados())
      dados_filtrados() |>
        dplyr::filter(series == input$serie_tabela) |>
        dplyr::arrange(data) |>
        write.csv(file, row.names = FALSE)
    }
  )
}

# ----- 7. Run -----------------------------------------------
shinyApp(ui = ui, server = server)