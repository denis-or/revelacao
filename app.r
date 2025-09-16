library(shiny)
library(bslib)
library(shinyWidgets)

# CONFIGURAÇÕES ---------
# Defina aqui a revelação ("menina" ou "menino")
sexo_revelado <- "menino"  # ou "menina"

cores <- if (tolower(sexo_revelado) == "menina") {
  list(base = "#ff6fa9", escuro = "#d94c8a", claro = "#ffd1e4")
} else {
  list(base = "#4da3ff", escuro = "#2e6fc9", claro = "#d6e9ff")
}

msg_revelacao <- if (tolower(sexo_revelado) == "menina") "É meninaaaa!\nParabéeens!" else "É meninoooo!\nParabéeens!"

# --------- UI ---------
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    primary = "yellow",
    secondary = "green",
    base_font = font_google("Nunito")
  ),
  tags$head(
    tags$style(HTML(sprintf('
      :root {
        --cor-base: %s;
        --cor-escuro: %s;
        --cor-claro: %s;
      }
      body { background: radial-gradient(circle at 50%% 20%%, var(--cor-claro) 0%%, #ffffff 60%%); }
      .center-wrap { height: 100vh; display: grid; place-items: center; }
      .card-reveal { width: min(680px, 92vw); text-align: center; padding: 2rem; border-radius: 1.25rem; box-shadow: 0 20px 60px rgba(0,0,0,.08); background: #fff; }
      .titulo { font-weight: 800; font-size: clamp(1.25rem, 3vw, 1.6rem); color: #333; }
      .subtitulo { color:#666; margin-top:.25rem }

      /* Botão/ícone da caixa */
      .gift-btn { background: transparent; border: none; cursor: pointer; }
      .gift-emoji { font-size: clamp(5rem, 12vw, 9rem); transition: transform .2s ease; }
      .gift-emoji:hover { transform: translateY(-4px) scale(1.03); }

      /* Estado revelado */
      .mensagem-revelacao { font-weight: 900; letter-spacing: .5px; font-size: clamp(2rem, 6vw, 3rem); margin-top: .5rem; background: linear-gradient(90deg, var(--cor-escuro), var(--cor-base)); -webkit-background-clip: text; -webkit-text-fill-color: transparent; }
      .linha { height: 4px; width: 100%%; background: linear-gradient(90deg, var(--cor-escuro), var(--cor-base)); border-radius: 999px; margin: 1rem 0 1.5rem; opacity:.8 }

      /* Balões */
      .baloes { position: fixed; inset: 0; pointer-events: none; overflow: hidden; }
      .balao { position: absolute; bottom: -20vh; width: 28px; height: 36px; border-radius: 50%% 50%% 45%% 55%% / 55%% 45%% 55%% 45%%; background: var(--cor-base); opacity: .9; box-shadow: inset 0 -8px 14px rgba(255,255,255,.45), 0 6px 14px rgba(0,0,0,.12); }
      .balao::after { content: ""; position: absolute; bottom: -12px; left: 12px; width: 4px; height: 12px; background: #666; transform: rotate(8deg); border-radius: 2px; opacity:.6 }

      @keyframes subir {
        0%%   { transform: translateY(0)     rotate(0deg);   opacity:.9 }
        60%%  { transform: translateY(-70vh) rotate(6deg);   opacity:.95 }
        100%% { transform: translateY(-110vh) rotate(-6deg); opacity:0 }
      }

      /* Confete simples (triângulos) */
      .confete { position: fixed; inset: 0; pointer-events: none; overflow: hidden; }
      .peda { position: absolute; width: 0; height: 0; border-left: 6px solid transparent; border-right: 6px solid transparent; border-bottom: 12px solid var(--cor-base); opacity:.95 }
      @keyframes cair {
        0%%   { transform: translateY(-20vh) rotate(0deg) }
        100%% { transform: translateY(120vh) rotate(720deg) }
      }

      /* Botão de repetir */
      .btn-repeat { margin-top: 1rem; }
    ', cores$base, cores$escuro, cores$claro)))
  ),
  div(style = "height: 2rem;background:#2e6fc9;"),  
  div(style = "height: 2rem;background:#d94c8a;"),

  div(class = "center-wrap",
      div(class = "card-reveal",
          div(class = "titulo", "Shiny Revelação 💕"),
          div(class = "titulo", "Vamos descobrir o sexo do bebê da Paula? 🎀🍼"),
          div(class = "subtitulo", "Clique na caixa para saber a surpresa!") ,
          uiOutput("area_caixa"),
          uiOutput("area_mensagem"),
          uiOutput("btn_repetir")
      ),
      uiOutput("camada_baloes"),
      uiOutput("camada_confete")
  ),

)

# SERVER ----
server <- function(input, output, session) {
  # padrão fechada
  rv <- reactiveValues(aberto = FALSE)
  
  output$area_caixa <- renderUI({
    if (!isTRUE(rv$aberto)) {
      tagList(
        br(),
        actionButton(
          "abrir", label = tagList(span(class = "gift-emoji", "🎁")),
          class = "gift-btn", width = "auto"
        )
      )
    } else {

      tagList(
        span(class = "gift-emoji", "🧸")
      )
    }
  })

  output$area_mensagem <- renderUI({
    req(rv$aberto)
    tagList(
      div(class = "linha"),
      div(class = "mensagem-revelacao", msg_revelacao)
    )
  })


  output$camada_baloes <- renderUI({
    req(rv$aberto)
    # balões
    set.seed(as.integer(Sys.time()))
    N <- 28
    xs <- runif(N, min = 0, max = 100)
    delays <- runif(N, 0, 1.2)
    dur <- runif(N, 6, 10)

    div(class = "baloes",
        lapply(seq_len(N), function(i) {
          style <- sprintf("left:%s%%; animation: subir %ss %ss forwards ease-in;", round(xs[i],1), round(dur[i],2), round(delays[i],2))
          div(class = "balao", style = style)
        })
    )
  })

  output$camada_confete <- renderUI({
    req(rv$aberto)

    set.seed(as.integer(Sys.time()) + 99)
    N <- 60
    xs <- runif(N, 0, 100)
    delays <- runif(N, 0, 0.8)
    dur <- runif(N, 3.5, 6.5)

    div(class = "confete",
        lapply(seq_len(N), function(i) {
          style <- sprintf("left:%s%%; top:-10vh; animation: cair %ss %ss linear forwards;", round(xs[i],1), round(dur[i],2), round(delays[i],2))
          div(class = "peda", style = style)
        })
    )
  })
  
  output$btn_repetir <- renderUI({
    req(rv$aberto)
    actionBttn("repetir", "Repetir 🎉", style = "fill", color = "primary", size = "sm", class = "btn-repeat")
  })

  observeEvent(input$abrir, {
    rv$aberto <- TRUE

    sendSweetAlert(
      session,
      title = "Surpreeeesaaa!",
      text = msg_revelacao,
      type = "success",
      btn_labels = "Uhuuulllll!",
      btn_colors = cores$base
    )
  })

  observeEvent(input$repetir, {
    rv$aberto <- FALSE
    later::later(function(){ rv$aberto <- TRUE }, 0.05)
  })
}

shinyApp(ui, server)
