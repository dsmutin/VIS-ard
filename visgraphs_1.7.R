ui <- fluidPage(
  
  h1("Котикам строят графики"),
  tags$hr(),
  
  # app загрузка ----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # тестовые данные
      checkboxInput("test", "Использовать тестовые данные", T),
      
      # выбор файла
      fileInput("file", label = "Данные в формате excel:", placeholder = "таблицу с котятами", 
                multiple = F, buttonLabel = "Загрузить"),
      
      # заголовок
      checkboxInput("header", "Заголовок", TRUE),
      
      # страница
      numericInput("page", label = "Номер страницы:", value = 1, min = 1)
    ),
    
    # предпросмотр
    mainPanel(
      width = 9,
      tableOutput("contents"),
    )
  ),
  
  # app основные спецификации ----
  uiOutput("mainUI"),
  fluidRow(column(1, h5("")),
           column(10, plotOutput("coord_plot", width = 900, height = 500))),
  
  # app слои ----
  tags$hr(),
  sidebarLayout(
    sidebarPanel(width = 6,
      fluidRow(column(4, uiOutput("layer_done_button")),
               column(4, uiOutput("layer_geom1")),
               column(4, uiOutput("layer_geom2"))),
      fluidRow(
          column(2, h5("")),
          column(4, h5("По переменной:")),
          column(5, h5("Параметры:"))
      ),
      tags$hr(),
      fluidRow(column(8, uiOutput("layer")),
               column(4,
                      uiOutput("color_sc"),
                      uiOutput("shape_sc"),
                      uiOutput("size_sc"),
                      uiOutput("fill_sc"),
                      uiOutput("linetype_sc"),
                      uiOutput("linewidth_sc"),
                      uiOutput("alpha_sc")
                      )
               )
    ),
    mainPanel(width = 5,
              uiOutput("geom_methods"),
              plotOutput('layer_plot', height = 700)
    )
  ),
  
  # app шкалы ----
  h3("Значения для переменных"),

  
  # app редактор слоев ----
  tags$hr(),
  h2("Выбор слоев для рендера:"),
  plotOutput("multilayer_plot"),
  uiOutput("multilayer"),
  
  # app темы ----

  tags$hr(),
  h2("Настройки темы"),
  fluidRow(column(2, actionButton("sizes_button", "Размеры графика", width = 220)),
           uiOutput("sizes")),
  fluidRow(column(2, actionButton("titles_button", "Названия", width = 220)),
           uiOutput("titles")),
  fluidRow(column(2, actionButton("font_families_button", "Шрифты", width = 220)),
           uiOutput("font_families")),
  fluidRow(column(2, actionButton("font_sizes_button", "Размеры текста", width = 220)),
           uiOutput("font_sizes")),
  fluidRow(column(2, actionButton("font_directions_button", "Направления текста", width = 220)),
           uiOutput("font_directions")),
  fluidRow(column(2, actionButton("positions_button", "Положение объектов", width = 220)),
           uiOutput("positions")),
  tags$hr(),
  fluidRow(column(2, actionButton("backgrounds_button", "Фоны", width = 220)),
           uiOutput("backgrounds")),
  fluidRow(column(2, actionButton("grids_button", "Сетка", width = 220)),
           uiOutput("grids")),
  fluidRow(column(2, actionButton("limits_button", "Границы по осям", width = 220)),
           column(4, uiOutput("lim_x")),
           column(4, uiOutput("lim_y"))),
  fluidRow(column(2, actionButton("download_button", "Сохранить настройки темы", width = 220)),
           column(3, fileInput("save_file", label = NULL, 
                               placeholder = NULL, multiple = F,
                               buttonLabel = "Использовать сохраненные настройки"))),
  
  
  # app финальный график ----
  tags$hr(),
  fluidRow(column(6, h2("Финальный график со всеми настройками:")),
           column(3, actionButton("reload_final_plot", "Обновить график", icon = icon("paw", "fa-3x")))),
  plotOutput("final_plot"),
  
  # app сохранение графика ----
  uiOutput("download_plot_button"),
  
  # app referneces ----
  tags$hr(),
  h5("References:"),
  h6("R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical
Computing, Vienna, Austria. URL https://www.R-project.org/."),
  h6("Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan
McPherson, Alan Dipert and Barbara Borges (2021). shiny: Web Application Framework for R. R package version
1.6.0. https://CRAN.R-project.org/package=shiny"),
  h6("H. Wickham (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York,"),
  h6("Jeffrey B. Arnold (2021). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.4.
https://CRAN.R-project.org/package=ggthemes"),
  h6("Baptiste Auguie (2017). gridExtra: Miscellaneous Functions for Grid Graphics. R package version 2.3.
https://CRAN.R-project.org/package=gridExtra"),
  tags$hr(),
  h6("D. Smutin, 2023.                     dvsmutin@gmail.com")
)

server <- function(input, output) {
  
  # ALL libs ----
  library("shiny")
  library("ggplot2")
  library("readxl")
  library("ggthemes")
  library("gridExtra")
  library("colourpicker")
  
  # UX vars ----
  coord <-         c("cartesian", "fixed", "trans", "flip", "quickmap", "polar", "sf")
  names(coord) <-  c("обычные", "фиксированные", "сжатые", "поменять X и Y", "для карты", "полярные", "простые")
  
  scales <-        c("fixed", "free", "free_x", "free_y")
  names(scales) <- c("фиксированные", "свободные", "фиксировать ось Y", "фиксировать ось X")
  
  
  #theme_vars
  theme_list <- c("light","classic", "bw", "linedraw", "dark", "minimal", 
                  "void", "test", "base", "economist", "few", "tufte",
                  "wsj", "solarized", "map")
  names(theme_list) <- c("легкая", "классическая", "четкая", "линии", "темная", "минимальная",
                         "пустая", "тестовая", "базовая", "экономическая", "малая", "by TUFTE",
                         "Wall Street Journal", "Solarized", " для карт")
  
  #geom_vars
  geoms_types_discr   <- c("точки или текст", "линии", "средние", "плотности", "ошибки", "простой")
  geoms_types_categor <- c("подсчет","средние", "ошибки", "простой")
  geoms_types_onevar  <- c("распределение", "простой")
  
  geoms_types_discr_list   <- list(c("point","label","text", "rug"),
                                   c("area", "line", "step"),
                                   c("quantile", "smooth"),
                                   c("bin2d", "density_2d", "hex", "contour_filled", "raster", "tile"),
                                   c("crossbar", "errorbar", "linerange", "pointrange"),
                                   c("blank","abline", "curve", "ribbon", "polygon", "path"))
  
  names(geoms_types_discr_list[[1]])   <- c("точка", "подпись", "текст", "значение по оси")
  names(geoms_types_discr_list[[2]])   <- c("область", "линия", "линия с шагами")
  names(geoms_types_discr_list[[3]])   <- c("квантили (разброс)", "средние")
  names(geoms_types_discr_list[[4]])   <- c("частоты", "линии", "шестиугольники", "заливка", "квадраты", "прямоугольники")
  names(geoms_types_discr_list[[5]])   <- c("ящики", "линии ошибки", "разлет", "разлет от средней")
  names(geoms_types_discr_list[[6]])   <- c("пустой", "линия", "кривая", "ломанная", "многоугольник", "путь")
  
  geoms_types_categor_list <- list(c("col", "count", "jitter"),
                                   c("violin","boxplot", "dotplot"),
                                   c("crossbar", "errorbar", "linerange", "pointrange"), 
                                   c("blank","abline", "curve", "ribbon", "polygon", "path"))
  
  names(geoms_types_categor_list[[1]])   <- c("гистограмма", "подсчет", "точки")
  names(geoms_types_categor_list[[2]])   <- c("скрипка", "ящик с усами", "точки")
  names(geoms_types_categor_list[[3]])   <- c("ящики", "линии ошибки", "разлет", "разлет от средней")
  names(geoms_types_categor_list[[4]])   <- c("пустой", "линия", "кривая", "ломанная", "многоугольник", "путь")
  
  
  geoms_types_onevar_list  <- list(c("bar", "area", "density", "dotplot", "freqpoly", "histogram"),
                                   c("blank","abline", "curve", "ribbon", "polygon", "path"))
  
  names(geoms_types_onevar_list[[1]])   <- c("частоты", "область", "распределение", "точки", "гистограмма")
  names(geoms_types_onevar_list[[2]])   <- c("пустой", "линия", "кривая", "ломанная", "многоугольник", "путь")
  
  smooth_methods <-        c("loess","glm", "gam")
  names(smooth_methods) <- c("сглаживание", "усреднение", "гамма")
  
  positions <-        c("nudge", "dodge", "fill", "jitter", "stack")
  names(positions) <- c("без изменений", "с уклонением", "заполнить", "с разбросом", "стопками")
  
  
  # UX functions ----
  # calling vars
  varf <- function(df, varc, do = F) {
    X <- df[, varc]
    X <- unlist(X)
    if (do == T)
      X <- as.character(X) else
        X <- as.numeric(X)
    return(X)
  }
  
  #calling aes spec
  laes <- function(df, varc) {
    if (varc == "-") NULL else
      unlist(df[,varc])
  }
  
  #calling inner geom cpes
  larg <- function(varc) {
    if (varc == "") NULL else
      varc
  }
  
  #scale renderings
  ifaes <- function (i1, i2, ou) {
    ifelse(i1 != "-", "", i2)
  }
  
  #main functions
  ecp <- function(...) eval(call(paste0(...)))
  ec  <- function(...) eval(call(       ... ))
  
  # UX работа с данными ----
  dfl     <- reactiveValues(data = NULL)
  gglayer <- reactiveValues(data = list(geom = "", arg = list()))
  #ggfull  <- reactiveValues(data = NULL)
  plot_width <- reactiveValues(data = 700)
  plot_height<- reactiveValues(data = 700)
  
  # загрузка
  observeEvent(req(input$file), {
    dfl$data <- as.data.frame(
      read_excel(input$file$datapath, sheet = input$page, col_names = input$header))
  })
  
  # тестовый файл
  observeEvent(req(input$test), {
    dfl$data <- diamonds[,c(1,7, 2:6, 8:10)]
  })
  
  # UI предпросмотр файла ----
  output$contents <- renderTable({
    
    # UX переменные костыли ----
    gg_multiply <<- list(nrow = 1)
    
    #тело функции
    if(is.null(dfl$data)) return()
    vr <<- as.character(names(dfl$data))
    head(dfl$data)
  })
  
  # UI основные спецификации ----
  output$mainUI <- renderUI({
    if(is.null(dfl$data)) return()
    fluidPage(
      fluidRow(
        column(2, selectInput("coord", label = "Координаты", choices = coord)),
        column(1, selectInput('xcol', 'ось X :', vr)),
        column(1, selectInput('ycol', 'ось Y :', vr, vr[2])),
        column(2, actionButton('only_x', "Только X", icon = icon("paw", "fa-3x"))),
        column(2, actionButton('only_y', "Только Y", icon = icon("paw", "fa-3x")))
      ),
      fluidRow(
        column(2, selectInput('pretheme', "Тема:", choices = theme_list)),
        column(2, checkboxInput('x_chr', "Х - категориальная")),
        column(2, checkboxInput('y_chr', "Y - категориальная"))
      ),
      tags$hr(),
      fluidRow(
        column(2, h4("Разбить на подграфики:")),
        column(2, selectInput('y_grid', 'Столбцы:', c(".", vr))),
        column(2, selectInput('x_grid', 'Строки: ', c(".", vr))),
        column(2, selectInput('scale_grid', 'Шкалы: ', scales))
      )
    )
  })
  
  # PL иллюстрация стартового графика ----
  output$coord_plot <- renderPlot({
    if(is.null(dfl$data)) return()
    req(input$coord)
    
    X <- varf(dfl$data, input$xcol, input$x_chr)
    Y <- varf(dfl$data, input$ycol, input$y_chr)
    
    #только X
    if(input$only_x %%2 == 1) {
      gg <<- ggplot(dfl$data, aes(x = X)) +
        xlab(input$xcol) +
        ylab(input$ycol) +
        ecp("coord_", input$coord) +
        facet_grid(str2lang(paste0(input$x_grid, "~", input$y_grid))) +
        ecp("theme_", input$pretheme)
      ggfull <<- gg
      return (gg + geom_bar())
    }
    #только Y
    if(input$only_y %%2 == 1) {
      gg <<- ggplot(dfl$data, aes(y = Y)) +
        xlab(input$xcol) +
        ylab(input$ycol) +
        ecp("coord_", input$coord) +
        facet_grid(str2lang(paste0(input$x_grid, "~", input$y_grid))) +
        ecp("theme_", input$pretheme)
      ggfull <<- gg
      return (gg + geom_bar())
    }
    #X и Y
    gg <<- ggplot(dfl$data, aes(X,Y)) +
      xlab(input$xcol) +
      ylab(input$ycol) +
      facet_grid(str2lang(paste0(input$x_grid, "~", input$y_grid))) +
      ecp("coord_", input$coord) +
      ecp("theme_", input$pretheme)
    ggfull <<- gg
    return (gg + geom_point(alpha = 0.3))
  })
  
  # слои графика
  # UI выбор geoms ----
  output$layer_geom1 <- renderUI({
    if(is.null(dfl$data)) return()
    req(input$coord)
    
    fluidPage(
      #DEBUG IN FUTURE!!!!!!!!!!! если неправильно прощелкать то тут все сломается
      if(input$only_x %%2 == 1 | input$only_y %%2 == 1) {
        selectInput('layer_geom_types', "Выбор типа", geoms_types_onevar)
      } else
        if(input$x_chr == T | input$y_chr == T ) {
          selectInput('layer_geom_types', "Выбор типа", geoms_types_categor)
        } else {
          selectInput('layer_geom_types', "Выбор типа", geoms_types_discr)
        }
    )
  })
  
  output$layer_geom2 <- renderUI({
    req(input$layer_geom_types)
    fluidPage(
      if(input$only_x %%2 == 1 | input$only_y %%2 == 1) {
        r <- which(input$layer_geom_types == geoms_types_onevar)
        selectInput('layer_geoms', "слой:", geoms_types_onevar_list[[r]])
      } else
        if(input$x_chr == T | input$y_chr == T ) {
          r <- which(input$layer_geom_types == geoms_types_categor)
          selectInput('layer_geoms', "слой:", geoms_types_categor_list[[r]])
        } else {
          r <- which(input$layer_geom_types == geoms_types_discr)
          selectInput('layer_geoms', "слой:", geoms_types_discr_list[[r]])
        }
    )
  })
  
  # UI спецификации слоя ----
  output$layer <- renderUI({
    
    req(input$layer_geom_types != "простой")
    fluidPage(
      fluidRow(
        column(2, h4("Цвет")),
        column(4, selectInput('color_aes', NULL, c("-", vr))),
        column(5, colourWidget(elementId = 'color', "#000"))
      ),
      fluidRow(
        column(2, h4("Заливка")),
        column(4, selectInput('fill_aes', NULL, c("-", vr))),
        column(5, colourWidget(elementId = 'fill', "#000"))
      ),
      fluidRow(
        column(2, h4("Форма")),
        column(4, selectInput('shape_aes', NULL, c("-", vr))),
        column(5, sliderInput('shape', NULL, min = 1, max = 25, value = 20, step = 1))
      ),
      fluidRow(
        column(2, h4("Размер")),
        column(4, selectInput('size_aes', NULL, c("-", vr))),
        column(5, sliderInput('size', NULL, min = 0, max = 10, value = 1, step = 0.1))
      ),
      fluidRow(
        column(2, h4("Штрихи")),
        column(4, selectInput('linetype_aes', NULL, c("-", vr))),
        column(5, sliderInput('linetype', NULL, min = 0, max = 6, value = 1, step = 1))
      ),
      fluidRow(
        column(2, h4("Группировка")),
        column(4, selectInput('group_aes', NULL, c("-", vr))),
        column(5, selectInput('group', NULL, positions))
      ),
      fluidRow(
        column(2, h4("Прозрачность")),
        column(4, selectInput('alpha_aes', NULL, c("-", vr))),
        column(5, sliderInput('alpha', NULL, min = 0, max = 1, value = 0.5, step = 0.05))
      ),
      fluidRow(
        column(2, h4("Ширина линии")),
        column(4, selectInput('linewidth_aes', NULL, c("-", vr))),
        column(5, sliderInput('linewidth', NULL, min = 0, max = 10, value = 1, step = 0.1))
      )
    )
  })
  
  output$layer_done_button <- renderUI({
    actionButton('layer_done', "Добавить слой", icon = icon("paw", "fa-3x"))
  })
  
  #UI шкалирования ----
  output$color_sc <- renderUI({
    nm2 <- input$color_aes
    nm3 <- "color_sc"
    req(nm2 != "-")
    textInput(nm3, NULL, placeholder = "цвета через запятую")
  })
  
  output$fill_sc <- renderUI({
    nm2 <- input$fill_aes
    nm3 <- "fill_sc"
    req(nm2 != "-")
    textInput(nm3, NULL, placeholder = "заливки через запятую")
  })
  
  output$shape_sc <- renderUI({
    nm2 <- input$shape_aes
    nm3 <- "shape_sc"
    req(nm2 != "-")
    textInput(nm3, NULL, placeholder = "формы через запятую")
  })
  
  output$size_sc <- renderUI({
    nm2 <- input$size_aes
    nm3 <- "size_sc"
    req(nm2 != "-")
    textInput(nm3, NULL, placeholder = "размеры через запятую")
  })
  
  output$linetype_sc <- renderUI({
    nm2 <- input$linetype_aes
    nm3 <- "linetype_sc"
    req(nm2 != "-")
    textInput(nm3, NULL, placeholder = "типы линий через запятую")
  })
  
  output$linewidth_sc <- renderUI({
    nm2 <- input$linewidth_aes
    nm3 <- "linewidth_sc"
    req(nm2 != "-")
    textInput(nm3, NULL, placeholder = "ширина линий через запятую")
  })
  
  output$alpha_sc <- renderUI({
    nm2 <- input$alpha_aes
    nm3 <- "alpha_sc"
    req(nm2 != "-")
    textInput(nm3, NULL, placeholder = "прозрачности через запятую")
  })
  
  # предпросмотр и формирование всех слоев
  output$interplotUI <- renderUI({
    if(is.null(dfl$data)) return()
    fluidPage(
      tags$hr(),
      h4("Предпросмотр всех слоев:")
    )
  })
  
  
  output$geom_methods <- renderUI({
    req(input$layer_geoms)
    if(input$layer_geoms == "smooth")
      selectInput('method', "Метод усреднения", smooth_methods) else
        if(input$layer_geoms %in% c("label", "text"))
          selectInput('label_aes', "Буквы по переменной", vr) else
            if(input$layer_geoms %in% c("contour_filled", "raster", "tile"))
              h4("Не забудьте выбрать заливку") else
                if(input$layer_geoms == "contour_filled")
                  selectInput('z_aes', "Заливка по контору", vr)
  })
  
  # PL предпросмотр графиков ----
  output$layer_plot <- renderPlot({
    req(input$layer_geoms)
    
    icolor =     ifaes(input$color_aes, input$color)
    ishape =     ifaes(input$shape_aes, input$shape)
    isize =      ifaes(input$size_aes, input$size)
    ifill =      ifaes(input$fill_aes, input$fill)
    ilinetype =  ifaes(input$linetype_aes, input$linetype)
    ilinewidth = ifaes(input$linewidth_aes, input$linewidth)
    ialpha =     ifaes(input$alpha_aes, input$alpha)
    
    #аргументы слоя
    argl <- list(aes(color =    laes(dfl$data, input$color_aes), 
                     shape =    laes(dfl$data, input$shape_aes),
                     size =     laes(dfl$data, input$size_aes),
                     fill =     laes(dfl$data, input$fill_aes),
                     linetype = laes(dfl$data, input$linetype_aes),
                     group =    laes(dfl$data, input$group_aes),
                     alpha =    laes(dfl$data, input$alpha_aes),
                     linewidth =laes(dfl$data, input$linewidth_aes),
                     label =                   input$label,
                     z =                       input$z
    ),
    
    color =     larg(icolor),
    shape =     larg(ishape),
    size =      larg(isize),
    fill =      larg(ifill),
    linetype =  larg(ilinetype),
    position =  larg(input$group),
    linewidth = larg(ilinewidth),
    alpha =     larg(ialpha),
    method =         input$method
    )
    
    #debug: AES specified for all layers
    #maybe freezeReactiveValue()
    argl <- argl[!unlist(lapply(argl, is.null))]
    geom_layer <- paste0("geom_", input$layer_geoms)
    
    gglayer_set <- do.call(geom_layer, args = argl)
    
    # UX шкалы ----
    if (!is.null(input$color_sc))
      gg <- gg + scale_color_manual(values = unlist(strsplit(input$color_sc, "\\,")))
    if (!is.null(input$fill_sc))
      gg <- gg + scale_fill_manual(values = unlist(strsplit(input$fill_sc, "\\,")))
    if (!is.null(input$shape_sc))
      gg <- gg + scale_shape_manual(values = unlist(strsplit(input$shape_sc, "\\,")))
    if (!is.null(input$size_sc))
      gg <- gg + scale_size_manual(values = unlist(strsplit(input$size_sc, "\\,")))
    if (!is.null(input$linetype_sc))
      gg <- gg + scale_linetype_manual(values = unlist(strsplit(input$linetype_sc, "\\,")))
    if (!is.null(input$alpha_sc))
      gg <- gg + scale_alpha_manual(values = unlist(strsplit(input$alpha_sc, "\\,")))
    if (!is.null(input$linewidth_sc))
      gg <- gg + scale_linewidth_manual(values = unlist(strsplit(input$linewidth_sc, "\\,")))
    
    # PL sumlayer + multilayer ----
    observeEvent(input$layer_done, {
      LVAR <- as.numeric(input$layer_done)
      gglayer$data[["geom"]][LVAR] <- geom_layer
      gglayer$data[["arg"]][[LVAR]] <- isolate(argl)
      gg_multiply[[LVAR + 1]] <<- gg + gglayer_set
      
      output$sumlayer_plot <- renderPlot({
        ggfull <<- ggfull + do.call(gglayer$data[["geom"]][LVAR],
                                    gglayer$data[["arg"]][[LVAR]])
        ggfull + theme(legend.position = "none")
      })
      
    # PL предпросмотр слоев и их удаление ----
      output$multilayer_plot <- renderPlot({
        do.call(grid.arrange, gg_multiply)
      })
      
      output$multilayer <- renderUI ({
        checkboxGroupInput("selected", "Выбрать слои: ", choices = 1:LVAR, inline = T, selected = 1:LVAR)
      })
    })
    
    return(gg + gglayer_set + theme(legend.position = "bottom", 
                                    legend.title = element_blank()))
  })
  
  # UI темы ----
  output$sizes <- renderUI({
    req(input$sizes_button%%2 == 1)
    fluidRow(column(2, h4("Ширина графика:")),
             column(2, sliderInput("plot_width", min = 0, max = 3000, label = NULL, value = 700)),
             column(2, h4("Высота графика:")),
             column(2, sliderInput("plot_height", min = 0, max = 3000, label = NULL, value = 700)),
    )
  })

  output$titles <- renderUI({
    req(input$titles_button %%2 == 1)
    fluidRow(column(2, textInput("plot.title", NULL, placeholder = "Название графика")),
             column(2, textInput("axis.title.x", NULL, placeholder = "Название оси X")),
             column(2, textInput("axis.title.y", NULL, placeholder = "Название оси Y")),
             column(2, textInput("legend.title", NULL, placeholder = "Название легенды"))
             )
  })
  
  output$font_families <- renderUI({
    req(input$font_families_button %%2 == 1)
    fluidRow(column(2, textInput("font_family_all", NULL, placeholder = "весь текст")),
             column(2, textInput("font_family_title", NULL, placeholder = "заголовки")),
             column(2, textInput("font_family_legend", NULL, placeholder = "легенда")),
             column(2, textInput("font_family_axis", NULL, placeholder = "оси")),
    )
  })
  
  output$font_sizes <- renderUI({
    req(input$font_sizes_button %%2 == 1)
    fluidRow(column(2, textInput("font_size_all", NULL, placeholder = "весь текст")),
             column(2, textInput("font_size_title", NULL, placeholder = "заголовки")),
             column(2, textInput("font_size_legend", NULL, placeholder = "легенда")),
             column(2, textInput("font_size_axis", NULL, placeholder = "оси")),
    )
  })
  
  output$font_directions <- renderUI({
    req(input$font_directions_button %%2 == 1)
    fluidRow(column(2, textInput("font_direction_all", NULL, placeholder = "весь текст")),
             column(2, textInput("font_direction_title", NULL, placeholder = "заголовки")),
             column(2, textInput("legend.direction", NULL, placeholder = "легенда")),
             column(2, textInput("font_direction_axis", NULL, placeholder = "оси")),
    )
  })
  
  output$positions <- renderUI({
    req(input$positions_button %%2 == 1)
    fluidRow(column(2, textInput("plot.title.position", NULL, placeholder = "заголовки")),
             column(2, textInput("legend.position", NULL, placeholder = "легенда")),
             column(2, textInput("position_axis_X", NULL, placeholder = "ось X")),
             column(2, textInput("position_axis_Y", NULL, placeholder = "ось Y")),
    )
  })
  
  output$backgrounds <- renderUI({
    req(input$backgrounds_button %%2 == 1)
    fluidRow(column(1, fluidRow(colourInput(inputId = "plot.background", value = "#FFF", allowTransparent = T, label = "Весь график")
                                )
                    ),
             column(1, fluidRow(colourInput(inputId = "panel.background", value = "#FFF", allowTransparent = T, label = "Область построения")
                                )
                    ),
             column(1, fluidRow(colourInput(inputId = "legend.background", value = "#FFF", allowTransparent = T, label = "Легенда")
                                )
                    )
    )
  })
  
  output$grids <- renderUI({
    req(input$grids_button %%2 == 1)
    fluidRow(column(1, fluidRow(colourInput(inputId = "line_color", value = "#888888", allowTransparent = T, label = "Все линии"),
                                sliderInput("line_width", NULL, min = 0, max = 10, value = 1, step = 0.1, width = 110)
                                )
                    ),
             column(1, fluidRow(colourInput(inputId = "axis.line_color", value = "#888888", allowTransparent = T, label = "Область построения"),
                                sliderInput("axis.line_width", NULL, min = 0, max = 10, value = 1, step = 0.1, width = 110)
                                )
                    ),
             column(1, fluidRow(colourInput(inputId = "panel.grid.major.x_color", value = "#888888", allowTransparent = T, label = "Основные по X"),
                                sliderInput("panel.grid.major.x_width", NULL, min = 0, max = 10, value = 1, step = 0.1, width = 110)
                                )
                    ),
             column(1, fluidRow(colourInput(inputId = "panel.grid.minor.x_color", value = "#888888", allowTransparent = T, label = "Побочные по X"),
                                sliderInput("panel.grid.minor.x_width", NULL, min = 0, max = 10, value = 1, step = 0.1, width = 110)
                                )
                    ),
             column(1, fluidRow(colourInput(inputId = "panel.grid.major.y_color", value = "#888888", allowTransparent = T, label = "Основные по Y"),
                                sliderInput("panel.grid.major.y_width", NULL, min = 0, max = 10, value = 1, step = 0.1, width = 110)
                                )
                    ),
             column(1, fluidRow(colourInput(inputId = "panel.grid.minor.y_color", value = "#888888", allowTransparent = T, label = "Побочные по Y"),
                                sliderInput("panel.grid.minor.y_width", NULL, min = 0, max = 10, value = 1, step = 0.1, width = 110)
                                )
                    ),
    )
  })
  
  output$lim_x <- renderUI({
    req(input$limits_button %%2 == 1)
    if(input$x_chr == 0) {
      X <- varf(dfl$data, input$xcol, input$x_chr)
      fluidRow(column(3, h5("Границы по X:")),
               column(3, sliderInput('scale_x', NULL,
                                      min = (min(X) - max(X)/5)%/%1, 
                                      max = (max(X) + max(X)/5)%/%1,
                                      value = c(min(X), max(X))))
               )
    }
  })
  
  output$lim_y <- renderUI({
    req(input$limits_button %%2 == 1)
    if(input$y_chr == 0) {
      X <- varf(dfl$data, input$ycol, input$y_chr)
      fluidRow(column(3, h5("Границы по Y:")),
               column(3, sliderInput('scale_y', NULL,
                                      min = (min(X) - max(X)/5)%/%1, 
                                      max = (max(X) + max(X)/5)%/%1,
                                      value = c(min(X), max(X))))
      )
    }
  })
  
  # UX темы: размеры графика ----
  observeEvent(req(input$plot_width), {
    plot_width$data <- as.numeric(input$plot_width)
  })
  
  observeEvent(req(input$plot_height), {
    plot_height$data <- as.numeric(input$plot_height)
  })
  
  # PL финальный график ----
  output$final_plot <- renderPlot(width = reactive(plot_width$data), 
                                  height = reactive(plot_height$data), 
                                  {
    req(input$reload_final_plot, input$layer_done)
    
    # UX слои графика ----
    ggfin <- gg
    for (r in as.numeric(input$selected)) {
      ggfin <- ggfin + do.call(gglayer$data[["geom"]][r],
                               gglayer$data[["arg"]][[r]])
    }
    
    # UX темы ----
    
    # UX лимиты ----
    if (!is.null(input$scale_x))
      ggfin <- ggfin + xlim(input$scale_x)
    if (!is.null(input$scale_y))
      ggfin <- ggfin + ylim(input$scale_y)
    
    ggfin <<- ggfin
    return(ggfin)
  })
  
  # скачивание ----
  output$download_plot_button <- renderUI({
    #req(input$layer_done, input$reload_final_plot)
    #actionButton("download_plot", label = "Скачать график", icon = icon("paw", "fa-3x"))
    downloadButton("download_plot", label = "Скачать график", icon = icon("paw", "fa-3x"))
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0('plot_','.png') },
    content = function() {
      png(plot(2,2))
      print(plot(2,2))
      dev.off()},
    contentType = 'image/png')
}

# RUN ----
shinyApp(ui, server, 
         #options = list(width = 700)
         )

