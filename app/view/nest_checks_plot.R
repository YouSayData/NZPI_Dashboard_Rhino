box::use(
  shiny[
    a,
    div,
    moduleServer,
    NS,
    req,
    plotOutput,
    renderImage,
    tags,
    tagList,
    renderUI,
    uiOutput,
  ],
  dplyr[
    filter,
    arrange,
    desc,
    pull,
    slice,
  ],
  magick[
    image_read,
    image_scale,
    image_write,
  ],
  shiny.semantic[
    card,
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  shiny::div(
    uiOutput(ns("img_link")),
    plotOutput(ns("image")) 
  )
}

#' @export
server <- function(id, data, access) {
  moduleServer(id, function(input, output, session) {
    output$image <- renderImage({
      tmp <- data() |>
        filter(!is.na(nest_img))
      
      req(nrow(tmp) > 0)
      
      img_link <- tmp |>
        arrange(desc(Date)) |>
        slice(1) |> 
        pull(nest_img)
    
      read_img <- try(
        image_read(img_link)
      )
      
      req(class(read_img) != "try-error")
      
      tmpfile <- read_img |> 
        image_scale("400") |> 
        image_write(tempfile(fileext='jpg'), format = 'jpg')
      
      list(src = tmpfile, contentType = "image/jpeg")
    }, deleteFile = T)
    
    output$img_link <- renderUI({
      tmp <- data() |>
        filter(!is.na(nest_img))
      
      req(nrow(tmp) > 0)
      
      img_link <- tmp |>
        arrange(desc(Date)) |>
        slice(1) |> 
        pull(nest_img)
      
      tagList("Image link: ", a(img_link, href=img_link, target="_blank"))
    })
  })
}