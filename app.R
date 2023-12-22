library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .article-header { font-size: 30px; font-weight: bold; margin-bottom: 20px; text-align: center; }
      .article-subheader { font-size: 24px; font-weight: bold; margin-bottom: 20px; }
      .article-body { font-size: 16px; line-height: 1.6; margin-bottom: 20px; }
      .article-image { width: 100%; height: auto; margin-bottom: 20px; }
      .container { width: 80%; margin: auto; }
    "))
  ),
  div(class = "container",
      div(class = "article-header", "A Stride Towards Equality: The Pioneering Tokyo Olympics"),
      img(src = "path_to_your_image.png", class = "article-image"),
      div(class = "article-body",
          p("The Tokyo Olympics marked a historic moment in the quest for gender equality, coming tantalizingly close to a 50/50 split between male and female athletes—a testament to the changing tides in the world of sports. The event became a beacon of progress, with nearly every Olympics since 1980 witnessing a surge in female participation, climaxing in Tokyo where women made up 48% of the competitors."),
          p("Yet, the Tokyo Games were about more than statistics; they unfolded under the shadow of the COVID-19 pandemic, which brought unprecedented challenges and highlighted the resilience of athletes worldwide. It also raised questions about inclusivity, as stringent measures initially restricted nursing mothers from bringing their infants, a decision later overturned following public outcry."),
          p("Delving into the roster of participating nations, it's noteworthy that countries like Angola, Nepal, and Zambia have tipped the scale, sending more female athletes than male. This shift suggests a broader cultural and institutional recognition of women in sports, though the motives and mechanisms behind these changes vary globally."),
          p("However, a deeper look reveals that progress is not uniformly spread. The Gender Inequality Index still paints a stark picture of disparities, hinting that representation at the Olympics is but a facet of a more significant societal structure that continues to grapple with gender biases."),
          p("Scrutinizing the spread across disciplines, the provided plot illustrates that numerous sports continue to have greater male representation, such as football and water polo, while others like handball and gymnastics display a more balanced or even female-skewed participation. These disparities highlight the nuanced reality of gender equality—it's not just about numbers but also about access, funding, media representation, and cultural support, aspects that require holistic efforts beyond the Olympic stage."),
          p("The Tokyo Olympics, therefore, stands as both a milestone and a mirror, reflecting the achievements in gender equality and the distance still to be covered. It's a reminder that while the spirit of competition brings us together, the pursuit of equity must continue in earnest in all arenas of life.")
      )
  )
)

server <- function(input, output, session) {
  # Server logic, if needed
}

shinyApp(ui, server)
