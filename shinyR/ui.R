
shinyServer(
  
  pageWithSidebar(
    
    headerPanel("My test shiny panel"),
    
    sidebarPanel ("My sidebar"),
    
    mainPanel ("Main panel")
    
  )
  
)