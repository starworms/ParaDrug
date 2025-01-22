library(shiny)
library(ParaDrug)
x = installed.packages()[, c("Package", "LibPath", "Version")]
cat(paste(sprintf("%s: %s --- %s", x[, "Package"], x[, "Version"], x[, "LibPath"]), collapse = "\n"))
shinyApp(
    ui = paradrugUI(id = "pd"),
    server = paradrugServer)