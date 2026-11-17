# remotes::install_github("jhelvy/renderthis")
# run from the Slides/ folder after "quarto render": reads the HTML in docs/Slides/,
# writes the pdf here in Slides/; then run "quarto render" again to copy the pdfs into docs/

renderthis::to_pdf("../docs/Slides/0.GettingStartedR.html", to = "0.GettingStartedR.pdf")
renderthis::to_pdf("../docs/Slides/1.1.FirstStepsR-1.html", to = "1.1.FirstStepsR-1.pdf")
renderthis::to_pdf("../docs/Slides/1.2.FirstStepsR-2.html", to = "1.2.FirstStepsR-2.pdf")
renderthis::to_pdf("../docs/Slides/2.1.DataStruc-1.html", to = "2.1.DataStruc-1.pdf")
renderthis::to_pdf("../docs/Slides/2.2.DataStruc-2.html", to = "2.2.DataStruc-2.pdf")
renderthis::to_pdf("../docs/Slides/2.3.DataStruc-3.html", to = "2.3.DataStruc-3.pdf")
renderthis::to_pdf("../docs/Slides/3.Programming.html", to = "3.Programming.pdf")





