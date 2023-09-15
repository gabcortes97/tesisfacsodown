td <- c("Democracia Representativa",
        "Democracia Delegativa",
        "Democracia Desarraigada")

va <- c("+", "+", "-")
ha <- c("+", "-", "+")

tab1 <- data.frame(td, va, ha)

tab1 <- tab1 %>% rename("Tipo de democracia"=td,
                        "Vertical Accountability"= va,
                        "Horizontal Accountability"= ha)

tab1 <- tab1 %>%
  kable(
    format="html",
    booktabs= TRUE,
    escape = FALSE,
    caption = "Comparación variantes democracia", align = "c") %>%
  row_spec(row=0, align = "c") %>%
  kable_styling(full_width = TRUE) %>% 
  footnote(general_title = "Nota.",
           general="Tabla basada en O'Donnell (1994) y en Luna (2016)",
           footnote_as_chunk = T) %>%   
  kable_classic(full_width = T, html_font = "Helvetica")
tab1
