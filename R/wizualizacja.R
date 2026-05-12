#' @title Motyw wykresów ClickbaitRankR
#' @description Ujednolicony, oszczędny styl wizualizacji pakietu.
#' @keywords internal
.motyw_mcda <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15, color = "#1F2933"),
      plot.subtitle = ggplot2::element_text(size = 10.5, color = "#52606D"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E4E7EB", linewidth = 0.35),
      axis.title = ggplot2::element_text(face = "bold", color = "#323F4B"),
      axis.text = ggplot2::element_text(color = "#52606D"),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 18, 10, 10)
    )
}

#' Wykres bąbelkowy VIKOR
#'
#' @description
#' Rysuje główną wizualizację pakietu: mapę kompromisu VIKOR.
#' Niższe wartości `S`, `R` i `Q` oznaczają korzystniejszą alternatywę,
#' dlatego najlepsze punkty znajdują się bliżej lewego dolnego rogu.
#'
#' @param x Obiekt klasy `rozmyty_vikor_wynik`.
#' @param liczba_etykiet Liczba najlepszych alternatyw podpisywanych na wykresie.
#' @param ... Dodatkowe argumenty ignorowane przez metodę.
#'
#' @return Obiekt `ggplot`.
#' @importFrom ggplot2 ggplot aes annotate coord_cartesian element_blank element_line element_text expansion geom_hline geom_point geom_vline labs margin scale_fill_manual scale_size_continuous scale_x_continuous scale_y_continuous theme theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @export
plot.rozmyty_vikor_wynik <- function(x, liczba_etykiet = 12, ...) {
  df <- x$wyniki
  if (is.null(df) || !all(c("Def_S", "Def_R", "Def_Q", "Ranking") %in% names(df))) {
    stop("Obiekt 'x' nie zawiera wynikow VIKOR w oczekiwanym formacie.")
  }

  q_zakres <- range(df$Def_Q, na.rm = TRUE)
  if (q_zakres[1] == q_zakres[2]) {
    df$Sila_kompromisu <- 1
  } else {
    df$Sila_kompromisu <- 1 - ((df$Def_Q - q_zakres[1]) / (q_zakres[2] - q_zakres[1]))
  }

  df$Grupa <- "Pozosta\u0142e"
  df$Grupa[df$Ranking <= 3] <- "Czo\u0142\u00f3wka"
  df$Grupa[df$Ranking == 1] <- "Najlepsza"
  df$Grupa <- factor(df$Grupa, levels = c("Najlepsza", "Czo\u0142\u00f3wka", "Pozosta\u0142e"))

  liczba_etykiet <- min(max(1, liczba_etykiet), nrow(df))
  df$Etykieta <- ifelse(df$Ranking <= liczba_etykiet, paste0("A", df$Alternatywa), "")

  s_mediana <- stats::median(df$Def_S, na.rm = TRUE)
  r_mediana <- stats::median(df$Def_R, na.rm = TRUE)

  ggplot2::ggplot(df, ggplot2::aes(x = Def_S, y = Def_R)) +
    ggplot2::annotate(
      "rect",
      xmin = -Inf, xmax = s_mediana, ymin = -Inf, ymax = r_mediana,
      fill = "#E6F4F1", alpha = 0.75
    ) +
    ggplot2::geom_vline(xintercept = s_mediana, linetype = "dashed", color = "#9AA5B1", linewidth = 0.45) +
    ggplot2::geom_hline(yintercept = r_mediana, linetype = "dashed", color = "#9AA5B1", linewidth = 0.45) +
    ggplot2::geom_point(
      ggplot2::aes(size = Sila_kompromisu, fill = Grupa),
      shape = 21, color = "#243B53", stroke = 0.55, alpha = 0.9
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = Etykieta),
      size = 3.2,
      color = "#1F2933",
      min.segment.length = 0,
      box.padding = 0.35,
      point.padding = 0.25,
      seed = 42,
      max.overlaps = Inf,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = c("Najlepsza" = "#0072B2", "Czo\u0142\u00f3wka" = "#009E73", "Pozosta\u0142e" = "#CBD2D9"),
      drop = FALSE
    ) +
    ggplot2::scale_size_continuous(
      range = c(3.5, 12.5),
      limits = c(0, 1),
      name = "Si\u0142a kompromisu"
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.16))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.16))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = "Mapa kompromisu VIKOR",
      subtitle = "Lewe dolne pole wskazuje alternatywy o ni\u017cszym S i R; wi\u0119kszy b\u0105bel oznacza ni\u017cszy Q.",
      x = "U\u017cyteczno\u015b\u0107 grupowa S",
      y = "Regret indywidualny R",
      fill = "Pozycja"
    ) +
    .motyw_mcda()
}

#' Wykres bąbelkowy VIKOR dla aliasu angielskiego
#'
#' @description Metoda zgodności dla obiektów zwracanych przez `fuzzy_vikor()`.
#'
#' @param x Obiekt klasy `fuzzy_vikor_res`.
#' @param ... Dodatkowe argumenty przekazywane do wykresu VIKOR.
#'
#' @return Obiekt `ggplot`.
#' @export
plot.fuzzy_vikor_res <- function(x, ...) {
  wynik <- list(
    wyniki = data.frame(
      Alternatywa = x$results$Alternative,
      Def_S = x$results$Def_S,
      Def_R = x$results$Def_R,
      Def_Q = x$results$Def_Q,
      Ranking = x$results$Ranking,
      row.names = NULL
    ),
    detale = x$details,
    parametry = x$params
  )
  class(wynik) <- "rozmyty_vikor_wynik"
  plot.rozmyty_vikor_wynik(wynik, ...)
}

#' Generowanie tabeli APA
#'
#' @description
#' Funkcja przeksztalca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS,
#' Meta-Ranking) w sformatowana tabele zgodna ze stylem APA, gotowa
#' do druku albo zapisu do dokumentu Word.
#'
#' @param x Obiekt wynikowy z funkcji pakietu, np. `rozmyty_topsis_wynik`.
#' @param tytul Opcjonalny tytul tabeli.
#' @param ... Dodatkowe argumenty przekazywane do metod S3.
#'
#' @return Obiekt klasy `flextable`.
#' @importFrom rempsyc nice_table
#' @importFrom flextable autofit save_as_docx
#' @export
tabela_apa <- function(x, tytul = NULL, ...) {
  UseMethod("tabela_apa")
}

#' @title Pomocnicze formatowanie tabel APA
#' @keywords internal
.utworz_tabele_apa <- function(df, numer_tabeli, tytul, uwaga) {
  tabela <- rempsyc::nice_table(
    df,
    title = c(paste("Tabela", numer_tabeli), tytul),
    note = uwaga
  )
  flextable::autofit(tabela)
}

#' @export
tabela_apa.rozmyty_topsis_wynik <- function(x, tytul = "Wyniki metody Fuzzy TOPSIS", ...) {
  df <- x$wyniki
  names(df) <- c("Alternatywa", "D+ (do idealu)", "D- (od antyidealu)", "Wynik (CC)", "Ranking")

  df[["D+ (do idealu)"]] <- round(df[["D+ (do idealu)"]], 3)
  df[["D- (od antyidealu)"]] <- round(df[["D- (od antyidealu)"]], 3)
  df[["Wynik (CC)"]] <- round(df[["Wynik (CC)"]], 4)

  .utworz_tabele_apa(
    df = df,
    numer_tabeli = 1,
    tytul = tytul,
    uwaga = "Uwaga. CC oznacza coefficient of closeness. Wyzsza wartosc wskazuje lepsza alternatywe."
  )
}

#' @export
tabela_apa.rozmyty_vikor_wynik <- function(x, tytul = "Wyniki metody Fuzzy VIKOR", ...) {
  df <- x$wyniki
  names(df) <- c("Alternatywa", "S (grupa)", "R (zal)", "Q (kompromis)", "Ranking")

  df[["S (grupa)"]] <- round(df[["S (grupa)"]], 3)
  df[["R (zal)"]] <- round(df[["R (zal)"]], 3)
  df[["Q (kompromis)"]] <- round(df[["Q (kompromis)"]], 4)

  .utworz_tabele_apa(
    df = df,
    numer_tabeli = 2,
    tytul = tytul,
    uwaga = "Uwaga. S oznacza uzytecznosc grupy, R indywidualny zal, a Q indeks kompromisu. Nizsza wartosc Q wskazuje lepsza alternatywe."
  )
}

#' @export
tabela_apa.fuzzy_vikor_res <- function(x, tytul = "Wyniki metody Fuzzy VIKOR", ...) {
  wynik <- list(
    wyniki = data.frame(
      Alternatywa = x$results$Alternative,
      Def_S = x$results$Def_S,
      Def_R = x$results$Def_R,
      Def_Q = x$results$Def_Q,
      Ranking = x$results$Ranking,
      row.names = NULL
    )
  )
  class(wynik) <- "rozmyty_vikor_wynik"
  tabela_apa(wynik, tytul = tytul, ...)
}

#' @export
tabela_apa.rozmyty_waspas_wynik <- function(x, tytul = "Wyniki metody Fuzzy WASPAS", ...) {
  df <- x$wyniki
  names(df) <- c("Alternatywa", "WSM (suma)", "WPM (iloczyn)", "Q (laczny)", "Ranking")

  df[["WSM (suma)"]] <- round(df[["WSM (suma)"]], 3)
  df[["WPM (iloczyn)"]] <- round(df[["WPM (iloczyn)"]], 3)
  df[["Q (laczny)"]] <- round(df[["Q (laczny)"]], 4)

  .utworz_tabele_apa(
    df = df,
    numer_tabeli = 3,
    tytul = tytul,
    uwaga = "Uwaga. WSM oznacza weighted sum model, a WPM weighted product model."
  )
}

#' @export
tabela_apa.list <- function(x, tytul = "Meta-Ranking (konsensus)", ...) {
  if (!is.null(x$comparison)) {
    df <- x$comparison
  } else if (!is.null(x$porownanie)) {
    df <- x$porownanie
  } else {
    stop("To nie jest obiekt meta-rankingu.")
  }

  names(df) <- gsub("_", " ", names(df), fixed = TRUE)

  .utworz_tabele_apa(
    df = df,
    numer_tabeli = 4,
    tytul = tytul,
    uwaga = "Uwaga. Tabela zestawia rangi uzyskane roznymi metodami oraz rankingi konsensusu."
  )
}

utils::globalVariables(c("Def_S", "Def_R", "Def_Q", "Sila_kompromisu", "Grupa", "Etykieta"))
