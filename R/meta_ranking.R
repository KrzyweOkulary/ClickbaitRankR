#' @title Ranking dominacyjny
#' @description Funkcja pomocnicza do wyznaczania konsensusu z kilku rankingów.
#' @keywords internal
calculate_dominance_ranking <- function(rank_mat) {
  n <- nrow(rank_mat)
  final_rank <- rep(0, n)
  available <- rep(TRUE, n)
  for (current_position in 1:n) {
    current_mat <- rank_mat
    current_mat[!available, ] <- Inf
    candidates <- apply(current_mat, 2, which.min)
    freq_table <- table(candidates)
    max_votes <- max(freq_table)
    winners <- as.numeric(names(freq_table)[freq_table == max_votes])
    if (length(winners) == 1) {
      winner_idx <- winners
    } else {
      sums <- rowSums(rank_mat[winners, , drop = FALSE])
      winner_idx <- winners[which.min(sums)]
    }
    final_rank[winner_idx] <- current_position
    available[winner_idx] <- FALSE
  }
  return(final_rank)
}

#' Rozmyty meta-ranking
#'
#' @description
#' Agreguje rankingi TOPSIS, VIKOR i WASPAS w jeden ranking porównawczy.
#' Wagi są wyznaczane obiektywnie metodą CRITIC albo entropii Shannona.
#'
#' @param decision_mat Rozmyta macierz decyzyjna w układzie TFN.
#' @param criteria_types Wektor `"max"` / `"min"` określający kierunek preferencji kryteriów.
#' @param weights Opcjonalny wektor wcześniej obliczonych wag obiektywnych.
#' @param metoda_wag Metoda obiektywnego ważenia: `"critic"` albo `"entropia"`.
#' @param lambda Parametr metody WASPAS.
#' @param v Parametr metody VIKOR.
#'
#' @return Lista z tabelą porównawczą, macierzą korelacji rang oraz użytymi wagami.
#' @export
fuzzy_meta_ranking <- function(decision_mat, criteria_types, weights = NULL,
                               metoda_wag = c("critic", "entropia"),
                               lambda = 0.5, v = 0.5) {
  .sprawdz_macierz_tfn(decision_mat)
  metoda_wag <- match.arg(metoda_wag)

  # Wagi liczymy raz, aby każda metoda MCDA korzystała z tego samego wektora.
  if (is.null(weights)) {
    weights <- switch(
      metoda_wag,
      critic = oblicz_wagi_critic(decision_mat, criteria_types),
      entropia = oblicz_wagi_entropii(decision_mat, criteria_types)
    )
  }

  res_topsis <- rozmyty_topsis(decision_mat, criteria_types, wagi = weights)
  res_vikor <- rozmyty_vikor(decision_mat, criteria_types, v = v, wagi = weights)
  res_waspas <- rozmyty_waspas(decision_mat, criteria_types, lambda = lambda, wagi = weights)

  rank_matrix <- cbind(res_topsis$wyniki$Ranking,
                       res_vikor$wyniki$Ranking,
                       res_waspas$wyniki$Ranking)
  colnames(rank_matrix) <- c("TOPSIS", "VIKOR", "WASPAS")

  rank_sum <- rank(rowSums(rank_matrix), ties.method = "first")
  rank_dom <- calculate_dominance_ranking(rank_matrix)

  comp_df <- data.frame(
    Alternatywa = .nazwy_alternatyw(decision_mat),
    R_TOPSIS = rank_matrix[, 1],
    R_VIKOR = rank_matrix[, 2],
    R_WASPAS = rank_matrix[, 3],
    Meta_Sum = rank_sum,
    Meta_Dominance = rank_dom
  )

  list(
    comparison = comp_df,
    correlations = stats::cor(comp_df[, -1], method = "spearman"),
    weights = .normalizuj_wagi(weights)
  )
}
