#' @title Teoria Dominacji dla Rankingu
#' @description Funkcja pomocnicza do wyznaczania konsensusu.
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

#' @title Rozmyty Meta-Ranking
#' @description Agreguje wyniki z wielu metod MCDA w jeden ranking.
#' @export
fuzzy_meta_ranking <- function(decision_mat, criteria_types, weights = NULL, 
                               bwm_best = NULL, bwm_worst = NULL, 
                               lambda = 0.5, v = 0.5) {
  # 1. Przygotowanie wag (jeśli brak, liczona jest Entropia)
  if (is.null(weights) && (is.null(bwm_best) || is.null(bwm_worst))) {
    message("Brak wag. Obliczam Entropię Shannona...")
    weights_raw <- oblicz_wagi_entropii(decision_mat)
    weights <- rep(weights_raw, each = 3)
  }
  
  # 2. Uruchomienie metod składowych
  res_topsis <- rozmyty_topsis(decision_mat, criteria_types, weights, 
                               bwm_najlepsze = bwm_best, bwm_najgorsze = bwm_worst)
  res_vikor <- rozmyty_vikor(decision_mat, criteria_types, v, weights, 
                             bwm_najlepsze = bwm_best, bwm_najgorsze = bwm_worst)
  res_waspas <- rozmyty_waspas(decision_mat, criteria_types, lambda, weights, 
                               bwm_najlepsze = bwm_best, bwm_najgorsze = bwm_worst)
  
  # 3. Zestawienie rankingów
  rank_matrix <- cbind(res_topsis$wyniki$Ranking, 
                       res_vikor$wyniki$Ranking, 
                       res_waspas$wyniki$Ranking)
  colnames(rank_matrix) <- c("TOPSIS", "VIKOR", "WASPAS")
  
  # 4. Agregacja (Suma Rang i Teoria Dominacji)
  rank_sum <- rank(rowSums(rank_matrix), ties.method = "first")
  rank_dom <- calculate_dominance_ranking(rank_matrix)
  
  # 5. Wynik końcowy
  comp_df <- data.frame(
    Alternative = rownames(decision_mat),
    R_TOPSIS = rank_matrix[,1],
    R_VIKOR = rank_matrix[,2],
    R_WASPAS = rank_matrix[,3],
    Meta_Sum = rank_sum,
    Meta_Dominance = rank_dom
  )
  
  return(list(comparison = comp_df, correlations = cor(comp_df[,-1], method="spearman")))
}