

make_d <- function(modulation,
                   magnitude,
                   x = seq(-pi, pi-2*pi/8, length.out = 8),
                   kv = 2,
                   center = -pi/16,
                   p = 0.6) {
  d <- tibble(raw = x) %>%
    crossing(
      Contrast = factor(c("Low", "High"), levels = c("Low", "High")),
      VTF = factor(
        c("Additive", "Multiplicative"),
        levels = c("Additive", "Multiplicative"))) %>%
    mutate(
      y = case_when(
        fct_match(Contrast, "Low") ~ CircStats::dmixedvm(x, center, center+pi, kv, kv, p),
        fct_match(VTF, "Additive") ~ 0.1 + CircStats::dmixedvm(x, center, center+pi, kv, kv, p),
        fct_match(VTF, "Multiplicative") ~ 1.4*CircStats::dmixedvm(x, center, center+pi, kv, kv, p)),
      orientation = CircStats::deg(x / 2))
  return(d)
}
