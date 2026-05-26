
cat("╔══════════════════════════════════════════════════╗\n")
cat("║  TÜİK Evlenme Tahmin Projesi — Alara Kuru       ║\n")
cat("║  ARIMA | ETS | Holt-Winters | Ensemble          ║\n")
cat("╚══════════════════════════════════════════════════╝\n\n")

paktler <- c("pacman","readxl","forecast","tseries",
              "ggplot2","dplyr","tidyr","lubridate","scales")
for (p in paktler) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)

cat("━━━ Adım 1/4: Veri Hazırlama ━━━\n")
source("01_data_prep.R")

cat("\n━━━ Adım 2/4: ARIMA Modeli ━━━\n")
source("02_arima_model.R")

cat("\n━━━ Adım 3/4: ETS & Holt-Winters ━━━\n")
source("03_ets_model.R")

cat("\n━━━ Adım 4/4: Karşılaştırma & Ensemble ━━━\n")
source("04_compare_forecast.R")

cat("\n╔══════════════════════════════════════════════════╗\n")
cat("║  ✓ Tamamlandı! Üretilen görseller:              ║\n")
cat("║  evlenme_zaman_serisi.png                       ║\n")
cat("║  evlenme_mevsimsel_kutu.png                     ║\n")
cat("║  evlenme_yillik_toplam.png                      ║\n")
cat("║  evlenme_acf_pacf.png                           ║\n")
cat("║  arima_tahmin.png                               ║\n")
cat("║  ets_tahmin.png                                 ║\n")
cat("║  ets_stl_dekomposizyon.png                      ║\n")
cat("║  ets_mevsimsel_etki.png                         ║\n")
cat("║  model_karsilastirma.png                        ║\n")
cat("║  model_rmse.png                                 ║\n")
cat("╚══════════════════════════════════════════════════╝\n")
