# Instalar e carregar pacotes
library(httr)
library(jsonlite)
library(dplyr)

# Configurar credenciais
ACCESS_TOKEN <- Sys.getenv("INSTAGRAM_ACCESS_TOKEN")
INSTAGRAM_BUSINESS_ACCOUNT_ID <- Sys.getenv("INSTAGRAM_BUSINESS_ACCOUNT_ID")

# Extrair dados da API
profile_data <- call_api(INSTAGRAM_BUSINESS_ACCOUNT_ID, 
  params = list(fields = "followers_count,media_count,name,username"))

# Obter métricas para cada post
metrics_to_fetch <- paste(c(
  "impressions", "reach", "saved", "shares", "profile_visits",
  "website_clicks", "engagement", "taps_forward", "taps_back",
  "exits", "plays", "total_interactions", "video_views", "replies",
  "clicks_on_hashtags", "clicks_on_stickers", "clicks_on_links",
  "clicks_on_call_to_action", "saved_from_hashtags", 
  "saved_from_locations", "impressions_from_hashtags",
  "impressions_from_locations", "reach_from_hashtags",
  "reach_from_locations"
), collapse = ",")

# Salvar como CSV
write.csv(final_df, "instagram_metrics.csv", row.names = FALSE)
