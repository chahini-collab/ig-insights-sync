# Instalar e carregar pacotes necessários (se ainda não estiverem instalados)
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(httr)
library(jsonlite)
library(dplyr)

# --- Configurações da API --- #
# Substitua pelo seu Token de Acesso de Longa Duração do Instagram Graph API
ACCESS_TOKEN <- Sys.getenv("INSTAGRAM_ACCESS_TOKEN")

# Substitua pelo seu ID de Usuário do Instagram Business (IG User ID)
INSTAGRAM_BUSINESS_ACCOUNT_ID <- Sys.getenv("INSTAGRAM_BUSINESS_ACCOUNT_ID")

# Versão da API
API_VERSION <- "v20.0"

BASE_URL <- paste0("https://graph.facebook.com/", API_VERSION, "/")

# --- Função para fazer chamadas à API --- #
call_api <- function(endpoint, params = list()) {
  url <- paste0(BASE_URL, endpoint)
  response <- GET(url, query = c(params, access_token = ACCESS_TOKEN))
  
  if (http_error(response)) {
    stop(paste("Erro na API:", http_status(response)$reason, 
               "-", content(response, "text", encoding = "UTF-8")))
  }
  
  content <- content(response, "text", encoding = "UTF-8")
  fromJSON(content, flatten = TRUE)
}

# --- 1. Obter informações do perfil --- #
cat("📊 Obtendo informações do perfil...\n")
profile_data <- call_api(endpoint = INSTAGRAM_BUSINESS_ACCOUNT_ID, 
                         params = list(fields = "followers_count,media_count,name,username,biography,profile_picture_url,website"))

followers_count <- profile_data$followers_count
username <- profile_data$username
cat(paste0("✓ Perfil: @", username, " | Seguidores: ", followers_count, "\n\n"))

# --- 2. Obter lista de mídias (posts) --- #
cat("📸 Obtendo lista de mídias...\n")
media_list <- list()
next_page <- paste0(INSTAGRAM_BUSINESS_ACCOUNT_ID, "/media")
media_count <- 0

while (!is.null(next_page)) {
  current_page_data <- call_api(next_page, 
                                params = list(fields = "id,caption,media_type,media_url,thumbnail_url,timestamp,permalink,like_count,comments_count"))
  
  if (length(current_page_data$data) > 0) {
    media_list <- c(media_list, list(current_page_data$data))
    media_count <- media_count + length(current_page_data$data)
  }
  
  if (!is.null(current_page_data$paging$`next`)) {
    next_page <- gsub(BASE_URL, "", current_page_data$paging$`next`)
  } else {
    next_page <- NULL
  }
}

all_media_df <- bind_rows(media_list)
cat(paste0("✓ Total de mídias encontradas: ", media_count, "\n\n"))

# --- 3. Obter TODOS os insights para cada mídia --- #
cat("📈 Obtendo insights para cada mídia...\n")
cat("   (Isso pode levar alguns minutos)\n\n")

if (nrow(all_media_df) > 0) {
  insights_data <- list()
  
  # TODOS os insights disponíveis na Meta Graph API
  # Referência: https://developers.facebook.com/docs/instagram-api/reference/ig-media/insights
  all_metrics <- c(
    # Métricas básicas
    "impressions",
    "reach",
    "saved",
    "shares",
    "profile_visits",
    "website_clicks",
    
    # Métricas de engajamento
    "engagement",
    "taps_forward",
    "taps_back",
    "exits",
    
    # Métricas de vídeo/REELS
    "plays",
    "total_interactions",
    "video_views",
    "replies",
    
    # Métricas de cliques
    "clicks_on_hashtags",
    "clicks_on_stickers",
    "clicks_on_links",
    "clicks_on_call_to_action",
    
    # Métricas de descoberta
    "saved_from_hashtags",
    "saved_from_locations",
    "impressions_from_hashtags",
    "impressions_from_locations",
    "reach_from_hashtags",
    "reach_from_locations"
  )
  
  metrics_to_fetch <- paste(all_metrics, collapse = ",")
  
  for (i in 1:nrow(all_media_df)) {
    media_id <- all_media_df$id[i]
    media_type <- all_media_df$media_type[i]
    caption_text <- substr(all_media_df$caption[i], 1, 50)
    
    cat(paste0("[", i, "/", nrow(all_media_df), "] ID: ", media_id, " | Tipo: ", media_type, "\n"))
    
    insights_result <- tryCatch({
      call_api(paste0(media_id, "/insights"), params = list(metric = metrics_to_fetch))
    }, error = function(e) {
      cat(paste0("           ⚠️  Erro: ", e$message, "\n"))
      return(NULL)
    })
    
    if (!is.null(insights_result) && length(insights_result$data) > 0) {
      # Transformar a lista de insights em um data frame
      insight_df <- data.frame(media_id = media_id)
      metrics_found <- 0
      
      for (j in 1:length(insights_result$data)) {
        metric_name <- insights_result$data[[j]]$name
        metric_value <- insights_result$data[[j]]$values[[1]]$value
        insight_df[[metric_name]] <- metric_value
        metrics_found <- metrics_found + 1
      }
      
      cat(paste0("           ✓ ", metrics_found, " métricas obtidas\n"))
      insights_data <- c(insights_data, list(insight_df))
    } else {
      cat(paste0("           ✗ Nenhuma métrica obtida\n"))
    }
  }
  
  if (length(insights_data) > 0) {
    all_insights_df <- bind_rows(insights_data)
    # Juntar os dados de mídia com os insights
    final_df <- left_join(all_media_df, all_insights_df, by = "media_id")
  } else {
    final_df <- all_media_df
    cat("\n⚠️  Nenhum insight de mídia foi obtido com sucesso.\n")
  }
  
} else {
  final_df <- data.frame()
  cat("⚠️  Nenhuma mídia encontrada para processar.\n")
}

# --- 4. Adicionar informações do perfil --- #
if (nrow(final_df) > 0) {
  final_df$followers <- followers_count
  final_df$username <- username
  final_df$profile_picture_url <- profile_data$profile_picture_url
  final_df$biography <- profile_data$biography
  final_df$website <- profile_data$website
} else if (followers_count > 0) {
  final_df <- data.frame(
    followers = followers_count,
    username = username,
    profile_picture_url = profile_data$profile_picture_url,
    biography = profile_data$biography,
    website = profile_data$website
  )
}

# --- 5. Renomear colunas para corresponder aos nomes comuns --- #
if (nrow(final_df) > 0) {
  final_df <- final_df %>%
    rename(
      like = like_count,
      comments = comments_count,
      media = media_type
    )
}

# --- 6. Salvar como CSV --- #
OUTPUT_FILE <- "instagram_metrics.csv"
write.csv(final_df, OUTPUT_FILE, row.names = FALSE, na = "")
cat(paste0("\n✅ Dados salvos em: ", OUTPUT_FILE, "\n"))
cat(paste0("📊 Total de linhas: ", nrow(final_df), "\n"))
cat(paste0("📋 Total de colunas: ", ncol(final_df), "\n"))
cat(paste0("🕐 Timestamp: ", Sys.time(), "\n"))

# Listar todas as colunas disponíveis
cat("\n📌 Colunas disponíveis no CSV:\n")
cat(paste(names(final_df), collapse = ", "))
cat("\n")
