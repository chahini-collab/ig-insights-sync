# ================================
# 📦 LIBS
# ================================
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(httr)
library(jsonlite)
library(dplyr)

# ================================
# 🔐 CONFIG
# ================================
ACCESS_TOKEN <- Sys.getenv("INSTAGRAM_ACCESS_TOKEN")
INSTAGRAM_BUSINESS_ACCOUNT_ID <- Sys.getenv("INSTAGRAM_BUSINESS_ACCOUNT_ID")
API_VERSION <- "v20.0"
BASE_URL <- paste0("https://graph.facebook.com/", API_VERSION, "/")

# ================================
# 🌐 FUNÇÃO API (ROBUSTA)
# ================================
call_api <- function(endpoint, params = list(), full_url = FALSE) {
  url <- if (full_url) endpoint else paste0(BASE_URL, endpoint)
  
  response <- GET(url, query = if (!full_url) c(params, access_token = ACCESS_TOKEN) else NULL)
  
  content_txt <- content(response, "text", encoding = "UTF-8")
  
  if (http_error(response)) {
    stop(paste("Erro na API:", status_code(response), "-", content_txt))
  }
  
  fromJSON(content_txt, flatten = TRUE)
}

# ================================
# 👤 PERFIL (CORRIGIDO)
# ================================
cat("📊 Obtendo perfil...\n")

profile_data <- call_api(
  INSTAGRAM_BUSINESS_ACCOUNT_ID,
  params = list(
    fields = "followers_count,name,username,biography,profile_picture_url,website"
  )
)

followers_count <- profile_data$followers_count
username <- profile_data$username

cat(paste0("✓ @", username, " | ", followers_count, " seguidores\n\n"))

# ================================
# 📸 MÍDIAS + PAGINAÇÃO
# ================================
cat("📸 Coletando mídias...\n")

media_list <- list()
next_page <- paste0(INSTAGRAM_BUSINESS_ACCOUNT_ID, "/media")
media_count <- 0

repeat {
  data <- call_api(
    next_page,
    params = list(
      fields = "id,caption,media_type,media_url,thumbnail_url,timestamp,permalink,like_count,comments_count",
      limit = 100
    )
  )
  
  if (!is.null(data$data) && length(data$data) > 0) {
    media_list <- append(media_list, list(data$data))
    media_count <- media_count + length(data$data)
  }
  
  if (!is.null(data$paging$`next`)) {
    next_page <- data$paging$`next`
    data <- call_api(next_page, full_url = TRUE)
  } else {
    break
  }
}

all_media_df <- bind_rows(media_list)

cat(paste0("✓ ", media_count, " posts coletados\n\n"))

# ================================
# 📈 INSIGHTS (RESILIENTE)
# ================================
cat("📈 Coletando insights...\n\n")

all_metrics <- c(
  "impressions","reach","saved","shares",
  "engagement","video_views","plays","replies"
)

metrics_string <- paste(all_metrics, collapse = ",")

insights_data <- list()

for (i in seq_len(nrow(all_media_df))) {
  media_id <- all_media_df$id[i]
  
  cat(paste0("[", i, "/", nrow(all_media_df), "] ", media_id, "\n"))
  
  insights <- tryCatch({
    call_api(
      paste0(media_id, "/insights"),
      params = list(metric = metrics_string)
    )
  }, error = function(e) {
    cat("   ⚠️ erro ignorado\n")
    return(NULL)
  })
  
  if (!is.null(insights$data)) {
    df <- data.frame(media_id = media_id)
    
    for (m in insights$data) {
      value <- tryCatch(m$values[[1]]$value, error = function(e) NA)
      df[[m$name]] <- value
    }
    
    insights_data <- append(insights_data, list(df))
  }
}

if (length(insights_data) > 0) {
  insights_df <- bind_rows(insights_data)
  final_df <- left_join(all_media_df, insights_df, by = c("id" = "media_id"))
} else {
  final_df <- all_media_df
}

# ================================
# 🧠 ENRIQUECIMENTO
# ================================
if (nrow(final_df) > 0) {
  final_df <- final_df %>%
    mutate(
      followers = followers_count,
      username = username,
      biography = profile_data$biography,
      website = profile_data$website,
      collected_at = Sys.time(),
      caption = ifelse(is.na(caption), "", caption)
    ) %>%
    rename(
      likes = like_count,
      comments = comments_count,
      media = media_type
    )
}

# ================================
# 💾 EXPORT
# ================================
OUTPUT_FILE <- "instagram_metrics.csv"

write.csv(final_df, OUTPUT_FILE, row.names = FALSE, na = "")

cat("\n✅ Export finalizado\n")
cat(paste0("📊 Linhas: ", nrow(final_df), "\n"))
cat(paste0("📋 Colunas: ", ncol(final_df), "\n"))
cat(paste0("🕐 ", Sys.time(), "\n"))
