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
# 🌐 API
# ================================
call_api <- function(endpoint, params = list(), full_url = FALSE) {
  url <- if (full_url) endpoint else paste0(BASE_URL, endpoint)
  
  response <- GET(url, query = if (!full_url) c(params, access_token = ACCESS_TOKEN) else NULL)
  txt <- content(response, "text", encoding = "UTF-8")
  
  if (http_error(response)) {
    stop(txt)
  }
  
  fromJSON(txt, flatten = TRUE)
}

# ================================
# 🧠 FUNÇÃO SEGURA (TESTA CAMPOS)
# ================================
get_safe_profile <- function(user_id, fields) {
  result <- list()
  
  for (field in fields) {
    value <- tryCatch({
      res <- call_api(user_id, params = list(fields = field))
      res[[field]]
    }, error = function(e) {
      NULL
    })
    
    result[[field]] <- ifelse(is.null(value), NA, value)
  }
  
  return(result)
}

# ================================
# 👤 PERFIL (AUTO-ADAPTÁVEL)
# ================================
cat("📊 Obtendo perfil...\n")

possible_fields <- c(
  "followers_count",
  "name",
  "username",
  "biography",
  "website",
  "profile_picture_url"
)

profile_data <- get_safe_profile(
  INSTAGRAM_BUSINESS_ACCOUNT_ID,
  possible_fields
)

followers_count <- profile_data$followers_count
username <- profile_data$username

cat(paste0("✓ @", username, " | ", followers_count, " seguidores\n\n"))

# ================================
# 📸 MÍDIAS
# ================================
cat("📸 Coletando mídias...\n")

media_list <- list()
next_url <- paste0(
  BASE_URL,
  INSTAGRAM_BUSINESS_ACCOUNT_ID,
  "/media?fields=id,caption,media_type,media_url,timestamp,permalink,like_count,comments_count&access_token=",
  ACCESS_TOKEN
)

repeat {
  response <- GET(next_url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  
  if (!is.null(data$data)) {
    media_list <- append(media_list, list(data$data))
  }
  
  if (!is.null(data$paging$`next`)) {
    next_url <- data$paging$`next`
  } else {
    break
  }
}

all_media_df <- bind_rows(media_list)

cat(paste0("✓ ", nrow(all_media_df), " posts coletados\n\n"))

# ================================
# 📈 INSIGHTS (INTELIGENTE)
# ================================
cat("📈 Coletando insights...\n\n")

metrics <- c("impressions","reach","engagement","saved","shares","video_views")

insights_data <- list()

for (i in seq_len(nrow(all_media_df))) {
  media_id <- all_media_df$id[i]
  media_type <- all_media_df$media_type[i]
  
  cat(paste0("[", i, "/", nrow(all_media_df), "] ", media_type, "\n"))
  
  insights <- tryCatch({
    call_api(
      paste0(media_id, "/insights"),
      params = list(metric = paste(metrics, collapse = ","))
    )
  }, error = function(e) NULL)
  
  if (!is.null(insights$data)) {
    df <- data.frame(media_id = media_id)
    
    for (m in insights$data) {
      df[[m$name]] <- tryCatch(m$values[[1]]$value, error = function(e) NA)
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
# 🧠 ENRIQUECER
# ================================
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

# ================================
# 💾 EXPORT
# ================================
write.csv(final_df, "instagram_metrics.csv", row.names = FALSE, na = "")

cat("\n✅ Script finalizado SEM QUEBRAR\n")
cat(paste0("📊 ", nrow(final_df), " linhas\n"))
cat(paste0("🕐 ", Sys.time(), "\n"))
