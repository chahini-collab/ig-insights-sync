# ================================
# 📦 LIBS (AUTO-INSTALL)
# ================================
packages <- c("httr", "jsonlite", "dplyr", "stringr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ================================
# 🔐 CONFIG
# ================================
ACCESS_TOKEN <- Sys.getenv("INSTAGRAM_ACCESS_TOKEN")
INSTAGRAM_BUSINESS_ACCOUNT_ID <- "17841411701744440"

if (ACCESS_TOKEN == "" || INSTAGRAM_BUSINESS_ACCOUNT_ID == "") {
  stop("❌ ERRO: Variáveis de ambiente não definidas")
}

API_VERSION <- "v20.0"
BASE_URL <- paste0("https://graph.facebook.com/", API_VERSION, "/")

# ================================
# 🌐 API
# ================================
call_api <- function(endpoint, params = list(), full_url = FALSE) {
  url <- if (full_url) endpoint else paste0(BASE_URL, endpoint)

  res <- GET(url, query = if (!full_url) c(params, access_token = ACCESS_TOKEN) else NULL)
  txt <- content(res, "text", encoding = "UTF-8")

  if (http_error(res)) stop(txt)

  fromJSON(txt, flatten = TRUE)
}

# ================================
# 👤 PERFIL (SAFE)
# ================================
get_safe_profile <- function(user_id, fields) {
  result <- list()

  for (field in fields) {
    value <- tryCatch({
      res <- call_api(user_id, params = list(fields = field))
      res[[field]]
    }, error = function(e) NA)

    result[[field]] <- value
  }

  result
}

cat("📊 Obtendo perfil...\n")

profile_fields <- c(
  "followers_count",
  "username",
  "biography",
  "website"
)

profile <- get_safe_profile(INSTAGRAM_BUSINESS_ACCOUNT_ID, profile_fields)

followers_count <- profile$followers_count
username <- profile$username

cat(paste0("✓ @", username, " | ", followers_count, " seguidores\n\n"))

# ================================
# 📸 MÍDIAS (PAGINAÇÃO)
# ================================
cat("📸 Coletando mídias...\n")

media_list <- list()

next_url <- paste0(
  BASE_URL,
  INSTAGRAM_BUSINESS_ACCOUNT_ID,
  "/media?fields=id,caption,media_type,media_url,timestamp,permalink,like_count,comments_count&limit=50&access_token=",
  ACCESS_TOKEN
)

repeat {
  res <- GET(next_url)
  data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

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

if (nrow(all_media_df) == 0) stop("❌ Nenhum post encontrado")

cat(paste0("✓ ", nrow(all_media_df), " posts coletados\n\n"))

# ================================
# 🧼 LIMPEZA CRÍTICA (🔥 CSV SAFE)
# ================================
all_media_df$caption <- all_media_df$caption %>%
  { ifelse(is.na(.), "", .) } %>%
  str_replace_all("[\r\n]+", " ") %>%
  str_replace_all('"', "'") %>%
  str_replace_all(";", " ") %>%
  str_replace_all(",", " ") %>%
  str_squish()

# ================================
# 📈 INSIGHTS
# ================================
cat("📈 Coletando insights...\n")

metrics <- c("impressions","reach","saved","video_views")

insights_list <- list()

for (i in seq_len(nrow(all_media_df))) {

  id <- all_media_df$id[i]
  cat(paste0("[", i, "/", nrow(all_media_df), "]\n"))

  ins <- tryCatch({
    call_api(
      paste0(id, "/insights"),
      params = list(metric = paste(metrics, collapse = ","))
    )
  }, error = function(e) NULL)

  if (!is.null(ins$data)) {
    df <- data.frame(media_id = id)

    for (m in ins$data) {
      df[[m$name]] <- m$values[[1]]$value
    }

    insights_list[[length(insights_list) + 1]] <- df
  }

  Sys.sleep(0.12)
}

insights_df <- if (length(insights_list) > 0) {
  bind_rows(insights_list)
} else {
  data.frame()
}

# ================================
# 🔗 JOIN FINAL
# ================================
final_df <- if (nrow(insights_df) > 0) {
  left_join(all_media_df, insights_df, by = c("id" = "media_id"))
} else {
  all_media_df
}

# ================================
# 🧠 ENRIQUECER
# ================================
final_df <- final_df %>%
  mutate(
    followers = followers_count,
    username = username,
    biography = profile$biography,
    website = profile$website,
    collected_at = Sys.time()
  ) %>%
  rename(
    likes = like_count,
    comments = comments_count,
    media = media_type
  )

# ================================
# 🧱 GARANTIR COLUNAS (ANTI-QUEBRA BI)
# ================================
required_cols <- c(
  "id","caption","media","likes","comments","followers",
  "username","biography","website","collected_at",
  "impressions","reach","saved","video_views"
)

for (col in required_cols) {
  if (!col %in% colnames(final_df)) {
    final_df[[col]] <- NA
  }
}

final_df <- final_df %>% select(all_of(required_cols))

# ================================
# 💾 EXPORT (🔥 POWER BI SAFE)
# ================================
cat("💾 Salvando CSV...\n")

# evita colunas quebradas
final_df[] <- lapply(final_df, function(x) {
  if (is.list(x)) as.character(x) else x
})

write.csv2(
  final_df,
  "instagram_metrics.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

cat("\n✅ FINALIZADO COM SUCESSO\n")
cat(paste0("📊 ", nrow(final_df), " linhas\n"))
cat(paste0("🕐 ", Sys.time(), "\n"))
