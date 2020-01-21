### Parsing HTML

library(stringr)
# pull out code into separate column
big_a$code_frags <- str_extract_all(big_a$body, "(?<=<code>)[^>]+(?=<\\/code>)")
big_q$code_frags <- str_extract_all(big_q$body, "(?<=<code>)[^>]+(?=<\\/code>)")

# remove code frags from some form of body
big_a$body_no_code <- gsub("<code>[^>]+<\\/code>", "", big_a$body)
big_q$body_no_code <- gsub("<code>[^>]+<\\/code>", "", big_q$body)

# remove html tags
big_a$body_no_html <- gsub("<[^>]+>", "", big_a$body_no_code)
big_q$body_no_html <- gsub("<[^>]+>", "", big_q$body_no_code)

# remove html of total body
big_a$body_all_no_html <- gsub("<[^>]+>", "", big_a$body)
big_q$body_all_no_html <- gsub("<[^>]+>", "", big_q$body)

# remove no code chunk (redundant)
big_a <- subset(big_a, select=-body_no_code)
big_q <- subset(big_q, select=-body_no_code)

# function to break by character, remove blanks, and count
character_counter <- function(x){
  v <- unlist(strsplit(x, ''))
  not_blanks <- v[which(v != " " & v != "\n")]
  return(length(not_blanks))
}

# call function
ans_code_chars <- lapply(big_a$code_frags, function(x){
  return(sum(character_counter(x)))
})

q_code_chars <- lapply(big_q$code_frags, function(x){
  return(sum(character_counter(x)))
})

big_a$code_chars <- ans_code_chars
big_a$code_chars <- as.numeric(big_a$code_chars)

big_q$code_chars <- q_code_chars
big_q$code_chars <- as.numeric(big_q$code_chars)

# calculate body and proportion
q_body_chars <- lapply(big_q$body_all_no_html, character_counter)
big_q$body_chars <- as.numeric(q_body_chars)
big_q$code_body_props <- 100*(big_q$code_chars/big_q$body_chars)

a_body_chars <- lapply(big_a$body_all_no_html, character_counter)
big_a$body_chars <- as.numeric(a_body_chars)
big_a$code_body_props <- 100*(big_a$code_chars/big_a$body_chars)

# subset to get what just done
big_q_added <- big_q[, 20:25]
big_a_added <- big_a[, 14:19]

unlist_paste <- function(x){
  tmp <- unlist(x)
  result <- paste(tmp, collapse = " + ")
  return(result)
}

q <- lapply(big_q_added$code_frags, unlist_paste)
big_q_added$new_code_frags <- unlist(q)

a <- lapply(big_a_added$code_frags, unlist_paste)
big_a_added$new_code_frags <- unlist(a)

big_q_added <- subset(big_q_added, select=-code_frags)
big_a_added <- subset(big_a_added, select=-code_frags)

write.csv(big_q_added, "stack_q_code.csv")
write.csv(big_a_added, "stack_a_code.csv")
