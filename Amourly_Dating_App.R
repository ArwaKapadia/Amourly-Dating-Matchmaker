# ----------------------------
# 💘 Amourly Dating Matchmaker
# ----------------------------

# Trait pool (updated: "curious" -> "flirty")
all_traits <- c("funny", "kind", "smart", "active", "creative", "calm", "flirty", "adventurous")

# Profile generator
generate_profiles <- function(names, gender) {
  lapply(names, function(name) {
    list(
      name = name,
      gender = gender,
      traits = sample(all_traits, 3)
    )
  })
}

# 56 Girls
girl_names <- c("Emma", "Olivia", "Sophia", "Ava", "Isabella", "Mia", "Charlotte", "Amelia", "Harper", "Evelyn",
                "Abigail", "Emily", "Ella", "Elizabeth", "Camila", "Luna", "Sofia", "Avery", "Mila", "Aria",
                "Scarlett", "Penelope", "Layla", "Chloe", "Victoria", "Madison", "Eleanor", "Grace", "Nora", "Riley",
                "Zoey", "Hannah", "Hazel", "Lily", "Ellie", "Violet", "Lillian", "Addison", "Aubrey", "Stella",
                "Natalie", "Zoe", "Leah", "Savannah", "Audrey", "Brooklyn", "Bella", "Claire", "Skylar", "Lucy",
                "Paisley", "Everly", "Anna", "Caroline", "Nova", "Genesis")

# 56 Boys
boy_names <- c("Liam", "Noah", "Oliver", "Elijah", "James", "William", "Benjamin", "Lucas", "Henry", "Alexander",
               "Mason", "Michael", "Ethan", "Daniel", "Jacob", "Logan", "Jackson", "Levi", "Sebastian", "Mateo",
               "Jack", "Owen", "Theodore", "Aiden", "Samuel", "Joseph", "John", "David", "Wyatt", "Matthew",
               "Luke", "Asher", "Carter", "Julian", "Grayson", "Leo", "Jayden", "Gabriel", "Isaac", "Lincoln",
               "Anthony", "Hudson", "Dylan", "Ezra", "Thomas", "Charles", "Christopher", "Jaxon", "Maverick", "Josiah",
               "Isaiah", "Andrew", "Elias", "Joshua", "Nathan", "Caleb")

# Generate profiles
girl_profiles <- generate_profiles(girl_names, "female")
boy_profiles <- generate_profiles(boy_names, "male")

# ----------------------------
# 👤 User Input
# ----------------------------

cat("💘 Welcome to the Amourly Dating Matchmaker!\n\n")

gender <- tolower(readline("What is your gender? (male/female): "))
while (!(gender %in% c("male", "female"))) {
  gender <- tolower(readline("Please enter 'male' or 'female': "))
}

name <- readline("What is your name? ")

cat("\nHere are some traits you can choose from:\n")
cat(paste("-", all_traits), sep = "\n")

# Trait selection
user_traits <- c()
cat("\nPlease enter 3 traits that describe you best:\n")
while (length(user_traits) < 3) {
  input <- tolower(trimws(readline(paste("Trait", length(user_traits) + 1, ": "))))
  if (!(input %in% all_traits)) {
    cat("❌ Not a valid trait. Choose from the list.\n")
  } else if (input %in% user_traits) {
    cat("❗ You've already selected that trait.\n")
  } else {
    user_traits <- c(user_traits, input)
  }
}

# ----------------------------
# 🔍 Matchmaking
# ----------------------------

# Choose pool of opposite gender
pool <- if (gender == "male") girl_profiles else boy_profiles

# Score calculation
matches <- lapply(pool, function(profile) {
  match_count <- sum(user_traits %in% profile$traits)
  list(name = profile$name, traits = profile$traits, match_count = match_count)
})

# Sort by number of matching traits (descending)
matches <- matches[order(sapply(matches, function(m) -m$match_count))]

# Initialize match slots
top_100 <- NULL
top_67 <- NULL
top_33 <- NULL

# Pick top matches based on score (and unique names)
for (m in matches) {
  if (m$match_count == 3 && is.null(top_100)) {
    top_100 <- m
  } else if (m$match_count == 2 && is.null(top_67)) {
    top_67 <- m
  } else if (m$match_count == 1 && is.null(top_33)) {
    top_33 <- m
  }
  if (!is.null(top_100) && !is.null(top_67) && !is.null(top_33)) {
    break
  }
}

# ----------------------------
# 💌 Output
# ----------------------------

cat("\n💖 Match Results for", name, "💖\n")
cat("Your traits:", paste(user_traits, collapse = ", "), "\n\n")

if (!is.null(top_100)) {
  cat("💘 Perfect Match: ", top_100$name, "\n")
  cat("Traits:", paste(top_100$traits, collapse = ", "), "\n")
  cat("Compatibility Score: 100%\n\n")
}

if (!is.null(top_67)) {
  cat("💞 Good Match: ", top_67$name, "\n")
  cat("Traits:", paste(top_67$traits, collapse = ", "), "\n")
  cat("Compatibility Score: 67%\n\n")
}

if (!is.null(top_33)) {
  cat("💬 Mild Match: ", top_33$name, "\n")
  cat("Traits:", paste(top_33$traits, collapse = ", "), "\n")
  cat("Compatibility Score: 33%\n\n")
}

if (is.null(top_100) && is.null(top_67) && is.null(top_33)) {
  cat("😢 Sorry, no compatible matches found. Try different traits!\n")
} else {
  cat("💌 Match Completed!\n")
}