

#### Create character ------------------------------------------------------####
#' Main function to create the character
#'
#' @description
#' Creates Character with Name race and class, in addition other functions
#' are used to determine the characters stats.
#'
#' @return An object of class \code{gamecharacter}
#' @export
#'
#' @examples
#' hero <- create_character()
create_character <- function(name = NULL, race = NULL, class = NULL) {
  cat("👤 Welcome to Character Creation!\n")

  # Ask for name
  if (is.null(name)) {
    name <- readline(prompt = "Enter your character's name: ")
  }

  # Ask for race if not provided
  if (is.null(race)) {
    cat("Choose your race:\n1: Orc\n2: Human\n")
    race_input <- readline(prompt = "Race (1 or 2): ")
    race <- ifelse(race_input == "1", "Orc", "Human")
  }

  # Ask for class if not provided
  if (is.null(class)) {
    cat("Choose your class:\n1: Warrior\n2: Mage\n")
    class_input <- readline(prompt = "Class (1 or 2): ")
    class <- ifelse(class_input == "1", "Warrior", "Mage")
  }

  # Generate stats
  stats <- Determine_stats(race, class)

  # Build the character
  obj <- list(
    name = name,
    race = race,
    class = class,
    level = 1,
    hp = stats$hp,
    xp = 0,
    point = 0,
    strength = stats$strength,
    intellect = stats$intellect
  )
  class(obj) <- "gamecharacter"

  summary.gamecharacter(obj)

  return(obj)
}



#### Stat determining at level 1 -------------------------------------------####
#' Initial stat function
#'
#' @param obj A gamecharacter object.
#' @param class string
#' @description
#' The function is only used in create_character function, to determine
#' initial stats.
#'
#' @return A list which holds the stats
#' @export
#'
Determine_stats <- function(race, class) {
  # Orc, Warrior
  if (race == "Orc" &  class == "Warrior") {
    strength <- 2+2*(round(abs(rnorm(1,2,6))));
    hp <- 100+(round((rnorm(1,2,6))));
    intellect <- (round((rnorm(1,2,6))));
  }
  # Orc, Mage
  else if (race == "Orc" & class == "Mage") {
    strength <- 1+(round((rnorm(1,2,6))));
    hp <- 100+(round((rnorm(1,2,6))));
    intellect <- 1+2*(round(abs(rnorm(1,2,6))));
  }
  # Human, Warrior
  else if (race == "Human" & class == "Warrior") {
    strength <- 1+2*(round(abs(rnorm(1,2,6))));
    hp <- 100+(round((rnorm(1,2,6))));
    intellect <- 1+(round((rnorm(1,2,6))));
  }
  # Human, Mage
  else if (race == "Human" & class == "Mage") {
    strength <- (round((rnorm(1,2,6))));
    hp <- 100+(round((rnorm(1,2,6))));
    intellect <- 2+2*(round(abs(rnorm(1,2,6))));
  }
  else {
    stop("Invalid race/class combination.")
  }
  return(list(strength = strength, hp = hp, intellect = intellect))
}

#### Character summary function --------------------------------------------####
#' Character summary function
#'
#' @param obj
#'
#' @return the summary for the character
#' @export
#'
#' @examples
#' Character Summary
#' ===========================
#' Name      | Keld
#' Race      | Human
#' Class     | Mage
#' Level     | 1
#' HP        | 104
#' Strength  | 2
#' Intellect | 6
#' ===========================
summary.gamecharacter <- function(object) {
  cat("Character Summary\n")
  cat("===========================\n")
  cat("Name      |", object$name, "\n")
  cat("Race      |", object$race, "\n")
  cat("Class     |", object$class, "\n")
  cat("Level     |", object$level, "\n")
  cat("HP        |", object$hp, "\n")
  cat("Strength  |", object$strength, "\n")
  cat("Intellect |", object$intellect, "\n")
  cat("===========================\n")

  invisible(
    list(
      name = object$name,
      race = object$race,
      class = object$class,
      level = object$level,
      hp = object$hp,
      strength = object$strength,
      intellect = object$intellect
    )
  )
}


#### Update stats: ---------------------------------------------------------####

#' Update character stats using available points
#'
#' @param obj A gamecharacter object.
#'
#' @return The updated character object.
#' @export
#'
#' @examples
#' my_hero <- Update_stats(my_hero)
Update_stats <- function(obj, choices = NULL) {
  if (!inherits(obj, "gamecharacter")) stop("Object is not of class 'gamecharacter'.")

  if (obj$point > 0) {
    cat("🛠️  Use your stat points to upgrade attributes.\n")

    for (i in seq_len(obj$point)) {
      cat("\nStat points left:", obj$point - i + 1, "\n")
      cat("Allocate point to:\n  1: Strength\n  2: Intellect\n")

      if (is.null(choices)) {
        point_input <- readline(prompt = "Choose (1 or 2): ")
      } else {
        if (i <= length(choices)) {
          point_input <- as.character(choices[i])
          cat("Auto-selected:", point_input, "\n")
        } else {
          point_input <- "0"  # invalid input if not enough choices
        }
      }

      if (point_input == "1") {
        obj$strength <- obj$strength + 2
        cat("💪 Strength increased!\n")
      } else if (point_input == "2") {
        obj$intellect <- obj$intellect + 2
        cat("🧠 Intellect increased!\n")
      } else {
        cat("❌ Invalid input. No point allocated.\n")
      }
    }

    obj$point <- 0  # Reset stat points after use
  } else {
    cat("⚠️  No stat points available.\n")
  }

  return(obj)
}


#### Update level and xp ---------------------------------------------------####

#' Update XP and Level of a Game Character
#'
#' This function updates a game character's XP by adding the gained amount and checks
#' if the character should level up. If XP exceeds 100, the level increases and
#' XP is reset to the remainder.
#'
#' @param obj A \code{gamecharacter} object.
#' @param gained_xp Numeric value. The amount of XP gained.
#'
#' @return An updated \code{gamecharacter} object with adjusted XP and level.
#' @export
level_XP_update <- function(obj, gained_xp) {
  if (!inherits(obj, "gamecharacter")) stop("Object is not of class 'gamecharacter'.")

  # Add the XP
  obj$xp <- obj$xp + gained_xp

  # Calculate how many full levels gained
  levels_gained <- floor(obj$xp / 100)

  # Update level and remaining XP
  if (levels_gained > 0) {
    obj$level <- obj$level + levels_gained
    obj$point <- obj$point + levels_gained
    obj$xp <- obj$xp %% 100  # remainder XP after leveling
    cat(paste0("🎉 ", obj$name, " leveled up! Now at level ", obj$level, "!\n"))
  } else {
    cat(paste0("✨ ", gained_xp, " XP gained. ", obj$xp, "/100 XP to next level.\n"))
  }
  return(obj)

}


#### Plotting XP bar -------------------------------------------------------####

#' Plot XP Progress of a Game Character
#'
#' Visualizes the XP progression of a \code{gamecharacter} using a horizontal bar plot.
#' The current XP is shown in purple and the remaining XP to level up is shown in gray.
#' The current and next levels are also displayed.
#'
#' @param obj A \code{gamecharacter} object created by \code{create_character()}.
#'
#' @return This function creates a plot and returns \code{NULL} invisibly.
#' @export
#'
#' @examples
#' plot(hero)
plot.gamecharacter <- function(obj) {
  if (!inherits(obj, "gamecharacter"))
    stop("Object is not of class 'gamecharacter'.")

  total_xp <- 100
  current_xp <- obj$xp
  remaining_xp <- total_xp - current_xp

  # Setup empty plot
  plot(0, type = "n", xlim = c(0, total_xp), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", main = paste("XP Progress"))

  # Draw XP bar (current XP in purple)
  rect(xleft = 0, ybottom = 0.4, xright = current_xp, ytop = 0.6,
       col = "purple", border = NA)

  # Draw remaining XP bar in gray
  rect(xleft = current_xp, ybottom = 0.4, xright = total_xp, ytop = 0.6,
       col = "lightgray", border = NA)

  # Add level labels
  text(x = 50, y = 0.8, labels = "->", cex = 1.5)
  text(x = 0, y = 0.8, labels = paste("Lvl", obj$level), adj = 0, cex = 1.2)
  text(x = total_xp, y = 0.8, labels = paste("Lvl", obj$level + 1),
       adj = 1, cex = 1.2)

  # Add centered XP text
  text(x = total_xp / 2, y = 0.5, labels = paste0(current_xp, " / ", total_xp),
       col = "white", cex = 1.2)
}
####------------------------------------------------------------------------####

#' Print method for gamecharacter
#'
#' @param x A gamecharacter object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns the object.
#' @export
#'
#' @examples
#' print(my_hero)
print.gamecharacter <- function(x, ...) {
  if (!inherits(x, "gamecharacter"))
    stop("Object is not of class 'gamecharacter'.")

  cat("🧙‍♂️ Character: ", x$name, "\n")
  cat("📜 A", x$race, x$class, "at level", x$level, "\n")
  cat("❤️ HP:", x$hp, " | 🔋 XP:", x$xp, "/100\n")
  cat("💪 Strength:", x$strength, " | 🧠 Intellect:", x$intellect, "\n")
  cat("✨ Stat Points Available:", x$point, "\n")

  invisible(x)
}





