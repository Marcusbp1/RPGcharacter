

library(tinytest)
library(RPGcharacter)

# Test: create_character returns a list with expected names
hero <- create_character(name = "TestHero", race = "Orc",
                         class = "Mage")  # Use non-interactive version

expect_true(is.list(hero))
expect_true("name" %in% names(hero))
expect_equal(hero$name, "TestHero")

# Test: Determine_stats returns a list with stats
stats <- Determine_stats("Orc", "Warrior")
expect_true(is.list(stats))
expect_true(all(c("strength", "intellect") %in% names(stats)))

# Test: level_XP_update correctly adds XP
hero <- level_XP_update(hero, 250)
expect_true(hero$xp == 50)
expect_true(hero$level == 3)

# Test: Update_stats returns a list
updated_hero <- Update_stats(hero, "1")
expect_true(is.list(updated_hero))

# Test: print returns invisibly and outputs character info
expect_silent(print(hero))

# Test: summary returns a summary list
s <- summary(hero)
expect_true(is.list(s))
expect_true("level" %in% names(s))

# You can also test plot doesn't throw error
expect_silent(plot(hero))
#expect_warning(plot(hero), NA)  # expect no warnings





