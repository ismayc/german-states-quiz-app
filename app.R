library(shiny)
library(sf)
library(leaflet)
library(tidyverse)
library(stringdist)

# https://en.wikipedia.org/wiki/List_of_cities_in_Germany_by_population
# https://en.wikipedia.org/wiki/States_of_Germany

#library(googlesheets4)
#googlesheets4::gs4_auth(path = "shiny-sheets-writer-key.json")


# 1. Read in your CSV of states + cities
states <- read_rds("states2023.rds")
country_outline <- st_union(states)

# Helper: if “A / B” and A == B, return just A; else keep “A / B”
collapse_if_same <- function(x) {
  map_chr(x, ~ {
    parts <- str_split(.x, "/", n = 2)[[1]] |> str_trim()
    if (length(parts) < 2) {
      parts[1]
    } else if (parts[1] == parts[2]) {
      parts[1]
    } else {
      paste(parts, collapse = " / ")
    }
  })
}

states <- states |>
  mutate(
    state_name          = collapse_if_same(state_name),
    capital             = collapse_if_same(capital),
    largest_city        = collapse_if_same(largest_city),
    second_largest_city = collapse_if_same(second_largest_city)
  )

# Helper to split “EN / DE” strings into two trimmed pieces
split_en_de <- function(x) {
  parts <- str_split_fixed(x, "/", 2)
  tibble(
    en = str_trim(parts[,1]),
    de = str_trim(parts[,2])
  )
}

# 1a) State map
state_pairs <- split_en_de(states$state_name)
state_map <- c(
  setNames(states$state_name, str_to_lower(state_pairs$en)),
  setNames(states$state_name, str_to_lower(state_pairs$de))
)

# after you load and mutate `states` and define split_en_de():
cap_pairs <- split_en_de(states$capital)
capital_map <- c(
  setNames(states$state_name, str_to_lower(cap_pairs$en)),
  setNames(states$state_name, str_to_lower(cap_pairs$de))
)

lg_pairs <- split_en_de(states$largest_city)
largest_map <- c(
  setNames(states$state_name, str_to_lower(lg_pairs$en)),
  setNames(states$state_name, str_to_lower(lg_pairs$de))
)

sl_pairs <- split_en_de(states$second_largest_city)
second_map <- c(
  setNames(states$state_name, str_to_lower(sl_pairs$en)),
  setNames(states$state_name, str_to_lower(sl_pairs$de))
)



# # 1b) Capital map
# cap_pairs <- split_en_de(states$capital)
# capital_map <- c(
#   setNames(states$capital, str_to_lower(cap_pairs$en)),
#   setNames(states$capital, str_to_lower(cap_pairs$de))
# )
# 
# # 1c) Largest city map
# lg_pairs <- split_en_de(states$largest_city)
# largest_map <- c(
#   setNames(states$largest_city, str_to_lower(lg_pairs$en)),
#   setNames(states$largest_city, str_to_lower(lg_pairs$de))
# )
# 
# # 1d) Second-largest city map
# sl_pairs <- split_en_de(states$second_largest_city)
# second_map <- c(
#   setNames(states$second_largest_city, str_to_lower(sl_pairs$en)),
#   setNames(states$second_largest_city, str_to_lower(sl_pairs$de))
# )




# UI
ui <- fluidPage(
  tags$head(
    # your existing CSS
    tags$style(HTML("
      /* 1) Remove the form‐group’s bottom margin entirely */
      .form-group.shiny-input-container {
        margin-bottom: 0 !important;
      }
      /* 2) Collapse the checkbox wrapper (<div class=\"checkbox\">) */
      .form-group.shiny-input-container .checkbox,
      .form-group.shiny-input-container .checkbox-inline {
        margin: 0 !important;
        padding: 0 !important;
      }
      /* 3) Shrink the label inside the checkbox */
      .form-group.shiny-input-container .checkbox label,
      .form-group.shiny-input-container .checkbox-inline label {
        margin: 0 !important;
        padding: 0 !important;
        line-height: 1 !important;
      }
    ")),
    
    # new JS for Enter‐to‐Submit
    tags$script(HTML("
      // When Enter is pressed in any guess‐field, trigger the Submit button
      $(document).on('keydown', '#state_guess, #capital_guess, #largest_guess, #second_guess', function(e) {
        if (e.keyCode === 13) {
          e.preventDefault();
          $('#submit').click();
        }
      });
    "))
  ),
  
  titlePanel("German Federal State Shape Quiz"),
  h4("Created by Chester Ismay"),
  p("Both German and English spellings are accepted."),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("question_number"),
      uiOutput("progress_bar"),
      plotOutput("state_shape", height = "200px"),
      textInput("state_guess", "Guess the state name:"),
      checkboxInput("guess_capital", "Also guess the capital?", value = FALSE),
      conditionalPanel(
        condition = "input.guess_capital",
        textInput("capital_guess", "Guess the capital city:")
      ),
      checkboxInput(
        "guess_largest",
        "Also guess the largest city?",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.guess_largest",
        textInput("largest_guess", "Guess the largest city:")
      ),
      checkboxInput(
        "guess_second",
        "Also guess the second largest city?",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.guess_second",
        textInput("second_guess", "Guess the second largest city:")
      ),
      actionButton("submit", "Submit Guess"),
      actionButton("giveup", "Give Up on This Question"),
      actionButton("restart", "Restart Quiz"),
      verbatimTextOutput("feedback"),
      # checkboxInput(
      #   "save",
      #   "Log overall quiz results to Google Sheets?",
      #   value = FALSE
      # ),
      conditionalPanel(
        condition = "input.save",
        textInput("user_name", "Enter your name:"),
        actionButton("save_results", "Save Results to Google Sheets")
      )
    ),
    mainPanel(
      helpText(
        paste("You can make unlimited guesses for each question by",
              "clicking on 'Submit Guess'.")
      ),
      helpText(
        paste("If you're stuck on a question, click",
              "'Give Up on This Question' to see the answer.")
      ),
      checkboxInput("show_states", "Show state outlines (for learning)", value = TRUE),
      leafletOutput("country_map", height = "600px"),
      verbatimTextOutput("score")
    )
  )
)

# Server
server <- function(input, output, session) {
  remaining_states <- reactiveVal(sample(states$state_name))
  guessed_states <- reactiveVal(character())
  score <- reactiveVal(0)
  total_attempts <- reactiveVal(0)
  current_state <- reactiveVal(NULL)
  wrong_states <- reactiveVal(character())
  wrong_capitals <- reactiveVal(character())
  wrong_largests <- reactiveVal(character())
  wrong_second_largests <- reactiveVal(character())
  
  output$question_number <- renderText({
    total_states <- nrow(states)
    attempted <- total_states - length(remaining_states())
    paste0("Question ", min(attempted + 1, total_states), " of ", total_states)
  })
  
  output$progress_bar <- renderUI({
    total <- nrow(states)
    attempted <- total - length(remaining_states())
    percent <- round((attempted / total) * 100)
    
    div(
      style = "margin-top: 5px; margin-bottom: 10px;",
      div("Progress:", style = "font-weight: bold;"),
      tags$div(
        style = "background-color: #e0e0e0; border-radius: 5px; height: 20px;",
        tags$div(
          style = paste0(
            "background-color: #428bca; width: ",
            percent,
            "%; ",
            "height: 100%; border-radius: 5px;"
          )
        )
      ),
      div(
        paste0(percent, "% completed"),
        style = "font-size: 12px; margin-top: 4px;"
      )
    )
  })
  
  # Initialize first state
  observe({
    rs <- remaining_states()
    if (is.null(current_state()) && length(rs) > 0) {
      current_state(rs[1])
    }
  })
  
  # Draw the state shape
  output$state_shape <- renderPlot({
    req(current_state())
    shape <- states |> filter(state_name == current_state())
    ggplot(shape) +
      geom_sf(fill = "black") +
      theme_void()
  })
  
  # Draw the country map
  output$country_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = 10.0, lat = 51.2, zoom = 6) |>
      # the always‐visible border:
      addPolygons(
        data      = country_outline,
        color     = "black",
        weight    = 2,
        fill      = FALSE,
        group     = "outline"
      ) |>
      # the toggleable internal state boundaries:
      addPolygons(
        data        = states,
        layerId     = ~state_name,
        fillColor   = "#000000",
        color       = "white",
        weight      = 1,
        fillOpacity = 0.95,
        group       = "states"
      )
  })
  
  
  # Observe the toggle and hide/show that group
  observeEvent(input$show_states, {
    proxy <- leafletProxy("country_map")
    if (input$show_states) {
      proxy |>
        clearGroup("states") |>
        addPolygons(
          data        = states,
          layerId     = ~state_name,
          fillColor   = "#000000",
          color       = "white",
          weight      = 1,
          fillOpacity = 0.95,
          group       = "states"
        )
    } else {
      proxy |> clearGroup("states")
    }
  })
  
  
  
  # Helpers for normalization
  normalize_state <- function(x) {
    x <- str_to_lower(x) |>
      str_replace_all("\\.", "") |>
      str_trim()
    if (x == "dc") "district of columbia" else x
  }
  normalize_capital <- function(x) {
    x <- str_to_lower(x) |>
      str_replace_all("\\.", "") |>
      str_replace_all("^st ", "saint ") |>
      str_trim()
    x
  }
  normalize_city <- function(x) {
    str_to_lower(x) |> str_replace_all("\\.", "") |> str_trim()
  }
  
  add_city_markers <- function(state_name) {
    state_data <- states |> filter(state_name == !!state_name)
    
    cap <- state_data$capital
    cap_lat <- state_data$latitude_capital
    cap_lon <- state_data$longitude_capital
    
    largest <- state_data$largest_city
    largest_lat <- state_data$latitude_largest_city
    largest_lon <- state_data$longitude_largest_city
    largest_pop <- state_data$largest_city_population_2022
    
    second <- state_data$second_largest_city
    second_lat <- state_data$latitude_second_largest_city
    second_lon <- state_data$longitude_second_largest_city
    second_pop <- state_data$second_largest_city_population_2022
    
    # Determine if capital is also a largest/second-largest city
    cap_role <- ""
    if (cap == largest) {
      cap_role <- paste0(
        " (Capital & Largest, Pop: ",
        format(largest_pop, big.mark = ","),
        ")"
      )
    } else if (cap == second) {
      cap_role <- paste0(
        " (Capital & Second Largest, Pop: ",
        format(second_pop, big.mark = ","),
        ")"
      )
    }
    
    proxy <- leafletProxy("country_map") |>
      clearGroup("capitals") |>
      clearGroup("largest") |>
      clearGroup("second_largest")
    
    if (!is.na(cap_lat) && !is.na(cap_lon)) {
      proxy <- proxy |>
        addAwesomeMarkers(
          lng = cap_lon,
          lat = cap_lat,
          icon = awesomeIcons(
            icon = 'star',
            markerColor = 'red',
            iconColor = 'white',
            library = 'fa'
          ),
          label = paste0("Capital: ", cap, ", ", state_name, cap_role),
          group = "capitals"
        )
    }
    
    if (!is.na(largest_lat) && !is.na(largest_lon) && cap != largest) {
      proxy <- proxy |>
        addCircleMarkers(
          lng = largest_lon,
          lat = largest_lat,
          radius = 5,
          color = "pink",
          fillColor = "pink",
          fillOpacity = 1,
          stroke = FALSE,
          label = paste0(
            "Largest: ",
            largest,
            " (",
            format(largest_pop, big.mark = ","),
            ")"
          ),
          group = "largest"
        )
    }
    
    if (!is.na(second_lat) && !is.na(second_lon) && cap != second) {
      proxy <- proxy |>
        addCircleMarkers(
          lng = second_lon,
          lat = second_lat,
          radius = 5,
          color = "green",
          fillColor = "green",
          fillOpacity = 1,
          stroke = FALSE,
          label = paste0(
            "Second: ",
            second,
            " (",
            format(second_pop, big.mark = ","),
            ")"
          ),
          group = "second_largest"
        )
    }
  }
  
  normalize_guess <- function(x) {
    str_to_lower(x) |> str_replace_all("\\.", "") |> str_trim()
  }
  
  # Submit handler
  observeEvent(input$submit, {
    req(input$state_guess, current_state())
    total_attempts(total_attempts() + 1)
    
    answered       <- current_state()
    feedback_parts <- character()
    dist_tol       <- 5
    
    # --- STATE GUESS ---
    key_state       <- normalize_guess(input$state_guess)
    mapped_state    <- state_map[key_state]
    is_strict_state <- !is.na(mapped_state) && mapped_state == answered
    
    if (is_strict_state) {
      feedback_parts <- c(feedback_parts,
                          paste0("✅ ", answered, " is the correct state!"))
    } else {
      # fuzzy fallback
      dists  <- stringdist(key_state, names(state_map), method="lv")
      i_best <- which.min(dists)
      best_key   <- names(state_map)[i_best]
      best_state <- state_map[best_key]
      best_dist  <- dists[i_best]
      
      if (best_state == answered && best_dist <= dist_tol) {
        feedback_parts <- c(feedback_parts,
                            paste0("❗ Misspelling: off by ", best_dist,
                                   " character", ifelse(best_dist>1,"s","")))
      } else {
        feedback_parts <- c(feedback_parts, "❌ State incorrect")
        wrong_states(c(wrong_states(), answered))
      }
    }
    
    # --- CAPITAL GUESS ---
    if (is_strict_state && input$guess_capital) {
      key_cap       <- normalize_guess(input$capital_guess)
      mapped_cap    <- capital_map[key_cap]
      is_strict_cap <- !is.na(mapped_cap) && mapped_cap == answered
      
      if (is_strict_cap) {
        feedback_parts <- c(feedback_parts,
                            paste0("✅ ", input$capital_guess,
                                   " is the correct capital of ", answered, "!"))
      } else {
        # typo fallback against the true (German) name
        true_cap <- states %>% filter(state_name==answered) %>% pull(capital)
        dist_cap <- stringdist(key_cap, normalize_city(true_cap), method="lv")
        if (dist_cap <= dist_tol) {
          feedback_parts <- c(feedback_parts,
                              paste0("❗ Misspelling (capital): off by ", dist_cap,
                                     " character", ifelse(dist_cap>1,"s","")))
        } else {
          feedback_parts <- c(feedback_parts, "❌ Capital incorrect")
          wrong_capitals(c(wrong_capitals(), answered))
        }
      }
    }
    
    # --- LARGEST CITY GUESS ---
    if (is_strict_state && input$guess_largest) {
      key_lg       <- normalize_guess(input$largest_guess)
      mapped_lg    <- largest_map[key_lg]
      is_strict_lg <- !is.na(mapped_lg) && mapped_lg == answered
      
      if (is_strict_lg) {
        feedback_parts <- c(feedback_parts,
                            paste0("✅ ", input$largest_guess,
                                   " is the correct largest city of ", answered, "!"))
      } else {
        true_lg <- states %>% filter(state_name==answered) %>% pull(largest_city)
        dist_lg <- stringdist(key_lg, normalize_city(true_lg), method="lv")
        if (dist_lg <= dist_tol) {
          feedback_parts <- c(feedback_parts,
                              paste0("❗ Misspelling (largest): off by ", dist_lg,
                                     " character", ifelse(dist_lg>1,"s","")))
        } else {
          feedback_parts <- c(feedback_parts, "❌ Largest city incorrect")
          wrong_largests(c(wrong_largests(), answered))
        }
      }
    }
    
    # --- SECOND-LARGEST CITY GUESS ---
    if (is_strict_state && input$guess_second) {
      key_sl       <- normalize_guess(input$second_guess)
      mapped_sl    <- second_map[key_sl]
      is_strict_sl <- !is.na(mapped_sl) && mapped_sl == answered
      
      if (is_strict_sl) {
        feedback_parts <- c(feedback_parts,
                            paste0("✅ ", input$second_guess,
                                   " is the correct second-largest city of ", answered, "!"))
      } else {
        true_sl <- states %>% filter(state_name==answered) %>% pull(second_largest_city)
        dist_sl <- stringdist(key_sl, normalize_city(true_sl), method="lv")
        if (dist_sl <= dist_tol) {
          feedback_parts <- c(feedback_parts,
                              paste0("❗ Misspelling (second): off by ", dist_sl,
                                     " character", ifelse(dist_sl>1,"s","")))
        } else {
          feedback_parts <- c(feedback_parts, "❌ Second largest city incorrect")
          wrong_second_largests(c(wrong_second_largests(), answered))
        }
      }
    }
    
    # render feedback
    output$feedback <- renderText(paste(feedback_parts, collapse="\n"))
    
    # advance only if *all* strict checks passed
    if (
      is_strict_state &&
      (!input$guess_capital || is_strict_cap) &&
      (!input$guess_largest || is_strict_lg) &&
      (!input$guess_second || is_strict_sl)
    ) {
      # record a correct answer and move on...
      guessed_states(c(guessed_states(), answered))
      score(score() + 1)
      remaining_states(setdiff(remaining_states(), answered))
      
      # update map
      guessed_df <- states %>% filter(state_name %in% guessed_states())
      labels <- paste0(
        guessed_df$state_name,
        "<br>Population: ",
        format(guessed_df$state_population_2023, big.mark=",")
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("country_map") %>%
        clearGroup("guessed") %>%
        addPolygons(
          data        = guessed_df,
          fillColor   = "steelblue",
          fillOpacity = 0.5,
          color       = "white",
          weight      = 1,
          group       = "guessed",
          label       = labels
        )
      add_city_markers(answered)
      
      # next question
      next_states <- remaining_states()
      current_state(if (length(next_states)>0) next_states[1] else NULL)
      
      # clear inputs
      updateTextInput(session, "state_guess",   value="")
      updateTextInput(session, "capital_guess", value="")
      updateTextInput(session, "largest_guess", value="")
      updateTextInput(session, "second_guess",  value="")
    }
  })
  
  
  # Give Up handler
  observeEvent(input$giveup, {
    req(current_state())
    
    total_attempts(total_attempts() + 1)
    
    given_up <- current_state()
    
    guessed_states(c(guessed_states(), given_up))
    remaining_states(setdiff(remaining_states(), given_up))
    
    # Show polygon for the given-up state
    leafletProxy("country_map") |>
      clearGroup("guessed") |>
      addPolygons(
        data = states |> filter(state_name %in% guessed_states()),
        fillColor = "steelblue",
        fillOpacity = 0.5,
        color = "white",
        weight = 1,
        label = ~ paste0(
          state_name,
          "<br>Population (2023): ",
          format(state_population_2023, big.mark = ",")
        ) |>
          lapply(htmltools::HTML),
        popup = ~paste0(
          "<strong>", state_name, "</strong><br/>",
          "Capital: ", capital, "<br/>",
          "Largest: ", largest_city, "<br/>",
          ifelse(second_largest_city != "",
                 paste0("2nd: ", second_largest_city, "<br/>"), "")
        )
      )
    
    # Show its capital, largest city, and second largest city
    add_city_markers(given_up)
    
    # Extract capital, largest, and second largest cities for display
    state_info <- states |> filter(state_name == given_up)
    
    capname <- state_info$capital
    largest <- state_info$largest_city
    second <- state_info$second_largest_city
    
    output$feedback <- renderText(
      paste0(
        "🙈 The correct answer was: ",
        given_up,
        "\n",
        "🏛️ Capital: ",
        capname,
        "\n",
        "🌆 Largest city: ",
        largest,
        "\n",
        "🏙️ Second largest city: ",
        second
      )
    )
    
    # Advance
    next_states <- remaining_states()
    current_state(if (length(next_states) > 0) next_states[1] else NULL)
    
    updateTextInput(session, "state_guess", value = "")
    updateTextInput(session, "capital_guess", value = "")
    updateTextInput(session, "largest_guess", value = "")
    updateTextInput(session, "second_guess", value = "")
  })
  # Restart handler
  observeEvent(input$restart, {
    if (!is.null(input$save) && input$save) {
      save_quiz_results()
    }
    # reset your quiz state
    total_attempts(0)
    remaining_states(sample(states$state_name))
    guessed_states(character())
    score(0)
    current_state(NULL)
    wrong_states(character())
    wrong_capitals(character())
    wrong_largests(character())
    wrong_second_largests(character())
    
    # reset the map: remove every shape/marker group, then re‑add the all‑black polygons
    leafletProxy("country_map") |>
      clearShapes() |>            # removes all polygons (including guessed blue ones)
      clearGroup("capitals") |>    # removes any capital markers
      clearGroup("largest") |>     # removes any largest‑city markers
      clearGroup("second_largest") |> # removes second‑largest markers
      addPolygons(
        data = states,
        layerId = ~state_name,
        fillColor = "#000000",
        fillOpacity = 0.95,
        color = "white",
        weight = 1
      )
    
    # always clear the old states, then respect the toggle
    proxy <- leafletProxy("country_map") |> clearGroup("states")
    if (input$show_states) {
      proxy |>
        addPolygons(
          data        = states,
          layerId     = ~state_name,
          fillColor   = "#000000",
          color       = "white",
          weight      = 1,
          fillOpacity = 0.95,
          group       = "states"
        )
    }
    
    output$feedback <- renderText("🌀 Quiz restarted! Good luck!")
    
    # clear out the inputs
    updateTextInput(session, "state_guess", value = "")
    updateTextInput(session, "capital_guess", value = "")
    updateTextInput(session, "largest_guess", value = "")
    updateTextInput(session, "second_guess", value = "")
  })
  
  
  # Score display
  output$score <- renderText({
    paste("Score:", score(), "/", total_attempts())
  })
  
  save_quiz_results <- function() {
    if (input$user_name == "") {
      showNotification(
        "⚠️ Please enter your name before saving.",
        type = "warning"
      )
      return()
    }
    
    result_row <- data.frame(
      date = as.character(Sys.Date()),
      name = input$user_name,
      score = score(),
      wrong_states = paste(wrong_states(), collapse = "; "),
      wrong_capitals = if (input$guess_capital)
        paste(wrong_capitals(), collapse = "; ") else NA,
      wrong_largests = if (input$guess_largest)
        paste(wrong_largests(), collapse = "; ") else NA,
      wrong_second_largests = if (input$guess_second)
        paste(wrong_second_largests(), collapse = "; ") else NA,
      stringsAsFactors = FALSE
    )
    
    sheet_url <- paste0(
      "https://docs.google.com/spreadsheets/d/",
      "13T0IL4pZO2RNwT-kWqbc7O4Trv2uRBkew8occILbWz8"
    )
    
    tryCatch(
      {
        googlesheets4::sheet_append(sheet_url, result_row)
        showNotification("✅ Results saved to Google Sheets!", type = "message")
      },
      error = function(e) {
        showNotification(paste("❌ Save failed:", e$message), type = "error")
      }
    )
  }
  
  observeEvent(input$save_results, {
    save_quiz_results()
  })
}

shinyApp(ui, server)
