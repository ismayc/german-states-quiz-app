# german-states-quiz-app

A [shiny app](https://chesterismay.shinyapps.io/german-states-quiz-app/) that quizzes users on German federal states and their capitals. 
The app provides a user-friendly interface for learning and testing knowledge about German geography.

## Features

* **Full 50-state + DC quiz**
  – Includes all 13 federal states **plus** the 3 city-states.

* **Randomized question order**
  – On start (and on "Restart"), the full list of states is shuffled so each quiz is new.

* **Multiple‐part guessing**
  – Core: "Guess the state/district name."
  – Optional: checkboxes to also guess the capital, the largest city, and the second largest city—each enabled independently.

* **Fuzzy matching & misspelling feedback**
  – Levenshtein‐distance check flags small typos ("❗ Misspelling: off by N characters") separately from outright wrong answers.

* **Progress tracking & score**
  – "Question X of 16" and a visual progress bar showing percent complete.
  – Live "Score: 7 / 9" display.

* **"Give Up" and "Restart" controls**
  – "Give Up" shows the correct answers and immediately moves on.
  – "Restart" reshuffles and resets all counters (optionally saving results first).

* **Enter-to-submit**
  – Pressing Enter in any text field triggers the "Submit Guess" button via custom JS.

* **State‐shape rendering**
  – 200 px tall `ggplot` of the silhouette (black fill, no axes).

* **Interactive German map**
  – Full‐screen Leaflet map in the main panel showing all states in black initially.
  – As you guess each state correctly (or give up), it’s recolored steel-blue with a hover label: "State Name – Population (2023)."

* **City markers on the map**
  – When a state is answered (correct or given up), its capital, largest, and second‐largest cities appear as distinct markers/groups (star icons for capitals, circles for others) with pop-up labels including population.

* **Responsive layout**
  – Uses Shiny’s `fluidPage()`/`sidebarLayout()` so both mobile and desktop screens render nicely.

* **Clean, minimal styling**
  – Collapsed margins on form groups; shrunk `checkbox` labels; consistent `theme_void()` on plots; clear grouping of map layers.

