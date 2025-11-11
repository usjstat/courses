# =========================
# Decision Tree: Openers ??? Pitch type ??? Runs (Loss/Regret)
# =========================

# ---- 0) Packages ----
install.packages(c("DiagrammeR","DiagrammeRsvg","rsvg"))  # uncomment if needed
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# ---- 1) Problem setup (edit these) ----
openers <- c("A", "B")
states  <- c("Pace-friendly", "Spin-friendly", "Neutral")
# Prior probabilities over pitch type:
p_state <- c(0.35, 0.30, 0.35)                # sum to 1

# Expected runs for each opener under each state:
#            Pace  Spin  Neutral
runs <- rbind(
  A = c(34,   27,   30),
  B = c(30,   33,   29)
)
colnames(runs) <- states 

stopifnot(all(abs(sum(p_state) - 1) < 1e-8),
          setequal(rownames(runs), openers),
          setequal(colnames(runs), states))

# ---- 2) Regret (loss) and Bayes/Minimax summaries ----
# Regret = best runs in a state - runs of chosen opener
best_by_state <- apply(runs, 2, max)  # named by states
regret <- sweep(runs, 2, best_by_state[colnames(runs)], FUN = function(x, m) m - x)

# Expected loss (Bayes risk) under prior for each opener:
bayes_risk <- as.numeric(regret %*% p_state)
names(bayes_risk) <- openers

# Worst-case loss (minimax criterion) for each opener:
minimax_risk <- apply(regret, 1, max)

# Bayes-optimal & Minimax-optimal
bayes_best  <- names(which.min(bayes_risk))
minimax_best <- names(which.min(minimax_risk))

# ---- 3) Build DOT graph for DiagrammeR ----
# Helper to create leaf labels with runs and regret
leaf_lab <- function(opener, st) {
  r <- runs[opener, st]
  rg <- regret[opener, st]
  paste0("Runs: ", r, "\\nRegret: ", round(rg, 1))
}

# Root label summarizing recommendations
root_label <- paste0(
  "Choose opener (Decision)\\n",
  "Bayes risk: A=", round(bayes_risk['A'],2), "  B=", round(bayes_risk['B'],2), "\\n",
  "Bayes-optimal: ", bayes_best, "   |   Minimax-optimal: ", minimax_best
)

# Colors / shapes
col_decision <- "#4C78A8"
col_chance   <- "#F58518"
col_leaf_A   <- "#54A24B"
col_leaf_B   <- "#E45756"

# Edge labels = state name + prob
edge_lab <- paste0(states, " (p=", p_state, ")")

# DOT graph (one decision node ??? two chance subtrees ??? six leaves)
dot <- paste0("
digraph decision_tree {
  graph [rankdir=LR, labelloc=t, fontsize=12];
  node  [fontname=Helvetica];

  // Decision node
  D [label='", root_label, "', shape=box, style=filled, fillcolor='", col_decision, 
              "', color='", col_decision, "', fontcolor=white];

  // Actions (choose A or B)
  A [label='Pick Opener A', shape=box, style='rounded,filled', fillcolor='#D8E6F2'];
  B [label='Pick Opener B', shape=box, style='rounded,filled', fillcolor='#F7DCC3'];

  D -> A [label=' action: A', fontsize=11, color='#6E90B3'];
  D -> B [label=' action: B', fontsize=11, color='#B37B4C'];

  // Chance nodes after choosing A
  C_A [label='Pitch type (Chance)', shape=circle, style=filled, fillcolor='", col_chance, 
              "', fontcolor=white];

  A -> C_A;

  // Leaves for A
  A_P [label='", states[1], "\\n", leaf_lab('A', states[1]), 
              "', shape=oval, style=filled, fillcolor='", col_leaf_A, "', fontcolor=white];
  A_S [label='", states[2], "\\n", leaf_lab('A', states[2]), 
              "', shape=oval, style=filled, fillcolor='", col_leaf_A, "', fontcolor=white];
  A_N [label='", states[3], "\\n", leaf_lab('A', states[3]), 
              "', shape=oval, style=filled, fillcolor='", col_leaf_A, "', fontcolor=white];

  C_A -> A_P [label='", edge_lab[1], "'];
  C_A -> A_S [label='", edge_lab[2], "'];
  C_A -> A_N [label='", edge_lab[3], "'];

  // Chance nodes after choosing B
  C_B [label='Pitch type (Chance)', shape=circle, style=filled, fillcolor='", col_chance, 
              "', fontcolor=white];

  B -> C_B;

  // Leaves for B
  B_P [label='", states[1], "\\n", leaf_lab('B', states[1]), 
              "', shape=oval, style=filled, fillcolor='", col_leaf_B, "', fontcolor=white];
  B_S [label='", states[2], "\\n", leaf_lab('B', states[2]), 
              "', shape=oval, style=filled, fillcolor='", col_leaf_B, "', fontcolor=white];
  B_N [label='", states[3], "\\n", leaf_lab('B', states[3]), 
              "', shape=oval, style=filled, fillcolor='", col_leaf_B, "', fontcolor=white];

  C_B -> B_P [label='", edge_lab[1], "'];
  C_B -> B_S [label='", edge_lab[2], "'];
  C_B -> B_N [label='", edge_lab[3], "'];

  // Aesthetics
  edge [fontname=Helvetica, fontsize=11];
}
")

# ---- 4) Render the tree ----
gr <- DiagrammeR::grViz(dot)
gr  # shows in RStudio Viewer / HTML device

# ---- 5) (Optional) Save to PNG ----
# Convert to SVG, then to PNG for slides
svg_txt <- export_svg(gr)
rsvg_png(charToRaw(svg_txt), file = "decision_tree_openers.png", width = 1800, height = 900)

message("Saved PNG to: ", normalizePath("decision_tree_openers.png"))
