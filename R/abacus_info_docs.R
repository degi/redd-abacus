### ==========================================================================
### REDD-Abacus SP — Module Documentation & Information Views
### ==========================================================================
### Comprehensive methodological guides, formulas, and references for each
### main tab page: Land Cover, Emission, Projection, and Economic Trade-Off.
### Sourced from REDD-Abacus SP User Manual (Harja et al. 2011) & ICRAF docs.
### ==========================================================================

#' Helper to generate a standardized documentation section header
doc_header <- function(title, subtitle, icon_name, badge_text = "Methodology & Reference") {
  tags$div(
    class = "d-flex justify-content-between align-items-start flex-wrap gap-2 pb-3 mb-4 border-bottom",
    tags$div(
      tags$h4(
        class = "fw-bold mb-1 d-flex align-items-center gap-2",
        style = "color: #540b0e;",
        icon(icon_name, class = "text-secondary"),
        title
      ),
      tags$div(class = "text-muted", style = "font-size: 0.9rem;", subtitle)
    ),
    tags$span(
      class = "badge bg-danger-subtle text-danger-emphasis border border-danger-subtle px-2 py-1 align-self-center",
      style = "font-size: 0.75rem; letter-spacing: 0.5px;",
      badge_text
    )
  )
}

#' Helper to generate a styled sub-tab documentation card
doc_subtab_card <- function(title, icon_name, description, details_list = NULL, badge = NULL) {
  tags$div(
    class = "card shadow-sm border mb-4",
    style = "border-radius: 8px; overflow: hidden;",
    tags$div(
      class = "card-header bg-light d-flex align-items-center justify-content-between py-2 px-3",
      tags$div(
        class = "fw-semibold d-flex align-items-center gap-2",
        style = "font-size: 0.95rem; color: #540b0e;",
        icon(icon_name, class = "text-primary opacity-75"),
        title
      ),
      if (!is.null(badge)) tags$span(class = "badge bg-secondary-subtle text-secondary-emphasis border", style = "font-size: 0.7rem;", badge)
    ),
    tags$div(
      class = "card-body p-3",
      style = "font-size: 0.88rem; line-height: 1.55;",
      tags$p(class = "mb-2", description),
      if (!is.null(details_list) && length(details_list) > 0) {
        tags$ul(
          class = "mb-0 ps-3",
          style = "font-size: 0.85rem;",
          lapply(details_list, function(item) {
            tags$li(class = "mb-1", item)
          })
        )
      }
    )
  )
}

#' Helper to generate a styled formula box with variable definitions
doc_formula_card <- function(title, formula_html, explanation = NULL, var_table = NULL) {
  tags$div(
    class = "card border mb-4",
    style = "border-radius: 8px; background-color: #fafbfc;",
    tags$div(
      class = "card-header bg-white border-bottom py-2 px-3 d-flex align-items-center gap-2",
      icon("square-root-variable", class = "text-info"),
      tags$span(class = "fw-semibold", style = "font-size: 0.9rem;", title)
    ),
    tags$div(
      class = "card-body p-3",
      tags$div(
        class = "p-3 my-2 text-center rounded border bg-white shadow-sm",
        style = "font-family: 'Cambria Math', 'STIX Two Math', 'Times New Roman', serif; font-size: 1.15rem; letter-spacing: 0.5px; color: #1a1a1a;",
        formula_html
      ),
      if (!is.null(explanation)) tags$p(class = "text-muted small mt-2 mb-2", explanation),
      if (!is.null(var_table) && nrow(var_table) > 0) {
        tags$div(
          class = "table-responsive mt-2",
          tags$table(
            class = "table table-sm table-borderless mb-0",
            style = "font-size: 0.82rem;",
            tags$tbody(
              apply(var_table, 1, function(row) {
                tags$tr(
                  tags$td(class = "fw-semibold py-1 pe-2 text-nowrap", style = "width: 15%; font-family: monospace; color: #540b0e;", row[1]),
                  tags$td(class = "py-1 text-muted", row[2])
                )
              })
            )
          )
        )
      }
    )
  )
}

#' Helper to render reference citations
doc_references_card <- function(refs) {
  tags$div(
    class = "card border shadow-sm mb-4",
    style = "border-radius: 8px; background-color: #fff;",
    tags$div(
      class = "card-header bg-light py-2 px-3 d-flex align-items-center gap-2",
      icon("book", class = "text-secondary"),
      tags$span(class = "fw-semibold", style = "font-size: 0.9rem;", "Key References & Citations")
    ),
    tags$div(
      class = "card-body p-3",
      tags$ol(
        class = "mb-0 ps-3",
        style = "font-size: 0.82rem; line-height: 1.5; color: #495057;",
        lapply(refs, function(r) tags$li(class = "mb-2", r))
      )
    )
  )
}


### ==========================================================================
### 1. LAND COVER MODULE DOCUMENTATION
### ==========================================================================

landcover_info_ui <- tags$div(
  class = "container-fluid py-3 px-3",
  style = "max-height: calc(100vh - 180px); overflow-y: auto;",
  
  doc_header(
    title = "Land Cover Module Documentation",
    subtitle = "Analysis of Land Use and Cover Trajectory (ALUCT) & Spatial Transition Dynamics",
    icon_name = "earth-asia",
    badge_text = "Spatial Baseline & Transitions"
  ),
  
  tags$div(
    class = "alert alert-light border shadow-sm mb-4 p-3",
    style = "border-left: 4px solid #540b0e !important; font-size: 0.9rem;",
    tags$strong("Module Overview: "),
    "The Land Cover module establishes the geographic, categorical, and temporal foundation for REDD-Abacus. ",
    "It implements the ", tags$strong("ALUCT (Analysis of Land Use and Cover Trajectory)"), " approach developed by ",
    "the World Agroforestry Centre (ICRAF) to ingest multi-temporal spatial datasets, validate spatial consistency, ",
    "compute land cover transition matrices across zonation strata, and calculate historical landscape area trends."
  ),
  
  tags$h5(class = "fw-bold text-dark mt-4 mb-3 d-flex align-items-center gap-2", icon("layer-group", class = "text-secondary"), "Sub-Tabs Description & Workflows"),
  
  doc_subtab_card(
    title = "1. Land Cover Maps",
    icon_name = "map",
    description = "Manages raster and vector spatial data layers representing multi-temporal land cover distributions and spatial planning zonations.",
    details_list = list(
      tagList(tags$strong("Supported Data Formats: "), "GeoTIFF rasters (.tif, .tiff) and ESRI Shapefiles (.shp, or .zip containing .shp, .shx, .dbf, .prj). Rasterization is handled automatically via high-performance spatial stars pipelines."),
      tagList(tags$strong("Grid Consistency: "), "All land cover rasters in a project must share identical spatial bounds, pixel resolution, coordinate reference system (CRS), and pixel grid alignment."),
      tagList(tags$strong("Categorical Legend: "), "Each land cover type has an integer class ID, label, color code, and description. Re-labeling or color changes update all maps reactively."),
      tagList(tags$strong("Spatial Zonation Maps: "), "Vector or raster planning boundaries (e.g., administrative districts, forest concession blocks, conservation zones, peat soil strata) can be overlaid on the land cover maps. This allows transition and emission analysis to be conducted independently per zone.")
    ),
    badge = "Input & Mapping"
  ),
  
  doc_subtab_card(
    title = "2. Land Cover Changes",
    icon_name = "code-compare",
    description = "Cross-tabulates land cover distributions between two time periods to quantify active landscape conversions and historical area dynamics.",
    details_list = list(
      tagList(tags$strong("Transition Matrix: "), "A two-way contingency matrix where rows represent initial land cover at time t1 and columns represent subsequent land cover at time t2. The diagonal cells (i = j) reflect persistence (land that did not change), while off-diagonal cells (i ≠ j) quantify active deforestation, agricultural expansion, or reforestation."),
      tagList(tags$strong("Land Cover Area Trend: "), "A continuous multi-year area visualization tracking total hectares of each land cover category over all uploaded historical map years."),
      tagList(tags$strong("Single-Map Area View: "), "Selecting a single map instead of a pair allows inspecting static area distributions across zones without transition overlay."),
      tagList(tags$strong("Zonation Filters: "), "The transition matrix can be filtered to a single zone or multi-select combined zones to inspect zone-specific drivers of land change.")
    ),
    badge = "Matrix & Trajectories"
  ),
  
  tags$h5(class = "fw-bold text-dark mt-4 mb-3 d-flex align-items-center gap-2", icon("calculator", class = "text-secondary"), "Mathematical Formulation"),
  
  doc_formula_card(
    title = "Transition Probability Matrix (Markovian Rate)",
    formula_html = tags$div(
      tags$div("P", tags$sub("i,j"), " = ", tags$span(style = "border-bottom: 1px solid black; padding-bottom: 2px;", "A", tags$sub("i,j")), " / ", tags$span("∑", tags$sub("k"), " A", tags$sub("i,k")), " = ", tags$span(style = "border-bottom: 1px solid black; padding-bottom: 2px;", "A", tags$sub("i,j")), " / ", "A", tags$sub("i"), "(t", tags$sub("1"), ")"),
      tags$div(style = "font-size: 0.9rem; margin-top: 6px; color: #555;", "subject to:  ∑", tags$sub("j"), " P", tags$sub("i,j"), " = 1.0  for all rows i")
    ),
    explanation = "The transition probability P(i,j) represents the fraction of land cover class i at time t1 that converts to land cover class j at time t2.",
    var_table = data.frame(
      Var = c("A_i,j", "A_i(t1)", "P_i,j", "r_i,j"),
      Desc = c(
        "Observed area (ha) that transitioned from class i to class j between t1 and t2",
        "Total area of class i at initial time t1 (row sum of all transitions from i)",
        "Empirical transition probability from class i to class j over the multi-year period",
        "Annualized transition rate: r_i,j = 1 - (1 - P_i,j)^(1/T), where T = t2 - t1 in years"
      ),
      stringsAsFactors = FALSE
    )
  ),
  
  doc_references_card(list(
    "Harja D, Dewi S, van Noordwijk M, Ekadinata A, Rahmanulloh A. (2011). REDD Abacus SP - User Manual and Software. World Agroforestry Centre (ICRAF), Southeast Asia Regional Office, Bogor, Indonesia. 89p.",
    "Ekadinata A, Rahmanulloh A, Pambudhi F, Ibrahim I, van Noordwijk M, Sofiyuddin M, Sardjono MA, Rahayu S, Dewi S, Budidarsono S, Said Z. (2010). Carbon Emissions from Land Use, Land Use Change and Forestry (LULUCF) in Berau District, East Kalimantan, Indonesia. ICRAF.",
    "Widayati A, Ekadinata A, Johana F, Said Z. (2010). Consequences of land use change for carbon emissions. In: Tata HL, van Noordwijk M (eds). Human livelihoods, ecosystem services and the habitat of the Sumatra orangutan. ICRAF Southeast Asia.",
    "World Agroforestry Centre. (2009). Analysis of Land Use and Cover Trajectory (ALUCT). Bogor, Indonesia."
  ))
)


### ==========================================================================
### 2. EMISSION MODULE DOCUMENTATION
### ==========================================================================

emission_info_ui <- tags$div(
  class = "container-fluid py-3 px-3",
  style = "max-height: calc(100vh - 180px); overflow-y: auto;",
  
  doc_header(
    title = "Emission Module Documentation",
    subtitle = "Carbon Stock Accounting, Stoichiometric Conversion & GHG Fluxes",
    icon_name = "industry",
    badge_text = "Carbon Accounting & Balance"
  ),
  
  tags$div(
    class = "alert alert-light border shadow-sm mb-4 p-3",
    style = "border-left: 4px solid #540b0e !important; font-size: 0.9rem;",
    tags$strong("Module Overview: "),
    "The Emission module quantifies net greenhouse gas emissions resulting from land use and cover changes. ",
    "It implements the ", tags$strong("time-averaged carbon stock difference method"), " (IPCC Tier 2/3 approach) for biomass changes, ",
    "alongside continuous annual non-biomass GHG fluxes (peat decomposition, drainage, oxidation, and non-CO₂ gases). ",
    "All emissions and removals are converted into carbon dioxide equivalent (t CO₂e) and mapped spatially across the landscape."
  ),
  
  tags$h5(class = "fw-bold text-dark mt-4 mb-3 d-flex align-items-center gap-2", icon("layer-group", class = "text-secondary"), "Sub-Tabs Description & Workflows"),
  
  doc_subtab_card(
    title = "1. Carbon stock change emissions",
    icon_name = "coins",
    description = "Calculates emissions and sequestration from living biomass changes based on the carbon density of each land cover class.",
    details_list = list(
      tagList(tags$strong("Time-Averaged Carbon Stock: "), "Each land cover type is assigned a representative carbon stock (t C ha⁻¹), reflecting the typical average biomass over its lifecycle (aboveground biomass, belowground roots, understory, and dead organic matter)."),
      tagList(tags$strong("Gross Emission vs. Sequestration: "), "When carbon stock decreases (Ci > Cj), carbon is released as gross emission (E > 0). When carbon stock increases (Ci < Cj, e.g. forest restoration, agroforestry), carbon is sequestered from the atmosphere (E < 0, carbon sink)."),
      tagList(tags$strong("Stoichiometric Conversion: "), "Elemental carbon (C) is converted to carbon dioxide (CO₂) using the stoichiometric ratio of their molecular weights: 44 / 12 ≈ 3.667."),
      tagList(tags$strong("Emission Intensity Map: "), "Renders a spatial raster map displaying carbon emission intensity (t CO₂e ha⁻¹) for each transition pixel across the landscape.")
    ),
    badge = "Biomass Stock Difference"
  ),
  
  doc_subtab_card(
    title = "2. GHG flux emissions",
    icon_name = "wind",
    description = "Accounts for continuous, recurring non-biomass emissions that occur annually throughout the period (e.g. peatland oxidation, fertilizer N₂O, and wetland CH₄).",
    details_list = list(
      tagList(tags$strong("Annual Emission Factors: "), "Defined as recurring annual flux rates (t CO₂e ha⁻¹ yr⁻¹) linked to specific land covers or transition pathways."),
      tagList(tags$strong("Peatland Drainage & Oxidation: "), "Drained peat soils undergo continuous aerobic heterotrophic decomposition, releasing large quantities of CO₂ each year independent of aboveground vegetation changes."),
      tagList(tags$strong("Temporal Multiplication: "), "Annual flux factors are multiplied by the duration of the transition period in years (T = t2 - t1) to compute cumulative flux emissions.")
    ),
    badge = "Peat & Soil Fluxes"
  ),
  
  doc_subtab_card(
    title = "3. Emissions summary",
    icon_name = "chart-simple",
    description = "Integrates carbon stock changes and GHG fluxes into combined landscape emissions and provides spatial and tabular summaries.",
    details_list = list(
      tagList(tags$strong("Total Combined Emissions: "), "Sums biomass carbon changes and all active GHG flux sources per transition and zone: E_total = E_carbon + E_flux."),
      tagList(tags$strong("Landscape Emission Intensity: "), "Combined emission intensity map showing total net emissions per hectare (t CO₂e ha⁻¹)."),
      tagList(tags$strong("Waterfall & Summary Tables: "), "Presents emissions categorized by major land transition groups, allowing rapid identification of the largest emission hotspots in the region.")
    ),
    badge = "Combined Landscape Totals"
  ),
  
  tags$h5(class = "fw-bold text-dark mt-4 mb-3 d-flex align-items-center gap-2", icon("calculator", class = "text-secondary"), "Mathematical Formulation"),
  
  doc_formula_card(
    title = "Biomass Carbon Emission & Stoichiometric Conversion",
    formula_html = tags$div(
      tags$div("E", tags$sub("C, i,j"), " = - (C", tags$sub("j"), " - C", tags$sub("i"), ") × ", tags$span(style = "font-weight: bold;", "44/12"), " × A", tags$sub("i,j"), "  (t CO", tags$sub("2"), "e)"),
      tags$div(style = "font-size: 0.9rem; margin-top: 6px; color: #555;", "where  44/12 ≈ 3.667  is the stoichiometric ratio of CO₂ to C")
    ),
    explanation = "Loss of biomass carbon (C_i > C_j) results in positive emissions (E > 0). Carbon accumulation (C_i < C_j) results in negative emissions (E < 0, removals).",
    var_table = data.frame(
      Var = c("C_i", "C_j", "44/12", "A_i,j", "E_C, i,j"),
      Desc = c(
        "Time-averaged carbon stock of initial land cover class i (t C ha⁻¹)",
        "Time-averaged carbon stock of subsequent land cover class j (t C ha⁻¹)",
        "Molecular weight ratio converting elemental carbon to carbon dioxide (3.6667)",
        "Area of transition from class i to class j (ha)",
        "Total carbon stock change emissions (t CO₂e) from transition i to j"
      ),
      stringsAsFactors = FALSE
    )
  ),
  
  doc_formula_card(
    title = "Total Combined Emissions (Biomass + Peat/GHG Fluxes)",
    formula_html = tags$div(
      tags$div("E", tags$sub("total, i,j"), " = E", tags$sub("C, i,j"), " + (F", tags$sub("i,j"), " × T × A", tags$sub("i,j"), ")  (t CO", tags$sub("2"), "e)"),
      tags$div(style = "font-size: 0.9rem; margin-top: 6px; color: #555;", "Emission Intensity (I", tags$sub("i,j"), "):   I", tags$sub("i,j"), " = E", tags$sub("total, i,j"), " / A", tags$sub("i,j"), "  (t CO", tags$sub("2"), "e ha⁻¹)")
    ),
    explanation = "F(i,j) represents the annual emission factor from organic soils/peat, and T is the duration of the observation period in years.",
    var_table = data.frame(
      Var = c("F_i,j", "T", "E_flux", "I_i,j"),
      Desc = c(
        "Annual flux emission factor (t CO₂e ha⁻¹ yr⁻¹) from peat decomposition, drainage, or agricultural management",
        "Duration of the transition period in years (T = Year_t2 - Year_t1)",
        "Total recurring flux emission: E_flux = F_i,j × T × A_i,j (t CO₂e)",
        "Emission intensity per unit area: I_i,j = (C_i - C_j) × 44/12 + F_i,j × T (t CO₂e ha⁻¹)"
      ),
      stringsAsFactors = FALSE
    )
  ),
  
  doc_references_card(list(
    "IPCC. (2006). 2006 IPCC Guidelines for National Greenhouse Gas Inventories: Volume 4 Agriculture, Forestry and Other Land Use (AFOLU). Eggleston HS, Buendia L, Miwa K, Ngara T, Tanabe K (eds). IGES, Japan.",
    "Hairiah K, Dewi S, Agus F, van Noordwijk M, Rahayu S, Velarde SJ. (2010). Measuring Carbon Stocks Across Land Use Systems: A Manual. World Agroforestry Centre (ICRAF), SE Asia Regional Office, Brawijaya University, and ICALRRD. Bogor, Indonesia.",
    "Page SE, Siegert F, Rieley JO, Boehm HV, Jayak A, Limin S. (2002). The amount of carbon released from peat and forest fires in Indonesia during 1997. Nature, 420: 61-65.",
    "Sitompul SM, Hairiah K, van Noordwijk M, Palm CA. (2001). Carbon stocks of tropical land use systems as part of the global C balance: effects of forest conversion and options for clean development activities. ASB Lecture Note 4A. ICRAF, Bogor."
  ))
)


### ==========================================================================
### 3. PROJECTION MODULE DOCUMENTATION
### ==========================================================================

projection_info_ui <- tags$div(
  class = "container-fluid py-3 px-3",
  style = "max-height: calc(100vh - 180px); overflow-y: auto;",
  
  doc_header(
    title = "Projection Module Documentation",
    subtitle = "Forward-Looking Baseline Projections (FREL/BAU) & Intervention Scenarios",
    icon_name = "arrow-trend-up",
    badge_text = "Scenario Modeling & FREL"
  ),
  
  tags$div(
    class = "alert alert-light border shadow-sm mb-4 p-3",
    style = "border-left: 4px solid #540b0e !important; font-size: 0.9rem;",
    tags$strong("Module Overview: "),
    "The Projection module simulates future land use change and emission trajectories over multiple multi-year planning iterations. ",
    "It establishes a ", tags$strong("Business-As-Usual (BAU) reference baseline"), " (Forest Reference Emission Level / FREL) ",
    "using Markov chain transition probabilities, and models policy, spatial planning, or conservation interventions ",
    "to evaluate emissions avoidance potential, offset percentages, and multi-scenario trajectories."
  ),
  
  tags$h5(class = "fw-bold text-dark mt-4 mb-3 d-flex align-items-center gap-2", icon("layer-group", class = "text-secondary"), "Sub-Tabs Description & Workflows"),
  
  doc_subtab_card(
    title = "1. Intervention scenarios",
    icon_name = "arrows-split-up-and-left",
    description = "Defines and configures alternative management scenarios by modifying transition probabilities, zonation filters, and carbon stocks.",
    details_list = list(
      tagList(tags$strong("Markov Chain Projection: "), "Future land cover areas are simulated by iteratively multiplying the current land distribution vector by the transition probability matrix (TPM): L(t+1) = L(t) × P."),
      tagList(tags$strong("Baseline Scenario (Scenario 0): "), "Represents the unmodified historical transition matrix continuing into the future without intervention (the Reference Level)."),
      tagList(tags$strong("Scenario Interventions: "), "Users can alter specific transition probabilities in the spreadsheet (e.g. reducing conversion from natural forest to monoculture plantations, promoting agroforestry, or enforcing zero-deforestation moratoria)."),
      tagList(tags$strong("Automatic Normalization: "), "Whenever off-diagonal transition probabilities are altered, row elements are automatically normalized so the sum of row probabilities remains exactly 1.0."),
      tagList(tags$strong("Scenario Settings: "), "Scenarios support custom iteration period counts, custom zonation restrictions, and custom carbon stocks or flux emission sources.")
    ),
    badge = "Policy & Spatial Interventions"
  ),
  
  doc_subtab_card(
    title = "2. Projection summary",
    icon_name = "chart-simple",
    description = "Compares cumulative emissions over time across all created scenarios and computes relative mitigation performance.",
    details_list = list(
      tagList(tags$strong("Multi-Scenario Comparison Chart: "), "Displays interactive trajectory curves for all scenarios alongside their corresponding baseline traces, with shaded deviation areas between baseline and scenario lines."),
      tagList(tags$strong("Emission Type Switcher: "), "Allows toggling cumulative projections between: (1) Net emission (sum of all carbon fluxes and removals), (2) Positive emission (gross deforestation & degradation emissions only), and (3) Negative emission (carbon sequestration/removals only)."),
      tagList(tags$strong("Deviation Summary Cards: "), "Each scenario displays its total cumulative emission offset (Δ kt CO₂e) and percentage deviation relative to baseline, alongside full scenario parameter settings (zonation, iterations, flux sources).")
    ),
    badge = "Trajectories & Mitigation Summary"
  ),
  
  tags$h5(class = "fw-bold text-dark mt-4 mb-3 d-flex align-items-center gap-2", icon("calculator", class = "text-secondary"), "Mathematical Formulation"),
  
  doc_formula_card(
    title = "Markov Chain State Transition & Cumulative Projections",
    formula_html = tags$div(
      tags$div("L(t + 1) = L(t) × P", style = "margin-bottom: 6px;"),
      tags$div("E", tags$sub("cum"), "(N) = ∑", tags$sub("k=0"), tags$sup("N"), " E(k)  (kt CO", tags$sub("2"), "e)")
    ),
    explanation = "L(t) is a 1×m row vector of land cover areas across m classes. P is the m×m Transition Probability Matrix. Cumulative emissions are summed over all projection iteration periods k = 0 to N.",
    var_table = data.frame(
      Var = c("L(t)", "P", "E(k)", "E_cum(N)"),
      Desc = c(
        "Row vector of land cover areas (ha) at iteration step t",
        "Transition Probability Matrix (Markov matrix) defining conversion probabilities between classes",
        "Emissions generated during projection period k (from iteration k-1 to k)",
        "Cumulative emissions accumulated from the reference baseline period through iteration N"
      ),
      stringsAsFactors = FALSE
    )
  ),
  
  doc_formula_card(
    title = "Projected Emission Deviation (Offset Percentage)",
    formula_html = tags$div(
      tags$div("ΔE = E", tags$sub("scenario, cum"), " - E", tags$sub("baseline, cum"), "  (kt CO", tags$sub("2"), "e)"),
      tags$div(style = "font-size: 0.95rem; margin-top: 6px;", "Offset % = (ΔE / |E", tags$sub("baseline, cum"), "|) × 100%")
    ),
    explanation = "Dividing by the absolute value |E_baseline| guarantees that the percentage sign strictly matches the direction of emission change (negative offset % = emission reduction / sink increase).",
    var_table = data.frame(
      Var = c("ΔE", "Offset %", "Negative ΔE", "Positive ΔE"),
      Desc = c(
        "Emission deviation between scenario cumulative total and baseline cumulative total",
        "Percentage emission change relative to reference baseline magnitude",
        "Desirable: scenario has lower emissions or higher carbon sequestration than baseline (green banner)",
        "Undesirable: scenario has higher emissions or lower carbon sequestration than baseline (red banner)"
      ),
      stringsAsFactors = FALSE
    )
  ),
  
  doc_references_card(list(
    "Harja D, Dewi S, van Noordwijk M, Ekadinata A, Rahmanulloh A. (2011). REDD Abacus SP - User Manual and Software. Section 5: Simulation Scenario with ABACUS. World Agroforestry Centre (ICRAF), Bogor, Indonesia.",
    "van Noordwijk, M., Dewi S., Suyanto, Minang P., White D., Robiglio V., Hoang MH., Ekadinata A, Mulia R., Harja D. (2011). Abatement cost curves relating past greenhouse gas emissions to the economic gains they allowed. Project Report. ICRAF Southeast Asia.",
    "Pagiola S, Bosquet B. (2009). Estimating the Costs of REDD+ at the Country Level. Version 2.2, Forest Carbon Partnership Facility (FCPF), World Bank, Washington DC. 22p."
  ))
)


### ==========================================================================
### 4. ECONOMIC TRADE-OFF MODULE DOCUMENTATION
### ==========================================================================

economic_info_ui <- tags$div(
  class = "container-fluid py-3 px-3",
  style = "max-height: calc(100vh - 180px); overflow-y: auto;",
  
  doc_header(
    title = "Economic Trade-Off Module Documentation",
    subtitle = "Opportunity Cost Analysis & Bottom-Up Abatement Cost Curves",
    icon_name = "dollar-sign",
    badge_text = "Opportunity Costs & Carbon Pricing"
  ),
  
  tags$div(
    class = "alert alert-light border shadow-sm mb-4 p-3",
    style = "border-left: 4px solid #540b0e !important; font-size: 0.9rem;",
    tags$strong("Module Overview: "),
    "The Economic Trade-Off module assesses the financial feasibility of emissions reduction strategies by evaluating the ",
    tags$strong("Opportunity Cost of REDD+"), ". It quantifies the economic benefits foregone by choosing not to convert ",
    "high-carbon forest landscapes into agricultural or commercial uses. By ranking all land use transitions by their cost per ",
    "unit of emission reduction ($/t CO₂e), it constructs an ", tags$strong("Abatement Cost Curve (Opportunity Cost Curve)"), " ",
    "to identify cost-effective mitigation options and calculate required compensation incentives."
  ),
  
  tags$h5(class = "fw-bold text-dark mt-4 mb-3 d-flex align-items-center gap-2", icon("layer-group", class = "text-secondary"), "Core Concepts & Workflows"),
  
  doc_subtab_card(
    title = "Net Present Value (NPV) & Profitability Analysis",
    icon_name = "hand-holding-dollar",
    description = "Measures the net discounted economic return of each land use system over its lifecycle, expressed on a per-hectare basis ($/ha).",
    details_list = list(
      tagList(tags$strong("Discounted Cash Flows: "), "Discounts future streams of revenues and establishment/operational costs back to present-day value using an annual discount rate r (typically 10-15% for private stance, 3-5% for social stance)."),
      tagList(tags$strong("Opportunity Cost of Land Conversion: "), "When land changes from class i to class j, the net economic gain is the difference in NPV plus any one-time land clearing/timber extraction benefits: ΔNPV = NPV_j - NPV_i + NPV_conversion."),
      tagList(tags$strong("Accounting Stances: "), "Private accounting reflects actual financial returns to landholders (market prices, subsidies, taxes). Social accounting adjusts for shadow prices, subsidies, and non-market environmental externalities."),
      tagList(tags$strong("Reactive Spreadsheet Editing: "), "NPV values can be adjusted directly in the editable table on the Abatement Cost Curve page to observe real-time shifts in the opportunity cost curve.")
    ),
    badge = "$ / ha Financial Return"
  ),
  
  doc_subtab_card(
    title = "Abatement Cost Curve (Opportunity Cost Curve)",
    icon_name = "chart-column",
    description = "Plots cumulative emission reductions on the horizontal axis against the opportunity cost per ton of CO₂e on the vertical axis.",
    details_list = list(
      tagList(tags$strong("Bar Width: "), "Represents the total emissions generated by that specific land transition (t CO₂e or Mt CO₂e). Wider bars represent transitions responsible for larger emissions volumes."),
      tagList(tags$strong("Bar Height: "), "Represents the opportunity cost per unit of emission avoided ($/t CO₂e): the ratio between the economic gain (ΔNPV) and the carbon emissions produced."),
      tagList(tags$strong("Sorting Order: "), "Bars are sorted from lowest cost (left) to highest cost (right). Transitions on the far left represent the cheapest opportunities to reduce emissions."),
      tagList(tags$strong("Negative Opportunity Costs ('Win-Win'): "), "Transitions below the zero line (negative cost) represent economically inefficient practices where emissions can be avoided with net positive economic gains."),
      tagList(tags$strong("Carbon Price Threshold ($/t CO₂e): "), "A user-defined carbon price (e.g. $5 or $10/t CO₂e) creates a horizontal cutoff threshold. Any transition below the threshold is economically viable to avoid via carbon incentive payments; transitions above the threshold represent land uses too lucrative to compensate with carbon finance alone.")
    ),
    badge = "$ / t CO₂e Abatement Ranking"
  ),
  
  tags$h5(class = "fw-bold text-dark mt-4 mb-3 d-flex align-items-center gap-2", icon("calculator", class = "text-secondary"), "Mathematical Formulation"),
  
  doc_formula_card(
    title = "Net Present Value (NPV) Formula",
    formula_html = tags$div(
      tags$div("NPV = ∑", tags$sub("t=0"), tags$sup("n"), " [ (B", tags$sub("t"), " - C", tags$sub("t"), ") / (1 + r)", tags$sup("t"), " ]  ($ / ha)")
    ),
    explanation = "B(t) is gross revenue in year t, C(t) is total cost in year t, r is the real discount rate, and n is the analytical horizon (typically 25-30 years).",
    var_table = data.frame(
      Var = c("B_t", "C_t", "r", "n", "NPV"),
      Desc = c(
        "Gross revenue / benefit ($ ha⁻¹) received in year t from crop harvest, timber, or products",
        "Total establishment, labor, input, and maintenance costs ($ ha⁻¹) incurred in year t",
        "Annual discount rate (e.g. 0.10 for 10% private discount rate)",
        "Analytical lifecycle horizon in years (standardized across comparable land use systems)",
        "Discounted sum of net revenues ($ ha⁻¹) over the full production lifecycle"
      ),
      stringsAsFactors = FALSE
    )
  ),
  
  doc_formula_card(
    title = "Opportunity Cost of Emission Avoidance (Abatement Cost)",
    formula_html = tags$div(
      tags$div("Opportunity Cost", tags$sub("i,j"), " = ", tags$span(style = "border-bottom: 1px solid black; padding-bottom: 2px;", "ΔNPV", tags$sub("i,j")), " / ", tags$span("I", tags$sub("i,j")), "  ($ / t CO", tags$sub("2"), "e)"),
      tags$div(style = "font-size: 0.95rem; margin-top: 6px; color: #555;", "Opportunity Cost", tags$sub("i,j"), " = ", tags$span(style = "border-bottom: 1px solid black; padding-bottom: 2px;", "(NPV", tags$sub("j"), " - NPV", tags$sub("i"), " + NPV", tags$sub("conversion"), ")"), " / ", tags$span("[(C", tags$sub("i"), " - C", tags$sub("j"), ") × 44/12 + F", tags$sub("i,j"), " × T]"))
    ),
    explanation = "Calculates the compensation required per ton of CO₂e to make land users indifferent between converting forest land or preserving it under REDD+.",
    var_table = data.frame(
      Var = c("ΔNPV_i,j", "I_i,j", "NPV_conversion", "Threshold Price (P*)"),
      Desc = c(
        "Net gain in profitability ($ ha⁻¹) from converting land cover i to land cover j",
        "Emission intensity (t CO₂e ha⁻¹) generated by transition i to j",
        "One-time net benefit ($ ha⁻¹) obtained during initial land clearing (e.g. commercial timber sales)",
        "Carbon price ($/t CO₂e) above which transitions are considered unavoidable without higher incentives"
      ),
      stringsAsFactors = FALSE
    )
  ),
  
  doc_references_card(list(
    "Monke EA, Pearson SR. (1995). The Policy Analysis Matrix for Agricultural Development. Cornell University Press, Ithaca and London. 201p.",
    "White D, Minang P (eds). (2010). Estimating the opportunity costs of REDD+: A training manual. World Bank Institute (WBI), Washington DC, USA. 212p.",
    "Swallow BM, van Noordwijk M, Dewi S, Murdiyarso D, White D, Gockowski J, Hyman G, Budidarsono S, Robiglio V, Meadu V, Ekadinata A, Agus F, Hairiah K, Mbile P, Sonwa DJ, Weise S. (2007). Opportunities for Avoided Deforestation with Sustainable Benefits: An interim report of the ASB Partnership for the Tropical Forest Margins. ASB Working Paper 42. Nairobi, Kenya.",
    "van Noordwijk M, Dewi S, Suyanto, Minang P, White D, Robiglio V, Hoang MH, Ekadinata A, Mulia R, Harja D. (2011). Abatement cost curves relating past greenhouse gas emissions to the economic gains they allowed. Project Report. ICRAF Southeast Asia, Bogor, Indonesia.",
    "Stern N. (2007). The Economics of Climate Change: The Stern Review. Cambridge University Press, Cambridge, UK. 692p."
  ))
)


### ==========================================================================
### REDD-Abacus 2 — Global Software "About" UI
### ==========================================================================

about_ui <- tags$div(
  class = "container py-4",
  style = "max-width: 1140px; margin: 0 auto;",
  
  # Hero Header
  tags$div(
    class = "p-4 mb-4 bg-white border rounded shadow-sm d-flex flex-wrap align-items-center justify-content-between gap-3",
    tags$div(
      class = "d-flex align-items-center gap-3",
      tags$img(
        src = "images/abacus_logo.svg",
        height = 64,
        alt = "REDD Abacus Logo"
      ),
      tags$div(
        tags$h2(
          class = "fw-bold mb-1",
          style = "color: #540b0e;",
          "REDD Abacus 2"
        ),
        tags$div(
          class = "text-muted",
          style = "font-size: 1.05rem;",
          "Reducing Emissions from Deforestation and Degradation — Abatement Cost and Scenario Planner"
        )
      )
    ),
    tags$div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      tags$span(class = "badge bg-danger-subtle text-danger-emphasis border border-danger-subtle px-3 py-2", "v2.0 Web Edition"),
      tags$a(
        href = "https://github.com/degi/redd-abacus",
        target = "_blank",
        class = "btn btn-outline-dark btn-sm d-inline-flex align-items-center gap-1",
        icon("github"), " GitHub Repository"
      )
    )
  ),
  
  # Section: Overview & 4 Pillars
  tags$div(
    class = "card shadow-sm border mb-4",
    style = "border-radius: 8px; overflow: hidden;",
    tags$div(
      class = "card-header bg-light d-flex align-items-center gap-2 py-2 px-3 fw-semibold",
      style = "font-size: 1rem; color: #540b0e;",
      icon("circle-info", class = "text-primary opacity-75"),
      "About the Software"
    ),
    tags$div(
      class = "card-body p-4",
      style = "font-size: 0.95rem; line-height: 1.6;",
      tags$p(
        tags$strong("REDD Abacus 2"),
        " is an open-source, interactive decision-support application designed for landscape-level greenhouse gas (GHG) accounting, land use dynamic modeling, and economic opportunity cost analysis. It provides subnational planners, policy analysts, conservation practitioners, and environmental economists with empirical tools to simulate emissions reduction interventions and evaluate the cost-effectiveness of REDD+ strategies via ",
        tags$strong("Abatement Cost Curves ($ / t CO₂e)"),
        "."
      ),
      tags$div(
        class = "row g-3 mt-1",
        tags$div(
          class = "col-md-6 col-lg-3",
          tags$div(
            class = "p-3 border rounded bg-light h-100 d-flex flex-column",
            tags$div(class = "fw-bold text-primary mb-1 d-flex align-items-center gap-2", icon("earth-asia"), "1. Land Cover"),
            tags$div(class = "small text-muted flex-grow-1", "Raster/vector map ingestion, legend cross-walking, multi-temporal changes, and Markov transition matrix analysis.")
          )
        ),
        tags$div(
          class = "col-md-6 col-lg-3",
          tags$div(
            class = "p-3 border rounded bg-light h-100 d-flex flex-column",
            tags$div(class = "fw-bold text-success mb-1 d-flex align-items-center gap-2", icon("industry"), "2. Emissions"),
            tags$div(class = "small text-muted flex-grow-1", "Time-averaged carbon accounting, stoichiometric 44/12 CO₂ conversion, peat oxidation, and recurring soil GHG fluxes.")
          )
        ),
        tags$div(
          class = "col-md-6 col-lg-3",
          tags$div(
            class = "p-3 border rounded bg-light h-100 d-flex flex-column",
            tags$div(class = "fw-bold text-danger mb-1 d-flex align-items-center gap-2", icon("arrow-trend-up"), "3. Projections"),
            tags$div(class = "small text-muted flex-grow-1", "Dynamic forward simulation of policy intervention scenarios vs. baseline Business-As-Usual (BAU) with trajectory deviations.")
          )
        ),
        tags$div(
          class = "col-md-6 col-lg-3",
          tags$div(
            class = "p-3 border rounded bg-light h-100 d-flex flex-column",
            tags$div(class = "fw-bold text-warning-emphasis mb-1 d-flex align-items-center gap-2", icon("dollar-sign"), "4. Economics"),
            tags$div(class = "small text-muted flex-grow-1", "Discounted cash flow (NPV) profitability, opportunity costs per unit of avoidable emission, and empirical Abatement Cost Curves.")
          )
        )
      )
    )
  ),
  
  # Two Columns: Author & Citation
  tags$div(
    class = "row g-4 mb-4",
    
    # Left: Author Information & Acknowledgments
    tags$div(
      class = "col-lg-6",
      tags$div(
        class = "card shadow-sm border h-100",
        style = "border-radius: 8px; overflow: hidden;",
        tags$div(
          class = "card-header bg-light d-flex align-items-center gap-2 py-2 px-3 fw-semibold",
          style = "font-size: 1rem; color: #540b0e;",
          icon("user-gear", class = "text-primary opacity-75"),
          "Author & Development"
        ),
        tags$div(
          class = "card-body p-4",
          style = "font-size: 0.92rem; line-height: 1.6;",
          tags$div(
            class = "d-flex align-items-start gap-3 mb-3",
            tags$div(
              class = "bg-primary-subtle text-primary rounded-circle d-flex align-items-center justify-content-center flex-shrink-0",
              style = "width: 48px; height: 48px; font-size: 1.4rem;",
              icon("user")
            ),
            tags$div(
              tags$h5(class = "fw-bold mb-0", "Degi Harja Asmara"),
              tags$div(class = "text-muted small", "Software Architect, Modeler & Lead Developer"),
              tags$div(
                class = "mt-1",
                tags$a(
                  href = "mailto:degiharja@gmail.com",
                  class = "text-decoration-none me-3 small",
                  icon("envelope"), " degiharja@gmail.com"
                ),
                tags$a(
                  href = "https://github.com/degi",
                  target = "_blank",
                  class = "text-decoration-none small",
                  icon("github"), " github.com/degi"
                )
              )
            )
          ),
          tags$hr(),
          tags$h6(class = "fw-bold mb-2", style = "color: #540b0e;", "Origins & Historical Acknowledgments:"),
          tags$p(
            class = "small text-muted mb-0",
            "REDD Abacus 2 is the modern R/Shiny successor to ",
            tags$strong("REDD Abacus SP"),
            ", originally developed at the ",
            tags$strong("World Agroforestry Centre (ICRAF) Southeast Asia Regional Program"),
            " in collaboration with Sonya Dewi, Meine van Noordwijk, Andree Ekadinata, and Arief Rahmanulloh. Supported historically through the ALLREDDI (European Commission) and REALU (BMU / Norad) initiatives."
          )
        )
      )
    ),
    
    # Right: Suggested Citation & Repository
    tags$div(
      class = "col-lg-6",
      tags$div(
        class = "card shadow-sm border h-100",
        style = "border-radius: 8px; overflow: hidden;",
        tags$div(
          class = "card-header bg-light d-flex align-items-center justify-content-between py-2 px-3 fw-semibold",
          style = "font-size: 1rem; color: #540b0e;",
          tags$div(
            class = "d-flex align-items-center gap-2",
            icon("quote-left", class = "text-primary opacity-75"),
            "Suggested Citation"
          ),
          tags$span(class = "badge bg-light text-dark border", "APA Style")
        ),
        tags$div(
          class = "card-body p-4",
          style = "font-size: 0.92rem; line-height: 1.6;",
          tags$p(
            class = "mb-2",
            "To cite REDD Abacus 2 in publications, reports, or modeling studies, please use:"
          ),
          tags$div(
            class = "p-3 bg-light border rounded mb-3 font-monospace user-select-all",
            style = "font-size: 0.88rem; line-height: 1.5; color: #1e293b; background-color: #f8fafc;",
            "Asmara, D. H. REDD Abacus 2: A Tool for Modeling Land Use Opportunity Costs and Emissions Reduction Scenarios. Software available from https://github.com/degi/redd-abacus"
          ),
          tags$p(
            class = "small text-muted mb-2",
            tags$strong("Repository: "),
            tags$a(
              href = "https://github.com/degi/redd-abacus",
              target = "_blank",
              "https://github.com/degi/redd-abacus"
            )
          ),
          tags$p(
            class = "small text-muted mb-0",
            tags$strong("License: "),
            "Open Source & Public Domain for non-commercial research, spatial planning, and education. Proper attribution is appreciated in all derived works."
          )
        )
      )
    )
  )
)
