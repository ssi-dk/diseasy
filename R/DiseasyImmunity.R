#' @title Diseasy' immunity handler
#'
#' @description
#'   The `DiseasyImmunity` module is responsible for implementing various models for the immunity dependency of the
#'   diseases. Meaning the different scenarios there can be of waning of immunity.
#'   The module implements a number immunity models with different functional forms.
#'
#'   See the vignette("diseasy-immunity") for examples of use.
#'  @return
#'   A new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
#' @export
DiseasyImmunity <- R6::R6Class(                                                                                           # nolint: object_name_linter
  classname = "DiseasyImmunity",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(...) {


      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Set no waning as the default
      self$use_no_waning()

    },

    #' @description
    #'   Sets the time scale for the waning of the model.
    #' @param time_scale (`numeric`)\cr
    #'    The time_scale determines the rate of decay in the model.
    set_time_scale = function(time_scale) {
      checkmate::assert_number(time_scale)
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to use the specified waning model.
    #' @param model_name (`character`)\cr
    #'   Name of the waning_model to use (calls the equivalent $use_<model_name>()).
    #' @param dots (`list`)\cr
    #'   Named list of arguments that will be passed at dot-ellipsis to the waning model.
    use_waning_model = function(model_name, dots = NULL) {

      checkmate::assert_choice(model_name, self$available_waning_models)

      # First parse the dot arguments
      dots_to_string <- ifelse(
        is.null(dots), "", glue::glue_collapse(purrr::map2(dots, names(dots), ~ glue::glue("{.y} = {.x}")), sep = ", ")
      )

      # Then reset the model
      eval(parse(text = glue::glue("self$use_{model_name}({dots_to_string})")))

    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to use an exponential model for waning.
    #' @param time_scale (`numeric`)\cr
    use_exponential_waning = function(time_scale = 20) {

      # Set the model
      private$.model <- \(t) exp(-t / (time_scale * log(2)))

      # Logging
      private$lg$info("Using exponential waning model")

    }

  ),

  # Make active bindings to the private variables
  active  = list(
    #' @field available_waning_models (`character`)\cr
    #'   The list of available waning models
    available_waning_models = purrr::partial(
      .f = active_binding,
      name = "available_waning_models",
      expr = {
        models <- purrr::keep(ls(self), ~ startsWith(., "use_")) |>
          purrr::map_chr(~ stringr::str_extract(., r"{(?<=use_).*}")) |>
          purrr::discard(~ . == "waning_model") # Filter out the generic setter
        return(models)
      }
    )
  )
)



#' @param K (`integer(1)`)\cr
    #'   The number of sequential compartments.
    #' @param functions (`list`)\cr
    #'   A list of target functions to optimisze for.
    #' @param tmax ((`integer(1)`)\cr
    #'   The maximum number of time steps.
# Et DiseasyImmunity modul skal som udgangspunkt bare have ingen waning (konstant beskyttelse)


# Familie af use_*_funktioner ligesom i DiseasySeason (enkel parameter - time_scale - der hvor den er halv (tau ændres))
# Dette kunne så gemme funktionen i et privat field (fx $.model)
# Eg. private$.model <- \(t) exp(-t / (tau * log(2)))

# Syntax eksempel - simpel case
# im <- DiseasyImmunity$new()
# im$use_exponential_waning()
# im$set_timescale(10)

# Alternativt:
# im$use_exponential_waning(scale = 10)

# Begge måder skal gerne virke (se DiseasySeason for eksempel)


# Det vil være en god ide at stjæle "$use_season_model("model")" fra DiseasySeason og lave en tilsvarende 
# "$use_waning_model("model")" her. Så kan vi meget nemmere interagere med modulet programatisk.




# Så skal vi have en måde at approximere de valgte modeller givet en ODE kasse model med N R-kasser.
# Fx. public$approximate_compartmental(N, t_max, method = "...")
# Så er spørgsmålet, skal dette gemme raterne i modulet?
# Altså, skal der være en private$.approximated_rates ?
# Eller skal funktionen bare returnere det fittede rater?
# En fordel ved at gemme dem, er at vi kan inkludere dem i et plot metode ($plot()) så det er nemmere at 
# inspicere modulets konfiguration.
# Nå raterne gemmes som en liste, skal vi lige blive enige om formatet.
# Rasmus foreslår at bruge en navngiven liste af vektorer.
# Fx. 
# list(rates = c(d1, d2, d3, .. dn-1), infection_risk = c(g1, g2, g3, ..., gn))
# Altså, at vi altid har "rates" først, og derefter en navngivet liste med de resterende fits.
# Navnenne på det resterende kan så matche de navne der gives med "$use_costom_waning()". 
# Vores $use_* funktioner har i denne analogi så navnet "infection_risk" .
# Altså, hvis man først kalder $use_costum_waning(list(infection_risk = \(t) exp(-t / 20), hospitalisation_risk = \(t) exp(-t/30)*0.8 + 0.2))))
# så får man outputtet
# list(rates = c(d1, d2, d3, .. dn-1), infection_risk = c(g1, g2, g3, ..., gn), hospitalisation_risk = c(g1, g2, g3, ..., gn))




# approximate_compartmental kommer nok til at blive en kompliceret funktion med flere valgfrie argumenter
# Til at starte med, synes jeg vi skal implementere en simpel udgave og så itererer derfra.
# Vi starter med at "t_max" skal sættes (senere kan vi lave en implementation med t_max = NULL som prøver heuristisk at læse problemet)
# Senere kan vi gøre så f(inf) også bliver givet til modellen (lige nu kan vi overveje bare at bruge f(t_max) some endepunkter)
# men til at starte med sætter vi bare gamma_N = f(t_max).

# Bemærk, at vi nok skal have en særlig logik når en konstant beskyttelse er sat, da modellen så ikke kan fitte problemet
# (Det burde være degenereret)




# Det skal også være muligt at give sine egne funktioner til modulet
# Fx. kan vi lave en public$use_custom_waning(...)
# Her kan det blive lidt komplekst.

# I det simple tilfælde, skal den kunne tage en enkelt "target funktion" f(t) og en tilsvarende timescale for funktionen.
# (Hvis vi kræver at timescale bliver givet med, så kan vi køre heuristiske elementer af programmet bedre tror jer)

# Men vi vil gerne have mulighed for at kunne tage en liste af target funktioner og gemme dem i modulet.
# Dette kommer til at have betydning for hvordan public$approximate_compartmental(..) skal fungere, da den så
# skal kunne simultant fitte alle target funkioner når en liste er gemt i modulet.

# Det vil sige, at der skal være noget logik i starten af funktionen ($approximate_compartmental) som tjekker om
# $model() er en liste af funktioner eller bare en funktion.
# Hvis det er en liste skal den så kunne håndtere dem også.
# Alternativt, skal der laves en fancy implementering (fx med purrr) som er ligeglad om det en liste af 1 element eller en liste af flere.

