launchEvidenceExplorer <- function(dataFolder, launch.browser = TRUE) {
  appDir <- system.file("shiny", "FeasExplore", package = "EmaCovidFeasibility")
  .GlobalEnv$shinySettings <- list(dataFolder = dataFolder)
  on.exit(rm(shinySettings, envir=.GlobalEnv))
  shiny::runApp(appDir) 
}

prepareShinyData <- function(outputFolders, dsNames, shinyDataFolder){
  
  if(!exists(file.path(shinyDataFolder))){dir.create(shinyDataFolder)}
  
  colours <- c('#ccebc5', '#8dd3c7','#80b1d3','#fb8072', '#fccde5', '#fdb462', '#b3de69', '#bc80bd')
  
  if(length(dsNames)!=length(outputFolders)){dsNames = paste0("DataSource",1:length(outputFolders))}
  
  colours <- setNames(colours[1:length(outputFolders)],c(dsNames))
  
  data_sources <- names(colours)
  
  x <- as.list(outputFolders)
  
  names(x) <- dsNames
  
  outputFolders
  
  t<-map2_df(x, names(x), 
             ~read_csv(file.path(.x,"binaryCovs.csv")) %>%
               left_join(read_csv(file.path(.x,"covariateRef.csv"))) %>%
               left_join(read_csv(file.path(.x,"CohortCounts.csv")) %>% select(cohortDefinitionId, total_count=count)) %>%
               left_join(get_readable_names()) %>%
               mutate(data_source = .y)
  )
  
  t[,c("cohortDefinitionId","name","covariateId",
       "conceptId","covariateName","startDayTemporalCharacterization",
       "data_source","mean","total_count" )] %>%
    write_rds(file.path(shinyDataFolder, "merged_cov_counts.rds"))
  
  
  t2<-map2_df(x, names(x), ~read_csv(file.path(.x,"continuousCovs.csv")) %>%
                left_join(read_csv(file.path(.x,"covariateRef.csv"))) %>%
                left_join(read_csv(file.path(.x,"CohortCounts.csv")) %>% select(cohortDefinitionId, total_count=count)) %>%
                left_join(get_readable_names()) %>%
                mutate(data_source = .y)
  )
  
  t2 %>%
    write_rds(file.path(shinyDataFolder, "merged_cov_cont.rds"))
  
  prepare_covariate_data(
    bind_rows(
      t[,c("cohortDefinitionId","name","covariateId",
           "conceptId","covariateName","startDayTemporalCharacterization",
           "data_source","mean","total_count" )], 
      t2[,c("cohortDefinitionId","name","covariateId",
            "conceptId","covariateName","startDayTemporalCharacterization",
            "data_source","mean","total_count")])) %>%
    write_rds(file.path(shinyDataFolder, "prepared_cov_counts.rds"))
  
  covariate_data <- read_rds(file.path(shinyDataFolder,"merged_cov_counts.rds"))
  covariate_data_cont <- read_rds(file.path(shinyDataFolder,"merged_cov_cont.rds"))
  prep_cov_data <- read_rds(file.path(shinyDataFolder,"prepared_cov_counts.rds"))
  
  map(x, function(x1){
    map(setNames(c("COVID-19", "Influenza"), c("COVID-19", "Influenza")),
        ~draw_attrition(x1, .x))
    
  }) %>%
    write_rds(file.path(shinyDataFolder,"all_attrition.rds"))
  map(setNames(c(2887,2899), c(2887,2899)), 
      ~draw_mortality(.x, data_sources = unlist(x, use.names=F))) %>%
    write_rds(file.path(shinyDataFolder,"all_mortality_dist.rds"))
  
  summary_table(data_sources = unlist(x, use.names=F)) %>%
    write_rds(file.path(shinyDataFolder,"summary_table.rds"))
  
  map(setNames(c(2887,2885,2899,2935), c(2887,2885,2899,2935)), 
      ~draw_table_dist(names(x), .x, covariate_data, cov_string = "index year and month: ", show_total = T)) %>%
    write_rds(file.path(shinyDataFolder,"all_month_dist.rds"))
  map(setNames(c(2887,2885,2899,2935), c(2887,2885,2899,2935)),
      ~plot_age_dist(names(x), .x, covariate_data, plotColours = colours)) %>%
    write_rds(file.path(shinyDataFolder,"all_age_plot.rds"))
  map(setNames(c(2887,2885,2899,2935), c(2887,2885,2899,2935)),
      ~draw_table_dist(names(x), .x, covariate_data, cov_string = "gender = ", show_total = F)) %>%
    write_rds(file.path(shinyDataFolder,"all_gender_dist.rds"))
  
  map(setNames(c(2887,2885,2899,2935), c(2887,2885,2899,2935)),
      ~draw_observation_table(names(x), .x, covariate_data_cont, "after index")) %>%
    write_rds(file.path(shinyDataFolder,"obs_post.rds"))
  map(setNames(c(2887,2885,2899,2935), c(2887,2885,2899,2935)),
      ~draw_observation_table(names(x), .x, covariate_data_cont, "prior to index")) %>%
    write_rds(file.path(shinyDataFolder,"obs_pre.rds"))
  
}

get_all_counts <- function(data_sources) {
  purrr::map_df(data_sources,
                ~ readr::read_csv(file.path(.x, "CohortCounts.csv")) %>%
                  mutate(data_source = .x))
  
}

get_readable_names <- function(input_condition = "Any") {
  if (input_condition == "Influenza") {
    names_df <-
      tidyr::tibble(
        cohortDefinitionId = c(3045, 3046, 2899, 2898, 2933, 2884, 2922, 2935),
        condition = "Influenza",
        name = c(
          "Any test for influenza",
          "Positive test for influenza",
          "Influenza diagnosis or positive test",
          "Hospitalized",
          "Receiving any oxygen supplementation",
          "Receiving intensive services",
          "Receiving ECMO",
          "Deceased"
        )
      )
    
  }
  
  if (input_condition == "COVID-19") {
    names_df <-
      tidyr::tibble(
        cohortDefinitionId = c(2897, 2895, 2887, 2885, 2932, 2880, 2920, 2934),
        condition = "COVID",
        name = c(
          "Any test for SARS-CoV-2",
          "Positive test for SARS-CoV-2",
          "COVID-19 diagnosis or positive test",
          "Hospitalized",
          "Receiving any oxygen supplementation",
          "Receiving intensive services",
          "Receiving ECMO",
          "Deceased"
        )
      )
    
  }
  
  if (input_condition == "Any") {
    names_df <-
      tidyr::tibble(
        cohortDefinitionId = c(
          3045, 3046, 2899, 2898, 2933, 2884, 2922, 2935,
          2897, 2895,2887,2885,2932,2880,2920,2934
        ),
        name = c(
          "Any test for influenza",
          "Positive test for influenza",
          "Influenza diagnosis or positive test",
          "Hospitalized with influenza",
          "Receiving any oxygen supplementation with influenza",
          "Receiving intensive services with influenza",
          "Receiving ECMO with influenza",
          "Deceased with prior influenza",
          "Any test for SARS-CoV-2",
          "Positive test for SARS-CoV-2",
          "COVID-19 diagnosis or positive test",
          "Hospitalized with COVID-19",
          "Receiving any oxygen supplementation with COVID-19",
          "Receiving intensive services with COVID-19",
          "Receiving ECMO with COVID-19",
          "Deceased with prior COVID-19"
        )
      )
    
  }
  
  return(names_df)
  
  
}

draw_attrition <- function(ds, condition  = NULL) {
  counts <- readr::read_csv(file.path(ds, "CohortCounts.csv"))
  
  joining_cols <- get_readable_names(condition)
  
  flow_counts <<- joining_cols %>%
    dplyr::left_join(counts %>% select(cohortDefinitionId, count)) %>%
    mutate(count = dplyr::coalesce(count, 0))
  
  
  DiagrammeR::grViz(
    "
    digraph boxes_and_circles {
    
    # a 'graph' statement
    graph [compound = true, nodesep = .5, ranksep = .25,
    color = crimson, fontsize = 10]
    
    # several 'node' statements
    node [shape = box,
    fontname = Helvetica]
    
    edge [arrowhead = none, arrowtail = none]
    
    A [label = '@@1'];
    B [label = '@@2'];
    C [label = '@@3'];
    D [label = '@@4', style = filled, fillcolor = PowderBlue];
    E [label = '@@5'];
    F [label = '@@6'];
    G [label = '@@7'];
    H [label = '@@8'];
    I [label = '@@9']
    
    # several 'edge' statements
    A->B->D
    C->D
    subgraph cluster1 {
    D->E->F->G->H
    }
    H->I [ltail = cluster1]
    }
    
    [1]: paste0(flow_counts$name[1],\"\\nn = \",dplyr::case_when(flow_counts$count[1] < 5 & flow_counts$count[1] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[1])))
    [2]: paste0(flow_counts$name[2],\"\\nn = \",dplyr::case_when(flow_counts$count[2] < 5 & flow_counts$count[2] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[2])))
    [3]: paste0(flow_counts$condition[1],\" diagnosis without\\npositive test\\nn = \",dplyr::case_when(flow_counts$count[3]-flow_counts$count[2] < 5 & flow_counts$count[3]-flow_counts$count[2] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[3]-flow_counts$count[2])))
    [4]: paste0(flow_counts$name[3],\"\\nn = \",dplyr::case_when(flow_counts$count[3] < 5 & flow_counts$count[3] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[3])))
    [5]: paste0(flow_counts$name[4],\"\\nn = \",dplyr::case_when(flow_counts$count[4] < 5 & flow_counts$count[4] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[4])))
    [6]: paste0(flow_counts$name[5],\"\\nn = \",dplyr::case_when(flow_counts$count[5] < 5 & flow_counts$count[5] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[5])))
    [7]: paste0(flow_counts$name[6],\"\\nn = \",dplyr::case_when(flow_counts$count[6] < 5 & flow_counts$count[6] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[6])))
    [8]: paste0(flow_counts$name[7],\"\\nn = \",dplyr::case_when(flow_counts$count[7] < 5 & flow_counts$count[7] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[7])))
    [8]: paste0(flow_counts$name[8],\"\\nn = \",dplyr::case_when(flow_counts$count[8] < 5 & flow_counts$count[8] > 0 ~ \"<5\", TRUE ~ scales::comma(flow_counts$count[8])))
    
    ")
  
  
}

draw_table_dist <-
  function(data_sources,
           cohort_id,
           covariate_data, 
           cov_string = "index year and month: ",
           show_total = F, show_cohort = T) {
    
    covs <- covariate_data  %>%
      filter(total_count >= 100) %>%
      filter(data_source %in% data_sources) %>%
      filter(cohortDefinitionId == cohort_id) %>%
      filter(grepl(cov_string, covariateName)) %>%
      mutate(formatted_covariateName = case_when(
        grepl("index year and month", covariateName) ~
          format(as.Date(paste0(
            str_replace(covariateName, ".*: ", ""), "01"
          ), format = "%Y%m%d"), "%b %Y"),
        TRUE ~ str_remove(covariateName, cov_string)
      ))
    
    unique_covs <- covs %>%
      arrange(covariateName) %>%
      .$formatted_covariateName %>%
      unique
    
    if (length(unique_covs) == 0) {
      return(NULL)
    }
    
    tab <- covs %>%
      select(-covariateName,
             -covariateId,
             -startDayTemporalCharacterization,
             -conceptId) %>%
      mutate(count = mean*total_count,
             count = scales::comma(count, accuracy = 1),
             count = str_replace(count, "-", "<="),
             mean = scales::percent(mean, accuracy = 0.1),
             mean = str_replace(mean, "-", "<="),
             mean = str_c(mean, " (",count,")")) %>%
      select(-count) %>%
      spread(formatted_covariateName, mean, fill = "-") %>%
      mutate(total_count = coalesce(scales::comma(total_count, accuracy = 1), "-")) %>%
      select(
        Database = data_source,
        Cohort = name,
        Total = total_count,
        matches(unique_covs)
      ) %>%
      arrange(Database) %>%
      mutate(odd_rn = floor(row_number() / 2) == row_number() / 2) %>%
      gt::gt() %>%
      tab_style(
        locations = list(
          cells_body(columns = vars(Cohort, Database)),
          cells_column_labels(columns = vars(Cohort, Database))
        ),
        style = cell_text(align = "left")
      ) %>%
      tab_style(
        locations = list(
          cells_body(columns = matches(c(
            unique_covs, "Total"
          ))),
          cells_column_labels(columns = matches(c(
            unique_covs, "Total"
          )))
        ),
        style = cell_text(align = "center")
      ) %>%
      tab_style(locations = list(cells_column_labels(columns = matches(
        c("Cohort", unique_covs, "Total", "Database")
      ))),
      style = cell_text(weight = "bold")) %>%
      cols_width(vars(Cohort) ~ px(390),
                 vars(Database) ~ px(125),
                 everything() ~ px(68)) %>%
      tab_options(table.align = "left") #%>%
    #tab_style(style = list(cell_fill(color = "#f0f0f0")),
    #         locations = cells_body(rows = odd_rn == FALSE))
    
    
    if (show_total) {
      tab <- cols_hide(tab, columns = vars(odd_rn))
    } else{
      tab <- cols_hide(tab, columns = vars(odd_rn, Total))
    }
    
    if (!show_cohort) {
      tab <- cols_hide(tab, columns = vars(Cohort))
    }
    
    return(tab)
    
  }

plot_age_dist <- function(data_sources, cohort_id, covariate_data, plotColours) {
  covariate_data %>%
    filter(cohortDefinitionId == cohort_id,
           data_source %in% data_sources) %>%
    filter(total_count >= 100) %>%
    dplyr::filter(grepl("age group", tolower(covariateName))) %>%
    dplyr::mutate(age_group = stringr::str_replace(covariateName, ".*: ", "")) %>%
    dplyr::mutate(lower_age = purrr::map_chr(age_group, ~ stringr::str_trim(stringr::str_split(.x, "-")[[1]][1]))) %>%
    mutate(
      lower_age = dplyr::case_when(
        as.integer(lower_age) >= 95 ~ as.integer(95),
        TRUE ~ as.integer(lower_age)
      ),
      percent = dplyr::case_when(mean < 0 ~ 0, TRUE ~ mean),
      upper_age = lower_age + 4
    ) %>%
    mutate(data_source = factor(data_source, levels = names(plotColours))) %>%
    group_by(cohortDefinitionId, data_source, name, lower_age, upper_age) %>%
    summarise(percent = sum(percent)) %>%
    mutate(
      age_group = stringr::str_c(lower_age, " - ", upper_age),
      age_group = dplyr::case_when(lower_age >= 95 ~ "95+", TRUE ~ age_group),
      age_group = factor(age_group, levels = c(stringr::str_c(
        seq(0, 90, 5), " - ", seq(4, 94, 5)
      ), "95+"))
    ) %>%
    ungroup %>%
    ggplot(aes(x = age_group, y = percent, fill = data_source)) +
    geom_col(position = "dodge") +
    scale_y_continuous(
      labels = function(x)
        scales::percent(x, accuracy = 1)
    ) +
    scale_fill_manual(values = plotColours) +
    facet_wrap(
      vars(data_source),
      #scales = "fixed",
      ncol = 4,
      drop = FALSE
    ) +
    theme_bw(base_size = 14) +
    labs(y = "Percent cohort",
         x = "Age group") +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none")
  
}

plot_observation_boxplot <-
  function(data_sources,
           cohort_id,
           covariate_data_cont,
           time_period) {
    plots <- covariate_data_cont %>%
      filter(cohortDefinitionId == cohort_id) %>%
      filter(total_count >= 100) %>%
      filter(grepl(pattern = "observation", tolower(covariateName))) %>%
      mutate(covariateName = map_chr(covariateName, ~ str_split(.x, ".*\\) ")[[1]][2])) %>%
      select(
        period = covariateName,
        data_source,
        minValue,
        p10Value,
        p25Value,
        medianValue,
        p75Value,
        p90Value,
        maxValue
      ) %>%
      split(.$period) %>%
      map(
        ~ .x %>%
          mutate(data_source = factor(data_source, levels = names(colours))) %>%
          mutate(
            upper = pmin(p75Value + 1.5 * (p75Value - p25Value), maxValue),
            lower = pmax(p25Value - 1.5 * (p75Value - p25Value), minValue)
          ) %>%
          ggplot(
            aes(
              x = data_source,
              ymin = lower,
              lower = p25Value,
              middle = medianValue,
              upper = p75Value,
              ymax = upper,
              fill = data_source
            )
          ) +
          scale_y_continuous(
            labels = function(x)
              scales::comma(x, accuracy = 1)
          ) +
          scale_x_discrete(labels = NULL, breaks = NULL) +
          geom_boxplot(stat = "identity") +
          scale_fill_manual(values = colours) +
          facet_wrap(
            vars(data_source),
            scales = "free",
            ncol = 7,
            drop = F
          ) +
          theme_bw(base_size = 14) +
          labs(y = "Days observation",
               x = "") +
          theme(legend.position = "none")
      )
    
    plots[[time_period]]
    
  }

draw_observation_table <- function(data_sources, cohort_id, covariate_data_cont, time_period){
  
  plots <- covariate_data_cont %>%
    filter(cohortDefinitionId == cohort_id) %>%
    filter(total_count >= 100) %>%
    filter(grepl(pattern = "observation", tolower(covariateName))) %>%
    mutate(covariateName = map_chr(covariateName, ~ str_split(.x, ".*\\) ")[[1]][2])) %>%
    mutate_at(vars(matches(c("value"))), function(x) scales::comma(x, accuracy = 1)) %>%
    select(
      period = covariateName,
      data_source,
      minValue,
      p10Value,
      p25Value,
      medianValue,
      p75Value,
      p90Value,
      maxValue
    ) %>%
    split(.$period) %>%
    map(~.x %>%
          rename(Database = data_source, `Observation period` = period) %>%
          mutate(odd_rn = floor(row_number()/2) == row_number()/2) %>%
          gt::gt() %>%
          tab_style(locations = list(cells_body(columns = vars(`Observation period`, Database)),
                                     cells_column_labels(columns = vars(`Observation period`, Database))),
                    style = cell_text(align = "left")) %>%
          tab_style(locations = list(cells_body(
            columns = matches(c("Value"))
          ),cells_column_labels(
            columns = matches(c("Value"))
          )),
          style = cell_text(align = "center")) %>%
          tab_style(locations = list(cells_column_labels(
            columns = matches(c("Observation","Database","Value"))
          )),
          style = cell_text(weight = "bold")) %>%
          cols_width(vars(`Observation period`) ~ px(200),
                     vars(`Database`) ~ px(125),
                     everything() ~ px(110)) %>%
          tab_options(
            table.align = "left"
          ) %>%
          #tab_style(
          #   style = list(
          #     cell_fill(color = "#f0f0f0")    
          #   ),
          #   locations = cells_body(
          #     rows = odd_rn == FALSE
          #   ) ) %>%
          cols_hide(columns = vars(odd_rn, `Observation period`)) %>%
          cols_label(
            minValue = "Min",
            maxValue = "Max",
            p10Value = "p10",
            p90Value = "p90",
            p25Value = "p25",
            p75Value = "p75",
            medianValue = "Median"
          ))
  
  plots[[time_period]]
  
}


prepare_covariate_data <-
  function(covariate_data
  ) {
    
    tp <- tibble(startDayTemporalCharacterization = c(-730,0),
                 time_period =  c("Pre-index (730d)",
                                  "Post-index (30d)"))
    
    domains <- tibble(domain = c("condition_era group",
                                 "drug_era group",
                                 "measurement",
                                 "measurement value",
                                 "observation",
                                 "procedure_occurrence"),
                      domain2 = c("condition",
                                  "drug",
                                  "measurement",
                                  "meas value",
                                  "observation",
                                  "procedure"))
    
    
    covariate_data %>%
      filter(total_count >= 100) %>%
      #filter(cohortDefinitionId == cohort_id) %>%
      left_join(tp) %>%
      #filter(startDayTemporalCharacterization == follow_up) %>%
      mutate(domain = map_chr(covariateName, ~ str_split(.x, ": ")[[1]][1])) %>%
      left_join(domains) %>%
      mutate(domain = domain2) %>%
      #filter(domain %in% domain_input) %>%
      group_by(covariateId, covariateName) %>%
      mutate(sortcol = max(pmax(0, mean))) %>%
      ungroup %>%
      mutate(count_n = case_when(mean < 0 ~ str_c("<=",scales::comma(-mean*total_count)), TRUE ~ scales::comma(mean*total_count))) %>%
      mutate(mean2 = scales::percent(mean, accuracy = 0.01),
             mean2 = str_replace(mean2, "-", "<=")) %>%
      mutate(mean2 = str_c(mean2, " (",count_n,")")) %>%
      ungroup %>%
      select(sortcol, cohortDefinitionId, domain, time_period, conceptId, covariateName, data_source, mean2) %>%
      spread(data_source, mean2, fill = "-") %>%
      mutate(time_period = factor(time_period),
             domain = factor(domain)) %>%
      mutate(conceptName = str_to_sentence(map_chr(covariateName, ~ str_split(.x, ": ")[[1]][2]))) %>%
      mutate(domain = factor(
        domain,
        levels = c("condition",
                   "drug",
                   "measurement",
                   "meas value",
                   "observation",
                   "procedure")
      )) %>%
      arrange(domain, desc(sortcol)) %>%
      select(-sortcol,-covariateName, -conceptId) %>%
      select(cohortDefinitionId, domain, time_period, conceptName, everything()) %>%
      filter(!is.na(domain))
    
  }

summary_table <- function(data_sources){
  
  get_readable_names() %>%
    filter(cohortDefinitionId %in% c(2899,2898,2887,2885)) %>%
    mutate(name = factor(name, levels = name)) %>%
    left_join(get_all_counts(data_sources)) %>%
    mutate(data_source = factor(data_source)) %>%
    mutate(link = "<url here>") %>%
    select(name, link, data_source, count) %>%
    mutate(count = case_when(count < 5 ~ "<5",
                             TRUE ~ scales::comma(count, accuracy = 1))) %>%
    spread(data_source, count, fill = 0) %>%
    mutate(row_group = case_when(grepl("influenza", tolower(name)) ~ "Influenza 2017-2018",
                                 TRUE ~ "COVID-19")) %>%
    arrange(row_group, name) %>%
    mutate(`<NA>`="rm") %>%
    select(-`<NA>`) %>%
    group_by(row_group) %>%
    mutate(odd_rn = floor(row_number()/2) == row_number()/2) %>% 
    ungroup %>%
    gt(groupname_col = "row_group") %>%
    tab_style(
      locations = list(
        cells_body(columns = everything()),
        cells_column_labels(columns = everything())
      ),
      style = cell_text(align = "center")
    ) %>%
    tab_style(
      locations = list(
        cells_column_labels(columns = everything()),
        cells_row_groups()
      ),
      style = cell_text(weight = "bold")
    ) %>%
    tab_style(
      locations = list(
        cells_body(columns = vars(name, link)),
        cells_column_labels(columns =  vars(name, link))
      ),
      style = cell_text(align = "left")
    ) %>%
    tab_style(
      locations = list(
        cells_body(columns = vars(name))
      ),
      style = cell_text(indent = "20px")
    ) %>%
    cols_label(
      name = "",
      link = ""
    ) %>%
    cols_width(vars(name) ~ px(425),
               vars(link) ~ px(125),
               everything() ~ px(85)) %>%
    tab_options(
      table.align = "left"
    ) %>%
    #tab_style(style = list(cell_fill(color = "#f0f0f0")),
    #          locations = cells_body(rows = odd_rn == FALSE)) %>%
    cols_hide(columns = vars(odd_rn, link))
  
}



draw_mortality <- function(cohort_id_in, show_total = F, show_cohort = T, data_sources = x){
  tab <- get_readable_names() %>%
    filter(cohortDefinitionId %in% c(2899,2935,2887,2934)) %>%
    mutate(name = factor(name, levels = name)) %>%
    left_join(get_all_counts(data_sources)) %>%
    mutate(cohort =  case_when(grepl("COVID", name) ~ "COVID-19 diagnosis or positive test ",
                               TRUE ~ "Influenza diagnosis or positive test"),
           cohort_id =  case_when(grepl("COVID", name) ~ 2887,
                                  TRUE ~ 2899),
           death_status = case_when(grepl("Deceased", name) ~ "Dead",
                                    TRUE ~ "Total")) %>%
    select(data_source, cohort_id, cohort, death_status, count) %>%
    spread(death_status, count, fill = 0) %>%
    mutate(Alive = Total - Dead) %>%
    mutate(Total_n = case_when(Total < 5 ~ 5, TRUE ~ Total),
           Alive = 
             case_when(
               Alive < 5 & Alive > 0 & Alive == Total ~  str_c(scales::percent(5/Total_n, accuracy = 0.1), " (<5)"),
               Alive < 5 & Alive > 0 ~ str_c("<",scales::percent(5/Total_n, accuracy = 0.1), " (<5)"),
               TRUE ~ str_c(scales::percent(Alive/Total, accuracy = 0.1), " (",scales::comma(Alive),")")),
           Dead = 
             case_when(Dead < 5 & Dead > 0 ~ str_c("<",scales::percent(5/Total, accuracy = 0.1), " (<5)"),
                       TRUE ~ str_c(scales::percent(Dead/Total, accuracy = 0.1), " (",scales::comma(Dead),")"))) %>%
    arrange(data_source) %>%
    mutate_at(vars("Alive","Dead"), ~case_when(.x=="0.0% (0)" ~ "0.00% (0)",
                                               TRUE ~ .x)) %>%
    mutate(Total = case_when(Total < 5 ~ "<=5", TRUE ~ as.character(Total))) %>%
    filter(cohort_id == cohort_id_in) %>%
    select(Database = data_source, Cohort = cohort, Total,Alive,Dead) %>%
    mutate(odd_rn = floor(row_number() / 2) == row_number() / 2) %>%
    gt::gt() %>%
    tab_style(
      locations = list(
        cells_body(columns = vars(Cohort, Database)),
        cells_column_labels(columns = vars(Cohort, Database))
      ),
      style = cell_text(align = "left")
    ) %>%
    tab_style(
      locations = list(
        cells_body(columns = matches(c(
          "Alive","Dead", "Total"
        ))),
        cells_column_labels(columns = matches(c(
          "Alive","Dead", "Total"
        )))
      ),
      style = cell_text(align = "center")
    ) %>%
    tab_style(locations = list(cells_column_labels(columns = matches(
      c("Cohort",  "Alive","Dead", "Total", "Database")
    ))),
    style = cell_text(weight = "bold")) %>%
    cols_width(vars(Cohort) ~ px(390),
               vars(Database) ~ px(125),
               everything() ~ px(68)) %>%
    tab_options(table.align = "left") #%>%
  #tab_style(style = list(cell_fill(color = "#f0f0f0")),
  #         locations = cells_body(rows = odd_rn == FALSE))
  
  
  if (show_total) {
    tab <- cols_hide(tab, columns = vars(odd_rn))
  } else{
    tab <- cols_hide(tab, columns = vars(odd_rn, Total))
  }
  
  if (!show_cohort) {
    tab <- cols_hide(tab, columns = vars(Cohort))
  }
  
  tab  
  
}

make_ds_desc_table <- function(){
  
  v <- read_csv("data/data_source_desc.csv") %>%
    gt() %>%
    tab_style(
      locations = list(
        cells_body(columns = everything()),
        cells_column_labels(columns = everything())
      ),
      style = cell_text(align = "left", v_align = "top")
    ) %>%
    tab_style(
      locations = list(
        cells_column_labels(columns = everything())
      ),
      style = cell_text(weight = "bold")
    ) %>%
    cols_width(vars(`Data source`) ~ px(250),
               vars(`Refreshed to`) ~ px(125),
               vars(Description) ~ px(900)) %>%
    tab_options(
      table.align = "left"
    )
  
  return(v)
  
}


