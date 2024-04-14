dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <- tags$img(src='tb_images/Teradata_logo-two_color_reversed.png',height='60',width='200')

dashboardPage(
  title = "Teradata Insurance Claims Explorer",
  dbHeader,
  skin = "black",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Selection", tabName = "data_selection"),
      menuItem("Outlier Executive Summary", tabName = "provider_anomalies_action"),
      menuItem('Provider Anomaly Analyzer', tabName = '',
        menuSubItem("Procedure Outliers", tabName = "suspicious_providers"),
        menuSubItem("Claim Cost Outliers", tabName = "cost_outliers")
      ),
      menuItem('Injury Pathway Anomalies', tabName = '',
        menuSubItem('Pathways Visualization', tabName = 'pathways_visualization'),
        menuSubItem('Pathways Underlying Med. Codes', tabName = "pathways_underlying_medical_codes"),
        menuSubItem('Pathways Download', tabName = 'pathways_download'),
        menuSubItem('Transition Analyzer', tabName = "transition_analyzer"),
        menuSubItem('Physical Therapy Pathways', tabName = 'physical_therapy_pathways')
      ),
      menuItem('Additional Exploration', tabName = '',
        menuSubItem('Exploration (in-memory)', tabName = 'exploration'),
        menuSubItem('Exploration (in-database)', tabName = 'exploration_in_database'),
        menuSubItem('Claim Trajectory', tabName = 'claim_trajectory')
        # menuSubItem('COVID Explorer', tabName = 'covid_overview'),
        # menuSubItem('Diagnosis Explorer', tabName = 'diagnosis_explorer'),
        # menuSubItem('Guided Exploration', tabName = 'guided_exploration')
      ),
      menuItem("Research", tabName = '',
        menuSubItem("Triangles", tabName = "triangles")
        # menuSubItem("Trace Clustering", tabName = "trace_clustering")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(
        '.myClass {
	      font-size: 20px; text-transform: uppercase; line-height: 50px; text-align: left; font-family: RidleyGrotesk-Medium,"Helvetica Neue",Helvetica,Arial,sans-serif; padding: 0 15px; overflow: hidden; color: white;
        }
	    ')),
        ## add to suppress warning messages    
          tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "_all-skins.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "skin-black.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "gallery.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    tags$script(HTML('
	   $(document).ready(function() {
	     $("header").find("nav").append(\'<span class="myClass"> Insurance Claims Explorer </span>\');
	   })
	  ')),
    add_busy_spinner(color = 'honeydew',
                     height = '25px',
                     width = '25px',
                     spin = "fading-circle"),
    tabItems(
      tabItem(tabName = "data_selection", tab_data_selection_ui("data_selection")
      ),
      tabItem(tabName = "suspicious_providers", tab_suspicious_providers_ui("suspicious_providers")
      ),
      tabItem(tabName = "cost_outliers", tab_cost_outliers_ui("cost_outliers")
      ),
      tabItem(tabName = "provider_anomalies_action", tab_provider_anomalies_action_ui("provider_anomalies_action")
      ),#provider_anomalies_action
      tabItem(tabName = "known_patterns_summary", tab_pattern_summary_ui("known_patterns_summary")
      ),#known_patterns_summary
      tabItem('pathways_visualization', tab_pathways_viz_ui("pathways_visualization")
      ),#pathways_visualization
      tabItem('pathways_underlying_medical_codes', tab_pathways_med_codes_ui("pathways_underlying_medical_codes")
      ),#pathways_underlying_medical_codes
      tabItem('pathways_download', tab_pathways_download_ui("pathways_download")
      ),#pathways_download
      tabItem('transition_analyzer', tab_transition_analyzer_ui("transition_analyzer")
      ),#transition_analyzer
      tabItem('physical_therapy_pathways', tab_physical_therapy_pathsways_ui("physical_therapy_pathways")
      ),#physical_therapy_pathways
      tabItem(tabName = "representative_pathways", tab_rep_pathways_ui("representative_pathways")
      ),#representative_pathways
      tabItem(tabName = "exploration", tab_exploration_ui("exploration")
      ),
      tabItem(tabName = "exploration_in_database", tab_exploration_in_database_ui("exploration_in_database")
      ),
      tabItem(tabName = "claim_trajectory", tab_claim_trajectory_ui("claim_trajectory")
      ),
      tabItem(tabName = "covid_overview", tab_covid_overview_ui("covid_overview")
      ),
      tabItem(tabName = "diagnosis_explorer", tab_diagnosis_explorer_ui("diagnosis_explorer")
      ),
      tabItem(tabName = "guided_exploration", tab_guided_exploration_ui("guided_exploration")
      ),
      tabItem(tabName = "triangles", tab_triangles_ui("triangles")
      ),
      tabItem(tabName = "trace_clustering", tab_trace_clustering_ui("trace_clustering")
      )
    )#tabItems
  )#dashboardBody
)