# =============================================================================
# user_guide_ui.R - "User Guide" page (last item in the sidebar).
# Displays the Vulnerability App User Guide PDF inline and offers a
# "Download Guide" button. This is a simple, non-module page (no ns()
# namespacing needed) - the matching downloadHandler lives directly in the
# main server() function in MetaApp_Final_v12.R.
#
# NOTE: expects the PDF to be present at "www/Vulnerability_App_User_Guide.pdf"
# relative to the app root, so Shiny can serve it as a static asset for the
# inline viewer, and so the download handler can read it from disk.
# =============================================================================
userGuideUI <- function() {
  tagList(
    div(
      class = "sidebar-card",
      style = "margin: 16px; background: #ffffff; border-radius: 8px; padding: 16px;",
      tags$iframe(
        src = "Vulnerability_App_User_Guide.pdf",
        type = "application/pdf",
        style = "width: 100%; height: 85vh; border: none; background: #ffffff;"
      )
    )
  )
}
