### Overview

This Shiny application allows users to explore various **baby drop-in activities** available in West Side Vancouver. The app provides an interactive interface (table view and map view) to filter, view, and explore activities based on activity type, location, time, price, and prebooking requirements.

The first version of the app can be found in the folder `b3_activities`, deployed on shiny at <https://jennychang.shinyapps.io/activities/> , while the `b4_activities` branch contains the updated version of the app, deployed on shiny at <https://jennychang.shinyapps.io/activities_newfeatures/>. Each 'activities' folder contains the dataset `activities.csv`, the code for the app `app.R`, and a www folder for storing the image file, if used.

### Features

-   **Activity Filtering:**\
    Filter activities by:

    -   Activity Type (e.g., Babytime, Storytime, Family playtime)

    -   Day of the Week

    -   Start Time

    -   Keyword Search

-   **Interactive Views:**\
    View results in two formats:

    -   **Table View:** Displays a detailed table with activity information.

    -   **Map View:** Displays locations of activities on an interactive map with pop-ups for additional details.

-   **Downloadable Results:**\
    Export filtered activity data to a `.csv` file for offline reference.

### Dataset

This dataset `activities.csv` was compiled by the author of this app. The information was gathered online from the Vancouver Public Library, Vancouver Recreation portal, University Neighborhoods Association, and the official websites where the activities are held.

**Disclaimer:** The drop-in schedule is subject to change. For the most up-to-date information, consult the official website of the respective activity provider.
