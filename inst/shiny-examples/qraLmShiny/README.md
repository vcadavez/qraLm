# qraLm Shiny Application for Listeria Monocytogenes Risk Assessment

![qraLm Logo](www/img/logo.svg)

## Table of Contents

- [qraLm Shiny Application for Listeria Monocytogenes Risk Assessment](#jemra-shiny-application-for-listeria-monocytogenes-risk-assessment)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Key Features](#key-features)
    - [Additional Features](#additional-features)
  - [Motivation](#motivation)
    - [Why Choose qraLm Shiny?](#why-choose-jemra-shiny)
  - [Getting Started](#getting-started)
    - [Installation](#installation)
    - [Usage](#usage)
      - [Advanced Usage](#advanced-usage)
  - [Software Requirements](#software-requirements)
    - [Additional Libraries](#additional-libraries)
  - [Architecture Overview](#architecture-overview)
    - [Data (`data/`)](#data-data)
    - [Modules (`modules/`)](#modules-modules)
    - [Pages (`pages/`)](#pages-pages)
      - [About (`about/`)](#about-about)
      - [Models (`models/`)](#models-models)
    - [Web Assets (`www/`)](#web-assets-www)
    - [App Entry (`app.R`)](#app-entry-appr)
  - [High-Level Design and Low-Level Design](#high-level-design-and-low-level-design)
    - [High-Level Design](#high-level-design)
    - [Low-Level Design](#low-level-design)
    - [Directory Structure](#directory-structure)
    - [High-Level Design and Low-Level Design](#high-level-design-and-low-level-design-1)
  - [Data Dictionary](#data-dictionary)
    - [Key Data Fields](#key-data-fields)
    - [Models](#models)
  - [User Roles](#user-roles)
  - [FAQ](#faq)
  - [Contributing](#contributing)
  - [Collaborators](#collaborators)
  - [License](#license)
  - [Acknowledgments](#acknowledgments)

## Overview

The qraLm Shiny application is a specialized tool for comprehensive risk assessment of Listeria monocytogenes in foods. Developed in collaboration with [IPB](https://www.ipb.pt), this application is part of a larger initiative aimed at promoting safer food practices.

*   [Download the Report](https://ipb.pt)

*   [Visit our GitHub Repository](https://github.com/YourUsername/qraLm_ShinyApp)

## Key Features

*   **Interactive Models**: Comprehensive risk assessment models for accurate insights.

*   **Fluid User Interface**: Navigate through interactive sections including Production, Cooking, Risk, and Testing.

*   **Reports & Resources**: Download detailed reports and access various resources.

*   **Libraries Used**: Developed using Shiny, and integrates popular R libraries like `dplyr`, `ggplot2`, and `plotly`.

*   **Modular Structure**: Designed with scalability in mind, enabling easy adaptations and expansions.

### Additional Features

*   **Data Visualization**: Advanced charts and graphs for better understanding.

*   **Real-time Updates**: Get live updates on risk levels.

*   **User-friendly Documentation**: Extensive guides and tooltips to help users.

## Motivation

Listeria monocytogenes pose significant health risks, especially in frozen vegetables. This tool empowers industries, researchers, and consumers by providing essential insights into managing these risks effectively.

### Why Choose qraLm Shiny?

With a variety of risk assessment tools available, qraLm Shiny stands out due to its comprehensive models, easy-to-use interface, and robust architecture.

## Getting Started

### Installation

1.  **Clone Repository**: Clone the repository to your local machine using `git clone https://github.com/YourUsername/qraLm_ShinyApp.git`.

2.  **Run R Console**: Open the R console by executing `R`.

3.  **Install Packages**: Install the required R packages using `install.packages(...)`.

4.  **Run Application**: Start the Shiny application by running `shiny::runApp("app.R")`.

### Usage

Navigate to `http://localhost:8000/` in your web browser after running the app. Select your desired model, input the required parameters, and execute the model to receive the risk assessment.

#### Advanced Usage

For more advanced features and customizations, refer to our [Advanced User Guide](ADVANCED_USER_GUIDE.md).

## Software Requirements

To run qraLm Shiny effectively, make sure you have the following software and libraries installed:

*   **R (>= 3.5)**: The programming language used to develop this application. Download it from [CRAN](https://cran.r-project.org/).

*   **RStudio (Optional)**: Recommended IDE for easier development. Download it from [RStudio's Website](https://rstudio.com/products/rstudio/download/).

*   **Shiny**: R package for building Shiny applications. Install it using `install.packages("shiny")`.

*   **dplyr**: Data manipulation library. Install it using `install.packages("dplyr")`.

*   **ggplot2**: Data visualization library. Install it using `install.packages("ggplot2")`.

*   **plotly**: Interactive graphing. Install it using `install.packages("plotly")`.

### Additional Libraries

Some functionalities might require additional libraries. Refer to the Installation guide for more details.

***

## Architecture Overview

The application follows a modular architecture, making it easy to manage, extend, and customize. Here's a deeper look into the directory structure and the role of each:
```Mermaid
graph TD
  A[qraLmshiny]
  
  subgraph "README"
    B[README.md]
  end
  
  subgraph "Data"
    C[data]
  end
  
  subgraph "Modules"
    D[modules]
  end

  subgraph "Pages"
    E[pages]
    
    subgraph "About"
      G[about]
    end
    
    subgraph "Models"
      H[models]
      
      subgraph "Sidebar"
        I[sidebar]
        I1[stageInputs]
        I2[stageRun]
        I3[stageSelector]
        I-->I1
        I-->I2
        I-->I3
      end
      
      subgraph "Views"
        J[views]
        N[Blanching]
        O[Cooking]
        P[Defrosting]
        Q[Partitioning]
        R[Portioning]
        S[Production]
        T[Risk]
        U[Testing]
        J-->N
        J-->O
        J-->P
        J-->Q
        J-->R
        J-->S
        J-->T
        J-->U
      end
      
      H-->I
      H-->J
    end
    
    E-->G
    E-->H
  end

  subgraph "Static Files"
    F[www]
    A4[img]
    F-->A4
  end
  
  A-->B
  A-->C
  A-->D
  A-->E
  A-->F
```
### Data (`data/`)

This directory houses datasets required for the risk assessment models. Files can include raw or cleaned CSV, Excel sheets, or R Data objects. The `readData.R` script reads these datasets into the application at runtime.

### Modules (`modules/`)

Custom Shiny modules that offer specific functionalities like form elements, data tables, or even smaller pieces of UI are stored here. Each module has its associated server logic, ensuring reusability across different parts of the application.

### Pages (`pages/`)

The `pages/` directory is the heart of the application, containing the UI and logic for each section. The structure is divided further to isolate each module's concerns:

#### About (`about/`)

The About section provides an overview of the application's purpose, features, and how to use it. This section aims to offer users quick insights into what they can achieve with the app.

#### Models (`models/`)

This directory holds the risk assessment models divided into different categories, like Defrosting, Cooking, and Risk. Each category contains:

*   **Sidebar (`sidebar/`)**: Shiny modules specific to each category for collecting user inputs, selecting models, and running them.

*   **Views (`views/`)**: These are the UIs for each stage of food preparation or testing. Views contain specific logic and user interface components to interact with each model. For instance, `area00` might cover risks related to uncooked foods, while `area01` might delve into partially cooked items.

### Web Assets (`www/`)

This directory stores static files such as images, CSS, and client-side JavaScript files. The application refers to these for styling and additional functionalities.

### App Entry (`app.R`)

This is the main R script that pulls together modules, data, and UI components to run the application. It initializes the Shiny server and renders the app.

***

## High-Level Design and Low-Level Design

### High-Level Design

The application is built on the MVC (Model-View-Controller) pattern. All data manipulation and business logic are encapsulated within the `modules/` and `pages/` directories. The `app.R` script acts as a controller that initializes and runs the app.

### Low-Level Design

*   **Model**: Data is read into the app from the `data/` directory. Pre-processing and computations are done in separate Shiny modules.

*   **View**: The `pages/` directory contains the UI definitions for each section. Shiny modules in the `modules/` directory can be thought of as reusable UI components.

*   **Controller**: `app.R` manages the flow of data between the Model and the View. It is responsible for initializing the app and ensuring that data flows correctly between the UI and server logic.

### Directory Structure

For better organization, the application follows a directory structure that separates different parts of the codebase into folders:

*   `data/`: Houses the data used in the application.

*   `modules/`: Holds reusable Shiny modules.

*   `pages/`: Contains the UI and server logic for various pages.

    *   `about/`: Information about the project and its purpose.

    *   `models/`: Different risk assessment models, with subsections for each stage.

        *   `sidebar/`: Houses sidebar functionalities like input selections.

        *   `views/`: Contains the view logic for different risk assessment stages.

*   `www/`: Contains static files like images and CSS.

*   `app.R`: The main R script that runs the application.

### High-Level Design and Low-Level Design

...

## Data Dictionary

### Key Data Fields

*   `sampleID`: Unique identifier for each sample.

*   `listeria_count`: Count of Listeria monocytogenes in the sample.

*   `temperature`: Storage temperature of the sample.

*   `pH_level`: pH level of the sample.

### Models

*   `Defrosting`: Assess risks during the defrosting stage.

*   `Cooking`: Assess risks during the cooking stage.

## User Roles

*   **Admin**: Full control over models and data.

*   **Researcher**: Can use all the models but can't modify them.

*   **Consumer**: Limited access to simplified models and resources.

*   **Data Analyst**: Limited access to data visualization and reporting tools.

*   **Guest**: Read-only access to basic features and documentation.

## FAQ

*   **How do I run the app locally?**: Follow the 'Getting Started' section.

*   **What kind of models are available?**: Models cover a range of food preparation stages, including defrosting, cooking, and partitioning.

## Contributing

We welcome contributions and value your feedback. To contribute, you can:

1.  Fork the repository.

2.  Create a new branch (`git checkout -b feature-branch`).

3.  Make your changes.

4.  Submit a pull request.

## Collaborators

This project was developed in collaboration with [IPB](https://www.ipb.pt). Special thanks to all contributors and the community for their ongoing support.

## License

This project is licensed under the MIT License. See the [LICENSE.md](/LICENSE.md) file for details.

## Acknowledgments

We would like to express our gratitude to our advisors, beta testers, and all those who have provided feedback to improve this application.
