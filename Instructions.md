# Research Compendium

## Learning goals

1.  Combine data, code, and manuscript in a *research compendium*
2.  Follow software conventions to take advantage of existing tools
3.  Introduction to how the core concepts of modularity, documentation, and validation fit together

## Prerequisites

RStudio ≥1.4

R ≥4.0

Packages: tidyverse, devtools, rrtools

Git

GitHub account

## Working example

*Why aren't the largest animals the fastest?* A horse is faster than a dog is faster than a mouse. As anyone who's gone for a walk with a taller friend knows, larger body sizes have biomechanical advantages for increased speed. However, the trend reverses at extremely large body sizes (e.g., an elephant is slower than a giraffe is slower than a horse). Hirt et al. (2017) present a model from physical first principles for explaining this pattern. In this exercise, we will replicate one of their figures in a **research compendium**.

## Exercise

### 1. Create a repository

Version control (e.g., git) is an essential tool for keeping track of your own computational work and sharing your code with others. You'll submit every exercise in this course by sending the instructors a link to your repository on GitHub.

1.  Follow the instructions in this [R for the Rest of Us tutorial](https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/) to make sure you have Git installed and connect RStudio to GitHub.
2.  Sign into [github.com](http://github.com) and create a new repository called "maxspeed".
3.  In RStudio, create a new project from version control (File \> New Project \> Version Control \> Git). Fill in the repository URL to match your new repository on GitHub (www.github.com/*yourusername*/maxspeed.git). This should automatically call the project directory "maxspeed". Pick any subdirectory you like (\~/Documents/GitHub is the default in most cases).

### 2. Create a research compendium

1.  At the R console, enter `rrtools::create_compendium()`. Confirm you want to overwrite the .Rproj file. RStudio will restart.
2.  After RStudio restarts, take a minute to browse the new files. In particular, make note of DESCRIPTION, README.Rmd, and analysis/paper/paper.Rmd.
3.  Commit and push to save your changes and push them to GitHub. Switch to the Git tab in RStudio, check all the boxes next to the files (the orange "?"s should turn into green "A"s), and click "Commit". Enter "initial commit" in the commit message and hit Commit. Then hit Push. Commit creates a local checkpoint of your changes and Push uploads it to the server.
4.  Return to your repo's page on GitHub (www.github.com/*yourusername*/maxspeed) and refresh. You should now see a list of files and an analysis/ folder.

### 3. Add data

1.  Download [maxspeeddatabase.xlsx](https://github.com/FlukeAndFeather/maxspeed/blob/main/maxspeeddatabase.xlsx) and put it in analysis/data/raw_data.

2.  Create an R script: analysis/data/derived_data/read_maxspddb.R. Add the following code:

    ``` {.r}
    readxl::read_excel("analysis/data/raw_data/maxspeeddatabase.xlsx") %>%
      dplyr::rename(tax_grp = `taxonomic group`,
                    locomotion = `locomotion mode`,
                    mass_kg = `body mass [kg]`,
                    mass_ref = `mass ref.`,
                    max_spd_kmh = `max. speed [km/h]`,
                    speed_ref = `speed ref.`,
                    diet = `primary diet`) %>%
      saveRDS("analysis/data/derived_data/maxspeed.RDS")
    ```

3.  Source analysis/data/derived_data/read_maxspddb.R, which creates the data file analysis/data/derived_data/maxspeed.RDS.

4.  analysis/data/derived_data/read_maxspddb.R uses functions from the readxl and dplyr packages. We need to keep track of these dependencies! Run the following at the console:

``` {.r}
library(devtools)
use_package("readxl")
use_package("dplyr")
```

Notice readxl and dplyr are now listed under "Imports" in the DESCRIPTION file. We'll cover the details of the DESCRIPTION file in another lesson.

### 4. Add functions

1.  Hirt et al. (2017) proposed a non-linear model for predicting maximum speed from body size. We'll fit that model to multiple subsets of the maximum speed database (e.g. by locomotion mode: running, swimming, flying). Any time we repeat a task more than twice, it's probably a good idea to put it in a function.
2.  Call `use_r("speed_model")`. This creates a file, R/speed_model.R, where you'll add functions related to the speed model.
3.  Create a function for fitting the model. Copy the following functions `fit_maxspeed()` and `predict_maxspeed()` into R/speed_model.R. Don't worry if the code is confusing right now, you'll learn more about writing functions in the Modularity track.

``` {.r}
#' Fit time-dependent maximum speed model
#'
#' @param dat `[data.frame]` Subset of the maximum speed database. Requires columns `max_spd_kmh` and `mass_kg`.
#'
#' @return `[nls]` A fitted non-linear regression model.
#' @export
fit_maxspeed <- function(dat) {
  # Numerical methods sometimes fail to fit the model, so just keep trying until
  # it works
  tryCatch({
    stats::nls(
      formula = max_spd_kmh ~ a * mass_kg^b * (1 - exp(-h * mass_kg^i)),
      data = dat,
      start = list(a = runif(1, 11.2 - 0.91, 142.8 + 16.7),
                   b = runif(1, 0.24 - 0.01, 0.36 + 0.02),
                   h = runif(1, 2.4 - 1.4, 19.5 + 13.6),
                   i = runif(1, -0.72 - 0.26, -0.56 + 0.07))
    )},
    error = function(e) fit_maxspeed(dat)
  )
}

#' Maximum speed predictions
#'
#' @param mass_kg `[numeric]` Mass of animals for model prediction (in kg)
#' @param mod `[nls]` A fitted time-dependent maximum speed model (see `fit_maxspeed()`)
#'
#' @return `[numeric]` Predicted maximum speeds for animals of size `mass_kg`
#' @export
predict_maxspeed <- function(mass_kg, mod) {
  stats::predict(mod, newdata = data.frame(mass_kg = mass_kg))
}
```

4.  The comments preceding each function that start with `#'` will produce help files for each function. You'll learn more about them in the Documentation track. For now, hit Ctrl/Cmd-Shift-D to generate documentation. You can see what an example with `?fit_maxspeed`.
5.  Now's a good time to commit your changes.

### 5. Create a figure

1.  Time to use those functions to replicate figure 2a from Hirt et al. (2017). Open analysis/paper/paper.Rmd.

2.  Under `# Results`, add a code chunk to load the data and required packages.

        ```{r load}
        library(maxspeed)
        library(dplyr)
        library(ggplot2)
        maxspeed <- readRDS("../data/derived_data/maxspeed.RDS")
        ``` 

3.  Keep track of your dependencies! At the console, run `use_package("dplyr")` and `use_package("ggplot2")`.

4.  Add a code chunk to generate the figure

        ```{r fig2a}
        palette <- list(green = "#83CE3D",
                        orange = "#E39F2F",
                        blue = "#5F9FA1")

        fly_dat <- filter(maxspeed, locomotion == "flying")
        fly_model <- fit_maxspeed(fly_dat)
        fly_spd <- tibble(
          mass_kg = seq(min(fly_dat$mass_kg), max(fly_dat$mass_kg), length.out = 1000),
          max_spd_kmh = predict_maxspeed(mass_kg, fly_model)
        )

        run_dat <- filter(maxspeed, locomotion == "running")
        run_model <- fit_maxspeed(run_dat)
        run_spd <- tibble(
          mass_kg = seq(min(run_dat$mass_kg), max(run_dat$mass_kg), length.out = 1000),
          max_spd_kmh = predict_maxspeed(mass_kg, run_model)
        )

        swm_dat <- filter(maxspeed, locomotion == "swimming")
        swm_model <- fit_maxspeed(swm_dat)
        swm_spd <- tibble(
          mass_kg = seq(min(swm_dat$mass_kg), max(swm_dat$mass_kg), length.out = 1000),
          max_spd_kmh = predict_maxspeed(mass_kg, swm_model)
        )

        ggplot(fly_dat, aes(x = mass_kg, y = max_spd_kmh)) +
          geom_point(color = palette$green, size = 0.5, shape = 15) +
          geom_point(data = run_dat, color = palette$orange, size = 0.5, shape = 16) +
          geom_point(data = swm_dat, color = palette$blue, size = 0.5, shape = 17) +
          geom_line(data = fly_spd, 
                    color = palette$green, 
                    size = 1.25) +
          geom_line(data = fly_spd, 
                    color = "white", 
                    size = 0.5) +
          geom_line(data = run_spd, 
                    color = palette$orange, 
                    size = 1.25) +
          geom_line(data = run_spd, 
                    color = "white", 
                    size = 0.5) +
          geom_line(data = swm_spd, 
                    color = palette$blue, 
                    size = 1.25) +
          geom_line(data = swm_spd, 
                    color = "white", 
                    size = 0.5) +
          scale_x_log10("Body mass (kg)",
                        limits = 10^c(-10, 7),
                        breaks = 10^c(-10, -6, -2, 1, 4, 7),
                        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
          scale_y_log10(bquote(Speed~(km~h^-1)),
                        limits = 10^c(-2, 3),
                        breaks = 10^(-2:3),
                        labels = c("0.01", "0.1", "1", "10", "100", "1,000")) +
          annotation_logticks() +
          theme_classic() +
          theme(aspect.ratio = 1)
        ```

5.  Click Knit on the toolbar above the Rmd file. This generates a Word document with the figure you created.

6.  Commit your changes and push to GitHub.

### 6. Wrapping up

1.  Update the DESCRIPTION file. Read [Chapter 8](https://r-pkgs.org/description.html) of the R packages book and update all the fields you can (Title, Description, etc).
2.  Add a license. Read [Chapter 9](https://r-pkgs.org/license.html) of the R packages book and add an MIT license.
3.  Update the README. Reach [Chapter 20.5](https://r-pkgs.org/release.html?q=readme#readme) of the R packages book. Update README.Rmd and knit.
4.  Update analysis/paper/paper.Rmd. Add your information to the YAML header (the first 30ish lines, with `---` before and after it). Remove the boilerplate R code chunks. Knit to update the Word document.
5.  Commit and push.
6.  Open an issue in FlukeAndFeather/maxspeed with a link to your repo. The instructors will leave feedback for you in that issue.
