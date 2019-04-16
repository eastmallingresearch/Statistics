#' Add an RMarkdown Template
#'
#' Copy of the usethis function of the same name with added functionality
#' Adds files and directories necessary to add a custom rmarkdown template to RStudio.
#'
#' @template_name The name as printed in the template menu.
#' @template_dir	Name of the directory the template will live in within inst/rmarkdown/templates.
#' @template_description	Sets the value of description in template.yml.
#' @template_create_dir Sets the value of create_dir in template.yml.
#' @default_template The default template to use as the basis for the new template
#' @package The package containing the default template
#'
#' @Example
#' use_rmarkdown_template2(default_template="default.Rmd",package="Templates")

use_rmarkdown_template2 <-
  function (template_name = "Template Name", template_dir = tolower(asciify(template_name)),
            template_description = "A description of the template",
            template_create_dir = FALSE,default_template="markdown-template.Rmd",package="usethis")
  {
    require(usethis)
    template_create_dir <- as.character(template_create_dir)
    template_dir <- path("inst", "rmarkdown", "templates", template_dir)
    use_directory(path(template_dir, "skeleton"))
    use_template("rmarkdown-template.yml", data = list(template_dir = template_dir,
                                                       template_name = template_name, template_description = template_description,
                                                       template_create_dir = template_create_dir), save_as = path(template_dir,
                                                                                                                  "template.yaml"))
    use_template(default_template, path(template_dir,
                                        "skeleton", "skeleton.Rmd"),package=package)
    invisible(TRUE)
  }
