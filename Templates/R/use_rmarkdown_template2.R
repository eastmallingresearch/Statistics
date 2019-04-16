#' Add an RMarkdown Template
#'
#' Copy of the usethis function of the same name with added functionality
#' Adds files and directories necessary to add a custom rmarkdown template to RStudio.
#'
#' @param template_name The name as printed in the template menu.
#' @param template_dir	Name of the directory the template will live in within inst/rmarkdown/templates.
#' @param template_description	Sets the value of description in template.yml.
#' @param template_create_dir Sets the value of create_dir in template.yml.
#' @param default_template The default template to use as the basis for the new template
#' @param package The package containing the default template
#'
#' @example
#' use_rmarkdown_template2(default_template="default.Rmd",package="Templates")
#' @export
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
