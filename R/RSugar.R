#' RSugar
#'
#' @name RSugar
#'
#'

pkg.env <- new.env()
pkg.env$oauth.code <- NULL # Global cache of the auth code.

## Configuration

## Example:
##    set_tier("https://rawsugar.parkerici.org", "<oauth-client-id>.apps.googleusercontent.com")

set_tier <- function(host, client.id) {
    pkg.env$base.url <- host
    pkg.env$client.id <- client.id
    pkg.env$oauth.code <- NULL
}


## API infrastructure

api.path <-  "/api/v2"

api_url <- function(path, args) {
    return(httr::modify_url(paste(pkg.env$base.url, api.path, path, sep = ""), query = args))
}

rawsugar_api_get_base <- function(path, args = list()) {
    check_login()
    args["oauth-code"] <- pkg.env$oauth.code 
    url <- api_url(path, args)
    print(url)
    return(httr::GET(url))
}

## get response as parsed json
rawsugar_api_get <- function(path, args = list()) {
    resp <- rawsugar_api_get_base(path, args)
    return(httr::content(resp, "parsed", type = "application/json"))
}

## get result as uninterpreted text
rawsugar_api_get_raw <- function(path, args = list()) {
    resp <- rawsugar_api_get_base(path, args)
    return(httr::content(resp, "text"))
}

## get result as dataframe
rawsugar_api_get_dataframe <- function(path, args) {
    raw <- rawsugar_api_get_raw(path, args)
    return(jsonlite::fromJSON(raw, simplifyDataFrame = TRUE))
}

## The project metadata is fetched once and cached
## TODO might need a way to force a refresh or turn off caching
cached.projects <- NULL

projects <- function() {
    if (is.null(pkg.env$cached.projects)) {
        pkg.env$cached.projects <- rawsugar_api_get("/projects/data")
        return(pkg.env$cached.projects)
    }
    else {
        return(pkg.env$cached.projects)
    }
}

## TODO there must be a more elegant way to do this.
find_project_ent <- function(project.name) {
    for (proj in projects()) { 
        if (proj$'project/name' == project.name) {
            return(proj) # $'db/id'
        }
    }
}

## Exported API calls

#' Find the id of a project given the name
#'
#' @param project.name The project name
#'
#' @export
find_project_id <- function(project.name) {
    return(find_project_ent(project.name)$'db/id')
}

find_batch_ent <- function(project.name, batch.name) {
    proj <- find_project_ent(project.name)
    batches <- proj$'project/batches'
    for (batch in batches) {
        if (batch$'batch/name' == batch.name) {
            return(batch) 
        }
    }
}

#' Find the id of a batch given the name
#'
#' @param project.name The project name
#' @param batch.name The batch name
#'
#' @export
find_batch_id <- function(project.name, batch.name) {
    return(find_batch_ent(project.name, batch.name)$'db/id')
}

find_sheet_ent <- function(project.name, batch.name, sheet.name) {
    batch <- find_batch_ent(project.name, batch.name)
    sheets <- batch$'batch/sheets'
    for (sheet in sheets) {
        if (sheet$'sheet/name' == sheet.name) {
            return(sheet) 
        }
    }
}

#' Find the id of a sheet given the name
#'
#' @param project.name The project name
#' @param batch.name The batch name
#' @param sheet.name The sheet name
#'
#' @export
find_sheet_id <- function(project.name, batch.name, sheet.name) {
    return(find_sheet_ent(project.name, batch.name, sheet.name)$'db/id')
}


#' Return the contents of a sheet as a dataframe
#'
#' @param sheet.id The id of the sheet
#'
#' @export
get_sheet_contents <- function(sheet.id) {
    return(rawsugar_api_get_dataframe("/sheet/data", list("sheet" = sheet.id, "columns" = "false")) )
}

#' Return the contents of a sheet ass a dataframe, given the project/batch/sheet names.
#'
#' @param project.name The project name
#' @param batch.name The batch name
#' @param sheet.name The sheet name
#'
#' @export
get_sheet_contents_by_name <- function(project.name, batch.name, sheet.name) {
    return(get_sheet_contents(find_sheet_id(project.name, batch.name, sheet.name)))
}


#' Return a list of files in a batch as a dataframe
#'
#' @param batch.id The id of the batch
#'
#' @export
get_batch_files <- function(batch.id) {
    return(rawsugar_api_get_dataframe("/file/list", list("batch" = batch.id)))
}

#' Return a list of files in a batch as a dataframe
#'
#' @param project.name The project name
#' @param batch.name The batch name
#'
#' @export
get_batch_files_by_name <- function(project.name, batch.name) {
    return(get_batch_fifles(find_batch_id(find_project_id(project.name), batch.name)))
}


#' Return the contents of a file given the id
#'
#' @param file.id The id of the sheet
#'
#' @export
get_file_contents <- function(file.id) {
    return(rawsugar_api_get_raw("/file", list("file" = file.id)))
}

login <- function() {
    url <- httr::modify_url("https://accounts.google.com/o/oauth2/auth",
                      query = list(scope = "https://www.googleapis.com/auth/userinfo.email",
                                   redirect_uri = "urn:ietf:wg:oauth:2.0:oob",
                                   response_type = "code",
                                   client_id = pkg.env$client.id))
    browseURL(url)
    code <- readline(prompt = "Enter code provided by Google: ")
    return(code)
}

check_login <- function() {
    if (is.null(pkg.env$oauth.code)) {
        pkg.env$oauth.code <- login()
    }
}
