#' Get entity detail
#'
#' This function returns entity details given a PIVA number and an API key.
#'
#' @param piva A string representing the PIVA number.
#' @param apikey A string representing the API key.
#'
#' @importFrom httr GET content
#' @importFrom httr add_headers content
#' @importFrom httr content content
#'
#' @return A string containing the HTTP response.
#' @export

get_entity_detail = function(piva, apikey) {
    if(!is.character(piva)) {
        print('PIVA is not a string')
        return()
    }

    header = c(
        "accept" = "application/json",
        "apikey" = apikey
    )

    path = "/v1/entitySearch/live"

    params = list(
        "testoricerca" = piva
    )

    resp = httr::GET(
        url = paste0("https://api.cerved.com/cervedApi", path),
        httr::add_headers(header),
        query = params
    )

    return(httr::content(resp, "text", encoding = "UTF-8"))
}


#' Get profile
#'
#' This function returns a profile given a Cerved ID and an API key.
#'
#' @param cerved_id A string representing the Cerved ID.
#' @param apikey A string representing the API key.
#'
#' @importFrom httr GET content
#' @importFrom httr add_headers content
#' @importFrom httr content content
#'
#' @return A string containing the HTTP response.
#' @export

get_profile = function(cerved_id, apikey) {
    if(!is.character(cerved_id)) {
        print('Cerved ID is not a string')
        return()
    }

    path = "/v1/entityProfile/live"

    header = c(
        "accept" = "application/json",
        "apikey" = apikey
    )

    params = list(
        "id_soggetto" = cerved_id
    )

    resp = httr::GET(
        url = paste0("https://api.cerved.com/cervedApi", path),
        httr::add_headers(header),
        query = params
    )

    return(httr::content(resp, "text", encoding = "UTF-8"))
}

#' Get score CGS
#'
#' This function returns a CGS score given a Cerved ID, an API key, and a score code.
#'
#' @param cerved_id A string representing the Cerved ID.
#' @param apikey A string representing the API key.
#' @param codice_score A string representing the score code. Default is "CGS".
#'
#' @importFrom httr GET content
#' @importFrom httr add_headers content
#' @importFrom httr content content
#'
#' @return A string containing the HTTP response.
#' @export

get_score_CGS = function(cerved_id, apikey, codice_score="CGS") {
    if(!is.character(cerved_id)) {
        print('Cerved ID is not a string')
        return()
    }

    header = c(
        "accept" = "application/json",
        "apikey" = apikey
    )

    path = paste0("/v1.1/score/impresa/corporate/", codice_score)

    params = list(
        "id_soggetto" = cerved_id
    )

    resp = httr::GET(
        url = paste0("https://api.cerved.com/cervedApi", path),
        httr::add_headers(header),
        query = params
    )

    return(content(resp, "text", encoding = "UTF-8"))
}


#' Get balance sheet list
#'
#' This function returns a list of balance sheets given a Cerved ID and an API key.
#'
#' @param cerved_id A string representing the Cerved ID.
#' @param apikey A string representing the API key.
#'
#' @importFrom httr GET content
#' @importFrom httr add_headers content
#' @importFrom httr content content
#'
#' @return A string containing the HTTP response.
#' @export

get_balancesheet_list = function(cerved_id, apikey) {
    if(!is.character(cerved_id)) {
        print('Cerved ID is not a string')
        return()
    }

    header = c(
        "accept" = "application/json",
        "apikey" = apikey
    )

    path = "/v1/entityProfile/balancesheetslist"

    params = list(
        "id_soggetto" = cerved_id
    )

    resp = httr::GET(
        url = paste0("https://api.cerved.com/cervedApi", path),
        httr::add_headers(header),
        query = params
    )

    return(httr::content(resp, "text", encoding = "UTF-8"))
}

#' Get balance sheet
#'
#' This function returns a balance sheet given a Cerved ID, a date, an API key, and a balance type.
#'
#' @param cerved_id A string representing the Cerved ID.
#' @param date A string representing the date in 'dd-MM-yyyy' format.
#' @param apikey A string representing the API key.
#' @param tipo A string representing the balance type. Default is "E".
#'
#' @importFrom httr GET content
#' @importFrom httr add_headers content
#' @importFrom httr content content
#'
#' @return A string containing the HTTP response.
#' @export

get_balancesheet = function(cerved_id, date, apikey, tipo="E") {
    if(!is.character(cerved_id)) {
        print('Cerved ID is not a string')
        return()
    }

    if(!is.character(date)) {
        print('Date is not a string')
        return()
    }

    pattern = "\\d{2}-\\d{2}-\\d{4}"

    if(!grepl(pattern, date)) {
        print("Date format is invalid. It must be 'dd-MM-yyyy'")
        return()
    }

    if(!is.character(tipo)) {
        print('Tipo Bilancio is not a string')
        return()
    }

    header = c(
        "accept" = "application/json",
        "apikey" = apikey
    )

    path = "/v1/entityProfile/balancesheet"

    params = list(
        "id_soggetto" = cerved_id,
        "data_chiusura" = date,
        "tipo_bilancio" = tipo
    )

    resp = httr::GET(
        url = paste0("https://api.cerved.com/cervedApi", path),
        httr::add_headers(header),
        query = params
    )

    return(httr::content(resp, "text", encoding = "UTF-8"))
}


#' Get economic outlook from LLM
#'
#' This function returns an economic outlook for a given business sector, based on a description.
#'
#' @param ateco_desc A string representing the description of the business sector.
#' @param apikey A string representing the API key.
#'
#' @importFrom httr POST content
#' @importFrom httr add_headers content
#' @importFrom httr content content
#' @importFrom jsonlite toJSON fromJSON
#'
#' @return A string containing the HTTP response.
#' @export

get_outlook_from_llm = function(ateco_desc, apikey) {

    if(!is.character(ateco_desc)) {
        stop('Ateco description is not a string')
    }

    headers = c(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", apikey)
    )

    url = "https://api.openai.com/v1/chat/completions"

    system_content = "
      You are an experienced management consultant with tons of experience in assessing economic outlook
      for business sectors. You will be asked to critically evaluate a given sector. Answer in Italian.
      Organize your answer in two sections: 'Key Factors' and 'Conclusion'
  "

    user_message = paste("Give me the economic outlook for this sector:", ateco_desc)

    system = list("role" = "system", "content" = system_content)
    current_message = list("role" = "user", "content" = user_message)
    messages = list(system, current_message)

    payload = list(
        "messages" = messages,
        "model" = "gpt-3.5-turbo",
        "max_tokens" = 500,
        "n" = 1,
        "stop" = NULL,
        "temperature" = 0
    )

    resp = httr::POST(url, httr::add_headers(headers), body = jsonlite::toJSON(payload, auto_unbox = TRUE), encode = "json")

    response = jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))

    return(response$choices[[1]]$message$content)
}
