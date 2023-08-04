#' AWS S3 Bucket Download client's file
#'
#' Get a file from the client's directory in the S3 Risk bucket
#'
#' @param object_path the object's full path in the S3 bucket
#' @param company_path the bucket's S3 internal path of the company
#' @param risk_bucket the aws bucket
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#'
#' @returns file imported from S3
#'
#' @import aws.s3
#'
#' @export
#'
s3_retrieve_object = function(object_path, company_path, risk_bucket = "s3://risk-clientele/clients") {

    aws.s3::save_object(
        object = object_path,
        bucket = risk_bucket,
        file = paste0(gsub(paste0("^.*", company_path, '/'), "", object_path))
    )

}


#' AWS S3 Bucket Download client directory
#'
#' Get the client's  directory from the S3 Risk bucket
#'
#' @param company the company's LID
#' @param date the date desired to download (or 'latest')
#' @param risk_bucket the aws bucket
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#' @import aws.s3
#' @import zip
#'
#' @returns directory imported from S3
#' @export
#'
#' @importFrom aws.s3 save_object
#'
s3_retrieve_directory = function(company, date, risk_bucket = "s3://risk-clientele/clients") {

    ## S3 Setup -----------------------------
    lid_company = company
    company_date = date

    risk_bucket = "s3://risk-clientele/"
    clientele_s3 = aws.s3::get_bucket_df(risk_bucket) |> as.data.table()
    clientele_s3
    snapshot = company_date


    ### INTERNALS  ----------------
    if(snapshot == "latest") {

        extracted_part = clientele_s3$Key[grepl(company, clientele_s3$Key)]
        snapshot_list = unique(regmatches(extracted_part, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", extracted_part)))
        company_date = max(snapshot_list)

    } else {

        company_date = snapshot

    }


    company_date = paste('clients', company, company_date, '', sep = '/')
    clientele_s3 = clientele_s3[Key %like% company_date][Key != company_date]


    ### Download and Store  ----------------

    lapply(clientele_s3$Key, s3_retrieve_object, company_path = company_date, risk_bucket = risk_bucket)

    ### ZIP  ----------------

    zip::zip(paste0(company, ".zip"), "clients")

    ### Clean  ----------------

    unlink("clients", recursive = TRUE)


}



#' AWS S3 Bucket Download client external files
#'
#' Get the client's files from external sources in the S3 Risk bucket
#'
#' @param company the company's LID
#' @param date the date desired to download (or 'latest')
#' @param risk_bucket the aws bucket
#'
#' @author Alejandro Abraham <alejandro@liqi.it>
#' @import data.table
#' @import aws.s3
#' @import zip
#'
#' @returns directory imported from S3
#' @export
#'
#' @importFrom aws.s3 save_object
#'
s3_retrieve_externals = function(company, date, risk_bucket = "s3://risk-clientele/clients") {

    ## S3 Setup -----------------------------
    lid_company = company
    company_date = 'latest'

    risk_bucket = "s3://risk-clientele/"
    clientele_s3 = aws.s3::get_bucket_df(risk_bucket) |> as.data.table()
    clientele_s3
    snapshot = company_date

    extracted_part = clientele_s3$Key[grepl(company, clientele_s3$Key) & grepl("external", clientele_s3$Key)]

    snapshot_list = unique(regmatches(extracted_part, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", extracted_part)))
    company_date = max(snapshot_list)

    company_date = paste('clients', company, company_date, '', sep = '/')
    clientele_s3 = clientele_s3[Key %like% company_date][Key != company_date]


    lapply(clientele_s3$Key, s3_retrieve_object, company_path = company_date, risk_bucket = risk_bucket)

    ### ZIP  ----------------

    zip::zip(paste0(company, ".zip"), "clients")

    ### Clean  ----------------

    unlink("clients", recursive = TRUE)


}

