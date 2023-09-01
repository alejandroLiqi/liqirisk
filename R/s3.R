

#' AWS S3 Bucket Download client's file
#'
#' Get a file from the client's directory in the S3 Risk bucket
#'
#' @param object_path the object's full path in the S3 bucket
#' @param dest_path the local path
#' @param company_path the bucket's S3 internal path of the company
#' @param complete recreate the bucket path locally
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

s3_retrieve_object = function(object_path, company_path, dest_path = 'clients', risk_bucket = "s3://risk-clientele/clients") {
    # Determine the save path
    object_dir = dirname(object_path)
    object_file = basename(object_path)

    if(dest_path == 'clients') {

        save_dir = file.path(dest_path, gsub(paste0("^.*", company_path, '/'), "", object_dir))
        save_path = file.path(save_dir, object_file)
        # Ensure the save directory exists
        dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    } else {
        save_path = save_path = file.path(dest_path, object_file)
    }

    aws.s3::save_object(
        object = object_path,
        bucket = risk_bucket,
        file = save_path
    )
}


#' AWS S3 Bucket Download client directory
#'
#' Get the client's  directory from the S3 Risk bucket
#'
#' @param company the company's LID
#' @param date the date desired to download (or 'latest')
#' @param dest_path the local path
#' @param data_type internal or external
#' @param zipped create zip folder
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

s3_retrieve_directory = function(company, date, dest_path = NULL, data_type = 'full', risk_bucket = "s3://risk-clientele/clients", zipped = TRUE) {

    ## S3 Setup -----------------------------
    lid_company = company
    company_date = date

    dest_path

    if(is.null(dest_path)) {
        dest_path = paste(lid_company, company_date, sep = '_')
    }


    ### Retrieve Bucket Data

    risk_bucket = "s3://risk-clientele/"
    clientele_s3 = aws.s3::get_bucket_df(risk_bucket) |> data.table::as.data.table()
    clientele_s3
    snapshot = company_date


    ### Filter Company & Date

    if(snapshot == "latest") {

        extracted_part = clientele_s3$Key[grepl(lid_company, clientele_s3$Key)]
        snapshot_list = unique(regmatches(extracted_part, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", extracted_part)))
        company_date = max(snapshot_list)

        cat('Lastest company_date available is', company_date)

    } else {

        company_date = snapshot

    }

    company_filter = paste(lid_company, company_date, sep = '/')
    clientele_s3 = clientele_s3[Key %like% company_filter]
    clientele_s3 = clientele_s3[Key != company_filter]


    ### Download Files

    if(data_type == "internal") {

        string = paste(lid_company, company_date, 'resources/data/raw', data_type, '', sep = '/')
        clientele_s3 = clientele_s3[Key %like% string]

        dest_path = paste0(dest_path, '/', data_type)

        dir.create(dest_path, showWarnings = FALSE, recursive = TRUE)
        lapply(clientele_s3$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path, risk_bucket = risk_bucket)


    } else if(data_type == "external") {

        string = paste(lid_company, company_date, 'resources/data/raw', data_type, '', sep = '/')
        clientele_s3 = clientele_s3[Key %like% string]

        dest_path = paste0(dest_path, '/', data_type)

        dir.create(dest_path, showWarnings = FALSE, recursive = TRUE)
        lapply(clientele_s3$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path, risk_bucket = risk_bucket)


    } else if(data_type == 'both') {

        string_i = paste(lid_company, company_date, 'resources/data/raw/internal', '', sep = '/')
        clientele_s3_i = clientele_s3[Key %like% string_i]

        dest_path_i = paste0(dest_path, '/internal')

        dir.create(dest_path_i, showWarnings = FALSE, recursive = TRUE)
        lapply(clientele_s3_i$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path_i, risk_bucket = risk_bucket)


        string_e = paste(lid_company, company_date, 'resources/data/raw/external', '', sep = '/')
        clientele_s3_e = clientele_s3[Key %like% string_e]

        dest_path_e = paste0(dest_path, '/external')

        dir.create(dest_path_e, showWarnings = FALSE, recursive = TRUE)
        lapply(clientele_s3_e$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path_e, risk_bucket = risk_bucket)


    } else if(data_type == 'full') {

        # Internals
        string_i = paste(lid_company, company_date, 'resources/data/raw/internal', '', sep = '/')
        clientele_s3_i = clientele_s3[Key %like% string_i]

        dest_path_i = paste0(dest_path, '/internal')

        dir.create(dest_path_i, showWarnings = FALSE, recursive = TRUE)
        lapply(clientele_s3_i$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path_i, risk_bucket = risk_bucket)

        # Externals
        string_e = paste(lid_company, company_date, 'resources/data/raw/external', '', sep = '/')
        clientele_s3_e = clientele_s3[Key %like% string_e]

        dest_path_e = paste0(dest_path, '/external')

        dir.create(dest_path_e, showWarnings = FALSE, recursive = TRUE)
        lapply(clientele_s3_e$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path_e, risk_bucket = risk_bucket)

        # Docs
        string_d = paste(lid_company, company_date, 'docs', '', sep = '/')
        clientele_s3_d = clientele_s3[Key %like% string_d]

        dest_path_d = paste0(dest_path, '/docs')
        lapply(clientele_s3_d$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path_d, risk_bucket = risk_bucket)

        # Params
        string_p = paste(lid_company, company_date, 'params', '', sep = '/')
        clientele_s3_p = clientele_s3[Key %like% string_p]

        dest_path_p = paste0(dest_path, '/params')
        lapply(clientele_s3_p$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path_p, risk_bucket = risk_bucket)


    } else if(data_type == 'docs') {

        # Docs
        string_d = paste(lid_company, company_date, 'docs', '', sep = '/')
        clientele_s3_d = clientele_s3[Key %like% string_d]

        dest_path_d = paste0(dest_path, '/docs')
        lapply(clientele_s3_d$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path_d, risk_bucket = risk_bucket)


    } else if(data_type == 'params') {

        # Params
        string_p = paste(lid_company, company_date, 'params', '', sep = '/')
        clientele_s3_p = clientele_s3[Key %like% string_p]

        dest_path_p = paste0(dest_path, '/params')
        lapply(clientele_s3_p$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path_p, risk_bucket = risk_bucket)


    } else {

        paste(data_type, 'Not a valid data_type option.')

    }


    ### ZIP  ----------------

    if(zipped) {

        files_and_subfolders = list.files(dest_path, full.names = TRUE, recursive = TRUE)
        zip_file = paste0(dest_path, '.zip')

        # Create a zip file containing all the files and subfolders
        utils::zip(zip_file, files_and_subfolders)

        # Check if the zip file was created successfully
        if (file.exists(zip_file)) {
            cat("Zip file created successfully:", zip_file, "\n")
        } else {
            cat("Failed to create zip file:", zip_file, "\n")
        }

        ### Clean  ----------------
        unlink(dest_path, recursive = TRUE)

    }

    cat('Download Completed:', lid_company, 'for date', company_date)

}




#' AWS S3 Client lastest date
#'
#' Get the client's  directory from the S3 Risk bucket
#'
#' @param company the company's LID
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

s3_client_latest = function(company, risk_bucket = "s3://risk-clientele/clients") {

    ## S3 Setup -----------------------------
    lid_company = company

    ### Retrieve Bucket Data

    risk_bucket = "s3://risk-clientele/"
    clientele_s3 = aws.s3::get_bucket_df(risk_bucket) |> data.table::as.data.table()


    ### Filter Company & Date

    extracted_part = clientele_s3$Key[grepl(lid_company, clientele_s3$Key)]
    snapshot_list = unique(regmatches(extracted_part, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", extracted_part)))
    company_date = max(snapshot_list)

    cat('Lastest company_date available is', company_date)

    return(company_date)

}




