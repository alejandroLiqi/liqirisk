#' AWS S3 Bucket Download client's file
#'
#' Get a file from the client's directory in the S3 Risk bucket
#'
#' @param object_path the object's full path in the S3 bucket
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

s3_retrieve_directory = function(company, date, dest_path, data_type, risk_bucket = "s3://risk-clientele/clients", zipped = FALSE) {

    ## S3 Setup -----------------------------
    lid_company = company
    company_date = date

    risk_bucket = "s3://risk-clientele/"
    clientele_s3 = aws.s3::get_bucket_df(risk_bucket) |> data.table::as.data.table()
    clientele_s3
    snapshot = company_date

    ### INTERNALS  ----------------
    if(snapshot == "latest") {
        extracted_part = clientele_s3$Key[grepl(lid_company, clientele_s3$Key)]
        snapshot_list = unique(regmatches(extracted_part, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", extracted_part)))
        company_date = max(snapshot_list)
    } else {
        company_date = snapshot
    }

    company_date = paste(lid_company, company_date, 'resources/data/raw', data_type, '', sep = '/')
    clientele_s3 = clientele_s3[Key %like% company_date][Key != company_date]

    ### Download and Store  ----------------
    dir.create(dest_path, showWarnings = FALSE)
    lapply(clientele_s3$Key, s3_retrieve_object, company_path = company_date, dest_path = dest_path, risk_bucket = risk_bucket)

    ### ZIP  ----------------

    if(zipped) {

        zip::zip(paste0(dest_path, "/", company, ".zip"), dest_path)

        ### Clean  ----------------
        unlink(dest_path, recursive = TRUE)

    }

}




