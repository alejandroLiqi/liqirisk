#' Load SQL Query
#'
#' This function generates a SQL query based on a company name and a table name.
#'
#' @param company A string representing the company name.
#' @param table A string representing the table name.
#' @return A string representing the SQL query.
#'
#' @export


load_query = function(company, table) {

    query_template = paste0("
    with companies as (
        select *
        from ca_mycompany
        where name like '%", company, "%'
    ),
    ias as (
        -- invoice accounts
        select ia.*
        from invoice_invoiceaccount ia
            join companies on company_id = companies.id
    ),
    ics as (
        select ic.*
        from invoice_receiptdocument ic
            join ias on ic.account_id = ias.id
    ),
    icsd as (
        select rpd.*, i.time_rilevazione
        from ics i
        join invoice_receiptpaymentdetail as rpd
        on i.id = rpd.receipt_id
    ),
    iids as (
        -- invoice issued documents
        select iid.*,
        iih.cedente_sede_comune, iih.cedente_sede_indirizzo, iih.cedente_sede_cap, iih.cedente_sede_provincia,
        iih.cessionario_sede_comune, iih.cessionario_sede_indirizzo, iih.cessionario_sede_cap, iih.cessionario_sede_provincia
        from invoice_invoiceissueddocument iid
            join ias on ias.id = iid.account_id
            left join invoice_invoiceissuedxmlheader iih on iid.id = iih.invoice_id
    ),
    iixb as (
        -- invoice issued with bodyid
        select iids.*, iixb.id as body_id
        from iids
        left join invoice_invoiceissuedxmlbody iixb
            on iids.id = iixb.invoice_id
    ),
    iibeni as (
        select iixb.*,
            b.num_linea,
            b.descrizione,
            b.quantity,
            b.unita_misura,
            b.prezzo_unitario,
            b.prezzo_totale,
            b.aliquota_iva
        from iixb
            left join invoice_invoiceissuedxmlbeniservizidettagliolinee b
        on iixb.body_id = b.invoice_body_id
    ),
    iipag as (
        -- invoice issued with payment detail
        select iixb.*,
            dp.modalita_pagamento,
            dp.data_riferimento_termini_pagamento,
            dp.giorni_termini_pagamento,
            dp.data_scadenza_pagamento,
            dp.importo_pagamento
        from iixb
            left join invoice_invoiceissuedxmldatipagamento p
        on iixb.body_id = p.invoice_body_id
            left join invoice_invoiceissuedxmldettagliopagamento dp
        on p.id = dp.dati_pagamento_id
    ),
    irds as (
        -- invoice received documents
        select ird.*,
        irh.cedente_sede_comune, irh.cedente_sede_indirizzo, irh.cedente_sede_cap, irh.cedente_sede_provincia,
        irh.cessionario_sede_comune, irh.cessionario_sede_indirizzo, irh.cessionario_sede_cap, irh.cessionario_sede_provincia
        from invoice_invoicereceiveddocument ird
            join ias on ias.id = ird.account_id
            left join invoice_invoicereceivedxmlheader irh on ird.id = irh.invoice_id
    ),
    irxb as (
        select irds.*, irxb.id as body_id
        from irds
        left join invoice_invoicereceivedxmlbody irxb
            on irds.id = irxb.invoice_id
    ),
    irbeni as (
        select irxb.*,
            b.num_linea,
            b.descrizione,
            b.quantity,
            b.unita_misura,
            b.prezzo_unitario,
            b.prezzo_totale,
            b.aliquota_iva
        from irxb
            left join invoice_invoicereceivedxmlbeniservizidettagliolinee b
        on irxb.body_id = b.invoice_body_id
    ),
    irpag as (
        -- invoice received with payment detail
        select irxb.*,
            dp.modalita_pagamento,
            dp.data_riferimento_termini_pagamento,
            dp.giorni_termini_pagamento,
            dp.data_scadenza_pagamento,
            dp.importo_pagamento
        from irxb
            left join invoice_invoicereceivedxmldatipagamento p
        on irxb.body_id = p.invoice_body_id
            left join invoice_invoicereceivedxmldettagliopagamento dp
        on p.id = dp.dati_pagamento_id
    ),

    uba as (
        select ub.*, b.name
        from bankdatams_bankaccount ub
            join companies on company_id = companies.id
            left join bankdatams_bankinstitution b on ub.bank_institution_id = b.id
     ),
    ubt as (
        select ub.*, c.name as category
        from bankdatams_banktransaction ub
            join uba on ub.account_id = uba.id
            left join bankdatams_banktransactioncategorization c on ub.id = c.transaction_id
     ),
    ubb as (
        select ubb.*, b.name, b.iban
            from bankdatams_bankbalance ubb
            join uba b on ubb.account_id = b.id
     )

    select *
    from ", table, ";
  ")

    return(query_template)
}

#' Query Company Data
#'
#' This function executes a series of SQL queries based on a company name,
#' writes the results to CSV files, and optionally prints messages.
#'
#' @param company A string representing the company name.
#' @param conn A connection object representing the database connection.
#' @param BASE_PATH A string representing the base path for the output files. Default is "".
#' @param verbose A logical indicating whether to print messages. Default is TRUE.
#' @importFrom DBI dbGetQuery
#' @importFrom data.table fwrite
#'
#' @return Invisible NULL.
#'
#' @export

query_company_data = function(company, conn, BASE_PATH = "", verbose = TRUE) {

    for(i in 1:nrow(table_list)) {
        query = load_query(company = company, table = table_list$table[i])
        df = dbGetQuery(conn, query)
        final_name = paste0(BASE_PATH, table_list$filename[i], ".csv")
        fwrite(df, final_name)

        if(verbose) {
            print(paste("Written file", final_name))
        }
    }
}

#' Query Company Detail
#'
#' This function executes a SQL query based on a company name,
#' writes the results to a CSV file, and optionally prints a message.
#'
#' @param company A string representing the company name.
#' @param conn A connection object representing the database connection.
#' @param BASE_PATH A string representing the base path for the output file. Default is "".
#' @param verbose A logical indicating whether to print messages. Default is TRUE.
#' @importFrom DBI dbGetQuery
#' @importFrom data.table fwrite
#'
#' @return Invisible NULL.
#'
#' @export
#'

query_company_detail = function(company, conn, BASE_PATH = "", verbose = TRUE) {

    query = paste0("
    SELECT id, name, vat_number, country, region, cap, city, address
    FROM ca_mycompany;

    ")

    DTS = dbGetQuery(conn, query)
    final_name = paste0(BASE_PATH, 'company_detail', '.csv')
    fwrite(DTS, final_name)

    if(verbose) {
        print(paste("Written file", final_name))
    }
}


#' Get Company Table
#'
#' This function retrieves from liqi-prod server the companies tables,
#'
#' @param conn A connection object representing the database connection.
#' @param BASE_PATH A string representing the base path for the output file. Default is "".
#' @param verbose A logical indicating whether to print messages. Default is TRUE.
#' @importFrom DBI dbGetQuery
#' @importFrom data.table fwrite
#'
#' @return Invisible NULL.
#'
#' @export
#'

get_company_table = function(conn, download = FALSE, BASE_PATH = "", verbose = TRUE) {

    query = paste0("
    SELECT id, name, vat_number, country, region, cap, city, address
    FROM ca_mycompany;
    ")

    DTS = dbGetQuery(conn, query)

    if(download) {

        final_name = paste0(BASE_PATH, 'company_table', '.csv')
        fwrite(DTS, final_name)

        if(verbose) {
            print(paste("Written file", final_name))
        }

    }

    return(DTS)

}


#' SQL Open Connection
#'
#' This function connects to a SQL DB
#'
#' @param ssh the user
#' @param url the complete string to the DB
#'
#' @return Open SQL Connection
#'
#' @export
#'

open_conn = function(ssh = 'alejandro@prod.bastion.liqi.it', url = '5432:prod-db-primary-20230426144532573700000011.cct8rb1f18rq.eu-south-1.rds.amazonaws.com:5432') {

    command = paste0('ssh ', ssh, ' -L ', url)
    system(paste("osascript -e 'tell app \"Terminal\" to do script \"", command, "\"'"))

}

#' SQL Close Connection
#'
#' This function connects to a SQL DB
#'
#' @return Close SQL Connection
#'
#' @export
#'

close_conn = function() {

    Sys.sleep(1)

    system("osascript -e 'tell app \"Terminal\" to close (every window whose name contains \"prod.bastion.liqi.it\")'")
    Sys.sleep(1)

    system("osascript -e 'tell application \"Terminal\" to activate'")
    Sys.sleep(1)

    system("osascript -e 'tell application \"System Events\" to tell process \"Terminal\" to keystroke \"Terminate\"'")
    Sys.sleep(1)

    system("osascript -e 'tell application \"System Events\" to tell process \"Terminal\" to keystroke return'")
    Sys.sleep(1)

    system("osascript -e 'tell application \"Terminal\" to quit'")

}



#' Interrogate italian companies DB
#'
#' This function connects and queries the SQLite DB
#'
#' @param conn A connection object representing the database connection.
#' @param base sth.
#' @param search_parameters query parameters.
#'
#' @return A data.table with the query result
#'
#' @export
#'

assembleQuery = function(conn, base, search_parameters) {

    parameter_names = names(search_parameters)
    partial_queries = ""

    # Iterate over all the parameters to assemble the query
    for(k in 1:length(parameter_names)){
        # Check if the values are numeric or not
        if(is.numeric(search_parameters[[parameter_names[k]]])){
            values = paste(search_parameters[[parameter_names[k]]], collapse = ", ")
        } else {
            # Surround each value with single straight quotes if it's not numeric
            values = paste("'", search_parameters[[parameter_names[k]]], "'", sep = "", collapse = ", ")
        }

        filter_k = paste(parameter_names[k], "IN (", values, ")")

        # If there is more than 1 parameter, add an AND statement before the parameter
        if(k > 1){
            filter_k = paste("AND", filter_k)
        }

        partial_queries = paste(partial_queries, filter_k)
    }

    # Paste all together into a single query using a WHERE statement
    final_query = paste(base, " WHERE", partial_queries)

    # Print the assembled query to show how it looks like
    print(final_query)

    # Run the final query
    result = dbGetQuery(conn, final_query)

    # return the executed query
    return(result)
}

