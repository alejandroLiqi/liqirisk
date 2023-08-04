
# Query Tables
table_list = data.table::data.table(
    table = c("irbeni", "iibeni", "irpag", "iipag", "irds", "iids", "ubt", "ubb", "uba", "ics", "icsd"),
    filename = c("ricevute_beni", "emesse_beni", "ricevute_pag", "emesse_pag", "ricevute", "emesse", "transazioni", "saldi", "account", "corrispettivi", "corrispettivi_detail")
)

# Save the data table as an R data file
save(table_list, file = "data/table_list.rda")
