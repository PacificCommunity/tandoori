## code to prepare `EXAMPLEDATASET` dataset goes here

om_dir <- "P:/SLOTH/BET/tandoori_dev_data/oms/A0B0C0D0EF.Rdata"
load(om_dir)

usethis::use_data(bet, bet_fish, srr_devs, mfcl_pop_n, mfcl_fmort, mfcl_catch_wt, overwrite = TRUE)
