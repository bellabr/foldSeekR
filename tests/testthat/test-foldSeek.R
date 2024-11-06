

test_that("ticket request success for all databases", {
  fasta_filepath <- file.path(getwd(), "Q40674.fasta")
  fasta <- ">tr|Q40674|Q40674_ORYSA Peptidyl-prolyl cis-trans isomerase OS=Oryza sativa OX=4530 GN=Cyp2 PE=2 SV=1
MSNTRVFFDMTVGGAPAGRIVMELYAKDVPRTAENFRALCTGEKGVGKSGKPLHYKGSTF
HRVIPEFMCQGGDFTRGNGTGGESIYGEKFADEVFKFKHDSPGILSTANAGPNTNGSQFF
ICTVPCSWLDGKHVVFGRVVEGMDVVKAIEKVGSRGGSTAKPVVIADCGQLS
"

  #ticket <- ticket(filepath = fasta)

  #print(ticket)

  #testthat::expect_type(ticket, "ticket")


})

# test_that("ticket request success for one database", {
#   path = getwd()
#
#   ticket <- ticket(filepath = path, databases = c('afdb50'))
#
#   testthat::expect_type(ticket, "ticket")
#
#
#
# })
#
# test_that("ticket request failure", {
#   path = getwd() # TODO
#
#   testthat::expect_error(
#     ticket(filepath = path, databases = c('afdb50')),
#     "Invalid filepath. Check the file is present."
#   )
#
# })



