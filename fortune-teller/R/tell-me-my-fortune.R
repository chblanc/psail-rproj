# tell-me-my-fortune.R
#
# A goofy program that prepares and prints a statement for any poor
# unfortunate soul duped into calling it
#
# Usage:
#     Rscript R/tell-me-my-fortune.R -n carlos

# packages -------------------------------------------------------------------
suppressPackageStartupMessages({
  require(fortunes)
  require(argparser)
  require(futile.logger)
})

# main-block -----------------------------------------------------------------

if (!interactive()) {
  
  # argument-parsing --------------------------------------
  parser <- argparser::arg_parser('Get your very own personalized fortune.
                                  Program must be run from the project root
                                  directory.')
  parser <- argparser::add_argument(parser, '--name', help='your name')
  args <- argparser::parse_args(parser)
  
  # argument-parsing --------------------------------------
  flog.info('Preparing a fortune for: %s ... ', args$name)
  Sys.sleep(5)
  fortunes::fortune()
}
