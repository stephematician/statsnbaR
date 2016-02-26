if (!file.exists('inst'))
    dir.create('inst')
if (!file.exists('inst/man'))
    dir.create('inst/man')

manual_temp <- paste0(tempfile(), '.pdf')
system(paste('R CMD Rd2pdf --no-preview -o',
       manual_temp,
       normalizePath('.')))
file.rename(manual_temp,
            file.path(normalizePath('./inst/man'),
                      'statsnbaR.pdf'))

