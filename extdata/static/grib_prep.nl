&filetimespec
 START_YEAR = 2001
 START_MONTH = 07
 START_DAY = 09
 START_HOUR = 12
 START_MINUTE = 00
 START_SECOND = 00
 END_YEAR = 2001
 END_MONTH = 07
 END_DAY = 10
 END_HOUR = 12
 END_MINUTE = 00
 END_SECOND = 00
 INTERVAL = 10800
/
&gpinput_defs
 SRCNAME = 'ETA', 'GFS', 'GFSN', 'GFSA', 'AVN', 'RUCH', 'RUCP', 'NNRP', 'NNRPSFC', 'SST'
 SRCVTAB = 'ETA', 'GFS', 'GFSN', 'GFSA', 'AVN', 'RUCH', 'RUCP', 'NNRP', 'NNRPSFC', 'SST'
 SRCPATH = '/home/admin/DRJACK/RASP/RUN/ETA/GRIB', 
		'/home/admin/DRJACK/RASP/RUN/GFS/GRIB', 
		'/home/admin/DRJACK/RASP/RUN/GFSN/GRIB', 
  		'/home/admin/DRJACK/RASP/RUN/GFSA/GRIB', 
		'/home/admin/DRJACK/RASP/RUN/AVN/GRIB', 
		'/home/admin/DRJACK/RASP/RUN/RUCH/GRIB', 
		'/home/admin/DRJACK/RASP/RUN/RUCP/GRIB', 
		'/path/to/nnrp/grib', 
		'/path/to/nnrp/sfc/grib', 
		'/public/data/grids/ncep/sst/grib'
 SRCCYCLE = 6, 6, 6, 6, 12, 12, 24
 SRCDELAY = 24, 4, 24, 3, 0, 0, 36
/
