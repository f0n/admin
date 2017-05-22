&project_id
 SIMULATION_NAME = 'WRF Model Simulation'
 USER_DESC = 'WRF User'
/
&filetimespec
 START_YEAR = 2002,
 START_MONTH = 05,
 START_DAY = 13,
 START_HOUR = 12,
 START_MINUTE = 00,
 START_SECOND = 00,
 END_YEAR = 2002,
 END_MONTH = 05,
 END_DAY = 14,
 END_HOUR = 00,
 END_MINUTE = 00,
 END_SECOND = 00,
 INTERVAL = 10800
/
&hgridspec
 NUM_DOMAINS = 1
 XDIM = 144
 YDIM = 144
 PARENT_ID = 1,
 RATIO_TO_PARENT = 1,
 DOMAIN_ORIGIN_LLI = 1,
 DOMAIN_ORIGIN_LLJ = 1,
 DOMAIN_ORIGIN_URI = 144,
 DOMAIN_ORIGIN_URJ = 144,
 MAP_PROJ_NAME = 'polar',
 MOAD_KNOWN_LAT = 42.0,
 MOAD_KNOWN_LON = -95.0,
 MOAD_STAND_LATS = 42.0, 90.0,
 MOAD_STAND_LONS = -95.0
 MOAD_DELTA_X = 12000.
 MOAD_DELTA_Y = 12000.
 SILAVWT_PARM_WRF = 0.
 TOPTWVL_PARM_WRF = 2.
/
&sfcfiles
 TOPO_30S = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/topo_30s',
 LANDUSE_30S = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/landuse_30s',
 SOILTYPE_TOP_30S = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/soiltype_top_30s',
 SOILTYPE_BOT_30S = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/soiltype_bot_30s',
 GREENFRAC = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/greenfrac',
 SOILTEMP_1DEG = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/soiltemp_1deg',
 ALBEDO_NCEP = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/albedo_ncep',
 MAXSNOWALB = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/maxsnowalb',
 ISLOPE = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/GEOG/islope',
/
&interp_control
 NUM_ACTIVE_SUBNESTS = 1,
 ACTIVE_SUBNESTS = 2,3,4,
 PTOP_PA = 5000,
 HINTERP_METHOD = 1,
 LSM_HINTERP_METHOD = 1,
 NUM_INIT_TIMES = 1, 
 INIT_ROOT = 'ETA',
 LBC_ROOT = 'ETA',
 LSM_ROOT = '',
 CONSTANTS_FULL_NAME = '',
 VERBOSE_LOG = .false.,
 OUTPUT_COORD = 'ETAP',
 LEVELS = 1.000, 0.993, 0.980, 0.966, 0.950, 0.933,
              0.913, 0.892, 0.869, 0.844, 0.816, 0.786,
              0.753, 0.718, 0.680, 0.639, 0.596, 0.550,
              0.501, 0.451, 0.398, 0.345, 0.290, 0.236,
              0.188, 0.145, 0.108, 0.075, 0.046, 0.021,
              0.000,
/
&si_paths
 ANALPATH = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/extprd',
 LBCPATH = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/extprd',
 LSMPATH = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/extprd',
 CONSTANTS_PATH = '/home/glendeni/DRJACK/INSTALLOVER/WRF/wrfsi/extdata/extprd',
/