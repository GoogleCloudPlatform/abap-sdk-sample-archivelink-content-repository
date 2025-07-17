*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGOOG_ALINK_BQ..................................*
DATA:  BEGIN OF STATUS_ZGOOG_ALINK_BQ                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGOOG_ALINK_BQ                .
CONTROLS: TCTRL_ZGOOG_ALINK_BQ
            TYPE TABLEVIEW USING SCREEN '1028'.
*.........table declarations:.................................*
TABLES: *ZGOOG_ALINK_BQ                .
TABLES: ZGOOG_ALINK_BQ                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
