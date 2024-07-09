*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGOOG_ARCH_BQ...................................*
DATA:  BEGIN OF STATUS_ZGOOG_ARCH_BQ                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGOOG_ARCH_BQ                 .
CONTROLS: TCTRL_ZGOOG_ARCH_BQ
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGOOG_ARCH_BQ                 .
TABLES: ZGOOG_ARCH_BQ                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
