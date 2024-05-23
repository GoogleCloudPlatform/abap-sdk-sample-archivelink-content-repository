*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGOOG_CONT_REPO.................................*
DATA:  BEGIN OF STATUS_ZGOOG_CONT_REPO               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGOOG_CONT_REPO               .
CONTROLS: TCTRL_ZGOOG_CONT_REPO
            TYPE TABLEVIEW USING SCREEN '1028'.
*.........table declarations:.................................*
TABLES: *ZGOOG_CONT_REPO               .
TABLES: ZGOOG_CONT_REPO                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
