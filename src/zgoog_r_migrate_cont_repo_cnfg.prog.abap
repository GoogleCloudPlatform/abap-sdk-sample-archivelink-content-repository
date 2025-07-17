**********************************************************************
*  Copyright 2024 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************
REPORT zgoog_r_migrate_cont_repo_cnfg.


TABLES: zgoog_cont_repo.

SELECT-OPTIONS: s_repo FOR zgoog_cont_repo-archive_id.
SELECTION-SCREEN COMMENT /01(73) TEXT-001.

START-OF-SELECTION.

  DATA: lt_z_config TYPE STANDARD TABLE OF zgoog_cont_repo.

  SELECT * FROM zgoog_cont_repo
    INTO TABLE lt_z_config
    WHERE archive_id IN s_repo[].

  WRITE: / | { lines( lt_z_config ) } Records Found in ZGOOG_CONT_REPO |.
  IF lt_z_config IS INITIAL.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS: <ls_z_config> TYPE zgoog_cont_repo.


  DATA: lt_ga_config TYPE STANDARD TABLE OF /goog/cont_repo.
  DATA: ls_ga_config TYPE /goog/cont_repo.
  ls_ga_config-file_naming = 'A'.
  LOOP AT lt_z_config ASSIGNING <ls_z_config>.

    MOVE-CORRESPONDING  <ls_z_config> TO ls_ga_config.
    APPEND ls_ga_config TO lt_ga_config.

  ENDLOOP.

  MODIFY /goog/cont_repo FROM TABLE lt_ga_config.
  COMMIT WORK.
  IF sy-subrc = 0.
    WRITE / |Configurations copied over to /GOOG/CONT_REPO|.
  ELSE.
    WRITE / |Failed to update /GOOG/CONT_REPO table|.
  ENDIF.
