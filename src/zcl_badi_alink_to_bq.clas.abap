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
class ZCL_BADI_ALINK_TO_BQ definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_ALINK_LINK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BADI_ALINK_TO_BQ IMPLEMENTATION.


  METHOD if_alink_link~link_changed.

    DATA: lv_bq_trkey TYPE /goog/trkey.

    SELECT SINGLE bq_transf_key FROM zgoog_alink_bq
      INTO lv_bq_trkey
      WHERE archive_id = link-archiv_id.

    IF lv_bq_trkey IS INITIAL.
      RETURN.
    ENDIF.

    SET UPDATE TASK LOCAL.

    CALL FUNCTION 'Z_SEND_ARCHIVE_LINK_TO_BQ' IN BACKGROUND TASK AS SEPARATE UNIT
      EXPORTING
        link     = link
        tab_name = tab_name
        bq_trkey = lv_bq_trkey.

  ENDMETHOD.
ENDCLASS.
