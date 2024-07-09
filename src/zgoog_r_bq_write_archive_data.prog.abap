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
REPORT zgoog_r_bq_write_archive_data.

PARAMETERS: p_aobj  TYPE objct_tr01 OBLIGATORY,
            p_mtkey TYPE char20 OBLIGATORY.

START-OF-SELECTION.


  DATA: lt_admi_files TYPE STANDARD TABLE OF arkey WITH DEFAULT KEY.
  DATA: lv_arkey TYPE arkey.
  DATA: ls_arch_key TYPE rng_archiv.
  DATA: lt_range_arkey TYPE as_t_rng_archiv.
  DATA: lv_sy_subrc TYPE syst_subrc.
  DATA: lt_return TYPE bapiret2_t.

  FIELD-SYMBOLS: <lv_arkey> TYPE arkey.

  lv_arkey = '%' && p_aobj.

  "Modify the filter criteria based on your requirements
  SELECT archiv_key
    FROM admi_files
    INTO TABLE lt_admi_files
    WHERE archiv_key LIKE lv_arkey
    AND filename <> ''.

  ls_arch_key-sign = 'I'.
  ls_arch_key-option = 'EQ'.

  LOOP AT lt_admi_files ASSIGNING <lv_arkey>.
    ls_arch_key-low = <lv_arkey>.
    APPEND ls_arch_key TO lt_range_arkey.
  ENDLOOP.

  zgoog_cl_archive_utility=>write_archive_filedata_to_bq(
    EXPORTING
      iv_object      = p_aobj
      iv_mass_tr_key = p_mtkey
   IMPORTING
      ev_error_code  = lv_sy_subrc
      et_return      = lt_return
    CHANGING
      ct_range_arkey = lt_range_arkey ).

  WRITE: / | Return code:  { lv_sy_subrc } |.
  WRITE: / | Messages: |.

  FIELD-SYMBOLS: <ls_bapiret2> TYPE bapiret2.
  LOOP AT lt_return ASSIGNING <ls_bapiret2>.
    WRITE: / | { zgoog_cl_archive_utility=>get_msgtext_frombapiret2( <ls_bapiret2> ) } |.
  ENDLOOP.
