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
class ZGOOG_CL_STORAGE_CONT_REPO_V1 definition
  public
  inheriting from /GOOG/CL_STORAGE_V1
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_KEY_NAME type /GOOG/KEYNAME optional
      !IV_LOG_OBJ type BALOBJ_D optional
      !IV_LOG_SUBOBJ type BALSUBOBJ optional
    raising
      /GOOG/CX_SDK .
protected section.
private section.
ENDCLASS.



CLASS ZGOOG_CL_STORAGE_CONT_REPO_V1 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_key_name       = iv_key_name
                        iv_log_obj        = iv_log_obj
                        iv_log_subobj     = iv_log_subobj ).

    gv_useragent_suffix = '/Solution:Content-Repo-GCS'.

  ENDMETHOD.
ENDCLASS.
