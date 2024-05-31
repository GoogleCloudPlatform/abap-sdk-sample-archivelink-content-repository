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
