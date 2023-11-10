REPORT zbackup4abap.

" v3.00.00
" 逻辑整理

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: tadir, sscrfields.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
INTERFACE lif_backup4abap_object DEFERRED.
CLASS lcl_backup4abap_filter_time DEFINITION DEFERRED.
CLASS lcl_backup4abap_filter_package DEFINITION DEFERRED.
CLASS lcl_backup4abap_screen_option DEFINITION DEFERRED.
CLASS lcl_backup4abap_folder DEFINITION DEFERRED.

CLASS lcl_backup4abap_objects DEFINITION DEFERRED.
CLASS lcl_backup4abap_object_prog DEFINITION DEFERRED.
CLASS lcl_backup4abap_object_fugr DEFINITION DEFERRED.
CLASS lcl_backup4abap_object_clas DEFINITION DEFERRED.
CLASS lcl_backup4abap_object_text DEFINITION DEFERRED.
CLASS lcl_backup4abap_object_msag DEFINITION DEFERRED.

CLASS lcl_backup4abap_object_ddls DEFINITION DEFERRED.

CLASS lcl_backup4abap_export DEFINITION DEFERRED.

" TOOLS
CLASS lcl_progress_bar DEFINITION DEFERRED.

DEFINE _stop.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
END-OF-DEFINITION.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
INTERFACE lif_backup4abap_object.

  TYPES: BEGIN OF ty_split_limit_normal,
           name TYPE string,
         END OF ty_split_limit_normal.
  TYPES: BEGIN OF ty_files,
           name TYPE string,
           data TYPE xstring,
         END OF ty_files.
  DATA: parent_folder TYPE string,
        files         TYPE TABLE OF ty_files.

  " loadfiles
  METHODS loadfiles.

  " split folder
  METHODS split_folder IMPORTING i_limit            TYPE data
                       RETURNING VALUE(rv_filename) TYPE string.

  " load to zip
  METHODS load2zip IMPORTING io_zip TYPE REF TO cl_abap_zip.

ENDINTERFACE.
CLASS lcl_backup4abap_filter_package DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_tdevc_parentcl,
             parentcl TYPE tdevc-parentcl,
           END OF ty_tdevc_parentcl.
    TYPES: tt_tdevc_parentcl TYPE TABLE OF ty_tdevc_parentcl.
    CLASS-DATA: lt_range TYPE RANGE OF tadir-devclass.

    CLASS-METHODS set EXCEPTIONS e_no_filter.
    CLASS-METHODS fix IMPORTING it_package TYPE tt_tdevc_parentcl.
ENDCLASS.
CLASS lcl_backup4abap_filter_time DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_delt_log,
             object TYPE trobjtype, " 对象类型
             ddate  TYPE datum,     " 增量日期
             dtime  TYPE uzeit,     " 增量时间
           END OF ty_delt_log,
           tt_delt_log TYPE STANDARD TABLE OF ty_delt_log WITH EMPTY KEY.
    DATA: lt_delt_log TYPE tt_delt_log.

    CLASS-METHODS factory IMPORTING iv_limit       TYPE sy-uname DEFAULT sy-uname
                          RETURNING VALUE(ro_objt) TYPE REF TO lcl_backup4abap_filter_time.
    METHODS constructor IMPORTING iv_limit TYPE sy-uname.

    METHODS get IMPORTING iv_object          TYPE trobjtype
                RETURNING VALUE(rs_delt_log) TYPE ty_delt_log.
  PROTECTED SECTION.
    CLASS-DATA objt TYPE REF TO lcl_backup4abap_filter_time.
    CONSTANTS c_delta_store_id_fix_part TYPE char18 VALUE 'DeltaDownload'.
ENDCLASS.
CLASS lcl_backup4abap_screen_option DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_option  TYPE char30,
           tt_options TYPE STANDARD TABLE OF ty_option WITH EMPTY KEY.
    CLASS-DATA: options      TYPE tt_options,
                option_seted TYPE abap_bool.

    CLASS-METHODS set_option IMPORTING iv_option TYPE ty_option.
    CLASS-METHODS selected_option RETURNING VALUE(rt_options) TYPE tt_options.

ENDCLASS.
CLASS lcl_backup4abap_folder DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_folder,
             tag    TYPE char10,
             folder TYPE char10,
           END OF ty_folder.
    CLASS-DATA: folder TYPE TABLE OF ty_folder.

    CLASS-METHODS: class_constructor.

    " 由于需要在 TEXT 中调用，将原本分散的分割方法合并到此
    CLASS-METHODS: split_folder_prog IMPORTING name TYPE string subc TYPE reposrc-subc
                                     RETURNING VALUE(rv_folder) TYPE string.
    CLASS-METHODS: split_folder_clas IMPORTING name             TYPE string
                                     RETURNING VALUE(rv_folder) TYPE string.
    CLASS-METHODS: split_folder_fugr IMPORTING name             TYPE string
                                     RETURNING VALUE(rv_folder) TYPE string.
    CLASS-METHODS: split_folder_ddls IMPORTING name             TYPE string
                                     RETURNING VALUE(rv_folder) TYPE string.
ENDCLASS.
CLASS lcl_backup4abap_objects DEFINITION.
  PUBLIC SECTION.
    CONSTANTS c_extension_abap TYPE string VALUE 'abap'.
    CONSTANTS c_extension_txt TYPE string VALUE 'txt'.
    CONSTANTS c_extension_ddl TYPE string VALUE 'ddl'.
    CONSTANTS c_newline TYPE abap_cr_lf VALUE cl_abap_char_utilities=>cr_lf.
    CONSTANTS c_tab TYPE abap_char1 VALUE cl_abap_char_utilities=>horizontal_tab.

    " init
    METHODS: constructor.

    " delta load file
    METHODS: is_delta RETURNING VALUE(rv_bool) TYPE abap_bool.

    " set map file

    " set log file

  PROTECTED SECTION.
    DATA: o_pb       TYPE REF TO lcl_progress_bar,
          o_conv_out TYPE REF TO cl_abap_conv_out_ce.

ENDCLASS.
CLASS lcl_backup4abap_object_prog DEFINITION INHERITING FROM lcl_backup4abap_objects CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_backup4abap_object.

    ALIASES loadfiles FOR lif_backup4abap_object~loadfiles.
    ALIASES load2zip FOR lif_backup4abap_object~load2zip.

    METHODS: constructor.

  PROTECTED SECTION.
    ALIASES split_folder FOR lif_backup4abap_object~split_folder.
    ALIASES parent_folder FOR lif_backup4abap_object~parent_folder.
    ALIASES files FOR lif_backup4abap_object~files.

    TYPES: BEGIN OF ty_split_limit,
             name TYPE string,
             subc TYPE reposrc-subc,
           END OF ty_split_limit.
ENDCLASS.
CLASS lcl_backup4abap_object_fugr DEFINITION INHERITING FROM lcl_backup4abap_objects CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_backup4abap_object.

    ALIASES loadfiles FOR lif_backup4abap_object~loadfiles.
    ALIASES load2zip FOR lif_backup4abap_object~load2zip.

    TYPES: BEGIN OF ty_function,
             functionname        TYPE tfdir-funcname,
             functiongroup       TYPE enlfdir-area,
             includenumber       TYPE tfdir-include,
             functionmaininclude TYPE tfdir-funcname,
             functiontitle       TYPE tftit-stext,
             progname            TYPE tfdir-pname,

             uname               TYPE reposrc-unam,
             udate               TYPE reposrc-udat,
             utime               TYPE reposrc-utime,

             reportname          TYPE reposrc-progname,
           END OF ty_function,
           tt_function TYPE STANDARD TABLE OF ty_function WITH EMPTY KEY.
    TYPES: tt_range_area TYPE RANGE OF reposrc-progname.

    METHODS: constructor.

    METHODS: fix_more IMPORTING io_func TYPE REF TO data.
    METHODS: get_more IMPORTING it_range_area TYPE tt_range_area.

  PROTECTED SECTION.
    ALIASES split_folder FOR lif_backup4abap_object~split_folder.
    ALIASES parent_folder FOR lif_backup4abap_object~parent_folder.
    ALIASES files FOR lif_backup4abap_object~files.

    TYPES: ty_split_limit TYPE string.
ENDCLASS.
CLASS lcl_backup4abap_object_clas DEFINITION INHERITING FROM lcl_backup4abap_objects CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_backup4abap_object.

    ALIASES loadfiles FOR lif_backup4abap_object~loadfiles.
    ALIASES load2zip FOR lif_backup4abap_object~load2zip.

    TYPES: BEGIN OF ty_type,
             type   TYPE seop_include_ext_app,
             exline TYPE i,
           END OF ty_type.
    DATA: BEGIN OF ls_class_key,
            classnamelength   TYPE i,
            publicclasskey    TYPE string,
            privateclasskey   TYPE string,
            protectedclasskey TYPE string,
            textelementkey    TYPE string,
            typesclasskey     TYPE string,
          END OF ls_class_key.
    DATA: lt_type TYPE TABLE OF ty_type.

    DATA: o_source   TYPE REF TO object,
          o_instance TYPE REF TO object.

    METHODS: constructor.

  PROTECTED SECTION.
    ALIASES split_folder FOR lif_backup4abap_object~split_folder.
    ALIASES parent_folder FOR lif_backup4abap_object~parent_folder.
    ALIASES files FOR lif_backup4abap_object~files.

    TYPES: ty_split_limit TYPE string.
ENDCLASS.
CLASS lcl_backup4abap_object_text DEFINITION INHERITING FROM lcl_backup4abap_objects CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_backup4abap_object.

    ALIASES loadfiles FOR lif_backup4abap_object~loadfiles.
    ALIASES load2zip FOR lif_backup4abap_object~load2zip.

    METHODS: constructor.

  PROTECTED SECTION.
    ALIASES split_folder FOR lif_backup4abap_object~split_folder.
    ALIASES parent_folder FOR lif_backup4abap_object~parent_folder.
    ALIASES files FOR lif_backup4abap_object~files.

    TYPES: BEGIN OF ty_split_limit,
             object   TYPE tadir-object,
             progname TYPE string,
           END OF ty_split_limit.
ENDCLASS.
CLASS lcl_backup4abap_object_msag DEFINITION INHERITING FROM lcl_backup4abap_objects CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_backup4abap_object.

    ALIASES loadfiles FOR lif_backup4abap_object~loadfiles.
    ALIASES load2zip FOR lif_backup4abap_object~load2zip.

    TYPES: BEGIN OF ty_arbgb,
             arbgb TYPE t100u-arbgb,
             datum TYPE t100u-datum,
           END OF ty_arbgb.

    METHODS: constructor.

  PROTECTED SECTION.
    ALIASES split_folder FOR lif_backup4abap_object~split_folder.
    ALIASES parent_folder FOR lif_backup4abap_object~parent_folder.
    ALIASES files FOR lif_backup4abap_object~files.

    TYPES: BEGIN OF ty_split_limit,
             object   TYPE tadir-object,
             progname TYPE string,
           END OF ty_split_limit.
ENDCLASS.
CLASS lcl_backup4abap_object_ddls DEFINITION INHERITING FROM lcl_backup4abap_objects CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_backup4abap_object.

    ALIASES loadfiles FOR lif_backup4abap_object~loadfiles.
    ALIASES load2zip FOR lif_backup4abap_object~load2zip.

    METHODS: constructor.

  PROTECTED SECTION.
    ALIASES split_folder FOR lif_backup4abap_object~split_folder.
    ALIASES parent_folder FOR lif_backup4abap_object~parent_folder.
    ALIASES files FOR lif_backup4abap_object~files.

    TYPES: ty_split_limit TYPE string.
ENDCLASS.



CLASS lcl_backup4abap_export DEFINITION.
  PUBLIC SECTION.
    DATA: file_path TYPE string.

    CLASS-METHODS factory RETURNING VALUE(ro_objt) TYPE REF TO lcl_backup4abap_export.

    METHODS: constructor.

    METHODS: path EXCEPTIONS e_no_choese.
    METHODS: download EXCEPTIONS e_no_file.

    METHODS: main EXCEPTIONS e_no_screen_choese.

  PROTECTED SECTION.
    CLASS-DATA objt TYPE REF TO lcl_backup4abap_export.
    DATA: o_zip TYPE REF TO cl_abap_zip.
ENDCLASS.

CLASS lcl_progress_bar DEFINITION.
  PUBLIC SECTION.

    DATA: count     TYPE i,
          base_desc TYPE string,
          curr      TYPE i.

    METHODS constructor IMPORTING " i_count     TYPE i OPTIONAL
                          i_base_desc TYPE string OPTIONAL.

    METHODS add IMPORTING i_add  TYPE i DEFAULT 1
                          i_desc TYPE data OPTIONAL.
  PRIVATE SECTION.
    DATA: percent     TYPE p DECIMALS 0 LENGTH 5,
          percent_old TYPE p DECIMALS 0 LENGTH 5.
    METHODS display IMPORTING desc TYPE data.
ENDCLASS.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_pack FOR FIELD s_pack.
    SELECT-OPTIONS: s_pack FOR tadir-devclass.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_delt FOR FIELD p_delt.
    PARAMETERS: p_delt TYPE c AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME.
  " 代码相关

  " report
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_prog FOR FIELD p_prog.
    PARAMETERS: p_prog AS CHECKBOX DEFAULT ''."'X'.
  SELECTION-SCREEN END OF LINE.

  " function -> 基于函数组
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_fugr FOR FIELD p_fugr.
    PARAMETERS: p_fugr AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  " Class
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_clas FOR FIELD p_clas.
    PARAMETERS: p_clas AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  " Text
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_text FOR FIELD p_text.
    PARAMETERS: p_text AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  " Message Class
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_msag FOR FIELD p_msag.
    PARAMETERS: p_msag AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blck3.

SELECTION-SCREEN BEGIN OF BLOCK blck4 WITH FRAME.
  " 表相关

  " Table
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_tabl FOR FIELD p_tabl.
    PARAMETERS: p_tabl AS CHECKBOX DEFAULT 'X' USER-COMMAND tab.
  SELECTION-SCREEN END OF LINE.

  " DDL
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_ddls FOR FIELD p_ddls.
    PARAMETERS: p_ddls AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  " Domain
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_doma FOR FIELD p_doma.
    PARAMETERS: p_doma AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  " Element
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_dtel FOR FIELD p_dtel.
    PARAMETERS: p_dtel AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  " table type
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_ttyp FOR FIELD p_ttyp.
    PARAMETERS: p_ttyp AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blck4.

SELECTION-SCREEN BEGIN OF BLOCK blck5 WITH FRAME.
  " 其他

  " Smw0
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_smw0 FOR FIELD p_smw0.
    PARAMETERS: p_smw0 AS CHECKBOX DEFAULT ''.
  SELECTION-SCREEN END OF LINE.

  " Tcode
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_tran FOR FIELD p_tran.
    PARAMETERS: p_tran AS CHECKBOX DEFAULT ''.
  SELECTION-SCREEN END OF LINE.

  " Strans
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_xslt FOR FIELD p_xslt.
    PARAMETERS: p_xslt AS CHECKBOX DEFAULT ''.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blck5.

SELECTION-SCREEN BEGIN OF BLOCK nodis WITH FRAME.
  SELECT-OPTIONS: s_objnam FOR tadir-obj_name NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK nodis.


*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  PERFORM frm_init_sets.

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group3 = 'PAR'
      AND lcl_backup4abap_screen_option=>option_seted = abap_false
      AND screen-name <> 'P_DELT'.
      lcl_backup4abap_screen_option=>set_option( |{ screen-name }| ).
    ENDIF.
  ENDLOOP.
  lcl_backup4abap_screen_option=>option_seted = abap_true.

*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  " 默认子开发包
  lcl_backup4abap_filter_package=>set( EXCEPTIONS e_no_filter = 1 ).
  _stop.

  " 获取文件路径
  lcl_backup4abap_export=>factory( )->path( ).

  " 处理逻辑
  lcl_backup4abap_export=>factory( )->main( EXCEPTIONS e_no_screen_choese = 1 ).
  _stop.

  lcl_backup4abap_export=>factory( )->download( EXCEPTIONS e_no_file = 1 ).
  _stop.

*&---------------------------------------------------------------------*
*& Form FRM_INIT_SETS
*&---------------------------------------------------------------------*
*& t 初始化设置
*&---------------------------------------------------------------------*
FORM frm_init_sets .
  t_prog = '报表程序'.
  t_fugr = '函数程序'.
  t_tabl = '表内容'.
  t_clas = '类程序'.
  t_ddls = 'CDS视图'.
  t_doma = '数据域'.
  t_dtel = '数据元素'.
  t_ttyp = '表类型'.
  t_smw0 = 'SMW0(Z*)附件'.
  t_tran = '事务码'.
  t_xslt = '转换'.
  t_text = '程序文本'.
  t_msag = '消息类'.

  t_pack = '开发类(包)'.
  t_delt  = '增量获取(不跨client)'.

  " 按钮
  sscrfields-functxt_01 = VALUE smp_dyntxt(
    quickinfo = '当前增量时戳'
    text      = '增量信息' ).

ENDFORM.


CLASS lcl_backup4abap_filter_package IMPLEMENTATION.
  METHOD set.

    MOVE-CORRESPONDING s_pack[] TO lt_range.

    IF lt_range IS INITIAL.
      " lt_range = VALUE #( sign = 'I' option = 'CP' ( low = 'Z*' ) ( low = 'Y*' ) ).
      s_objnam[] = VALUE #( sign = 'I' option = 'CP' ( low = 'Z*' ) ( low = 'Y*' ) ).
    ELSE.
      SELECT
        devclass
        FROM tdevc
        WHERE devclass IN @lt_range[]
        INTO TABLE @DATA(lt_tdev).

      IF lt_tdev IS INITIAL.
        MESSAGE '请填写正确的包名' TYPE 'E' RAISING e_no_filter.
      ENDIF.

      lt_range = VALUE #( BASE lt_range FOR item IN lt_tdev
        sign = 'I' option = 'EQ' ( low = item-devclass ) ).

      fix( lt_tdev ).

    ENDIF.

    MOVE-CORRESPONDING lt_range TO s_pack[].

  ENDMETHOD.

  METHOD fix.

    CHECK it_package IS NOT INITIAL.

    SELECT
      devclass
      FROM tdevc
      FOR ALL ENTRIES IN @it_package
      WHERE parentcl = @it_package-parentcl
      INTO TABLE @DATA(lt_tdev).
    IF sy-subrc = 0.

      lt_range = VALUE #( BASE lt_range FOR item IN lt_tdev
        sign = 'I' option = 'EQ' ( low = item-devclass ) ).

      fix( lt_tdev ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_backup4abap_filter_time IMPLEMENTATION.
  METHOD factory.
    IF objt IS INITIAL.
      objt = NEW #( iv_limit ).
    ENDIF.

    ro_objt = objt.
  ENDMETHOD.

  METHOD constructor.
    DATA lv_delta_store_id TYPE char30.

    lv_delta_store_id = c_delta_store_id_fix_part && iv_limit.

    " DeltaDownload && SY-UNAME
    IMPORT delta = me->lt_delt_log FROM DATABASE demo_indx_blob(zd) ID lv_delta_store_id.
    SORT me->lt_delt_log BY object.

    " 时间减3s
    LOOP AT me->lt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>).
      <ls_delt_log>-dtime = <ls_delt_log>-dtime - 3.
    ENDLOOP.
  ENDMETHOD.

  METHOD get.

    GET TIME.

    READ TABLE lt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = iv_object.
    IF sy-subrc <> 0.
      APPEND VALUE #( object = iv_object ddate = sy-datum dtime = sy-uzeit ) TO lt_delt_log.
    ELSE.
      rs_delt_log = <ls_delt_log>.
      <ls_delt_log>-ddate = sy-datum.
      <ls_delt_log>-dtime = sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_backup4abap_screen_option IMPLEMENTATION.
  METHOD set_option.
    APPEND iv_option TO options.
  ENDMETHOD.

  METHOD selected_option.
    LOOP AT options INTO DATA(l_option).
      ASSIGN (l_option) TO FIELD-SYMBOL(<lv_option>).
      IF sy-subrc = 0.
        IF <lv_option> = 'X'.
          APPEND l_option+2 TO rt_options.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_backup4abap_folder IMPLEMENTATION.
  METHOD class_constructor.
    folder = VALUE #( ( tag = 'MM' ) ( tag = 'PP' )
                      ( tag = 'SD' ) ( tag = 'FI' ) ( tag = 'FICO' folder = 'FI' )
                      ( tag = 'HR' )
                      ( tag = 'CO' )
                      ( tag = 'RAF' )  " RAF 框架
                      ( tag = 'AKIT' ) " AKIT 工具集
                      ( tag = 'TEST' )
                      ( tag = 'DEMO' )
                      ( tag = 'ABAP' )
                      ( tag = 'API' )  " 接口处理
                      ( tag = 'JOB' )  " job 处理
                      ( tag = 'FILE' ) " 文件处理
                     ).
    SORT folder BY tag.
  ENDMETHOD.
  METHOD split_folder_prog.
    DATA: lv_strlen   TYPE i,
          lv_filename TYPE string,
          lv_folder   TYPE ty_folder-tag.

    " 1. 清除 _|\d 后的所有内容
    " 2. 正则匹配（为兼容不能使用PCRE 贪婪模式）

    " 标准程序不做分目录
    IF name(1) = 'Y' OR name(1) = 'Z'.

      lv_filename = name+1.
      REPLACE REGEX `[_\d].*$` IN lv_filename WITH ''.

      lv_strlen = strlen( lv_filename ).

      IF lv_strlen <= 1.
        " 无父目录 PASS
      ELSEIF lv_strlen = 2.
        " 无特殊标识
        lv_folder = lv_filename.

        READ TABLE folder INTO DATA(ls_folder) WITH KEY tag = lv_folder BINARY SEARCH.
        IF sy-subrc = 0.
          rv_folder = COND #( WHEN ls_folder-folder IS INITIAL
                              THEN ls_folder-tag
                              ELSE ls_folder-folder ) && '/'.
        ENDIF.
      ELSE.

        " 分页反查
        LOOP AT folder INTO ls_folder.
          FIND REGEX |^{ ls_folder-tag }| IN lv_filename.
          IF sy-subrc = 0.
            rv_folder = COND #( WHEN ls_folder-folder IS INITIAL
                                THEN ls_folder-tag
                                ELSE ls_folder-folder ) && '/'.
            EXIT.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.

    IF subc = 'I'.
      IF name+1(1) = 'X'. " 特殊
        rv_folder = `CMOD/` && rv_folder.
      ELSE.
        rv_folder = rv_folder && 'INCLUDE/'.
      ENDIF.
    ELSEIF subc = 'M'.
      rv_folder = 'MODULEPOOLS/' && rv_folder.
    ELSEIF subc = 'T'.
      rv_folder = 'TYPEPOOLS/' && rv_folder.
    ENDIF.

  ENDMETHOD.
  METHOD split_folder_clas.
    " 类 特殊判定，不走配置逻辑
    DATA: lv_filename TYPE string,
          lv_strlen   TYPE i.

    DATA: l_off TYPE i,
          l_len TYPE i.

    IF strlen( name ) > 5 AND name+4(2) = 'SI'.
      rv_folder = 'IF/'.
      RETURN.
    ELSEIF name+1(2) = 'BP'.
      rv_folder = 'BP/'.
      RETURN.
    ENDIF.

    lv_filename = name.

    REPLACE REGEX `[_\d].*$` IN lv_filename WITH ``.

    lv_strlen = strlen( lv_filename ).

    IF lv_strlen = 3.
      rv_folder = name.
      rv_folder = rv_folder+4.

      REPLACE REGEX `_.*$` IN rv_folder WITH ``.
    ELSE.
      rv_folder = name.
      rv_folder = rv_folder+1.

      REPLACE REGEX `[(:?CL)|(:?CO)|(:?CX)|(:?IF)]$` IN rv_folder WITH ``.
    ENDIF.

    IF rv_folder IS NOT INITIAL.
      rv_folder = rv_folder && '/'.
    ENDIF.

  ENDMETHOD.
  METHOD split_folder_fugr.
    DATA: lv_filename TYPE string,
          lv_folder   TYPE ty_folder-tag.

    " 1. 清除 ^ZFM_?|^Z
    " 2. 清除 _ 后的所有内容

    " 标准程序不做分目录
    CHECK name(1) = 'Y' OR name(1) = 'Z'.

    lv_filename = name.

    REPLACE REGEX `^[YZ]FM_?|^[YZ]` IN lv_filename WITH ''.
    REPLACE REGEX `_.*$` IN lv_filename WITH ''.

    lv_folder = lv_filename.

    READ TABLE folder INTO DATA(ls_folder) WITH KEY tag = lv_folder BINARY SEARCH.
    IF sy-subrc = 0.
      rv_folder = COND #( WHEN ls_folder-folder IS INITIAL
                          THEN ls_folder-tag
                          ELSE ls_folder-folder ) && '/'.
    ENDIF.
  ENDMETHOD.

  METHOD split_folder_ddls.
    " 1. 清除 `^ZV`
    " 2. 清除 `_.*$`

    rv_folder = name.

    REPLACE REGEX `^[YZ]V|^[YZ]` IN rv_folder WITH ''.
    REPLACE REGEX `_.*$` IN rv_folder WITH ''.

    IF rv_folder IS NOT INITIAL.
      rv_folder = rv_folder && '/'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_backup4abap_objects IMPLEMENTATION.

  METHOD constructor.
    me->o_conv_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
  ENDMETHOD.

  METHOD is_delta.

    IF p_delt = 'X' AND s_pack[] IS INITIAL.
      rv_bool = abap_true.
    ELSE.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_backup4abap_object_prog IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    me->o_pb = NEW #( `Process Report & ` ).
    me->parent_folder = 'SE38' && '/'.
  ENDMETHOD.

  METHOD loadfiles.
    DATA: lv_filename TYPE string.
    DATA: lt_source TYPE TABLE OF text1000 WITH EMPTY KEY,
          lv_source TYPE string.

    " 读取报表数据
    SELECT
      rep~progname,
      rep~r3state,
      rep~subc,
      rep~unam,      " 修改人
      rep~udat,
      rep~utime,
      des~text
      FROM reposrc AS rep
      INNER JOIN tadir AS tad ON tad~obj_name = rep~progname AND tad~object = 'PROG'
      LEFT JOIN trdirt AS des ON des~name = rep~progname AND des~sprsl = @sy-langu
      WHERE ( ( rep~subc = '1' )
           OR ( rep~subc = 'I' AND rep~rload = '1' )
           OR ( rep~subc = 'M' )
           OR ( rep~subc = 'T' ) )
        AND rep~r3state = 'A'
        AND tad~obj_name IN @s_objnam
        AND tad~devclass IN @s_pack
      INTO TABLE @DATA(lt_list).
    SORT lt_list BY progname.

    me->o_pb->count = lines( lt_list ).
    DATA(ls_delt_log) = lcl_backup4abap_filter_time=>factory( )->get( 'PROG' ).

    LOOP AT lt_list INTO DATA(ls_list).
      me->o_pb->add( i_desc = ls_list-progname ).

      IF is_delta( ) = 'X'.
        IF ls_delt_log-ddate > ls_list-udat
          OR ( ls_delt_log-ddate = ls_list-udat AND ls_delt_log-dtime > ls_list-utime ).

          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO me->files ASSIGNING FIELD-SYMBOL(<ls_file>).

      <ls_file>-name = split_folder( VALUE ty_split_limit( name = ls_list-progname
                                                           subc = ls_list-subc ) )
                    && ls_list-progname && '.' && c_extension_abap.

      <ls_file>-name = me->parent_folder && <ls_file>-name.

      " 读取激活状态的代码
      READ REPORT ls_list-progname INTO lt_source STATE 'A'." MAXIMUM WIDTH INTO lv_max.
      IF sy-subrc = 0.
        " 内表转换为长字符串
        CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY c_newline.
        me->o_conv_out->convert( EXPORTING data = lv_source
                                 IMPORTING buffer = <ls_file>-data ).
      ENDIF.

      REFRESH lt_source.
      CLEAR lv_source.
    ENDLOOP.

  ENDMETHOD.

  METHOD split_folder.
    DATA: ls_limit TYPE ty_split_limit.

    ls_limit = i_limit.

    rv_filename = lcl_backup4abap_folder=>split_folder_prog( name = ls_limit-name
                                                             subc = ls_limit-subc ).

  ENDMETHOD.

  METHOD load2zip.

    CHECK io_zip IS BOUND.

    LOOP AT me->files INTO DATA(file).
      io_zip->add( name    = file-name
                   content = file-data ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_backup4abap_object_fugr IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    me->o_pb = NEW #( `Process Function & ` ).
    me->parent_folder = 'SE37' && '/'.
  ENDMETHOD.

  METHOD loadfiles.
    " V_FDIR 查找功能模块的视图（函数名 函数组名）
    "  TFDIR   功能模块 （实际程序名）
    "  ENLFDIR 功能模块的附加属性（函数的函数组）
    " TFTIT  功能模块描述信息
    " TLIBV  负责功能组（函数组 函数组负责人）
    " TADIR  资源对象库（不仅函数部分数据）（包 关联函数）

    DATA: lt_func TYPE tt_function.

    DATA: lt_source TYPE TABLE OF text1000 WITH EMPTY KEY,
          lv_source TYPE string.

    " 此处获取函数组
    SELECT
      fu~funcname AS functionname,
      gr~area     AS functiongroup,
      fu~pname    AS progname,
      fu~include  AS includenumber
      FROM enlfdir AS gr
      INNER JOIN tfdir AS fu ON fu~funcname = gr~funcname
      LEFT JOIN tadir AS tad ON tad~obj_name = gr~area AND tad~object = 'FUGR'
      WHERE gr~area     IN @s_objnam    " 限制函数组（部分标准程序也会有Z开头的函数）
        AND gr~funcname IN @s_objnam
        AND tad~devclass IN @s_pack
      INTO CORRESPONDING FIELDS OF TABLE @lt_func.
    SORT lt_func BY functionname.

    me->o_pb->count = lines( lt_func ).
    DATA(ls_delt_log) = lcl_backup4abap_filter_time=>factory( )->get( 'FUGR' ).

    " 补充更多信息
    fix_more( REF #( lt_func ) ).

    LOOP AT lt_func INTO DATA(ls_func).
      me->o_pb->add( i_desc = ls_func-functionname ).

      " 检查增量
      IF is_delta( ) = 'X'.
        IF ls_delt_log-ddate > ls_func-udate
          OR ( ls_delt_log-ddate = ls_func-udate AND ls_delt_log-dtime > ls_func-utime ).

          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO me->files ASSIGNING FIELD-SYMBOL(<ls_file>).

      <ls_file>-name = split_folder( ls_func-functionname )
                    && ls_func-functionname && '.' && c_extension_abap.

      <ls_file>-name = me->parent_folder && <ls_file>-name.

      " 读取激活状态的代码
      READ REPORT ls_func-reportname INTO lt_source STATE 'A'." MAXIMUM WIDTH INTO lv_max.
      IF sy-subrc = 0.
        " 内表转换为长字符串
        CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY c_newline.
        me->o_conv_out->convert( EXPORTING data = lv_source
                                 IMPORTING buffer = <ls_file>-data ).
      ENDIF.

      REFRESH lt_source.
      CLEAR lv_source.
    ENDLOOP.

    " --> get more(rfc\incloud\)
    DATA: lt_area TYPE tt_range_area.

    lt_area = VALUE #( FOR _ IN lt_func sign = 'I' option = 'CP' ( low = |L{ _-functiongroup }*| ) ).
    SORT lt_area BY low.
    DELETE ADJACENT DUPLICATES FROM lt_area COMPARING low.

    REFRESH lt_func.

    get_more( lt_area ).
    " <--

  ENDMETHOD.

  METHOD fix_more.
    FIELD-SYMBOLS: <lt_func> TYPE tt_function.

    ASSIGN io_func->* TO <lt_func>.

    IF <lt_func> IS NOT INITIAL. " 描述
      SELECT
        funcname,
        stext
        FROM tftit
        FOR ALL ENTRIES IN @<lt_func>
        WHERE spras = @sy-langu
          AND funcname = @<lt_func>-functionname
        INTO TABLE @DATA(lt_tftit).
      SORT lt_tftit BY funcname.
    ENDIF.

    LOOP AT <lt_func> ASSIGNING FIELD-SYMBOL(<ls_func_fixt>).
      READ TABLE lt_tftit INTO DATA(ls_tftit) WITH KEY funcname = <ls_func_fixt>-functionname BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_func_fixt>-functiontitle = ls_tftit-stext.
      ENDIF.

      " 函数对应的实体
      CONCATENATE 'L' <ls_func_fixt>-functiongroup 'U' <ls_func_fixt>-includenumber INTO <ls_func_fixt>-reportname.
    ENDLOOP.

    IF <lt_func> IS NOT INITIAL.
      SELECT
        progname,
        unam,
        udat,
        utime
        FROM reposrc
        FOR ALL ENTRIES IN @<lt_func>
        WHERE progname = @<lt_func>-reportname
          AND appl = 'S'
        INTO TABLE @DATA(lt_rep).
      SORT lt_rep BY progname.

      LOOP AT <lt_func> ASSIGNING <ls_func_fixt>.
        READ TABLE lt_rep INTO DATA(ls_rep) WITH KEY progname = <ls_func_fixt>-reportname BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_func_fixt>-uname = ls_rep-unam.
          <ls_func_fixt>-udate = ls_rep-udat.
          <ls_func_fixt>-utime = ls_rep-utime.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_more.
    DATA: lt_source TYPE TABLE OF text1000 WITH EMPTY KEY,
          lv_source TYPE string.

    DATA: lv_str_len TYPE i.

    " 读取 函数组其他信息
    SELECT
      rep~progname,
      rep~r3state,
      rep~subc,
      rep~unam,      " 修改人
      rep~udat,
      rep~utime
      FROM reposrc AS rep
      WHERE ( " ( rep~progname LIKE 'LZ%' AND rep~subc = 'I' AND rep~appl = '' ) " 索引页
              ( rep~rstat = 'S' ) " 索引页 + RFC
           OR ( rep~subc = 'I' AND rep~appl = 'S' AND rep~dbapl = '' )  " O/I/F
           OR ( rep~subc = 'I' AND rep~appl = 'S' ) ) " TOP
        AND rep~progname IN @it_range_area
        AND rep~r3state = 'A'
      INTO TABLE @DATA(lt_list).
    SORT lt_list BY progname.

    me->o_pb->count = lines( lt_list ).
    me->o_pb->base_desc = 'Process Function More & '.
    DATA(ls_delt_log) = lcl_backup4abap_filter_time=>factory( )->get( 'FINC' ).

    LOOP AT lt_list INTO DATA(ls_list).
      me->o_pb->add( i_desc = ls_list-progname ).

      " 检查增量
      IF is_delta( ) = 'X'.
        IF ls_delt_log-ddate > ls_list-udat
          OR ( ls_delt_log-ddate = ls_list-udat AND ls_delt_log-dtime > ls_list-utime ).

          CONTINUE.
        ENDIF.
      ENDIF.

      lv_str_len = strlen( ls_list-progname ) - 3.

      CHECK NOT ls_list-progname+lv_str_len(1) = 'U'. " 取值重复

      APPEND INITIAL LINE TO me->files ASSIGNING FIELD-SYMBOL(<ls_file>).

      " 文件名
      <ls_file>-name = |{ ls_list-progname }.{ c_extension_abap }|.

      CASE ls_list-progname+lv_str_len(1).
        WHEN 'V'.
          <ls_file>-name = me->parent_folder && `Rfc/` && <ls_file>-name.
        WHEN '$'.
          <ls_file>-name = me->parent_folder && `Rfc/Unit/` && <ls_file>-name.
        WHEN OTHERS.
          <ls_file>-name = me->parent_folder && `More/` && <ls_file>-name.
      ENDCASE.

      " 读取激活状态的代码
      READ REPORT ls_list-progname INTO lt_source STATE 'A'." MAXIMUM WIDTH INTO lv_max.
      IF sy-subrc = 0.
        " 内表转换为长字符串
        CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY c_newline.
        me->o_conv_out->convert( EXPORTING data = lv_source
                                 IMPORTING buffer = <ls_file>-data ).
      ENDIF.

      REFRESH lt_source.
      CLEAR: lv_str_len, lv_source.
    ENDLOOP.

  ENDMETHOD.

  METHOD split_folder.
    DATA: lv_limit    TYPE ty_split_limit.

    lv_limit = i_limit.

    rv_filename = lcl_backup4abap_folder=>split_folder_fugr( name = lv_limit ).

  ENDMETHOD.

  METHOD load2zip.

    CHECK io_zip IS BOUND.

    LOOP AT me->files INTO DATA(file).
      io_zip->add( name    = file-name
                   content = file-data ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
CLASS lcl_backup4abap_object_clas IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    me->o_pb = NEW #( `Process Class & ` ).
    me->parent_folder = 'SE24' && '/'.

    me->lt_type = VALUE #( ( type = seop_ext_class_locals_def  exline = 4 )
                           ( type = seop_ext_class_locals_imp  exline = 4 )
                           ( type = seop_ext_class_macros      exline = 3 )
                           ( type = seop_ext_class_testclasses exline = 1 ) ).
  ENDMETHOD.

  METHOD loadfiles.
    DATA: lt_source TYPE rswsourcet,
          lv_source TYPE string.
    DATA: ls_include  TYPE progstruc,
          lv_filename TYPE string.

    SELECT
      ss~clsname, " SEOCLASS
      ss~langu,
      ss~descript,
      ss~exposure,
      ss~state,
      ss~changedby,
      ss~changedon,
      CAST( '000000' AS TIMS ) AS changetm
      FROM vseoclass AS ss
      INNER JOIN tadir AS ta ON ta~obj_name = ss~clsname AND ta~object = 'CLAS'
      WHERE ta~obj_name IN @s_objnam
        AND version = '1' " 激活
        AND ( state = '0' OR state = '1' )
        AND ta~devclass IN @s_pack
      INTO TABLE @DATA(lt_class).
    " 排序去重
    SORT lt_class BY clsname langu.
    DELETE ADJACENT DUPLICATES FROM lt_class COMPARING clsname.

    SELECT
      ss~clsname,
      ss~langu,
      ss~descript,
      ss~exposure,
      ss~state,
      ss~changedby,
      ss~changedon,
      CAST( '000000' AS TIMS ) AS changetm
      FROM vseoclif AS ss
      INNER JOIN tadir AS ta ON ta~obj_name = ss~clsname AND ta~object = 'INTF'
      WHERE ta~obj_name IN @s_objnam
        AND version = '1' " 激活
        AND state = '1'
        AND ta~devclass IN @s_pack
      APPENDING TABLE @lt_class.

    " 多语言去重
    SORT lt_class BY clsname langu.
    DELETE ADJACENT DUPLICATES FROM lt_class COMPARING clsname.

    " --> 获取真实修改时间（类下的子节点）
    " 参考 LSEODF1X 948 行
    DATA: lt_range_name TYPE RANGE OF progdir-name.
    lt_range_name = VALUE #( FOR itm IN lt_class
                              " WHERE ( changedon IS INITIAL )
                                ( sign = 'I' option = 'CP' low = |{ itm-clsname }*| ) ).
    IF lt_range_name IS NOT INITIAL.
      SELECT
        progname AS name,
        unam,
        udat,
        utime
        FROM reposrc
        WHERE progname IN @lt_range_name
        INTO TABLE @DATA(lt_progdir).

      LOOP AT lt_progdir ASSIGNING FIELD-SYMBOL(<ls_progdir>).
        " __ 当名称足够长时 无标识 `=` 正则无效
        " <ls_progdir>-name = replace( val = <ls_progdir>-name pcre = `=+.*$` with = `` occ = -1 ).
        <ls_progdir>-name = replace( val = <ls_progdir>-name regex = `=*(?:CCAU|CCDEF|CCIMP|CCMAC|CI|CM\d{3}|CO|CP|CS|CT|CU|IP|IT|IU)$` with = `` occ = -1 ).
      ENDLOOP.
      SORT lt_progdir BY name udat DESCENDING utime DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_progdir COMPARING name.
    ENDIF.
    " <--

    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = me->o_instance.

    DATA: lr_source TYPE REF TO cl_oo_source,
          ls_clskey TYPE seoclskey.

    me->o_pb->count = lines( lt_class ).
    DATA(ls_delt_log) = lcl_backup4abap_filter_time=>factory( )->get( 'CLAS' ).

    LOOP AT lt_class INTO DATA(ls_class).
      me->o_pb->add( i_desc = ls_class-clsname ).

      " 真实修改时间
      READ TABLE lt_progdir INTO DATA(ls_prodir_udat) WITH KEY name = ls_class-clsname BINARY SEARCH.
      IF sy-subrc = 0.
        IF ls_class-changedon IS INITIAL OR ls_prodir_udat-udat > ls_class-changedon.
          ls_class-changedon = ls_prodir_udat-udat.
        ENDIF.
        ls_class-changedby = ls_prodir_udat-unam.
        ls_class-changetm  = ls_prodir_udat-utime.
      ENDIF.

      " 检查增量
      IF is_delta( ) = 'X'.
        IF ls_delt_log-ddate > ls_class-changedon
          OR ( ls_delt_log-ddate = ls_class-changedon AND ls_delt_log-dtime > ls_class-changetm ).

          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO me->files ASSIGNING FIELD-SYMBOL(<ls_file>).

      <ls_file>-name = split_folder( ls_class-clsname )
                    && ls_class-clsname && '.' && c_extension_abap.

      <ls_file>-name = me->parent_folder && <ls_file>-name.

      " > 读取源码
      CALL METHOD me->o_instance->('CREATE_CLIF_SOURCE')
        EXPORTING
          clif_name = ls_class-clsname
          version   = 'A'
        RECEIVING
          result    = me->o_source.

      CALL METHOD me->o_source->('GET_SOURCE')
        IMPORTING
          source = lt_source.

      " 内表转换为长字符串
      CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY c_newline.

      me->o_conv_out->convert( EXPORTING data   = lv_source
                               IMPORTING buffer = <ls_file>-data ).

      REFRESH lt_source.
      CLEAR lv_source.

      " --> add 更多信息
      LOOP AT me->lt_type INTO DATA(ls_type).
        ls_include-rootname = ls_class-clsname.
        TRANSLATE ls_include-rootname USING ' ='.
        ls_include-categorya = ls_type-type(1).
        ls_include-codea = ls_type-type+1(4).

        READ REPORT ls_include INTO lt_source STATE 'A'." MAXIMUM WIDTH INTO lv_max.
        IF sy-subrc = 0 AND lines( lt_source ) > ls_type-exline.

          APPEND INITIAL LINE TO me->files ASSIGNING <ls_file>.

          <ls_file>-name = 'More/'
                        && split_folder( ls_class-clsname )
                        && ls_class-clsname && |-{ ls_type-type }.| && c_extension_abap.

          <ls_file>-name = me->parent_folder && <ls_file>-name.

          " 内表转换为长字符串
          CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY c_newline.

          " string -> xstring
          me->o_conv_out->convert( EXPORTING data   = lv_source
                                   IMPORTING buffer = <ls_file>-data ).

        ENDIF.

        REFRESH lt_source.
        CLEAR: ls_include, lv_source.
      ENDLOOP.
      " <--

    ENDLOOP.

  ENDMETHOD.

  METHOD split_folder.
    DATA: lv_limit    TYPE ty_split_limit.

    lv_limit = i_limit.

    rv_filename = lcl_backup4abap_folder=>split_folder_clas( name = lv_limit ).

  ENDMETHOD.

  METHOD load2zip.

    CHECK io_zip IS BOUND.

    LOOP AT me->files INTO DATA(file).
      io_zip->add( name    = file-name
                   content = file-data ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
CLASS lcl_backup4abap_object_text IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    me->o_pb = NEW #( `Process Text & ` ).
    me->parent_folder = 'TEXT' && '/'.
  ENDMETHOD.

  METHOD loadfiles.
    DATA: ls_include TYPE progstruc.
    DATA: lt_textpool TYPE TABLE OF textpool,
          lv_textpool TYPE string.

    " 获取文本程序名
    SELECT
      ta~obj_name,
      ta~object
      FROM tadir AS ta
      WHERE ta~pgmid = 'R3TR'
        AND ta~object IN ('PROG','FUGR','CLAS')
        AND ta~obj_name IN @s_objnam
        AND ta~devclass IN @s_pack
      INTO TABLE @DATA(lt_tadir).

    CHECK lt_tadir IS NOT INITIAL.

    LOOP AT lt_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>) WHERE object = 'FUGR' OR object = 'CLAS'.

      " CLAS
      IF <ls_tadir>-object = 'CLAS'.
        ls_include = VALUE #( rootname = <ls_tadir>-obj_name categorya = 'C' codea = 'P' ).
        TRANSLATE ls_include-rootname USING ' ='.

        <ls_tadir>-obj_name = ls_include.
      ENDIF.

      " FUGR
      IF <ls_tadir>-object = 'FUGR'.
        <ls_tadir>-obj_name = 'SAPL' && <ls_tadir>-obj_name.
      ENDIF.
    ENDLOOP.
    SORT lt_tadir BY obj_name object.

    SELECT
      progname,
      r3state,
      language,
      unam,
      udat,
      utime
      FROM repotext
      FOR ALL ENTRIES IN @lt_tadir
      WHERE progname = @lt_tadir-obj_name
        AND r3state  = 'A'
      INTO TABLE @DATA(lt_repot).

    CHECK lt_repot IS NOT INITIAL.

    SELECT spras, laiso FROM t002 INTO TABLE @DATA(lt_t002).
    SORT lt_t002 BY spras.

    me->o_pb->count = lines( lt_repot ).
    DATA(ls_delt_log) = lcl_backup4abap_filter_time=>factory( )->get( 'TEXT' ).

    LOOP AT lt_repot INTO DATA(ls_repot).
      me->o_pb->add( i_desc = ls_repot-progname ).

      " 检查增量
      IF is_delta( ) = 'X'.
        IF ls_delt_log-ddate > ls_repot-udat
          OR ( ls_delt_log-ddate = ls_repot-udat AND ls_delt_log-dtime > ls_repot-utime ).

          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO me->files ASSIGNING FIELD-SYMBOL(<ls_file>).

      READ TABLE lt_tadir INTO DATA(ls_tadir) WITH KEY obj_name = ls_repot-progname BINARY SEARCH.

      <ls_file>-name = split_folder( VALUE ty_split_limit( object   = ls_tadir-object
                                                           progname = ls_repot-progname ) )
                    && '.' && c_extension_txt.

      CLEAR ls_tadir.

      " 文件夹匹配 -> 文件名
      READ TABLE lt_t002 INTO DATA(ls_t002) WITH KEY spras = ls_repot-language BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_file>-name = me->parent_folder && |{ ls_t002-laiso }/| && <ls_file>-name.
      ELSE.
        <ls_file>-name = me->parent_folder && <ls_file>-name.
      ENDIF.

      READ TEXTPOOL ls_repot-progname LANGUAGE ls_repot-language STATE 'A' INTO lt_textpool.
      IF sy-subrc = 0.
        " 标题
        CONCATENATE 'ID' c_tab 'KEY_____' c_tab 'ENTRY' c_newline INTO lv_textpool.
        " 内表转换为长字符串
        LOOP AT lt_textpool INTO DATA(ls_textpool).
          " ID
          lv_textpool = lv_textpool && ls_textpool-id && c_tab.
          " key => 为方便好看补齐8位
          lv_textpool = lv_textpool && |{ ls_textpool-key WIDTH = 8 }| && c_tab.
          " value
          lv_textpool = lv_textpool && ls_textpool-entry.
          " newline
          lv_textpool = lv_textpool && c_newline.
        ENDLOOP.

        me->o_conv_out->convert( EXPORTING data = lv_textpool
                                 IMPORTING buffer = <ls_file>-data ).
      ENDIF.

      REFRESH lt_textpool.
      CLEAR: lv_textpool.
    ENDLOOP.

  ENDMETHOD.

  METHOD split_folder.
    DATA: ls_limit    TYPE ty_split_limit,
          lv_filename TYPE string.
    ls_limit = i_limit.

    CASE ls_limit-object.
      WHEN ''.
        " pass
        rv_filename = ls_limit-progname.
      WHEN 'CLAS'.
        lv_filename = ls_limit-progname.
        REPLACE REGEX '=*CP$' IN lv_filename WITH ''.

        rv_filename = 'SE24/'
                   && lcl_backup4abap_folder=>split_folder_clas( name = lv_filename ) && lv_filename.
      WHEN 'FUGR'.
        lv_filename = ls_limit-progname.
        REPLACE REGEX '^SAPL' IN lv_filename WITH ''.

        rv_filename = 'SE37/'
                   && lcl_backup4abap_folder=>split_folder_fugr( name = lv_filename ) && lv_filename.
      WHEN OTHERS.
        lv_filename = ls_limit-progname.

        rv_filename = 'SE38/'
                   && lcl_backup4abap_folder=>split_folder_prog( name = lv_filename subc = '1' ) && lv_filename.
    ENDCASE.

  ENDMETHOD.

  METHOD load2zip.

    CHECK io_zip IS BOUND.

    LOOP AT me->files INTO DATA(file).
      io_zip->add( name    = file-name
                   content = file-data ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
CLASS lcl_backup4abap_object_msag IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    me->o_pb = NEW #( `Process MSAG & ` ).
    me->parent_folder = 'SE91' && '/'.
  ENDMETHOD.

  METHOD loadfiles.

    " T100 T100U

    DATA: lt_arbgb TYPE TABLE OF ty_arbgb.
    DATA: lv_spras      TYPE t002-spras,
          lv_spras_last TYPE t002-spras,
          lv_string     TYPE string.
    FIELD-SYMBOLS: <ls_file> LIKE LINE OF me->files.

    SELECT
      tu~arbgb,
      tu~msgnr,
      tu~name,
      tu~datum,
      tu~selfdef
      FROM tadir AS ta
      LEFT JOIN t100u AS tu ON tu~arbgb = ta~obj_name
      WHERE ta~pgmid = 'R3TR'
        AND ta~object = 'MSAG'
        AND ta~obj_name IN @s_objnam
        AND ta~devclass IN @s_pack
      INTO TABLE @DATA(lt_t100u).
    SORT lt_t100u BY arbgb msgnr.

    CHECK lt_t100u IS NOT INITIAL.

    me->o_pb->count = lines( lt_t100u ).
    DATA(ls_delt_log) = lcl_backup4abap_filter_time=>factory( )->get( 'MSAG' ).

    MOVE-CORRESPONDING lt_t100u TO lt_arbgb.
    SORT lt_arbgb BY arbgb datum DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_arbgb COMPARING arbgb.

    IF is_delta( ) = 'X'.
      DELETE lt_arbgb WHERE datum < ls_delt_log-ddate.
    ENDIF.

    SELECT
      arbgb,
      msgnr,
      sprsl,
      text
      FROM t100
      FOR ALL ENTRIES IN @lt_t100u
      WHERE arbgb = @lt_t100u-arbgb
        AND msgnr = @lt_t100u-msgnr
      INTO TABLE @DATA(lt_t100).
    SORT lt_t100 BY arbgb sprsl msgnr.

    SELECT spras, laiso FROM t002 INTO TABLE @DATA(lt_t002).
    SORT lt_t002 BY spras.
    DATA: ls_t002 LIKE LINE OF lt_t002.

    DEFINE mc_process.
      APPEND INITIAL LINE TO me->files ASSIGNING <ls_file>.

      " 文件名
      READ TABLE lt_t002 INTO ls_t002 WITH KEY spras = lv_spras_last BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_file>-name = ls_t002-laiso && '/' && ls_arbgb-arbgb && '.' && c_extension_txt.
      ELSE.
        <ls_file>-name = ls_arbgb-arbgb && '.' && c_extension_txt.
      ENDIF.

      <ls_file>-name = me->parent_folder && <ls_file>-name.

      " 内容标题
      CONCATENATE 'KEY' c_tab 'CHANGER_____' c_tab 'CDATUM__' c_tab 'T' c_tab 'TEXT' c_newline lv_string INTO lv_string.

      " string -> xstring
      me->o_conv_out->convert( EXPORTING data   = lv_string
                               IMPORTING buffer = <ls_file>-data ).

      CLEAR: lv_string.
    END-OF-DEFINITION.

    LOOP AT lt_arbgb INTO DATA(ls_arbgb).
      me->o_pb->add( i_desc = ls_arbgb-arbgb ).

      READ TABLE lt_t100 TRANSPORTING NO FIELDS WITH KEY arbgb = ls_arbgb-arbgb BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT lt_t100 INTO DATA(ls_t100) FROM sy-tabix.
          IF ls_t100-arbgb <> ls_arbgb-arbgb.
            EXIT.
          ENDIF.

          IF lv_spras IS INITIAL.
            lv_spras = ls_t100-sprsl.
          ENDIF.

          " 多个语言
          IF lv_spras IS NOT INITIAL AND lv_spras <> ls_t100-sprsl.
            lv_spras_last = lv_spras.
            lv_spras = ls_t100-sprsl.

            mc_process.
          ENDIF.

          " 消息编号
          lv_string = lv_string && ls_t100-msgnr && c_tab.

          READ TABLE lt_t100u INTO DATA(ls_t100u) WITH KEY arbgb = ls_t100-arbgb msgnr = ls_t100-msgnr BINARY SEARCH.
          " 修改人
          lv_string = lv_string && |{ ls_t100u-name WIDTH = 12 }| && c_tab.
          " 修改时间
          lv_string = lv_string && ls_t100u-datum && c_tab.
          " 类型
          lv_string = lv_string && ls_t100u-selfdef && c_tab.
          " 内容
          lv_string = lv_string && ls_t100-text && c_newline.
          CLEAR ls_t100u.
        ENDLOOP.

        " 多个语言 最后一个
        IF lv_spras_last <> lv_spras.
          lv_spras_last = lv_spras.
          mc_process.
        ENDIF.

      ENDIF.

      CLEAR: lv_spras, lv_spras_last.
    ENDLOOP.

  ENDMETHOD.

  METHOD split_folder.
    " pass
  ENDMETHOD.

  METHOD load2zip.

    CHECK io_zip IS BOUND.

    LOOP AT me->files INTO DATA(file).
      io_zip->add( name    = file-name
                   content = file-data ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_backup4abap_object_ddls IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    me->o_pb = NEW #( `Process DDLS & ` ).
    me->parent_folder = 'DDL' && '/'.
  ENDMETHOD.

  METHOD loadfiles.

    " 读取 DDL
    SELECT
      rc~ddlname,
      rc~as4user,
      rc~as4date,
      rc~as4time,
      rc~source,
      ct~ddtext
      FROM ddddlsrc AS rc
      INNER JOIN tadir AS ta ON ta~obj_name = rc~ddlname AND object = 'DDLS' " 存在 STOB 结构化对象
      LEFT JOIN ddddlsrct AS ct ON ct~ddlname    = rc~ddlname
                               AND ct~ddlanguage = '1'
                               AND ct~as4local   = rc~as4local
      WHERE ta~obj_name IN @s_objnam "rc~ddlname
        AND rc~as4local = 'A'
        AND ta~devclass IN @s_pack
      INTO TABLE @DATA(lt_ddlsrc).
    SORT lt_ddlsrc BY ddlname.

    CHECK lt_ddlsrc IS NOT INITIAL.

    me->o_pb->count = lines( lt_ddlsrc ).
    DATA(ls_delt_log) = lcl_backup4abap_filter_time=>factory( )->get( 'DDLS' ).

    LOOP AT lt_ddlsrc INTO DATA(ls_ddl).
      me->o_pb->add( i_desc = ls_ddl-ddlname ).
      " 文件夹匹配 -> 文件名

      " 检查增量
      IF is_delta( ) = 'X'.
        IF ls_delt_log-ddate > ls_ddl-as4date
          OR ( ls_delt_log-ddate = ls_ddl-as4date AND ls_delt_log-dtime > ls_ddl-as4time ).

          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO me->files ASSIGNING FIELD-SYMBOL(<ls_file>).

      <ls_file>-name = split_folder( ls_ddl-ddlname )
                    && ls_ddl-ddlname && '.' && c_extension_ddl.

      <ls_file>-name = me->parent_folder && <ls_file>-name.

      " --> Begin 清除末尾注释
      REPLACE FIRST OCCURRENCE OF REGEX `\/\*\+\[internal\].*\*\/` IN ls_ddl-source WITH ``.
      " <--

      " string -> xstring
      me->o_conv_out->convert( EXPORTING data   = ls_ddl-source
                               IMPORTING buffer = <ls_file>-data ).

    ENDLOOP.

  ENDMETHOD.

  METHOD split_folder.
    DATA: l_limit TYPE ty_split_limit.

    l_limit = i_limit.

    rv_filename = lcl_backup4abap_folder=>split_folder_ddls( l_limit ).
  ENDMETHOD.

  METHOD load2zip.

    CHECK io_zip IS BOUND.

    LOOP AT me->files INTO DATA(file).
      io_zip->add( name    = file-name
                   content = file-data ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_backup4abap_export IMPLEMENTATION.

  METHOD constructor.
    me->o_zip = NEW #( ).
  ENDMETHOD.

  METHOD factory.
    IF objt IS INITIAL.
      objt = NEW #( ).
    ENDIF.

    ro_objt = objt.
  ENDMETHOD.

  METHOD path.
    DATA: lv_path     TYPE string,
          lv_fullpath TYPE string,
          lv_filename TYPE string.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title         = 'download zip'
        default_extension    = 'zip'
        default_file_name    = |{ sy-datum }{ sy-uzeit }|
        file_filter          = '(*.zip)|*.zip|'
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = lv_fullpath
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0 OR lv_fullpath IS INITIAL.
      MESSAGE '用户取消' TYPE 'E' RAISING e_no_choese.
    ENDIF.

    me->file_path = lv_fullpath.
  ENDMETHOD.

  METHOD download.
    TYPES: ty_hex TYPE x LENGTH 200.
    DATA: lt_xdata TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY,
          lv_xlen  TYPE i.

    IF lines( o_zip->files ) = 0.
      MESSAGE '未找到增量文件，取消导出' TYPE 'E' RAISING e_no_file.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = o_zip->save( )
      IMPORTING
        output_length = lv_xlen
      TABLES
        binary_tab    = lt_xdata.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize = lv_xlen
        filename     = me->file_path
        filetype     = 'BIN'
      TABLES
        data_tab     = lt_xdata.

  ENDMETHOD.

  METHOD main.
    DATA: lv_class TYPE seoclsname.
    DATA: li_object TYPE REF TO lif_backup4abap_object.

    " 获取选中的数据
    DATA(lt_option) = lcl_backup4abap_screen_option=>selected_option( ).

    LOOP AT lt_option INTO DATA(lv_option).
      lv_class = 'LCL_BACKUP4ABAP_OBJECT_' && lv_option.

      TRY.
          CREATE OBJECT li_object TYPE (lv_class).
        CATCH cx_sy_create_object_error.
          " PASS
          CONTINUE.
      ENDTRY.

      li_object->loadfiles( ).
      li_object->load2zip( me->o_zip ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_progress_bar IMPLEMENTATION.

  METHOD constructor.
    " me->count = i_count.
    me->base_desc = i_base_desc.
  ENDMETHOD.

  METHOD add.

    me->curr = me->curr + i_add.

    me->percent = me->curr / me->count * 100.

    CHECK me->percent_old <> me->percent.

    me->percent_old = me->percent.

    me->display( i_desc ).

  ENDMETHOD.

  METHOD display.
    DATA: lv_text TYPE string.

    lv_text = |[{ me->curr }/{ me->count }] | && me->base_desc.

    REPLACE FIRST OCCURRENCE OF '&' IN lv_text WITH desc.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = me->percent
        text       = lv_text.
  ENDMETHOD.

ENDCLASS.