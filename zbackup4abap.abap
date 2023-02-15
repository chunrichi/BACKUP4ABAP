*&---------------------------------------------------------------------*
*& Report zbackup4abap
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbackup4abap.

" 借用 DEMO_INDX_BLOB 记录一些增量信息 以加快运行速度

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: tadir.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ty_folder,
         tag    TYPE char10,
         folder TYPE char10,
       END OF ty_folder.

TYPES: BEGIN OF ty_function,
         functionname        LIKE tfdir-funcname,
         functiongroup       LIKE enlfdir-area,
         includenumber       LIKE tfdir-include,
         functionmaininclude LIKE tfdir-funcname,
         functiontitle       LIKE tftit-stext,
         progname            LIKE tfdir-pname,
       END OF ty_function.

TYPES: BEGIN OF ty_log_list,
         main  TYPE text30,
         uname TYPE text12,
         datum TYPE datum,
         uzeit TYPE uzeit,
       END OF ty_log_list.
TYPES: tt_log_list TYPE STANDARD TABLE OF ty_log_list WITH DEFAULT KEY.
TYPES: BEGIN OF ty_log_flow,
         type TYPE text30,
         info TYPE tt_log_list,
       END OF ty_log_flow.

TYPES: BEGIN OF ty_delt_log,
         object TYPE trobjtype, " 对象类型
         ddate  TYPE datum,     " 增量日期
         dtime  TYPE uzeit,     " 增量时间
       END OF ty_delt_log.

CLASS lcl_export_ddldict DEFINITION DEFERRED.
CLASS lcl_pretty_json DEFINITION DEFERRED.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gr_zip       TYPE REF TO cl_abap_zip,         " abap 压缩类
      gr_cover_out TYPE REF TO cl_abap_conv_out_ce. " abap 编码转换类

DATA: gv_parent_folder TYPE string.
DATA: gt_folder TYPE TABLE OF ty_folder.

DATA: gc_extension_name TYPE string VALUE 'abap'.

DATA: gv_export_fullpath TYPE string.

DATA: gt_map_file TYPE TABLE OF string.

DATA: gt_log_flow TYPE TABLE OF ty_log_flow.

DATA: gt_range_devclass TYPE RANGE OF tadir-devclass.

"CONSTANTS: gc_newline type ABAP_CHAR1 VALUE CL_ABAP_CHAR_UTILITIES=>NEWLINE.
CONSTANTS: gc_newline TYPE abap_cr_lf VALUE cl_abap_char_utilities=>cr_lf.


DATA: gt_range_append_class TYPE RANGE OF vseoclass-clsname.

DATA: gt_delt_log TYPE TABLE OF ty_delt_log.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  " 代码相关

  " report
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_prog FOR FIELD p_prog.
    PARAMETERS: p_prog AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  " function
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_func FOR FIELD p_func.
    PARAMETERS: p_func AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

  " Class
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_clas FOR FIELD p_clas.
    PARAMETERS: p_clas AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  " 表相关

  " Table
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_tabl FOR FIELD p_tabl.
    PARAMETERS: p_tabl AS CHECKBOX DEFAULT 'X' USER-COMMAND tab.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 15(10) t_txml MODIF ID gp1 FOR FIELD p_txml.
    PARAMETERS: p_txml TYPE c RADIOBUTTON GROUP gp1 USER-COMMAND gp1 MODIF ID gp1.
    SELECTION-SCREEN COMMENT 35(10) t_tddl MODIF ID gp1 FOR FIELD p_tddl.
    PARAMETERS: p_tddl TYPE c RADIOBUTTON GROUP gp1 MODIF ID gp1 DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN COMMENT 5(70) t_dddl MODIF ID gp1.

  " DDL
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_cdsv FOR FIELD p_cdsv.
    PARAMETERS: p_cdsv AS CHECKBOX DEFAULT 'X'.
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

SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME.
  " 其他

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_smw0 FOR FIELD p_smw0.
    PARAMETERS: p_smw0 AS CHECKBOX DEFAULT ''.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blck3.

SELECTION-SCREEN BEGIN OF BLOCK blck4 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_pack FOR FIELD s_pack.
    SELECT-OPTIONS: s_pack FOR tadir-devclass.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck4.

SELECTION-SCREEN BEGIN OF BLOCK blck5 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(23) t_delt FOR FIELD p_delt.
    PARAMETERS: p_delt TYPE c AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck5.

" 标识用于 接口获取内容
SELECTION-SCREEN BEGIN OF BLOCK brun WITH FRAME.
  " __ 借用 blob 表存储 xstring 数据，读取后删除
  "    同时为返回的文件名
  PARAMETERS: p_brun TYPE char30 NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK brun.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  PERFORM frm_init_text.

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'GP1' AND p_tabl = ''.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    " 功能未有，此处暂时不可显示
    IF screen-name = 'P_TXML'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.


*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
START-OF-SELECTION.

  IF s_pack[] IS NOT INITIAL.
    gt_range_devclass = VALUE #( FOR _ IN s_pack[] ( _ ) ).
  ENDIF.

  PERFORM frm_check.

  PERFORM frm_get_more.

  PERFORM frm_init_variables.

  IF p_brun IS INITIAL.
    " 不获取文件路径
    PERFORM frm_get_path.
  ENDIF.

  PERFORM frm_get_code.
  PERFORM frm_get_ddic.

  PERFORM frm_get_others.

  PERFORM frm_export_zip.

*&---------------------------------------------------------------------*
*& Form frm_init_text
*&---------------------------------------------------------------------*
*&  文本初始化
*&---------------------------------------------------------------------*
FORM frm_init_text .
  " 类限制
  gt_range_devclass = VALUE #( ).

  t_prog = '报表程序'.
  t_func = '函数程序'.
  t_tabl = '表内容'.
  t_txml = '导出 XML'.
  t_tddl = '导出 DDL'.
  t_clas = '类程序'.
  t_cdsv = 'CDS视图'.
  t_doma = '数据域'.
  t_dtel = '数据元素'.
  t_ttyp = '表类型'.
  t_smw0 = 'SMW0(Z*)附件'.

  t_pack = '开发类(包)'.
  t_delt  = '增量获取(不跨client)'.

  t_dddl = '导出DDL文件, 用于 SAP NetWeaver AS for ABAP 7.52 SP00 以上版本 ADT'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_check
*&---------------------------------------------------------------------*
*&  检查输入
*&---------------------------------------------------------------------*
FORM frm_check .
  " 指定包后 不在进行增量获取
  IF s_pack[] IS NOT INITIAL.
    p_delt = ''.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_init_variables
*&---------------------------------------------------------------------*
*&  初始化压缩相关类
*&---------------------------------------------------------------------*
FORM frm_init_variables .
  " 压缩对象
  CREATE OBJECT gr_zip.

  " 编码转换
  gr_cover_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

  " 文件目录
  gt_folder = VALUE #( ( tag = 'MM' ) ( tag = 'PP' )
                       ( tag = 'SD' ) ( tag = 'FI' ) ( tag = 'FICO' folder = 'FI' )
                       ( tag = 'HD' )   " 汉得包
                       ( tag = 'TX' )   " 推送测试平台类
                       ( tag = 'API' )  " 接口处理
                       ( tag = 'JOB' )  " job 处理
                       ( tag = 'JO' folder = 'JOB' )
                       ( tag = 'FILE' ) " 文件处理
                      ).
  SORT gt_folder BY tag.

  " 增量数据获取
  IF p_delt = 'X'.
    IMPORT gt_delt_log FROM DATABASE demo_indx_blob(zd) ID 'DeltaDownload'.
    SORT gt_delt_log BY object.

    " 时间减3s
    LOOP AT gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>).
      <ls_delt_log>-dtime -= 3.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_code
*&---------------------------------------------------------------------*
*&  获取代码数据
*&---------------------------------------------------------------------*
FORM frm_get_code .

  IF p_prog = abap_true.
    gv_parent_folder = `SE38/`.
    PERFORM frm_get_report.
  ENDIF.

  IF p_func = abap_true.
    gv_parent_folder = `SE37/`.
    PERFORM frm_get_function.
  ENDIF.

  IF p_clas = abap_true.
    gv_parent_folder = `SE24/`.
    PERFORM frm_get_class.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_DDIC
*&---------------------------------------------------------------------*
*&  获取表相关内容
*&---------------------------------------------------------------------*
FORM frm_get_ddic .

  IF p_tabl = abap_true.
    gv_parent_folder = `SE11/`.
    IF p_txml = abap_true.
      " no realization
    ELSE.
      PERFORM frm_get_tables_ddl USING gr_zip gr_cover_out `SE11/DDL/TABLE/`.
      PERFORM frm_get_structs_ddl USING gr_zip gr_cover_out `SE11/DDL/STRUCT/`.
    ENDIF.

  ENDIF.

  IF p_cdsv = abap_true.
    gv_parent_folder = `DDL/`.
    PERFORM frm_get_ddl.
  ENDIF.

  IF p_doma = abap_true.
    gv_parent_folder = `SE11/field/Domain/`.
    PERFORM frm_get_domain.
  ENDIF.

  IF p_dtel = abap_true.
    gv_parent_folder = `SE11/field/Element/`.
    PERFORM frm_get_element.
  ENDIF.

  IF p_ttyp = abap_true.
    gv_parent_folder = `SE11/TABLETYPE/`.
    PERFORM frm_get_tabletypes.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_report
*&---------------------------------------------------------------------*
*&  获取报表数据
*&---------------------------------------------------------------------*
FORM frm_get_report .

  " D010SINF REPOSRC 视图（INF 部分） => REPOSRC  程序含代码
  " D010TINF REPOTEXT 视图（INF 部分）=> REPOTEXT 程序文本
  " trdirt   标题文本

  DATA: lv_filename TYPE string.
  DATA: lt_source  TYPE TABLE OF text1000 WITH EMPTY KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: lv_c_flag TYPE char2.

  DATA: lv_folder TYPE char10,
        lv_max    TYPE i.

  " 读取 报表
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
    WHERE (
         ( rep~progname LIKE 'Z%' AND rep~subc = '1' )
      OR ( rep~progname LIKE 'Z%' AND rep~subc = 'I' AND rep~rload = '1' ) )
      AND rep~r3state = 'A'
      AND tad~devclass IN @gt_range_devclass
    INTO TABLE @DATA(lt_list).
  SORT lt_list BY progname.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = 'PROG' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'PROG' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  LOOP AT lt_list INTO DATA(ls_list).
    " 忽略当前程序
    IF ls_list-progname = sy-cprog.
      CONTINUE.
    ENDIF.

    " 文件夹匹配 -> 文件名
    lv_c_flag = |R{ ls_list-subc }|.
    PERFORM frm_get_folder_name USING lv_c_flag ls_list-progname lv_folder.

    IF ls_list-subc = 'I'.
      IF lv_folder+0(1) = 'X'.
        " 特殊
        lv_filename = |CMOD/{ ls_list-progname }.{ gc_extension_name }|.
      ELSE.
        lv_filename = COND #( WHEN lv_folder IS NOT INITIAL
                              THEN |{ lv_folder }/| ).
        lv_filename &&= |INCLUDE/{ ls_list-progname }.{ gc_extension_name }|.
      ENDIF.
    ELSE.
      lv_filename = COND #( WHEN lv_folder IS NOT INITIAL
                            THEN |{ lv_folder }/| ).
      lv_filename &&= |{ ls_list-progname }.{ gc_extension_name }|.
    ENDIF.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_list-text.

    lv_filename = gv_parent_folder && lv_filename.

    " 日志 生成
    PERFORM frm_set_log_flow USING 'PROG' ls_list-progname ls_list-unam ls_list-udat ls_list-utime.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_list-udat
        OR ( ls_delt_log-ddate = ls_list-udat AND ls_delt_log-dtime > ls_list-utime ).
        REFRESH lt_source.
        CLEAR: lv_filename, lv_xstring.

        CONTINUE.
      ENDIF.
    ENDIF.

    " 读取激活状态的代码
    READ REPORT ls_list-progname INTO lt_source STATE 'A' MAXIMUM WIDTH INTO lv_max.
    IF sy-subrc = 0.
      " 内表转换为长字符串
      CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY gc_newline.

      " string -> xstring
      gr_cover_out->convert(
            EXPORTING data = lv_source
            IMPORTING buffer = lv_xstring ).
    ENDIF.

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " 清除缓存
    REFRESH lt_source.
    CLEAR: lv_filename, lv_xstring.
  ENDLOOP.

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_function
*&---------------------------------------------------------------------*
*&  获取函数
*&---------------------------------------------------------------------*
FORM frm_get_function .

  " V_FDIR 查找功能模块的视图（函数名 函数组名）
  "  TFDIR   功能模块 （实际程序名）
  "  ENLFDIR 功能模块的附加属性（函数的函数组）
  " TFTIT  功能模块描述信息
  " TLIBV  负责功能组（函数组 函数组负责人）
  " TADIR  资源对象库（不仅函数部分数据）（包 关联函数）

  DATA: lt_func TYPE TABLE OF ty_function.

  DATA: lv_filename TYPE string.
  DATA: lt_source  TYPE TABLE OF text1000 WITH EMPTY KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: lv_report_name TYPE char50.

  DATA: lv_folder TYPE char10,
        lv_max    TYPE i.

  " 此处获取所有的 Z* 函数组数据
  SELECT
    fu~funcname AS functionname,
    gr~area     AS functiongroup,
    fu~pname    AS progname,
    fu~include  AS includenumber
    FROM enlfdir AS gr
    INNER JOIN tfdir AS fu ON fu~funcname = gr~funcname
    LEFT JOIN tadir AS tad ON tad~obj_name = gr~area
    WHERE gr~area LIKE 'Z%'    " 会包含表维护
      AND gr~funcname LIKE 'Z%'
      AND tad~devclass IN @gt_range_devclass
    INTO CORRESPONDING FIELDS OF TABLE @lt_func.
  IF lt_func IS NOT INITIAL.
    SELECT
      funcname,
      stext
      FROM tftit
      FOR ALL ENTRIES IN @lt_func
      WHERE spras = @sy-langu
        AND funcname = @lt_func-functionname
      INTO TABLE @DATA(lt_tftit).
    SORT lt_tftit BY funcname.
  ENDIF.
  SORT lt_func BY functionname.

  LOOP AT lt_func ASSIGNING FIELD-SYMBOL(<ls_func_fixt>).
    READ TABLE lt_tftit INTO DATA(ls_tftit) WITH KEY funcname = <ls_func_fixt>-functionname BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_func_fixt>-functiontitle = ls_tftit-stext.
    ENDIF.
  ENDLOOP.

  " 更新人获取
  SELECT
    progname,
    unam,
    udat,
    utime
    FROM reposrc
    WHERE progname LIKE 'LZ%' AND appl = 'S'
    INTO TABLE @DATA(lt_rep).
  SORT lt_rep BY progname.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = 'FUNC' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'FUNC' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  LOOP AT lt_func INTO DATA(ls_func).

    PERFORM frm_get_folder_name USING 'F' ls_func-functionname lv_folder.

    IF lv_folder IS NOT INITIAL.
      lv_filename = |{ lv_folder }/{ ls_func-functionname }.abap|.
    ELSE.
      CONCATENATE ls_func-functionname '.abap' INTO lv_filename.
    ENDIF.

    CONCATENATE 'L' ls_func-functiongroup 'U' ls_func-includenumber INTO lv_report_name.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_func-functiontitle.

    lv_filename = gv_parent_folder && lv_filename.

    " 日志 生成
    READ TABLE lt_rep INTO DATA(ls_rep) WITH KEY progname = lv_report_name BINARY SEARCH.
    PERFORM frm_set_log_flow USING 'FUNC' ls_func-functionname ls_rep-unam ls_rep-udat ls_rep-utime.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_rep-udat
        OR ( ls_delt_log-ddate = ls_rep-udat AND ls_delt_log-dtime > ls_rep-utime ).
        REFRESH lt_source.
        CLEAR: lv_filename, lv_xstring, lv_report_name.

        CONTINUE.
      ENDIF.
    ENDIF.

    CLEAR ls_rep.

    " 读取激活状态的代码
    READ REPORT lv_report_name INTO lt_source STATE 'A' MAXIMUM WIDTH INTO lv_max.
    IF sy-subrc = 0.
      " 内表转换为长字符串
      CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY gc_newline.

      " string -> xstring
      gr_cover_out->convert(
            EXPORTING data = lv_source
            IMPORTING buffer = lv_xstring ).
    ENDIF.

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " 清除缓存
    REFRESH lt_source.
    CLEAR: lv_filename, lv_xstring, lv_report_name.
  ENDLOOP.

  " --> more
  " 读取 函数组其他信息
  SELECT
    rep~progname,
    rep~r3state,
    rep~subc,
    rep~unam,      " 修改人
    rep~udat,
    rep~utime
    FROM reposrc AS rep
    LEFT JOIN tadir AS tad ON tad~obj_name = rep~progname
    WHERE ( " ( rep~progname LIKE 'LZ%' AND rep~subc = 'I' AND rep~appl = '' ) " 索引页
            ( rep~progname LIKE 'LZ%' AND rep~rstat = 'S' ) " 索引页 + RFC
         OR ( rep~progname LIKE 'LZ%' AND rep~subc = 'I' AND rep~appl = 'S' AND rep~dbapl = '' )  " O/I/F
         OR ( rep~progname LIKE 'LZ%TOP' AND rep~subc = 'I' AND rep~appl = 'S' ) ) " TOP
      AND rep~r3state = 'A'
      AND tad~devclass IN @gt_range_devclass
    INTO TABLE @DATA(lt_list).
  SORT lt_list BY progname.

  DATA: lv_str_len TYPE i.

  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'FINC' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'FINC' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  LOOP AT lt_list INTO DATA(ls_list).
    " 文件名
    lv_filename = |{ ls_list-progname }.{ gc_extension_name }|.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ''.

    lv_str_len = strlen( ls_list-progname ) - 3.
    CASE ls_list-progname+lv_str_len(1).
      WHEN 'V'.
        lv_filename = gv_parent_folder && `Rfc/` && lv_filename.
      WHEN '$'.
        lv_filename = gv_parent_folder && `Rfc/Unit/` && lv_filename.
      WHEN OTHERS.
        lv_filename = gv_parent_folder && `More/` && lv_filename.
    ENDCASE.

    " 日志 生成
    PERFORM frm_set_log_flow USING 'FINC' ls_list-progname ls_list-unam ls_list-udat ls_list-utime.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_list-udat
        OR ( ls_delt_log-ddate = ls_list-udat AND ls_delt_log-dtime > ls_list-utime ).
        REFRESH lt_source.
        CLEAR: lv_filename, lv_xstring.

        CONTINUE.
      ENDIF.
    ENDIF.

    " 读取激活状态的代码
    READ REPORT ls_list-progname INTO lt_source STATE 'A' MAXIMUM WIDTH INTO lv_max.
    IF sy-subrc = 0.
      " 内表转换为长字符串
      CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY gc_newline.

      " string -> xstring
      gr_cover_out->convert(
            EXPORTING data = lv_source
            IMPORTING buffer = lv_xstring ).
    ENDIF.

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " 清除缓存
    REFRESH lt_source.
    CLEAR: lv_filename, lv_xstring.
  ENDLOOP.
  " <--

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_export_zip
*&---------------------------------------------------------------------*
*&  导出压缩文件
*&---------------------------------------------------------------------*
FORM frm_export_zip .
  DATA: lv_zip_xstr TYPE xstring.

  TYPES: ty_hex TYPE x LENGTH 200.

  DATA: lt_xdata TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY,
        lv_xlen  TYPE i.

  " 压缩文件保存
  lv_zip_xstr = gr_zip->save( ).

  lv_xlen = xstrlen( lv_zip_xstr ).

  " __接口获取时导入 blob表

  IF p_brun IS INITIAL.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_zip_xstr
      IMPORTING
        output_length = lv_xlen
      TABLES
        binary_tab    = lt_xdata.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize = lv_xlen
        filename     = gv_export_fullpath
        filetype     = 'BIN'
      TABLES
        data_tab     = lt_xdata.
  ELSE.
    EXPORT xlen = lv_xlen
           zip  = lv_zip_xstr TO DATABASE demo_indx_blob(zb) ID p_brun.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_path
*&---------------------------------------------------------------------*
*&  获取存储路径
*&---------------------------------------------------------------------*
FORM frm_get_path .

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
    MESSAGE '用户取消' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  gv_export_fullpath = lv_fullpath.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_map_file
*&---------------------------------------------------------------------*
*&  设置 map 文件内容
*&---------------------------------------------------------------------*
FORM frm_set_map_file USING VALUE(p_file_path)
                            VALUE(p_file_desc).

  APPEND |<a href="./{ p_file_path }">{ p_file_desc }</a>| TO gt_map_file.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_add_map_file
*&---------------------------------------------------------------------*
*& 添加 map 文件到 zip
*&---------------------------------------------------------------------*
FORM frm_add_map_file USING VALUE(p_folder).
  DATA: lv_source  TYPE string,
        lv_xstring TYPE xstring.

  CONCATENATE LINES OF gt_map_file INTO lv_source SEPARATED BY gc_newline.
  gr_cover_out->convert(
        EXPORTING data = lv_source
        IMPORTING buffer = lv_xstring ).
  gr_zip->add( name = |{ gv_parent_folder }map.html|
               content = lv_xstring ).

  REFRESH gt_map_file.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_CLASS
*&---------------------------------------------------------------------*
*&  获取类代码
*&---------------------------------------------------------------------*
FORM frm_get_class .
  DATA: BEGIN OF ls_class_key,
          classnamelength   TYPE i,
          publicclasskey    TYPE string,
          privateclasskey   TYPE string,
          protectedclasskey TYPE string,
          textelementkey    TYPE string,
          typesclasskey     TYPE string,
        END OF ls_class_key.
  DATA: ls_include TYPE progstruc.
  DATA: lt_str     TYPE rswsourcet,
        lt_source  TYPE TABLE OF text1000 WITH EMPTY KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.

  DATA: lv_filename TYPE string,
        lv_more     TYPE string.

  TYPES: BEGIN OF ty_type,
           type   TYPE seop_include_ext_app,
           exline TYPE i,
         END OF ty_type.

  DATA: lt_type TYPE TABLE OF ty_type,
        lv_max  TYPE i.

  lt_type = VALUE #( ( type = seop_ext_class_locals_def  exline = 4 )
                     ( type = seop_ext_class_locals_imp  exline = 4 )
                     ( type = seop_ext_class_macros      exline = 3 )
                     ( type = seop_ext_class_testclasses exline = 1 ) ).

  SELECT
    clsname,
    langu,
    descript,
    msg_id,

    exposure,
    state,
    clsfinal,
    r3release,
    changedby,
    changedon
    FROM vseoclass AS ss
    INNER JOIN tadir AS ta ON ta~obj_name = ss~clsname
    WHERE "langu = @sy-langu " 语言会限制内容
          ( ( clsname LIKE 'ZC%' OR clsname LIKE 'YC%' )
       OR ( clsname LIKE 'ZHD%' OR clsname LIKE 'YHD%' ) )
      AND version = '1' " 激活
      AND ( state = '0' OR state = '1' )
      AND ta~devclass IN @gt_range_devclass
    INTO TABLE @DATA(lt_class).
  " 排序去重
  SORT lt_class BY clsname langu.
  DELETE ADJACENT DUPLICATES FROM lt_class COMPARING clsname.

  IF gt_range_append_class IS NOT INITIAL.
    SELECT
      clsname,
      langu,
      descript,
      msg_id,

      exposure,
      state,
      clsfinal,
      r3release,
      changedby,
      changedon
      FROM vseoclass
      WHERE clsname IN @gt_range_append_class
        AND version = '1' " 激活
        AND ( state = '0' OR state = '1' )
      APPENDING TABLE @lt_class.
    " 多语言去重
    SORT lt_class BY clsname langu.
    DELETE ADJACENT DUPLICATES FROM lt_class COMPARING clsname.
  ENDIF.

  " --> 获取真实修改时间（类下的子节点）
  " 参考 LSEODF1X 948 行
  DATA: lt_range_name TYPE RANGE OF progdir-name.
  lt_range_name = VALUE #( FOR itm IN lt_class
                            WHERE ( changedon IS INITIAL )
                              ( sign = 'I' option = 'CP' low = |{ itm-clsname }*| ) ).
  IF lt_range_name IS NOT INITIAL.
    SELECT
      name,
      unam,
      udat
      FROM progdir
      WHERE name IN @lt_range_name
      INTO TABLE @DATA(lt_progdir).

    LOOP AT lt_progdir ASSIGNING FIELD-SYMBOL(<ls_progdir>).
      <ls_progdir>-name = replace( val = <ls_progdir>-name pcre = `=+.*$` with = `` occ = -1 ).
    ENDLOOP.
    SORT lt_progdir BY name udat DESCENDING.
  ENDIF.
  " <--

  " 读取类
  DATA: lo_source   TYPE REF TO object,
        lo_instance TYPE REF TO object.
  DATA: lr_source TYPE REF TO cl_oo_source,
        ls_clskey TYPE seoclskey.

  CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
    RECEIVING
      result = lo_instance.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = 'CLSS' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'CLSS' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  LOOP AT lt_class INTO DATA(ls_class).

    lv_filename = ls_class-clsname && '.abap'.

    " >> 特殊指定
    IF ls_class-clsname+1(2) = 'HD'.
      lv_filename = 'HD/' && lv_filename.
    ENDIF.
    IF ls_class-clsname+4(2) = 'SI'.
      lv_filename = 'IF/' && lv_filename.
    ENDIF.

    "ls_class_key-classnamelength = strlen( ls_class-clsname ).
    "
    "ls_class_key-publicclasskey = cl_oo_classname_service=>get_pubsec_name( clsname = ls_class-clsname ).
    "ls_class_key-privateclasskey = cl_oo_classname_service=>get_prisec_name( clsname = ls_class-clsname ).
    "ls_class_key-protectedclasskey = cl_oo_classname_service=>get_prosec_name( clsname = ls_class-clsname ).
    "
    "ls_class_key-textelementkey = ls_class-clsname.
    "DO 30 - ls_class_key-classnamelength TIMES.
    "  CONCATENATE ls_class_key-textelementkey '=' INTO ls_class_key-textelementkey.
    "ENDDO.
    "CONCATENATE ls_class_key-textelementkey 'CP' INTO ls_class_key-textelementkey.
    "
    "ls_class_key-typesclasskey = ls_class-clsname.
    "DO 30 - ls_class_key-classnamelength TIMES.
    "  CONCATENATE ls_class_key-typesclasskey '=' INTO ls_class_key-typesclasskey.
    "ENDDO.
    "CONCATENATE ls_class_key-typesclasskey 'CT' INTO ls_class_key-typesclasskey.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_class-descript.

    lv_filename = gv_parent_folder && lv_filename.

    " 真实修改时间
    IF ls_class-changedon IS INITIAL.
      READ TABLE lt_progdir INTO DATA(ls_prodir_udat) WITH KEY name = ls_class-clsname BINARY SEARCH.
      IF sy-subrc = 0.
        ls_class-changedon = ls_prodir_udat-udat.
        ls_class-changedby = ls_prodir_udat-unam.
      ENDIF.
    ENDIF.

    " 日志 生成
    PERFORM frm_set_log_flow USING 'CLASS' ls_class-clsname ls_class-changedby ls_class-changedon '00000000'.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_class-changedon.
        REFRESH lt_str.
        CLEAR: lv_filename, lv_xstring.

        CONTINUE.
      ENDIF.
    ENDIF.

    " > 读取源码
    CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = ls_class-clsname
        version   = 'A'
      RECEIVING
        result    = lo_source.

    CALL METHOD lo_source->('GET_SOURCE')
      IMPORTING
        source = lt_str.

    " 内表转换为长字符串
    CONCATENATE LINES OF lt_str INTO lv_source SEPARATED BY gc_newline.

    " string -> xstring
    gr_cover_out->convert(
          EXPORTING data = lv_source
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " --> add 更多信息

    LOOP AT lt_type INTO DATA(ls_type).
      ls_include-rootname = ls_class-clsname.
      TRANSLATE ls_include-rootname USING ' ='.
      ls_include-categorya = ls_type-type(1).
      ls_include-codea = ls_type-type+1(4).

      READ REPORT ls_include INTO lt_source STATE 'A' MAXIMUM WIDTH INTO lv_max.
      IF sy-subrc = 0 AND lines( lt_source ) > ls_type-exline.
        lv_more = lv_filename.
        REPLACE FIRST OCCURRENCE OF |{ ls_class-clsname }.abap|
           IN lv_more WITH |More/{ ls_class-clsname }-{ ls_type-type }.abap|.

        " map 文件路径
        PERFORM frm_set_map_file USING lv_filename ls_type-type.

        " 内表转换为长字符串
        CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY gc_newline.

        " string -> xstring
        gr_cover_out->convert(
              EXPORTING data = lv_source
              IMPORTING buffer = lv_xstring ).

        " 添加到压缩包
        gr_zip->add( name    = lv_more
                     content = lv_xstring ).
      ENDIF.
      CLEAR ls_include.
    ENDLOOP.
    " <--

    REFRESH lt_str.
    CLEAR: lv_filename, lv_xstring.
  ENDLOOP.

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_logs
*&---------------------------------------------------------------------*
*&  日志记录
*&---------------------------------------------------------------------*
FORM frm_get_logs .

  DATA: lv_xstring TYPE xstring.
  DATA: lv_filename TYPE string.

  " json
  lv_filename = `logs/log_flow_` && sy-datum && `.json`.

  GET REFERENCE OF gt_log_flow INTO DATA(lr_log_flow).
  DATA: lv_req_json TYPE string.
  lv_req_json = /ui2/cl_json=>serialize( data = lr_log_flow
                                          pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  gr_cover_out->convert(
        EXPORTING data = lv_req_json
        IMPORTING buffer = lv_xstring ).

  " 添加到压缩包
  gr_zip->add( name    = lv_filename
               content = lv_xstring ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_log_flow
*&---------------------------------------------------------------------*
*&  获取流日志
*&---------------------------------------------------------------------*
FORM frm_set_log_flow  USING VALUE(p_type) TYPE text30
                             VALUE(p_progname)
                             VALUE(p_unam)
                             VALUE(p_udat)
                             VALUE(p_utime).

  DATA: lv_uname TYPE text12.

  lv_uname = p_unam.

  IF p_unam IS INITIAL.
    lv_uname = `Other`.
  ENDIF.

  READ TABLE gt_log_flow ASSIGNING FIELD-SYMBOL(<ls_log_flow>) WITH KEY type = p_type.
  IF sy-subrc <> 0.
    APPEND INITIAL LINE TO gt_log_flow ASSIGNING <ls_log_flow>.
    <ls_log_flow>-type = p_type.
  ENDIF.

  APPEND VALUE #( main = |{ p_progname }|
                  uname = lv_uname
                  datum = |{ p_udat }|
                  uzeit = |{ p_utime }| ) TO <ls_log_flow>-info.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_ddl
*&---------------------------------------------------------------------*
*&  获取 DDL
*&---------------------------------------------------------------------*
FORM frm_get_ddl .

  DATA: lv_filename TYPE string.
  DATA: lt_source  TYPE TABLE OF text1000 WITH EMPTY KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.

  DATA: lv_folder TYPE char2,
        lv_max    TYPE i.

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
    LEFT JOIN ddddlsrct AS ct ON ct~ddlname = rc~ddlname
                             AND ct~ddlanguage = '1'
                               AND ct~as4local = rc~as4local
    WHERE rc~ddlname LIKE 'Z%'
      AND rc~as4local = 'A'
      AND ta~devclass IN @gt_range_devclass
    INTO TABLE @DATA(lt_ddlsrc).

  SORT lt_ddlsrc BY ddlname.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = 'DDLS' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'DDLS' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  LOOP AT lt_ddlsrc INTO DATA(ls_ddl).
    " 文件夹匹配 -> 文件名

    PERFORM frm_get_folder_name USING 'D' ls_ddl-ddlname lv_folder.

    IF lv_folder IS NOT INITIAL.
      lv_filename = |{ lv_folder }/{ ls_ddl-ddlname }.ddl|.
    ELSE.
      CONCATENATE ls_ddl-ddlname '.ddl' INTO lv_filename.
    ENDIF.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_ddl-ddtext.

    lv_filename = gv_parent_folder && lv_filename.

    " 日志 生成
    PERFORM frm_set_log_flow USING 'DDLS' ls_ddl-ddlname ls_ddl-as4user ls_ddl-as4date ls_ddl-as4time.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_ddl-as4date
        OR ( ls_delt_log-ddate = ls_ddl-as4date AND ls_delt_log-dtime > ls_ddl-as4time ).
        REFRESH lt_source.
        CLEAR: lv_filename, lv_xstring.

        CONTINUE.
      ENDIF.
    ENDIF.

    " --> Begin 清除末尾注释
    REPLACE FIRST OCCURRENCE OF REGEX `\/\*\+\[internal\].*\*\/` IN ls_ddl-source WITH ``.
    " <--

    " string -> xstring
    gr_cover_out->convert(
          EXPORTING data = ls_ddl-source
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " 清除缓存
    REFRESH lt_source.
    CLEAR: lv_filename, lv_xstring.
  ENDLOOP.

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_more
*&---------------------------------------------------------------------*
*&  获取更多数据
*&---------------------------------------------------------------------*
FORM frm_get_more .

  IF s_pack[] IS INITIAL.
    " 更多 类

  ENDIF.
ENDFORM.

CLASS lcl_export_ddldict DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: ty_type TYPE string.
    CONSTANTS: BEGIN OF types,
                 label  TYPE ty_type VALUE 'label',
                 encat  TYPE ty_type VALUE 'encat',
                 tabty  TYPE ty_type VALUE 'tabty',
                 edits  TYPE ty_type VALUE 'edits',
                 deliv  TYPE ty_type VALUE 'deliv',
                 actty  TYPE ty_type VALUE 'actty',
                 repla  TYPE ty_type VALUE 'repla',
                 define TYPE ty_type VALUE 'define',
               END OF types.
    TYPES: BEGIN OF ty_foreignkey_eq,
             fortable   TYPE fortable,
             forkey     TYPE forkey,
             checkfield TYPE fieldname,
           END OF ty_foreignkey_eq.
    TYPES: tty_foreignkey_eq TYPE TABLE OF ty_foreignkey_eq WITH EMPTY KEY.
    TYPES: BEGIN OF ty_foreignkey,
             label         TYPE string,
             screencheck   TYPE text6,
             keytype       TYPE frkart,
             cardleft      TYPE cardleft,
             card          TYPE card,
             checktable    TYPE string,
             foreignkey_eq TYPE tty_foreignkey_eq,
           END OF ty_foreignkey.
    TYPES: BEGIN OF ty_field,
             name       TYPE string,
             key        TYPE char1,
             nonull     TYPE char1,
             type       TYPE string,
             addtion    TYPE string, " 补充
             adddesc    TYPE string,
             foreignkey TYPE ty_foreignkey,
           END OF ty_field.
    TYPES: tty_fields TYPE TABLE OF ty_field WITH EMPTY KEY.

    TYPES: BEGIN OF ty_table,
             name      TYPE string, " 表名
             desc      TYPE string, " 表描述
             fields    TYPE tty_fields,
             field_max TYPE i,

             deliv     TYPE string, " 提交类
             actty     TYPE string, " 激活类型
             edits     TYPE string, " 表维护
             encat     TYPE string, " 增强类别
           END OF ty_table.

    TYPES: BEGIN OF ty_string,
             table  TYPE string,
             string TYPE string,
           END OF ty_string.

    DATA: gt_tables TYPE TABLE OF ty_table,
          gt_string TYPE TABLE OF ty_string.

    METHODS: table_maintain.
    METHODS: struct_maintain.

  PRIVATE SECTION.
    DATA: ddldicts TYPE TABLE OF text1000.

    METHODS: add_line IMPORTING str TYPE string OPTIONAL type TYPE ty_type.

ENDCLASS.


CLASS lcl_pretty_json DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: pretty IMPORTING json               TYPE string
                          RETURNING VALUE(pretty_json) TYPE string.
ENDCLASS.

CLASS lcl_export_ddldict IMPLEMENTATION.

  METHOD: table_maintain.
    DATA: lv_string  TYPE string,
          lv_xstring TYPE xstring.

    LOOP AT gt_tables INTO DATA(ls_table).
      " 描述
      add_line( str = ls_table-desc  type = me->types-label ).
      " 增强类别
      add_line( str = ls_table-encat type = me->types-encat ).
      " 表类别
      add_line( str = '' type = me->types-tabty ).
      " 交付类
      add_line( str = ls_table-deliv type = me->types-deliv ).
      " 维护方式
      add_line( str = ls_table-edits type = me->types-edits ).
      " ??
      "add_line( str = ls_table-actty type = me->types-actty ).
      " 表名称
      add_line( str = |{ ls_table-name CASE = LOWER }|  type = me->types-define ).

      LOOP AT ls_table-fields INTO DATA(ls_field).
        IF ls_field-foreignkey-label IS NOT INITIAL.
          APPEND |  @AbapCatalog.foreignKey.label : '{ ls_field-foreignkey-label }'| TO me->ddldicts.
        ENDIF.
        IF ls_field-foreignkey-keytype IS NOT INITIAL.
          CASE ls_field-foreignkey-keytype.
            WHEN 'KEY'.
              APPEND |  @AbapCatalog.foreignKey.keyType : #{ 'KEY' }| TO me->ddldicts.
            WHEN 'REF'.
              APPEND |  @AbapCatalog.foreignKey.keyType : #{ 'NON_KEY' }| TO me->ddldicts.
            WHEN 'TEXT'.
              APPEND |  @AbapCatalog.foreignKey.keyType : #{ 'TEXT_KEY' }| TO me->ddldicts.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.
        IF ls_field-foreignkey-screencheck IS NOT INITIAL.
          APPEND |  @AbapCatalog.foreignKey.screenCheck : { ls_field-foreignkey-screencheck }| TO me->ddldicts.
        ENDIF.

        lv_string = COND #( WHEN ls_field-key = 'X' THEN `key ` ELSE `` ) && ls_field-name.

        IF ls_field-adddesc IS NOT INITIAL.
          APPEND |  @EndUserText.label : '{ ls_field-adddesc }'| TO me->ddldicts.
        ENDIF.

        IF ls_field-addtion IS NOT INITIAL.
          APPEND ls_field-addtion TO me->ddldicts.
        ENDIF.

        IF ls_field-foreignkey-checktable IS INITIAL.
          IF ls_field-name IS INITIAL.
            APPEND |  { ls_field-type }{ COND #( WHEN ls_field-nonull = 'X' THEN | not null| ) };| TO me->ddldicts.
          ELSE.
            APPEND |  { lv_string WIDTH = ls_table-field_max + 1 }: { ls_field-type }{ COND #( WHEN ls_field-nonull = 'X' THEN | not null| ) };| TO me->ddldicts.
          ENDIF.
        ELSE.
          IF ls_field-name IS INITIAL.
            APPEND |  { ls_field-type }{ COND #( WHEN ls_field-nonull = 'X' THEN | not null| ) }| TO me->ddldicts.
          ELSE.
            APPEND |  { lv_string WIDTH = ls_table-field_max + 1 }: { ls_field-type }{ COND #( WHEN ls_field-nonull = 'X' THEN | not null| ) }| TO me->ddldicts.
          ENDIF.

          IF ls_field-foreignkey-card = '' AND ls_field-foreignkey-cardleft = ''.
            APPEND |    with foreign key { ls_field-foreignkey-checktable }| TO me->ddldicts.
          ELSE.
            APPEND |    with foreign key [{ COND string( WHEN ls_field-foreignkey-card = '1' THEN '1'
                              WHEN ls_field-foreignkey-card = 'C' THEN '0..1'
                              ELSE '0..*' ) },{ COND string( WHEN ls_field-foreignkey-cardleft = '1' THEN '1' ELSE '0..1' ) }] { ls_field-foreignkey-checktable }| TO me->ddldicts.
          ENDIF.
          LOOP AT ls_field-foreignkey-foreignkey_eq ASSIGNING FIELD-SYMBOL(<ls_eq>).
            AT FIRST.
              APPEND |      where { <ls_eq>-checkfield } = { COND string( WHEN <ls_eq>-forkey IS INITIAL THEN <ls_eq>-fortable
                                                                          ELSE |{ <ls_eq>-fortable CASE = LOWER }.{ <ls_eq>-forkey }| ) }| TO me->ddldicts.
              CONTINUE.
            ENDAT.

            APPEND |        and { <ls_eq>-checkfield } = { COND string( WHEN <ls_eq>-forkey IS INITIAL THEN <ls_eq>-fortable
                                                                          ELSE |{ <ls_eq>-fortable CASE = LOWER }.{ <ls_eq>-forkey }| ) }| TO me->ddldicts.
          ENDLOOP.
          IF sy-subrc = 0.
            me->ddldicts[ lines( me->ddldicts ) ] = me->ddldicts[ lines( me->ddldicts ) ] && ';'.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " 结束
      add_line( str = '' type = '' ).
      add_line( str = '}' type = '' ).

      APPEND INITIAL LINE TO me->gt_string ASSIGNING FIELD-SYMBOL(<ls_string>).
      <ls_string>-table = ls_table-name.

      CONCATENATE LINES OF me->ddldicts INTO <ls_string>-string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      REFRESH me->ddldicts.
    ENDLOOP.

  ENDMETHOD.

  METHOD struct_maintain.
    DATA: lv_string  TYPE string.

    LOOP AT gt_tables INTO DATA(ls_table).
      " 描述
      add_line( str = ls_table-desc  type = me->types-label ).
      " 增强类别
      add_line( str = ls_table-encat type = me->types-encat ).

      " 表名称
      add_line( str = |define structure { ls_table-name CASE = LOWER } \{| type = '' ).

      LOOP AT ls_table-fields INTO DATA(ls_field).
        IF ls_field-adddesc IS NOT INITIAL.
          APPEND |  @EndUserText.label : '{ ls_field-adddesc }'| TO me->ddldicts.
        ENDIF.

        IF ls_field-addtion IS NOT INITIAL.
          APPEND ls_field-addtion TO me->ddldicts.
        ENDIF.

        IF ls_field-name IS INITIAL.
          APPEND |  { ls_field-type }{ COND #( WHEN ls_field-nonull = 'X' THEN | not null| ) };| TO me->ddldicts.
        ELSE.
          APPEND |  { ls_field-name WIDTH = ls_table-field_max + 1 }: { ls_field-type }{ COND #( WHEN ls_field-nonull = 'X' THEN | not null| ) };| TO me->ddldicts.
        ENDIF.
      ENDLOOP.

      " 结束
      add_line( str = '' type = '' ).
      add_line( str = '}' type = '' ).

      APPEND INITIAL LINE TO me->gt_string ASSIGNING FIELD-SYMBOL(<ls_string>).
      <ls_string>-table = ls_table-name.

      CONCATENATE LINES OF me->ddldicts INTO <ls_string>-string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      REFRESH me->ddldicts.
    ENDLOOP.

  ENDMETHOD.

  METHOD: add_line.
    CASE type.
      WHEN me->types-label.
        " 描述
        APPEND |@EndUserText.label : '{ str }'| TO me->ddldicts.
      WHEN me->types-encat.
        " 增强类别

        " #NOT_CLASSIFIED         - 未分类
        " #NOT_EXTENSIBLE         - 无法增强（扩展）
        " #EXTENSIBLE_CHARACTER   - 可以增强（扩展）并且类似于角色
        " #EXTENSIBLE_CHARACTER_NUMERIC - 可以增强（扩展），并且是类似字符或数字的
        " #EXTENSIBLE_ANY         - 可以以任何方式增强（扩展）
        CASE str.
          WHEN '0'.
            APPEND |@AbapCatalog.enhancementCategory : #{ 'NOT_CLASSIFIED' }| TO me->ddldicts.
          WHEN '1'.
            APPEND |@AbapCatalog.enhancementCategory : #{ 'NOT_EXTENSIBLE' }| TO me->ddldicts.
          WHEN '2'.
            APPEND |@AbapCatalog.enhancementCategory : #{ 'EXTENSIBLE_CHARACTER' }| TO me->ddldicts.
          WHEN '3'.
            APPEND |@AbapCatalog.enhancementCategory : #{ 'EXTENSIBLE_CHARACTER_NUMERIC' }| TO me->ddldicts.
          WHEN '4'.
            APPEND |@AbapCatalog.enhancementCategory : #{ 'EXTENSIBLE_ANY' }| TO me->ddldicts.
          WHEN OTHERS.
        ENDCASE.
      WHEN me->types-tabty.
        " 表类别

        " #TRANSPARENT            - 透明表
        " #GLOBAL_TEMPORARY       - 全局临时表 （GTT）
        APPEND |@AbapCatalog.tableCategory : { COND #( WHEN str IS INITIAL THEN '#TRANSPARENT' ELSE str ) }| TO me->ddldicts.
      WHEN me->types-edits.
        " 维护方式

        " RESTRICTED   - 补充
        " #NOT_ALLOWED - 无显示/编辑
        " #LIMITED     - 有限的显示/编辑
        " #ALLOWED     - 允许显示/编辑
        CASE str.
          WHEN ''.
            APPEND |@AbapCatalog.dataMaintenance : #{ 'RESTRICTED' }| TO me->ddldicts.
          WHEN 'D'.
            APPEND |@AbapCatalog.dataMaintenance : #{ 'LIMITED' }| TO me->ddldicts.
          WHEN 'N'.
            APPEND |@AbapCatalog.dataMaintenance : #{ 'NOT_ALLOWED' }| TO me->ddldicts.
          WHEN 'X'.
            APPEND |@AbapCatalog.dataMaintenance : #{ 'ALLOWED' }| TO me->ddldicts.
          WHEN OTHERS.
        ENDCASE.
      WHEN me->types-deliv.
        " 交付类

        " #A - delivery class A
        " #C - delivery class C
        " #L - delivery class L
        " #G - delivery class G
        " #E - delivery class E
        " #S - delivery class S
        " #W - delivery class W
        APPEND |@AbapCatalog.deliveryClass : #{ COND #( WHEN str IS INITIAL THEN 'A' ELSE str ) }| TO me->ddldicts.
      WHEN me->types-actty.
        " 激活类型

        " #NOT_CLASSIFIED             - 激活类型 00
        " #NAMETAB_GENERATION_OFFLINE - 激活类型 01
        " #ADAPT_C_STRUCTURES         - 激活类型 02
        " #INITIAL_TABLE_REQUIRED     - 激活类型 10
        CASE str.
          WHEN '00'.
            APPEND |@AbapCatalog.activationType : #{ 'NOT_CLASSIFIED' }| TO me->ddldicts.
          WHEN '01'.
            APPEND |@AbapCatalog.activationType : #{ 'NAMETAB_GENERATION_OFFLINE' }| TO me->ddldicts.
          WHEN '02'.
            APPEND |@AbapCatalog.activationType : #{ 'ADAPT_C_STRUCTURES' }| TO me->ddldicts.
          WHEN '10'.
            APPEND |@AbapCatalog.activationType : #{ 'INITIAL_TABLE_REQUIRED' }| TO me->ddldicts.
          WHEN OTHERS.
        ENDCASE.
      WHEN me->types-repla.
        " 替换对象

        APPEND |@AbapCatalog.replacementObject : { str }| TO me->ddldicts.
      WHEN me->types-define.
        " 表名
        APPEND |define table { str } \{| TO me->ddldicts.
      WHEN OTHERS.
        APPEND str TO me->ddldicts.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_pretty_json IMPLEMENTATION.
  METHOD pretty.

    "cloud
    DATA(json_xstring) = cl_abap_conv_codepage=>create_out( )->convert( json ).
    "on_premise
    "DATA(json_xstring) = cl_abap_codepage=>convert_to( json_string_in ).

    "Check and pretty print JSON

    DATA(reader) = cl_sxml_string_reader=>create( json_xstring ).
    DATA(writer) = CAST if_sxml_writer(
                          cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    writer->set_option( option = if_sxml_writer=>co_opt_indent ).
    reader->next_node( ).
    reader->skip_node( writer ).

    "cloud
    DATA(json_formatted_string) = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( writer )->get_output( ) ).
    "on premise
    "DATA(json_formatted_string) = cl_abap_codepage=>convert_from( CAST cl_sxml_string_writer( writer )->get_output( ) ).

    pretty_json = escape( val = json_formatted_string format = cl_abap_format=>e_xml_text  ).

  ENDMETHOD.
ENDCLASS.

FORM frm_get_tables_ddl USING pr_zip TYPE REF TO cl_abap_zip
                              pr_cover_out TYPE REF TO cl_abap_conv_out_ce
                              p_filename TYPE string.
  CHECK pr_zip IS NOT INITIAL
    AND pr_cover_out IS NOT INITIAL.

  DATA: lr_ddl TYPE REF TO lcl_export_ddldict.

  lr_ddl = NEW lcl_export_ddldict( ).

  PERFORM frm_get_xx_ddl USING 'TRANSP' lr_ddl.

  lr_ddl->table_maintain( ).
  DATA: lv_filename TYPE string,
        lv_xstring  TYPE xstring.

  LOOP AT lr_ddl->gt_string INTO DATA(ls_string).

    lv_filename = p_filename && ls_string-table && `.ddl`.

    pr_cover_out->convert(
          EXPORTING data = ls_string-string
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    pr_zip->add( name    = lv_filename
                 content = lv_xstring ).
  ENDLOOP.

ENDFORM.

FORM frm_get_structs_ddl USING pr_zip TYPE REF TO cl_abap_zip
                              pr_cover_out TYPE REF TO cl_abap_conv_out_ce
                              p_filename TYPE string.
  CHECK pr_zip IS NOT INITIAL
    AND pr_cover_out IS NOT INITIAL.

  DATA: lr_ddl TYPE REF TO lcl_export_ddldict.

  lr_ddl = NEW lcl_export_ddldict( ).

  PERFORM frm_get_xx_ddl USING 'INTTAB' lr_ddl.

  lr_ddl->struct_maintain( ).
  DATA: lv_filename TYPE string,
        lv_xstring  TYPE xstring.

  LOOP AT lr_ddl->gt_string INTO DATA(ls_string).

    lv_filename = p_filename && ls_string-table && `.ddl`.

    pr_cover_out->convert(
          EXPORTING data = ls_string-string
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    pr_zip->add( name    = lv_filename
                 content = lv_xstring ).
  ENDLOOP.

ENDFORM.


FORM frm_get_xx_ddl USING p_type TYPE tabclass pr_ddl TYPE REF TO lcl_export_ddldict.

  " 查询基础数据
  SELECT
   dd~tabname,
   dd~contflag,
   dd~authclass,
   dd~mainflag,
   dd~exclass,
   dd~as4date,
   dd~as4time
   FROM dd02l AS dd
   INNER JOIN tadir AS ta ON dd~tabname = ta~obj_name
   WHERE dd~tabname LIKE 'Z%'
     AND dd~tabclass = @p_type
     AND dd~as4local = 'A'
     AND ta~pgmid = 'R3TR'
     AND ta~object = 'TABL'
     AND ta~devclass IN @gt_range_devclass
   INTO TABLE @DATA(lt_dd02l).

  CHECK lt_dd02l IS NOT INITIAL.

  SELECT
    tabname,
    ddlanguage,
    ddtext
    FROM dd02t
    FOR ALL ENTRIES IN @lt_dd02l
    WHERE tabname = @lt_dd02l-tabname
      AND ddlanguage IN ('1','E')
    INTO TABLE @DATA(lt_dd02t).
  SORT lt_dd02t BY tabname ddlanguage.
  DELETE ADJACENT DUPLICATES FROM lt_dd02t COMPARING tabname.

  " 字段 与 字段描述
  SELECT
    *
    FROM dd03l AS 3l
    FOR ALL ENTRIES IN @lt_dd02l
    WHERE tabname  = @lt_dd02l-tabname
      AND adminfield = '0' " 仅处理第一级别的字段/结构 => 在函数里有效 直接查表需要下方的逻辑
      AND depth = '00'     " adminfield 针对结构有效 depth 针对结构套结构有效
      AND as4local   = 'A'
    INTO TABLE @DATA(lt_dd03l).
  SORT lt_dd03l BY tabname position.

  SELECT
    tabname,
    fieldname,
    ddlanguage,
    ddtext
    FROM dd03t AS 3t
    FOR ALL ENTRIES IN @lt_dd02l
    WHERE tabname = @lt_dd02l-tabname
      AND as4local = 'A'
      AND ddlanguage IN ('1','E')
    INTO TABLE @DATA(lt_dd03t).
  SORT lt_dd03t BY tabname fieldname ddlanguage.
  DELETE ADJACENT DUPLICATES FROM lt_dd03t COMPARING tabname fieldname.


  " --> 补充 外键逻辑
  IF p_type = 'TRANSP'.
    SELECT
      tabname,
      fieldname,
      primpos,
      fortable,
      forkey,
      checktable,
      checkfield
      FROM dd05q
      FOR ALL ENTRIES IN @lt_dd02l
      WHERE tabname = @lt_dd02l-tabname
      INTO TABLE @DATA(lt_dd05q).
    SORT lt_dd05q BY tabname fieldname primpos.

    SELECT
      8l~tabname,
      8l~fieldname,
      8l~checktable,
      8l~frkart,
      8l~cardleft,
      8l~card,
      8t~ddlanguage,
      8t~ddtext
      FROM dd08l AS 8l
      LEFT JOIN dd08t AS 8t ON 8t~tabname = 8l~tabname
                           AND 8t~fieldname = 8l~fieldname
                           AND 8t~as4local = 8l~as4local
                           AND 8t~as4vers = 8l~as4vers
                           AND 8t~ddlanguage IN ('1','E')
      FOR ALL ENTRIES IN @lt_dd02l
      WHERE 8l~tabname = @lt_dd02l-tabname
      INTO TABLE @DATA(lt_dd08l).
    SORT lt_dd08l BY tabname fieldname ddlanguage.
    DELETE ADJACENT DUPLICATES FROM lt_dd08l COMPARING tabname fieldname.
  ENDIF.
  " <--

  DATA: lv_gotstate TYPE dcobjif-gotstate.
  DATA: lt_dd03p TYPE TABLE OF dd03p,
        ls_dd02v TYPE dd02v.

  DATA: lv_tabix TYPE sytabix.


  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = p_type+0(4) BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = p_type+0(4) ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.


  LOOP AT lt_dd02l INTO DATA(ls_dd02l).

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_dd02l-as4date
        OR ( ls_delt_log-ddate = ls_dd02l-as4date AND ls_delt_log-dtime > ls_dd02l-as4time ).
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO pr_ddl->gt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).

    " 表名
    <ls_table>-name = ls_dd02l-tabname.

    " 表描述
    READ TABLE lt_dd02t INTO DATA(ls_dd02t) WITH KEY tabname = ls_dd02l-tabname BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_table>-desc = ls_dd02t-ddtext.
    ENDIF.

    " 额外数据
    <ls_table>-deliv = ls_dd02l-contflag.  " 提交类
    <ls_table>-actty = ls_dd02l-authclass. " 激活类型
    <ls_table>-edits = ls_dd02l-mainflag.  " 表维护
    <ls_table>-encat = ls_dd02l-exclass.   " 增强类别

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS WITH KEY tabname = ls_dd02l-tabname BINARY SEARCH.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.

      LOOP AT lt_dd03l INTO DATA(ls_dd03l) FROM lv_tabix.
        IF ls_dd03l-tabname <> ls_dd02l-tabname.
          EXIT.
        ENDIF.

        APPEND INITIAL LINE TO <ls_table>-fields ASSIGNING FIELD-SYMBOL(<ls_field>).

        " 字段名
        <ls_field>-name   = |{ ls_dd03l-fieldname CASE = LOWER }|.

        IF ls_dd03l-fieldname = '.INCLUDE' OR ls_dd03l-fieldname = '.INCLU--AP'.
          CLEAR <ls_field>-name.

          IF ls_dd03l-reffield IS NOT INITIAL.
            <ls_field>-name = |{ ls_dd03l-reffield CASE = LOWER }|.
          ENDIF.
        ENDIF.

        DATA(lv_fieldlen) = strlen( <ls_field>-name ).

        IF ls_dd03l-keyflag = 'X'.
          lv_fieldlen = lv_fieldlen + 3 + 1.
        ENDIF.

        IF <ls_table>-field_max < lv_fieldlen.
          <ls_table>-field_max = lv_fieldlen.
        ENDIF.

        <ls_field>-key    = ls_dd03l-keyflag.
        <ls_field>-nonull = ls_dd03l-notnull.

        IF ls_dd03l-rollname IS INITIAL
          AND NOT ( ls_dd03l-fieldname = '.INCLUDE' " 切换为表取值后 include 的 rollname 为空
                 OR ls_dd03l-fieldname = '.INCLU--AP' ).  " append 特殊类型
          " 数据元素为空
          CASE ls_dd03l-datatype.
            WHEN 'DATS' OR 'TIMS'.
              <ls_field>-type = |abap.{ ls_dd03l-datatype CASE = LOWER }|.
            WHEN 'CHAR' OR 'NUMC'.
              <ls_field>-type = |abap.{ ls_dd03l-datatype CASE = LOWER }({ CONV i( ls_dd03l-leng ) })|.
            WHEN 'CURR'.
              <ls_field>-type = |abap.{ ls_dd03l-datatype CASE = LOWER }({ CONV i( ls_dd03l-leng ) },{  CONV i( ls_dd03l-decimals ) })|.
            WHEN 'DEC'.
              <ls_field>-type = |abap.{ ls_dd03l-datatype CASE = LOWER }({ CONV i( ls_dd03l-leng ) },{  CONV i( ls_dd03l-decimals ) })|.
            WHEN 'QUAN'.
              <ls_field>-type = |abap.{ ls_dd03l-datatype CASE = LOWER }({ CONV i( ls_dd03l-leng ) },{  CONV i( ls_dd03l-decimals ) })|.
            WHEN 'STRG'.
              <ls_field>-type = |abap.string({ CONV i( ls_dd03l-leng ) })|.
            WHEN 'INT4' OR 'INT1' OR 'INT2' OR 'INT8'.
              <ls_field>-type = |abap.{ ls_dd03l-datatype CASE = LOWER }|.
            WHEN OTHERS.
              <ls_field>-type = ls_dd03l-datatype && '-' && ls_dd03l-leng.
          ENDCASE.
          " 补充描述
          READ TABLE lt_dd03t INTO DATA(ls_dd03t) WITH KEY tabname = ls_dd03l-tabname fieldname = ls_dd03l-fieldname BINARY SEARCH.
          IF sy-subrc = 0.
            <ls_field>-adddesc = ls_dd03t-ddtext.
          ENDIF.
        ELSE.
          " 数据元素不为空

          IF ls_dd03l-fieldname = '.INCLUDE'.
            "<ls_field>-type = |include { ls_dd03l-rollname CASE = LOWER }|. " 函数有效
            <ls_field>-type = |include { ls_dd03l-precfield CASE = LOWER }|. " 表有效
          ELSEIF ls_dd03l-fieldname = '.INCLU--AP'.
            " append 的类型
            <ls_field>-type = |append { ls_dd03l-precfield CASE = LOWER } /* append 类型在 ddl 不会有, 导入时需删除 */|.
          ELSE.
            <ls_field>-type = |{ ls_dd03l-rollname CASE = LOWER }|.
          ENDIF.

        ENDIF.

        CASE ls_dd03l-datatype.
          WHEN 'CURR'.
            <ls_field>-addtion = |  @Semantics.amount.currencyCode : '{ |{ ls_dd03l-reftable CASE = LOWER }| }.{ ls_dd03l-reffield CASE = LOWER }'|.
          WHEN 'QUAN'.
            <ls_field>-addtion = |  @Semantics.quantity.unitOfMeasure : '{ |{ ls_dd03l-reftable CASE = LOWER }| }.{ ls_dd03l-reffield CASE = LOWER }'|.
          WHEN 'LANG'.
            <ls_field>-addtion = |  @AbapCatalog.textLanguage|.
          WHEN OTHERS.
        ENDCASE.

        READ TABLE lt_dd08l INTO DATA(ls_dd08l) WITH KEY tabname = ls_dd03l-tabname fieldname = ls_dd03l-fieldname BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_field>-foreignkey-label = ls_dd08l-ddtext.
          <ls_field>-foreignkey-screencheck = COND #( WHEN ls_dd08l-frkart IS INITIAL THEN 'false' ELSE 'true' ).
          IF ls_dd08l-cardleft IS INITIAL
            AND ls_dd08l-card IS INITIAL.
            <ls_field>-foreignkey-screencheck = 'true'.
          ENDIF.

          <ls_field>-foreignkey-keytype  = ls_dd08l-frkart.
          <ls_field>-foreignkey-cardleft = ls_dd08l-cardleft.
          <ls_field>-foreignkey-card     = ls_dd08l-card.
          <ls_field>-foreignkey-checktable  = |{ ls_dd08l-checktable CASE = LOWER }|.
        ENDIF.

        READ TABLE lt_dd05q TRANSPORTING NO FIELDS WITH KEY tabname = ls_dd03l-tabname fieldname = ls_dd03l-fieldname BINARY SEARCH.
        IF sy-subrc = 0.
          lv_tabix = sy-tabix.

          LOOP AT lt_dd05q INTO DATA(ls_dd05q) FROM lv_tabix.
            IF ls_dd05q-tabname <> ls_dd03l-tabname
            OR ls_dd05q-fieldname <> ls_dd03l-fieldname.
              EXIT.
            ENDIF.

            CHECK ls_dd05q-fortable <> '*'.

            APPEND VALUE #( fortable = ls_dd05q-fortable
                            forkey   = |{ ls_dd05q-forkey CASE = LOWER }|
                            checkfield = |{ ls_dd05q-checkfield CASE = LOWER }| ) TO <ls_field>-foreignkey-foreignkey_eq.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_folder_name
*&---------------------------------------------------------------------*
*&  获取文件名
*&---------------------------------------------------------------------*
FORM frm_get_folder_name USING p_type TYPE char2
                               p_name
                               p_folder.
* MG|[RBM]_
  DATA: lv_name  TYPE string,
        lv_regex TYPE string.

  lv_name = p_name.

  CLEAR p_folder.

  DATA: lv_folder TYPE ty_folder-tag.

  CASE p_type+0(1).
    WHEN 'R'.
      " mg 结尾 或 R/B 结尾 或 M 结尾
      lv_regex = `(MG|[RB]|M)_`.

      SPLIT lv_name AT '_' INTO TABLE DATA(lt_split_name).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF lines( lt_split_name ) <> 1.
        READ TABLE lt_split_name INTO DATA(lv_split_name) INDEX 1.
        lv_split_name = lv_split_name && '_'.

        FIND PCRE lv_regex IN lv_split_name
          MATCH OFFSET DATA(l_off)
          MATCH LENGTH DATA(l_len).
        IF sy-subrc = 0.
          l_off = l_off - 1.
          lv_folder = lv_split_name+1(l_off).
        ELSE.
          lv_folder = lv_name+1(2).
        ENDIF.
      ELSE.
        " 无下划线
        lv_regex = `(MG|[RB]|M)$`.

        FIND PCRE lv_regex IN lv_name
          MATCH OFFSET l_off
          MATCH LENGTH l_len.
        IF sy-subrc = 0.
          l_off -= l_off.
          lv_folder = lv_name+1(l_off).
        ELSE.
          lv_folder = lv_name+1(2).
          IF p_type+1(1) = 'I' AND lv_folder+0(1) = 'X'.
            " 包含程序
            p_folder = lv_folder+0(1).
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'F'.
      lv_regex = `ZFM_.+?_|ZFM.+?_`.

      FIND PCRE lv_regex IN lv_name
        MATCH OFFSET l_off
        MATCH LENGTH l_len.
      IF sy-subrc = 0.
        lv_name = substring( val = lv_name off = l_off len = l_len ).
        lv_name = replace( val = lv_name pcre = '^ZFM' with = '' ).
        lv_name = replace( val = lv_name sub = '_' with = '' occ = 0 ).
        lv_folder = lv_name.
      ENDIF.

    WHEN 'T'.
      lv_regex = `ZT\w+`.

      FIND PCRE lv_regex IN lv_name
        MATCH OFFSET l_off
        MATCH LENGTH l_len.
      IF sy-subrc = 0.
        lv_name = substring( val = lv_name off = l_off len = l_len ).
        lv_name = replace( val = lv_name pcre = '^ZT' with = '' ).
        lv_folder = lv_name.
      ENDIF.

    WHEN 'D'.
      lv_regex = `ZV.*?_`.

      FIND PCRE lv_regex IN lv_name
        MATCH OFFSET l_off
        MATCH LENGTH l_len.
      IF sy-subrc = 0.
        lv_name = substring( val = lv_name off = l_off len = l_len ).
        lv_name = replace( val = lv_name pcre = '^ZV' with = '' ).
        lv_name = replace( val = lv_name sub = '_' with = '' ).
        lv_folder = lv_name.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

  READ TABLE gt_folder INTO DATA(ls_folder) WITH KEY tag = lv_folder BINARY SEARCH.
  IF sy-subrc = 0.

    IF ls_folder-folder IS INITIAL.
      p_folder = ls_folder-tag.
    ELSE.
      p_folder = ls_folder-folder.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_domain
*&---------------------------------------------------------------------*
*&  获取数据域
*&---------------------------------------------------------------------*
FORM frm_get_domain .

  " 数据元素和数据域对应关系 DD04L
  " 数据域值     DD07L
  " 数据域文本   DD01T
  " 数据域       DD01L

  " 数据域
  " 目的便于查看与还原
  TYPES: BEGIN OF ty_domavalues,
           valpos     TYPE i,
           ddtext     TYPE string,
           domvalue_l TYPE string,
           domvalue_h TYPE string,
           appval     TYPE abap_bool, " 附加
         END OF ty_domavalues.
  TYPES: BEGIN OF ty_doma,
           domname TYPE dd01l-domname,
           as4user TYPE dd01l-as4user,
           as4date TYPE dd01l-as4date,
           as4time TYPE dd01l-as4time,
           values  TYPE TABLE OF ty_domavalues WITH DEFAULT KEY,
         END OF ty_doma.

  DATA: lv_filename TYPE string.

  DATA: lt_source  TYPE TABLE OF text1000 WITH EMPTY KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: ls_doma  TYPE ty_doma,
        lv_tabix TYPE sytabix.

  " 数据获取
  SELECT
    dd~domname,
    dd~as4user,
    dd~as4date,
    dd~as4time,
    dt~ddtext
    FROM dd01l AS dd
    INNER JOIN tadir AS ta ON dd~domname = ta~obj_name
    LEFT JOIN dd01t AS dt ON dt~domname = dd~domname AND dt~ddlanguage = @sy-langu
                         AND dt~as4local = dd~as4local AND dt~as4vers = dd~as4vers
    WHERE ta~pgmid = 'R3TR'
      AND ta~object = 'DOMA'
      AND ta~devclass IN @gt_range_devclass
      AND dd~as4local = 'A'
      AND dd~domname  LIKE 'Z%'
      AND dd~as4user <> 'SAP'
    INTO TABLE @DATA(lt_dd01l).

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = 'DOMA' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'DOMA' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  " 获取域值
  IF lt_dd01l IS NOT INITIAL.
    SELECT
      dl~domname,
      dl~valpos,
      dt~ddtext,
      dl~domvalue_l,
      dl~domvalue_h,
      dl~appval     " 附加
      FROM dd07l AS dl
      LEFT JOIN dd07t AS dt ON dt~domname  = dl~domname
                           AND dt~valpos   = dl~valpos
                           AND dt~as4vers  = dl~as4vers
                           AND dt~ddlanguage = @sy-langu
      FOR ALL ENTRIES IN @lt_dd01l
      WHERE dl~domname = @lt_dd01l-domname
      INTO TABLE @DATA(lt_dd07).
    SORT lt_dd07 BY domname valpos.
  ENDIF.

  LOOP AT lt_dd01l INTO DATA(ls_dd01).
    lv_filename = ls_dd01-domname && '.json'.

    lv_filename = gv_parent_folder && lv_filename.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_dd01-ddtext.

    " 日志 生成
    PERFORM frm_set_log_flow USING 'DOMA' ls_dd01-domname ls_dd01-as4user ls_dd01-as4date ls_dd01-as4time.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_dd01-as4date
        OR ( ls_delt_log-ddate = ls_dd01-as4date AND ls_delt_log-dtime > ls_dd01-as4time ).
        REFRESH lt_source.
        CLEAR: lv_filename, lv_xstring.

        CONTINUE.
      ENDIF.
    ENDIF.

    " 生成 json 文件
    ls_doma-domname = ls_dd01-domname.
    ls_doma-as4user = ls_dd01-as4user.
    ls_doma-as4date = ls_dd01-as4date.
    ls_doma-as4time = ls_dd01-as4time.

    READ TABLE lt_dd07 TRANSPORTING NO FIELDS WITH KEY domname = ls_dd01-domname BINARY SEARCH.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.

      LOOP AT lt_dd07 INTO DATA(ls_dd07) FROM lv_tabix.
        IF ls_dd07-domname <> ls_dd01-domname.
          EXIT.
        ENDIF.

        APPEND CORRESPONDING #( ls_dd07 ) TO ls_doma-values.
      ENDLOOP.
    ENDIF.

    GET REFERENCE OF ls_doma INTO DATA(lo_doma).

    lv_source = /ui2/cl_json=>serialize( data = lo_doma
                                  pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    CLEAR ls_doma.

    lv_source = lcl_pretty_json=>pretty( lv_source ).

    " string -> xstring
    gr_cover_out->convert(
          EXPORTING data = lv_source
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " 清除缓存
    CLEAR: lv_filename, lv_xstring, lv_source.
  ENDLOOP.

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_element
*&---------------------------------------------------------------------*
*&  获取数据元素
*&---------------------------------------------------------------------*
FORM frm_get_element .

  " 数据元素和数据域对应关系 DD04L

  " 数据元素
  " 目的便于查看与还原
  TYPES: BEGIN OF ty_elem,
           rollname  TYPE dd04l-rollname,
           domname   TYPE dd04l-domname,
           as4user   TYPE dd04l-as4user,
           as4date   TYPE dd04l-as4date,
           as4time   TYPE dd04l-as4time,
           datatype  TYPE dd04l-datatype,
           leng      TYPE dd04l-leng,
           decimals  TYPE dd04l-decimals,
           outputlen TYPE dd04l-outputlen,
           lowercase TYPE dd04l-lowercase,
           convexit  TYPE dd04l-convexit,
           entitytab TYPE dd04l-entitytab,
           refkind   TYPE dd04l-refkind,
           ddtext    TYPE dd04t-ddtext,
           reptext   TYPE dd04t-reptext,
           scrtext_s TYPE dd04t-scrtext_s,
           scrtext_m TYPE dd04t-scrtext_m,
           scrtext_l TYPE dd04t-scrtext_l,
         END OF ty_elem.

  DATA: lv_filename TYPE string.

  DATA: lt_source  TYPE TABLE OF text1000 WITH EMPTY KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: ls_elem  TYPE ty_elem,
        lv_tabix TYPE sytabix.

  " 数据获取
  SELECT
    dd~rollname,
    dd~domname,
    dd~as4user,
    dd~as4date,
    dd~as4time,
    dd~datatype,
    dd~leng,
    dd~decimals,
    dd~outputlen,
    dd~lowercase,
    dd~convexit,
    dd~entitytab,
    dd~refkind,
    dt~ddtext,
    dt~reptext,
    dt~scrtext_s,
    dt~scrtext_m,
    dt~scrtext_l
    FROM dd04l AS dd
    INNER JOIN tadir AS ta ON dd~rollname = ta~obj_name
    LEFT JOIN dd04t AS dt ON dt~rollname = dd~rollname AND dt~ddlanguage = dd~dtelmaster
                         AND dt~as4local = dd~as4local AND dt~as4vers = dd~as4vers
    WHERE ta~pgmid = 'R3TR'
      AND ta~object = 'DTEL'
      AND ta~devclass IN @gt_range_devclass
      AND dd~as4local = 'A'
      AND dd~rollname  LIKE 'Z%'
      AND dd~as4user <> 'SAP'
    INTO TABLE @DATA(lt_dd04l).

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = 'DTEL' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'DTEL' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  LOOP AT lt_dd04l INTO DATA(ls_dd04).
    lv_filename = ls_dd04-rollname && '.json'.

    lv_filename = gv_parent_folder && lv_filename.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_dd04-ddtext.

    " 日志 生成
    PERFORM frm_set_log_flow USING 'DTEL' ls_dd04-domname ls_dd04-as4user ls_dd04-as4date ls_dd04-as4time.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_dd04-as4date
        OR ( ls_delt_log-ddate = ls_dd04-as4date AND ls_delt_log-dtime > ls_dd04-as4time ).
        REFRESH lt_source.
        CLEAR: lv_filename, lv_xstring.

        CONTINUE.
      ENDIF.
    ENDIF.

    " 生成 json 文件
    MOVE-CORRESPONDING ls_dd04 TO ls_elem.

    GET REFERENCE OF ls_elem INTO DATA(lo_elem).

    lv_source = /ui2/cl_json=>serialize( data = lo_elem
                                  pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    CLEAR ls_elem.

    lv_source = lcl_pretty_json=>pretty( lv_source ).

    " string -> xstring
    gr_cover_out->convert(
          EXPORTING data = lv_source
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " 清除缓存
    CLEAR: lv_filename, lv_xstring, lv_source.
  ENDLOOP.

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_TABLETYPES_DDL
*&---------------------------------------------------------------------*
*&  获取表类型
*&---------------------------------------------------------------------*
FORM frm_get_tabletypes.
  " 表类型     DD40L
  " 表类型文本 DD40T

  " 目的便于查看
  TYPES: BEGIN OF ty_ttyp,
           typename   TYPE dd40l-typename,
           rowtype    TYPE dd40l-rowtype,
           rowkind    TYPE dd40l-rowkind,
           datatype   TYPE dd40l-datatype,
           leng       TYPE dd40l-leng,
           decimals   TYPE dd40l-decimals,
           accessmode TYPE dd40l-accessmode,
           keydef     TYPE dd40l-keydef,
           keykind    TYPE dd40l-keykind,
           keyfdcount TYPE dd40l-keyfdcount,
           generic    TYPE dd40l-generic,
           typelen    TYPE dd40l-typelen,
           as4user    TYPE dd40l-as4user,
           as4date    TYPE dd40l-as4date,
           as4time    TYPE dd40l-as4time,
           ddtext     TYPE dd40t-ddtext,
         END OF ty_ttyp.

  " rowkind => 'E'  基本类型\'S'  结构类型\'L'  表格类型\' ' 直接类型条目\'R' 参考类型\'D'  域

  DATA: lv_filename TYPE string.

  DATA: lt_source  TYPE TABLE OF text1000 WITH EMPTY KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: ls_ttyp  TYPE ty_ttyp,
        lv_tabix TYPE sytabix.

  " 数据获取
  SELECT
    dd~typename,
    dd~rowtype,
    dd~rowkind,
    dd~datatype,
    dd~leng,
    dd~decimals,
    dd~accessmode,
    dd~keydef,
    dd~keykind,
    dd~keyfdcount,
    dd~generic,
    dd~typelen,
    dd~as4user,
    dd~as4date,
    dd~as4time,
    dt~ddtext
    FROM dd40l AS dd
    INNER JOIN tadir AS ta ON dd~typename = ta~obj_name
    LEFT JOIN dd40t AS dt ON dt~typename = dd~typename AND dt~ddlanguage = @sy-langu
    WHERE ta~pgmid = 'R3TR'
      AND ta~object = 'TTYP'
      AND ta~devclass IN @gt_range_devclass
      AND dd~as4local = 'A'
      AND dd~typename  LIKE 'Z%'
    INTO TABLE @DATA(lt_dd40l).

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = 'TTYP' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'TTYP' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  LOOP AT lt_dd40l INTO DATA(ls_dd40).
    lv_filename = ls_dd40-typename && '.json'.

    lv_filename = gv_parent_folder && lv_filename.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_dd40-ddtext.

    " 日志 生成
    PERFORM frm_set_log_flow USING 'DTEL' ls_dd40-typename ls_dd40-as4user ls_dd40-as4date ls_dd40-as4time.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_dd40-as4date
        OR ( ls_delt_log-ddate = ls_dd40-as4date AND ls_delt_log-dtime > ls_dd40-as4time ).
        REFRESH lt_source.
        CLEAR: lv_filename, lv_xstring.

        CONTINUE.
      ENDIF.
    ENDIF.

    " 生成 json 文件
    MOVE-CORRESPONDING ls_dd40 TO ls_ttyp.

    GET REFERENCE OF ls_ttyp INTO DATA(lo_ttyp).

    lv_source = /ui2/cl_json=>serialize( data = lo_ttyp
                                  pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    CLEAR ls_ttyp.

    lv_source = lcl_pretty_json=>pretty( lv_source ).

    " string -> xstring
    gr_cover_out->convert(
          EXPORTING data = lv_source
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " 清除缓存
    CLEAR: lv_filename, lv_xstring, lv_source.
  ENDLOOP.

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_others
*&---------------------------------------------------------------------*
*&  保存其他
*&---------------------------------------------------------------------*
FORM frm_get_others .

  IF p_smw0 = abap_true.
    gv_parent_folder = `SMW0/`.
    " 由于文件名包含中文，设置编码格式为 UTF-8
    gr_zip->support_unicode_names = abap_true.

    PERFORM frm_get_smw0.

    " 还原设置
    gr_zip->support_unicode_names = abap_false.
  ENDIF.

  PERFORM frm_get_ench IN PROGRAM zsltest18 IF FOUND USING gr_zip gr_cover_out `ENCH/`.

  " 结果存储
  gv_parent_folder = `logs/flow/`.
  PERFORM frm_get_logs.

  DATA: ls_blob TYPE demo_indx_blob.

  IF p_delt = 'X'.
    ls_blob-userid = sy-uname.
    GET TIME STAMP FIELD ls_blob-timestamp.
    EXPORT gt_delt_log TO DATABASE demo_indx_blob(zd) FROM ls_blob ID 'DeltaDownload'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_smw0
*&---------------------------------------------------------------------*
*&  获取 SMW0
*&---------------------------------------------------------------------*
FORM frm_get_smw0 .
  DATA: lv_filename TYPE string.
  DATA: lt_mime TYPE TABLE OF w3mime.
  DATA: lv_xstring TYPE xstring,
        lv_size    TYPE i.

  SELECT
    relid,
    objid,
    chname,
    tdate,
    ttime,
    text
    FROM wwwdata
   WHERE srtf2 = 0
     AND relid = 'MI'
     AND objid LIKE 'Z%'
    INTO TABLE @DATA(lt_smw0).
  IF sy-subrc <> 0.
    " 未找到数据不进行下载
    RETURN.
  ENDIF.

  " 获取文件参数
  SELECT
    objid,
    name,
    value
    FROM wwwparams
    WHERE relid = 'MI'
      AND objid LIKE 'Z%'
    INTO TABLE @DATA(lt_param).

  SORT lt_smw0 BY objid.
  SORT lt_param BY objid name.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING FIELD-SYMBOL(<ls_delt_log>) WITH KEY object = 'SMW0' BINARY SEARCH.
  IF sy-subrc <> 0.
    APPEND VALUE #( object = 'SMW0' ddate = sy-datum dtime = sy-uzeit ) TO gt_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  LOOP AT lt_smw0 INTO DATA(ls_smw0).
    lv_filename = ls_smw0-objid && `_` && ls_smw0-text.

    " 后缀名
    READ TABLE lt_param INTO DATA(ls_param_ext) WITH KEY objid = ls_smw0-objid name = 'fileextension' BINARY SEARCH.
    IF sy-subrc = 0.
      lv_filename = lv_filename && ls_param_ext-value.
    ENDIF.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_smw0-text.

    lv_filename = gv_parent_folder && lv_filename.

    " 日志 生成
    PERFORM frm_set_log_flow USING 'SMW0' ls_smw0-objid ls_smw0-chname ls_smw0-tdate ls_smw0-ttime.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_smw0-tdate
        OR ( ls_delt_log-ddate = ls_smw0-tdate AND ls_delt_log-dtime > ls_smw0-ttime ).
        REFRESH lt_mime.
        CLEAR: lv_filename, lv_xstring, lv_size.

        CONTINUE.
      ENDIF.
    ENDIF.

    " 文件长度
    READ TABLE lt_param INTO DATA(ls_param_size) WITH KEY objid = ls_smw0-objid name = 'filesize' BINARY SEARCH.
    IF sy-subrc = 0.
      lv_size = ls_param_size-value.
    ENDIF.

    IMPORT mime = lt_mime FROM DATABASE wwwdata(mi) ID ls_smw0-objid .
    IF sy-subrc = 0.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_size
        IMPORTING
          buffer       = lv_xstring
        TABLES
          binary_tab   = lt_mime
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
    ENDIF.

    " 添加到压缩包
    gr_zip->add( name    = lv_filename
                 content = lv_xstring ).

    " 清除缓存
    REFRESH lt_mime.
    CLEAR: lv_filename, lv_xstring, lv_size.
  ENDLOOP.

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.