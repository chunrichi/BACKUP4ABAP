*&---------------------------------------------------------------------*
*& Report ZBACKUP4ABAP_ECC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbackup4abap_ecc.

" 需创建 demo_indx_table 表

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------
TYPE-POOLS: seop.

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
CLASS lcl_progress_bar DEFINITION DEFERRED.
CLASS lcl_pretty_json DEFINITION DEFERRED.
CLASS lcl_ui2_json DEFINITION DEFERRED.

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
    PARAMETERS: p_clas AS CHECKBOX DEFAULT ''.
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
    gt_range_devclass = s_pack[].
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

*----------------------------------------------------------------------*
*       CLASS lcl_progress_bar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_progress_bar DEFINITION.
  PUBLIC SECTION.

    DATA: count     TYPE i,
          base_desc TYPE string,
          curr      TYPE i.

    METHODS constructor IMPORTING i_count     TYPE i OPTIONAL
                                  i_base_desc TYPE string OPTIONAL.

    METHODS add IMPORTING i_add  TYPE i DEFAULT 1
                          i_desc TYPE data OPTIONAL.
  PRIVATE SECTION.
    DATA: percent TYPE p DECIMALS 2 LENGTH 5.
    METHODS display IMPORTING desc TYPE data.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Form frm_init_text
*&---------------------------------------------------------------------*
*&  文本初始化
*&---------------------------------------------------------------------*
FORM frm_init_text .
  " 类限制
  " GT_RANGE_DEVCLASS = VALUE #( ).
  REFRESH gt_range_devclass.

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
  t_tran = '事务码'.
  t_xslt = '转换'.

  t_pack = '开发类(包)'.
  t_delt  = '增量获取(不跨client)'.

  t_dddl = '导出DDL文件, 用于 SAP NetWeaver AS for ABAP 7.52 SP00 以上版本 ADT'.
ENDFORM.                    "frm_init_text
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

ENDFORM.                    "frm_check
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
  DATA: ls_folder LIKE LINE OF gt_folder.

  DEFINE zappend.
    ls_folder-tag = &1.
    ls_folder-folder = &2.
    APPEND ls_folder TO gt_folder.
    CLEAR ls_folder.
  END-OF-DEFINITION.

  zappend 'MM'   ''.
  zappend 'PP'   ''.
  zappend 'SD'   ''.
  zappend 'FI'   ''.
  zappend 'FICO' 'FI'.
  zappend 'HD'   ''.    " 汉得包
  zappend 'TX'   ''.    " 推送测试平台类
  zappend 'API'  ''.    " 接口处理
  zappend 'JOB'  ''.    " job 处理
  zappend 'JO'   'JOB'. " job 处理
  zappend 'FILE' ''.    " 文件处理
  SORT gt_folder BY tag.

  " 增量数据获取
  IF p_delt = 'X'.
    IMPORT gt_delt_log FROM DATABASE demo_indx_table(zd) ID 'DeltaDownload'.
    SORT gt_delt_log BY object.

    FIELD-SYMBOLS: <ls_delt_log> LIKE LINE OF gt_delt_log.

    " 时间减3s
    LOOP AT gt_delt_log ASSIGNING <ls_delt_log>.
      <ls_delt_log>-dtime = <ls_delt_log>-dtime - 3.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "frm_init_variables
*&---------------------------------------------------------------------*
*& Form frm_get_code
*&---------------------------------------------------------------------*
*&  获取代码数据
*&---------------------------------------------------------------------*
FORM frm_get_code .

  IF p_prog = abap_true.
    PERFORM frm_set_parent_folder USING `SE38/`.
    PERFORM frm_get_report.
  ENDIF.

  IF p_func = abap_true.
    PERFORM frm_set_parent_folder USING `SE37/`.
    PERFORM frm_get_function.
  ENDIF.

  IF p_clas = abap_true.
    PERFORM frm_set_parent_folder USING `SE24/`.
    PERFORM frm_get_class.
  ENDIF.

ENDFORM.                    "frm_get_code
*&---------------------------------------------------------------------*
*& Form frm_get_DDIC
*&---------------------------------------------------------------------*
*&  获取表相关内容
*&---------------------------------------------------------------------*
FORM frm_get_ddic .

  IF p_tabl = abap_true.
    "PERFORM frm_set_parent_folder USING `SE11/`.
    IF p_txml = abap_true.
      " no realization
    ELSE.
      PERFORM frm_get_tables_ddl USING gr_zip gr_cover_out `SE11/DDL/TABLE/`.
      PERFORM frm_get_structs_ddl USING gr_zip gr_cover_out `SE11/DDL/STRUCT/`.
    ENDIF.

  ENDIF.

  IF p_cdsv = abap_true.
    PERFORM frm_set_parent_folder USING `DDL/`.
    PERFORM frm_get_ddl.
  ENDIF.

  IF p_doma = abap_true.
    PERFORM frm_set_parent_folder USING `SE11/field/Domain/`.
    PERFORM frm_get_domain.
  ENDIF.

  IF p_dtel = abap_true.
    PERFORM frm_set_parent_folder USING `SE11/field/Element/`.
    PERFORM frm_get_element.
  ENDIF.

  IF p_ttyp = abap_true.
    PERFORM frm_set_parent_folder USING `SE11/TABLETYPE/`.
    PERFORM frm_get_tabletypes.
  ENDIF.

ENDFORM.                    "frm_get_ddic
*&---------------------------------------------------------------------*
*& Form frm_get_report
*&---------------------------------------------------------------------*
*&  获取报表数据
*&---------------------------------------------------------------------*
FORM frm_get_report .

  " D010SINF REPOSRC 视图（INF 部分） => REPOSRC  程序含代码
  " D010TINF REPOTEXT 视图（INF 部分）=> REPOTEXT 程序文本
  " trdirt   标题文本

  TYPES: BEGIN OF ty_list,
           progname TYPE reposrc-progname,
           r3state  TYPE reposrc-r3state,
           subc     TYPE reposrc-subc,
           unam     TYPE reposrc-unam,
           udat     TYPE reposrc-udat,
           utime    TYPE reposrc-utime,
           text     TYPE trdirt-text,
         END OF ty_list.

  DATA: lv_filename TYPE string.
  DATA: lt_source  TYPE TABLE OF text1000 WITH DEFAULT KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: lv_c_flag TYPE char2.

  DATA: lv_folder TYPE char10,
        lv_max    TYPE i.

  DATA: lt_list TYPE TABLE OF ty_list,
        ls_list TYPE ty_list.

  DATA: lr_pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT lr_pb.

  " 读取 报表
  SELECT
    rep~progname
    rep~r3state
    rep~subc
    rep~unam       " 修改人
    rep~udat
    rep~utime
    des~text
    FROM reposrc AS rep
    INNER JOIN tadir AS tad ON tad~obj_name = rep~progname AND tad~object = 'PROG'
    LEFT JOIN trdirt AS des ON des~name = rep~progname AND des~sprsl = sy-langu
    INTO TABLE lt_list
    WHERE (
         ( rep~progname LIKE 'Z%' AND rep~subc = '1' )
      OR ( rep~progname LIKE 'Z%' AND rep~subc = 'I' AND rep~rload = '1' )
      OR ( rep~progname LIKE 'Z%' AND rep~subc = 'M' )
      OR ( rep~progname LIKE 'Z%' AND rep~subc = 'T' ) )
      AND rep~r3state = 'A'
      AND tad~devclass IN gt_range_devclass.
  SORT lt_list BY progname.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  FIELD-SYMBOLS: <ls_delt_log> LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'PROG' BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = 'PROG'.
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  DESCRIBE TABLE lt_list LINES lr_pb->count.
  lr_pb->base_desc = 'Process Report & '.

  LOOP AT lt_list INTO ls_list.
    lr_pb->add( i_desc = ls_list-progname ).

    " 忽略当前程序
    IF ls_list-progname = sy-cprog.
      CONTINUE.
    ENDIF.

    " 文件夹匹配 -> 文件名
    CONCATENATE 'R' ls_list-subc INTO lv_c_flag.
    PERFORM frm_get_folder_name USING lv_c_flag ls_list-progname lv_folder.

    IF ls_list-subc = 'I'.
      IF lv_folder+0(1) = 'X'.
        " 特殊
        CONCATENATE 'CMOD/' ls_list-progname '.' gc_extension_name INTO lv_filename.
      ELSE.
        IF lv_folder IS NOT INITIAL.
          CONCATENATE lv_filename lv_folder '/' INTO lv_filename.
        ENDIF.
        CONCATENATE lv_filename 'INCLUDE/' ls_list-progname '.' gc_extension_name INTO lv_filename.
      ENDIF.
    ELSEIF ls_list-subc = '1'.
      IF lv_folder IS NOT INITIAL.
        CONCATENATE lv_filename lv_folder '/' INTO lv_filename.
      ENDIF.
      CONCATENATE lv_filename ls_list-progname '.' gc_extension_name INTO lv_filename.
    ELSEIF ls_list-subc = 'M'.
      CONCATENATE 'MODULEPOOLS/' lv_filename INTO lv_filename.
      IF lv_folder IS NOT INITIAL.
        CONCATENATE lv_filename lv_folder '/' INTO lv_filename.
      ENDIF.
      CONCATENATE lv_filename ls_list-progname '.' gc_extension_name INTO lv_filename.
    ELSEIF ls_list-subc = 'T'.
      CONCATENATE 'TYPEPOOLS/' lv_filename INTO lv_filename.
      IF lv_folder IS NOT INITIAL.
        CONCATENATE lv_filename lv_folder '/' INTO lv_filename.
      ENDIF.
      CONCATENATE lv_filename ls_list-progname '.' gc_extension_name INTO lv_filename.
    ENDIF.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_list-text.

    CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

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

ENDFORM.                    "frm_get_report
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

  TYPES: BEGIN OF ty_tftit,
           funcname TYPE tftit-funcname,
           stext    TYPE tftit-stext,
         END OF ty_tftit.

  TYPES: BEGIN OF ty_rep,
           progname TYPE reposrc-progname,
           unam     TYPE reposrc-unam,
           udat     TYPE reposrc-udat,
           utime    TYPE reposrc-utime,
         END OF ty_rep.

  TYPES: BEGIN OF ty_list,
           progname TYPE reposrc-progname,
           r3state  TYPE reposrc-r3state,
           subc     TYPE reposrc-subc,
           unam     TYPE reposrc-unam,
           udat     TYPE reposrc-udat,
           utime    TYPE reposrc-utime,
         END OF ty_list.

  DATA: lt_func TYPE TABLE OF ty_function.

  DATA: lt_tftit TYPE TABLE OF ty_tftit.

  DATA: lv_filename TYPE string.
  DATA: lt_source  TYPE TABLE OF text1000 WITH DEFAULT KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: lv_report_name TYPE char50.

  DATA: lv_folder TYPE char10,
        lv_max    TYPE i.

  DATA: lr_pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT lr_pb.

  " 此处获取所有的 Z* 函数组数据
  SELECT
    fu~funcname AS functionname
    gr~area     AS functiongroup
    fu~pname    AS progname
    fu~include  AS includenumber
    FROM enlfdir AS gr
    INNER JOIN tfdir AS fu ON fu~funcname = gr~funcname
    INNER JOIN tadir AS tad ON tad~obj_name = gr~area
    INTO CORRESPONDING FIELDS OF TABLE lt_func
    WHERE gr~area LIKE 'Z%'    " 会包含表维护
      AND gr~funcname LIKE 'Z%'
      AND tad~devclass IN gt_range_devclass.
  IF lt_func IS NOT INITIAL.
    SELECT
      funcname
      stext
      FROM tftit
      INTO TABLE lt_tftit
      FOR ALL ENTRIES IN lt_func
      WHERE spras = sy-langu
        AND funcname = lt_func-functionname.
    SORT lt_tftit BY funcname.
  ENDIF.
  SORT lt_func BY functionname.

  FIELD-SYMBOLS: <ls_func_fixt> LIKE LINE OF lt_func.
  DATA: ls_tftit LIKE LINE OF lt_tftit.

  LOOP AT lt_func ASSIGNING <ls_func_fixt>.
    READ TABLE lt_tftit INTO ls_tftit WITH KEY funcname = <ls_func_fixt>-functionname BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_func_fixt>-functiontitle = ls_tftit-stext.
    ENDIF.
  ENDLOOP.

  " 更新人获取
  DATA: lt_rep TYPE TABLE OF ty_rep,
        ls_rep TYPE ty_rep.
  SELECT
    progname
    unam
    udat
    utime
    FROM reposrc
    INTO TABLE lt_rep
    WHERE progname LIKE 'LZ%' AND appl = 'S'.
  SORT lt_rep BY progname.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  FIELD-SYMBOLS: <ls_delt_log> LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'FUNC' BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = 'FUNC'.
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  DATA: ls_func LIKE LINE OF lt_func.

  DESCRIBE TABLE lt_func LINES lr_pb->count.
  lr_pb->base_desc = 'Process Function & '.

  LOOP AT lt_func INTO ls_func.
    lr_pb->add( i_desc = ls_func-functionname ).

    PERFORM frm_get_folder_name USING 'F' ls_func-functionname lv_folder.

    IF lv_folder IS NOT INITIAL.
      CONCATENATE lv_folder '/' ls_func-functionname '.abap' INTO lv_filename.
    ELSE.
      CONCATENATE ls_func-functionname '.abap' INTO lv_filename.
    ENDIF.

    CONCATENATE 'L' ls_func-functiongroup 'U' ls_func-includenumber INTO lv_report_name.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_func-functiontitle.

    CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

    " 日志 生成
    READ TABLE lt_rep INTO ls_rep WITH KEY progname = lv_report_name BINARY SEARCH.
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
  DATA: lt_list TYPE TABLE OF ty_list,
        ls_list TYPE ty_list.
  SELECT
    rep~progname
    rep~r3state
    rep~subc
    rep~unam      " 修改人
    rep~udat
    rep~utime
    FROM reposrc AS rep
    INNER JOIN tadir AS tad ON tad~obj_name = rep~progname
    INTO TABLE lt_list
    WHERE ( " ( rep~progname LIKE 'LZ%' AND rep~subc = 'I' AND rep~appl = '' ) " 索引页
            ( rep~progname LIKE 'LZ%' AND rep~rstat = 'S' ) " 索引页 + RFC
         OR ( rep~progname LIKE 'LZ%' AND rep~subc = 'I' AND rep~appl = 'S' AND rep~dbapl = '' )  " O/I/F
         OR ( rep~progname LIKE 'LZ%TOP' AND rep~subc = 'I' AND rep~appl = 'S' ) ) " TOP
      AND rep~r3state = 'A'
      AND tad~devclass IN gt_range_devclass.
  SORT lt_list BY progname.

  DATA: lv_str_len TYPE i.

  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'FINC' BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = 'FINC'.
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  DESCRIBE TABLE lt_list LINES lr_pb->count.
  lr_pb->base_desc = 'Process Function More & '.
  lr_pb->curr      = 0.

  LOOP AT lt_list INTO ls_list.
    lr_pb->add( i_desc = ls_list-progname ).

    " 文件名
    CONCATENATE ls_list-progname '.' gc_extension_name INTO lv_filename.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ''.

    lv_str_len = strlen( ls_list-progname ) - 3.
    CASE ls_list-progname+lv_str_len(1).
      WHEN 'V'.
        CONCATENATE gv_parent_folder `Rfc/` lv_filename INTO lv_filename.
      WHEN '$'.
        CONCATENATE gv_parent_folder `Rfc/Unit/` lv_filename INTO lv_filename.
      WHEN OTHERS.
        CONCATENATE gv_parent_folder `More/` lv_filename INTO lv_filename.
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

ENDFORM.                    "frm_get_function
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
           zip  = lv_zip_xstr TO DATABASE demo_indx_table(zb) ID p_brun.
  ENDIF.
ENDFORM.                    "frm_export_zip
*&---------------------------------------------------------------------*
*& Form frm_get_path
*&---------------------------------------------------------------------*
*&  获取存储路径
*&---------------------------------------------------------------------*
FORM frm_get_path .

  DATA: lv_path     TYPE string,
        lv_fullpath TYPE string,
        lv_filename TYPE string.

  DATA: lv_default_file TYPE string.

  CONCATENATE sy-datum sy-uzeit INTO lv_default_file.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = 'download zip'
      default_extension    = 'zip'
      default_file_name    = lv_default_file
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
ENDFORM.                    "frm_get_path
*&---------------------------------------------------------------------*
*& Form frm_set_map_file
*&---------------------------------------------------------------------*
*&  设置 map 文件内容
*&---------------------------------------------------------------------*
FORM frm_set_map_file USING VALUE(p_file_path)
                            VALUE(p_file_desc).
  DATA: lv_map_file TYPE string.

  CONCATENATE `<a href="./` p_file_path `">` p_file_desc `</a>` INTO lv_map_file.

  APPEND lv_map_file TO gt_map_file.
ENDFORM.                    "frm_set_map_file
*&---------------------------------------------------------------------*
*& Form frm_add_map_file
*&---------------------------------------------------------------------*
*& 添加 map 文件到 zip
*&---------------------------------------------------------------------*
FORM frm_add_map_file USING VALUE(p_folder).
  DATA: lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: lv_name TYPE string.

  CONCATENATE gv_parent_folder `map.html` INTO lv_name.

  CONCATENATE LINES OF gt_map_file INTO lv_source SEPARATED BY gc_newline.
  gr_cover_out->convert(
        EXPORTING data = lv_source
        IMPORTING buffer = lv_xstring ).
  gr_zip->add( name = lv_name
               content = lv_xstring ).

  REFRESH gt_map_file.
ENDFORM.                    "frm_add_map_file
*&---------------------------------------------------------------------*
*& Form FRM_GET_CLASS
*&---------------------------------------------------------------------*
*&  获取类代码
*&---------------------------------------------------------------------*
FORM frm_get_class .
  TYPES: BEGIN OF ty_class,
           clsname   TYPE vseoclass-clsname,
           langu     TYPE vseoclass-langu,
           descript  TYPE vseoclass-descript,
           msg_id    TYPE vseoclass-msg_id,
           exposure  TYPE vseoclass-exposure,
           state     TYPE vseoclass-state,
           clsfinal  TYPE vseoclass-clsfinal,
           r3release TYPE vseoclass-r3release,
           changedby TYPE vseoclass-changedby,
           changedon TYPE vseoclass-changedon,
           changetm  TYPE uzeit,
         END OF ty_class.

  TYPES: BEGIN OF ty_progdir,
           name  TYPE reposrc-progname,
           unam  TYPE reposrc-unam,
           udat  TYPE reposrc-udat,
           utime TYPE reposrc-utime,
         END OF ty_progdir.

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
        lt_source  TYPE TABLE OF text1000 WITH DEFAULT KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.

  DATA: lv_filename TYPE string,
        lv_more     TYPE string.

  TYPES: BEGIN OF ty_type,
           type   TYPE seop_include_ext_app,
           exline TYPE i,
         END OF ty_type.

  DATA: lt_type TYPE TABLE OF ty_type,
        ls_type TYPE ty_type,
        lv_max  TYPE i.

  DATA: lt_class TYPE TABLE OF ty_class,
        ls_class TYPE ty_class.

  DEFINE zappend.
    ls_type-type = &1.
    ls_type-exline = &2.
    APPEND ls_type TO lt_type.
    CLEAR ls_type.
  END-OF-DEFINITION.

  zappend seop_ext_class_locals_def  4.
  zappend seop_ext_class_locals_imp  4.
  zappend seop_ext_class_macros      3.
  zappend seop_ext_class_testclasses 1.

  SELECT
    clsname
    langu
    descript
    msg_id

    exposure
    state
    clsfinal
    r3release
    changedby
    changedon
    FROM vseoclass AS ss
    INNER JOIN tadir AS ta ON ta~obj_name = ss~clsname
    INTO CORRESPONDING FIELDS OF TABLE lt_class
    WHERE "langu = @sy-langu " 语言会限制内容
          ( ( clsname LIKE 'ZC%' OR clsname LIKE 'YC%' )
       OR ( clsname LIKE 'ZHD%' OR clsname LIKE 'YHD%' ) )
      AND version = '1' " 激活
      AND ( state = '0' OR state = '1' )
      AND ta~devclass IN gt_range_devclass.
  " 排序去重
  SORT lt_class BY clsname langu.
  DELETE ADJACENT DUPLICATES FROM lt_class COMPARING clsname.

  IF gt_range_append_class IS NOT INITIAL.
    SELECT
      clsname
      langu
      descript
      msg_id

      exposure
      state
      clsfinal
      r3release
      changedby
      changedon
      FROM vseoclass
      APPENDING CORRESPONDING FIELDS OF TABLE lt_class
      WHERE clsname IN gt_range_append_class
        AND version = '1' " 激活
        AND ( state = '0' OR state = '1' ).
    " 多语言去重
    SORT lt_class BY clsname langu.
    DELETE ADJACENT DUPLICATES FROM lt_class COMPARING clsname.
  ENDIF.

  " --> 获取真实修改时间（类下的子节点）
  " 参考 LSEODF1X 948 行
  DATA: lt_range_name TYPE RANGE OF progdir-name,
        ls_range_name LIKE LINE OF lt_range_name.

  LOOP AT lt_class INTO ls_class WHERE changedon IS INITIAL.
    ls_range_name-sign = 'I'.
    ls_range_name-option = 'CP'.
    CONCATENATE ls_class-clsname `*` INTO ls_range_name-low.
    APPEND ls_range_name TO lt_range_name.
    CLEAR ls_range_name.
  ENDLOOP.

  DATA: lt_progdir     TYPE TABLE OF ty_progdir,
        ls_progdir     TYPE ty_progdir,
        ls_prodir_udat TYPE ty_progdir.
  FIELD-SYMBOLS <ls_progdir> TYPE ty_progdir.

  IF lt_range_name IS NOT INITIAL.
    SELECT
      progname AS name
      unam
      udat
      utime
      FROM reposrc
      INTO TABLE lt_progdir
      WHERE progname IN lt_range_name.

    LOOP AT lt_progdir ASSIGNING <ls_progdir>.
      " __ 当名称足够长时 无标识 `=` 正则无效
      " <ls_progdir>-name = replace( val = <ls_progdir>-name pcre = `=+.*$` with = `` occ = -1 ).
      " <ls_progdir>-name = replace( val = <ls_progdir>-name pcre = `=*(?:CCAU|CCDEF|CCIMP|CCMAC|CI|CM\d{3}|CO|CP|CS|CT|CU)$` with = `` occ = -1 ).
      REPLACE FIRST OCCURRENCE OF `=*(?:CCAU|CCDEF|CCIMP|CCMAC|CI|CM\d{3}|CO|CP|CS|CT|CU)$` IN <ls_progdir>-name WITH ``.
    ENDLOOP.
    SORT lt_progdir BY name udat DESCENDING utime DESCENDING.
  ENDIF.
  " <--

  " 读取类
  DATA: "lo_source   TYPE REF TO object,
        lo_instance TYPE REF TO object.
  DATA: lr_source TYPE REF TO cl_oo_source,
        ls_clskey TYPE seoclskey.

  DATA: lv_regex   TYPE string,
        lv_replace TYPE string.

  DATA: lo_source TYPE REF TO cl_oo_source.

  DATA: lr_pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT lr_pb.

  " CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
  "   RECEIVING
  "     result = lo_instance.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  FIELD-SYMBOLS <ls_delt_log> LIKE LINE OF gt_delt_log.

  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'CLSS' BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = 'CLSS'.
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  DESCRIBE TABLE lt_class LINES lr_pb->count.
  lr_pb->base_desc = 'Process Class & '.

  LOOP AT lt_class INTO ls_class.
    lr_pb->add( i_desc = ls_class-clsname ).

    CONCATENATE ls_class-clsname `.abap` INTO lv_filename.

    " >> 特殊指定
    IF ls_class-clsname+1(2) = 'HD'.
      CONCATENATE `HD/` lv_filename INTO lv_filename.
    ENDIF.
    IF ls_class-clsname+4(2) = 'SI'.
      CONCATENATE `IF/` lv_filename INTO lv_filename.
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

    CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

    " 真实修改时间
    READ TABLE lt_progdir INTO ls_prodir_udat WITH KEY name = ls_class-clsname BINARY SEARCH.
    IF sy-subrc = 0.
      IF ls_class-changedon IS INITIAL OR ls_prodir_udat-udat > ls_class-changedon.
        ls_class-changedon = ls_prodir_udat-udat.
      ENDIF.
      ls_class-changedby = ls_prodir_udat-unam.
      ls_class-changetm  = ls_prodir_udat-utime.
    ENDIF.


    " 日志 生成
    PERFORM frm_set_log_flow USING 'CLASS' ls_class-clsname ls_class-changedby ls_class-changedon ls_class-changetm.

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_class-changedon
        OR ( ls_delt_log-ddate = ls_class-changedon AND ls_delt_log-dtime > ls_class-changetm ).
        REFRESH lt_source.
        CLEAR: lv_filename, lv_xstring.

        CONTINUE.
      ENDIF.
    ENDIF.

    ls_clskey-clsname = ls_class-clsname.
    CREATE OBJECT lo_source
      EXPORTING
        clskey             = ls_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.

    " > 读取源码
    " CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
    "   EXPORTING
    "     clif_name = ls_class-clsname
    "     version   = 'A'
    "   RECEIVING
    "     result    = lo_source.

    "
    " CALL METHOD lo_source->('GET_SOURCE')
    "   IMPORTING
    "     SOURCE = lt_str.

    lt_str = lo_source->source.

    FREE lo_source.

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

    LOOP AT lt_type INTO ls_type.
      ls_include-rootname = ls_class-clsname.
      TRANSLATE ls_include-rootname USING ' ='.
      ls_include-categorya = ls_type-type(1).
      ls_include-codea = ls_type-type+1(4).

      READ REPORT ls_include INTO lt_source STATE 'A' MAXIMUM WIDTH INTO lv_max.
      IF sy-subrc = 0 AND lines( lt_source ) > ls_type-exline.
        lv_more = lv_filename.

        CONCATENATE ls_class-clsname `.abap` INTO lv_regex.
        CONCATENATE `more/` ls_class-clsname `-` ls_type-type `.abap` INTO lv_replace.
        REPLACE FIRST OCCURRENCE OF lv_regex
           IN lv_more WITH lv_replace.

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

ENDFORM.                    "frm_get_class


*----------------------------------------------------------------------*
*       CLASS lcl_ui2_json DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_ui2_json DEFINITION.

  PUBLIC SECTION.
    TYPE-POOLS abap .
    CLASS cl_abap_tstmp DEFINITION LOAD .
    CLASS cx_sy_conversion_error DEFINITION LOAD .

    TYPES json TYPE string .
    TYPES:
      BEGIN OF name_mapping,
        abap TYPE abap_compname,
        json TYPE string,
      END OF name_mapping .
    TYPES:
      name_mappings TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY abap .
    TYPES bool TYPE char1 .
    TYPES tribool TYPE char1 .
    TYPES pretty_name_mode TYPE char1 .

    CONSTANTS:
      BEGIN OF pretty_mode,
        none          TYPE char1  VALUE ``,
        low_case      TYPE char1  VALUE `L`,
        camel_case    TYPE char1  VALUE `X`,
        extended      TYPE char1  VALUE `Y`,
        user          TYPE char1  VALUE `U`,
        user_low_case TYPE char1  VALUE `C`,
      END OF  pretty_mode .
    CONSTANTS:
      BEGIN OF c_bool,
        true  TYPE bool  VALUE `X`,
        false TYPE bool  VALUE ``,
      END OF  c_bool .
    CONSTANTS:
      BEGIN OF c_tribool,
        true      TYPE tribool  VALUE c_bool-true,
        false     TYPE tribool  VALUE `-`,
        undefined TYPE tribool  VALUE ``,
      END OF  c_tribool .
    CLASS-DATA sv_white_space TYPE string READ-ONLY .
    CONSTANTS mc_key_separator TYPE string VALUE `-`. "##no_text.
    CLASS-DATA mc_bool_types TYPE string READ-ONLY VALUE `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=BOOLE_D\TYPE=XFELD`.
    CLASS-DATA mc_bool_3state TYPE string READ-ONLY VALUE `\TYPE=BOOLEAN`.
    CONSTANTS version TYPE i VALUE 14.
    CLASS-DATA mc_json_type TYPE string READ-ONLY .

    CLASS-METHODS class_constructor .
    CLASS-METHODS string_to_xstring
      IMPORTING
        !in        TYPE string
      CHANGING
        VALUE(out) TYPE any .
    CLASS-METHODS xstring_to_string
      IMPORTING
        !in        TYPE any
      RETURNING
        VALUE(out) TYPE string .
    CLASS-METHODS raw_to_string
      IMPORTING
        !iv_xstring      TYPE xstring
        !iv_encoding     TYPE abap_encoding OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS string_to_raw
      IMPORTING
        !iv_string        TYPE string
        !iv_encoding      TYPE abap_encoding OPTIONAL
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
    CLASS-METHODS dump
      IMPORTING
        !data          TYPE data
        !compress      TYPE bool DEFAULT c_bool-false
        !type_descr    TYPE REF TO cl_abap_typedescr OPTIONAL
        !pretty_name   TYPE pretty_name_mode DEFAULT pretty_mode-none
        !assoc_arrays  TYPE bool DEFAULT c_bool-false
        !ts_as_iso8601 TYPE bool DEFAULT c_bool-false
      RETURNING
        VALUE(r_json)  TYPE json .
    CLASS-METHODS deserialize
      IMPORTING
        !json             TYPE json OPTIONAL
        !jsonx            TYPE xstring OPTIONAL
        !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
        !assoc_arrays     TYPE bool DEFAULT c_bool-false
        !assoc_arrays_opt TYPE bool DEFAULT c_bool-false
        !name_mappings    TYPE name_mappings OPTIONAL
        !conversion_exits TYPE bool DEFAULT c_bool-false
      CHANGING
        !data             TYPE data .
    CLASS-METHODS serialize
      IMPORTING
        !data             TYPE data
        !compress         TYPE bool DEFAULT c_bool-false
        !name             TYPE string OPTIONAL
        !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
        !type_descr       TYPE REF TO cl_abap_typedescr OPTIONAL
        !assoc_arrays     TYPE bool DEFAULT c_bool-false
        !ts_as_iso8601    TYPE bool DEFAULT c_bool-false
        !expand_includes  TYPE bool DEFAULT c_bool-true
        !assoc_arrays_opt TYPE bool DEFAULT c_bool-false
        !numc_as_string   TYPE bool DEFAULT c_bool-false
        !name_mappings    TYPE name_mappings OPTIONAL
        !conversion_exits TYPE bool DEFAULT c_bool-false
      RETURNING
        VALUE(r_json)     TYPE json .
    METHODS deserialize_int
      IMPORTING
        !json  TYPE json OPTIONAL
        !jsonx TYPE xstring OPTIONAL
      CHANGING
        !data  TYPE data
      RAISING
        cx_sy_move_cast_error .
    CLASS-METHODS generate
      IMPORTING
        !json          TYPE json
        !pretty_name   TYPE pretty_name_mode DEFAULT pretty_mode-none
        !name_mappings TYPE name_mappings OPTIONAL
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
    METHODS serialize_int
      IMPORTING
        !data         TYPE data
        !name         TYPE string OPTIONAL
        !type_descr   TYPE REF TO cl_abap_typedescr OPTIONAL
      RETURNING
        VALUE(r_json) TYPE json .
    METHODS generate_int
      IMPORTING
        !json          TYPE json
        VALUE(length)  TYPE i OPTIONAL
      RETURNING
        VALUE(rr_data) TYPE REF TO data
      RAISING
        cx_sy_move_cast_error .
    METHODS constructor
      IMPORTING
        !compress         TYPE bool DEFAULT c_bool-false
        !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-none
        !assoc_arrays     TYPE bool DEFAULT c_bool-false
        !ts_as_iso8601    TYPE bool DEFAULT c_bool-false
        !expand_includes  TYPE bool DEFAULT c_bool-true
        !assoc_arrays_opt TYPE bool DEFAULT c_bool-false
        !strict_mode      TYPE bool DEFAULT c_bool-false
        !numc_as_string   TYPE bool DEFAULT c_bool-false
        !name_mappings    TYPE name_mappings OPTIONAL
        !conversion_exits TYPE bool DEFAULT c_bool-false .
    CLASS-METHODS bool_to_tribool
      IMPORTING
        !iv_bool          TYPE bool
      RETURNING
        VALUE(rv_tribool) TYPE tribool .
    CLASS-METHODS tribool_to_bool
      IMPORTING
        !iv_tribool    TYPE tribool
      RETURNING
        VALUE(rv_bool) TYPE bool .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF t_s_symbol,
        header       TYPE string,
        name         TYPE string,
        type         TYPE REF TO cl_abap_datadescr,
        value        TYPE REF TO data,
        convexit_out TYPE string,
        convexit_in  TYPE string,
        compressable TYPE abap_bool,
        read_only    TYPE abap_bool,
      END OF t_s_symbol .
    TYPES:
      t_t_symbol TYPE STANDARD TABLE OF t_s_symbol WITH DEFAULT KEY .
    TYPES:
      BEGIN OF t_s_field_cache,
        name         TYPE string,
        type         TYPE REF TO cl_abap_datadescr,
        convexit_out TYPE string,
        convexit_in  TYPE string,
        value        TYPE REF TO data,
      END OF t_s_field_cache .
    TYPES:
      t_t_field_cache TYPE HASHED TABLE OF t_s_field_cache WITH UNIQUE KEY name .
    TYPES:
      name_mappings_ex TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY json .

    DATA mv_compress TYPE bool .
    DATA mv_pretty_name TYPE pretty_name_mode .
    DATA mv_assoc_arrays TYPE bool .
    DATA mv_ts_as_iso8601 TYPE bool .
    DATA mt_name_mappings TYPE name_mappings .
    DATA mt_name_mappings_ex TYPE name_mappings_ex .
    DATA mv_expand_includes TYPE bool .
    DATA mv_assoc_arrays_opt TYPE bool .
    DATA mv_strict_mode TYPE bool .
    DATA mv_numc_as_string TYPE bool .
    DATA mv_conversion_exits TYPE bool .

    CLASS-METHODS unescape
      IMPORTING
        !escaped         TYPE string
      RETURNING
        VALUE(unescaped) TYPE string .
    CLASS-METHODS get_convexit_func
      IMPORTING
        !elem_descr    TYPE REF TO cl_abap_elemdescr
        !input         TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_func) TYPE string .
    METHODS dump_symbols
      FINAL
      IMPORTING
        !it_symbols   TYPE t_t_symbol
      RETURNING
        VALUE(r_json) TYPE json .
    METHODS get_symbols
      FINAL
      IMPORTING
        !type_descr      TYPE REF TO cl_abap_typedescr
        !data            TYPE REF TO data OPTIONAL
        !object          TYPE REF TO object OPTIONAL
        !include_aliases TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE t_t_symbol .
    METHODS get_fields
      FINAL
      IMPORTING
        !type_descr      TYPE REF TO cl_abap_typedescr
        !data            TYPE REF TO data OPTIONAL
        !object          TYPE REF TO object OPTIONAL
      RETURNING
        VALUE(rt_fields) TYPE t_t_field_cache .
    METHODS dump_int
      IMPORTING
        !data         TYPE data
        !type_descr   TYPE REF TO cl_abap_typedescr OPTIONAL
        !convexit     TYPE string OPTIONAL
      RETURNING
        VALUE(r_json) TYPE json .
    METHODS is_compressable
      IMPORTING
        !type_descr        TYPE REF TO cl_abap_typedescr
        !name              TYPE csequence
      RETURNING
        VALUE(rv_compress) TYPE abap_bool .
    METHODS restore
      IMPORTING
        !json             TYPE json
        !length           TYPE i
        VALUE(type_descr) TYPE REF TO cl_abap_typedescr OPTIONAL
        !field_cache      TYPE t_t_field_cache OPTIONAL
      CHANGING
        !data             TYPE data OPTIONAL
        !offset           TYPE i DEFAULT 0
      RAISING
        cx_sy_move_cast_error .
    METHODS restore_type
      IMPORTING
        !json             TYPE json
        !length           TYPE i
        VALUE(type_descr) TYPE REF TO cl_abap_typedescr OPTIONAL
        !field_cache      TYPE t_t_field_cache OPTIONAL
        !convexit         TYPE string OPTIONAL
      CHANGING
        !data             TYPE data OPTIONAL
        !offset           TYPE i DEFAULT 0
      RAISING
        cx_sy_move_cast_error .
    METHODS dump_type
      IMPORTING
        !data         TYPE data
        !type_descr   TYPE REF TO cl_abap_elemdescr
        !convexit     TYPE string
      RETURNING
        VALUE(r_json) TYPE json .
    METHODS dump_type_ex
      IMPORTING
        !data         TYPE data
      RETURNING
        VALUE(r_json) TYPE json .
    METHODS pretty_name_ex
      IMPORTING
        !in        TYPE csequence
      RETURNING
        VALUE(out) TYPE string .
    METHODS generate_int_ex
      FINAL
      IMPORTING
        !json   TYPE json
        !length TYPE i
      CHANGING
        !data   TYPE data
        !offset TYPE i .
    METHODS pretty_name
      IMPORTING
        !in        TYPE csequence
      RETURNING
        VALUE(out) TYPE string .
    CLASS-METHODS escape
      IMPORTING
        !in        TYPE any
      RETURNING
        VALUE(out) TYPE string .
    CLASS-METHODS edm_datetime_to_ts
      IMPORTING
        !ticks        TYPE string
        !offset       TYPE string OPTIONAL
        !typekind     TYPE abap_typekind
      RETURNING
        VALUE(r_data) TYPE string .
  PRIVATE SECTION.

    DATA mv_extended TYPE bool .
    CLASS-DATA mc_me_type TYPE string .
    CLASS-DATA mc_cov_error TYPE c .
*"* private components of class lcl_ui2_json
*"* do not include other source files here!!!
ENDCLASS.                    "lcl_ui2_json DEFINITION




*&---------------------------------------------------------------------*
*& Form frm_get_logs
*&---------------------------------------------------------------------*
*&  日志记录
*&---------------------------------------------------------------------*
FORM frm_get_logs .

  DATA: lv_xstring TYPE xstring.
  DATA: lv_filename TYPE string.

  DATA: lr_log_flow TYPE REF TO data.

  " json
  CONCATENATE `logs/log_flow_` sy-datum `.json` INTO lv_filename.

  GET REFERENCE OF gt_log_flow INTO lr_log_flow.
  DATA: lv_req_json TYPE string.
  lv_req_json = lcl_ui2_json=>serialize( data = lr_log_flow
                                          pretty_name = lcl_ui2_json=>pretty_mode-low_case ).

  gr_cover_out->convert(
        EXPORTING data = lv_req_json
        IMPORTING buffer = lv_xstring ).

  " 添加到压缩包
  gr_zip->add( name    = lv_filename
               content = lv_xstring ).

ENDFORM.                    "frm_get_logs
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

  FIELD-SYMBOLS <ls_log_flow> LIKE LINE OF gt_log_flow.

  READ TABLE gt_log_flow ASSIGNING <ls_log_flow> WITH KEY type = p_type.
  IF sy-subrc <> 0.
    APPEND INITIAL LINE TO gt_log_flow ASSIGNING <ls_log_flow>.
    <ls_log_flow>-type = p_type.
  ENDIF.

  DATA: ls_info TYPE LINE OF ty_log_flow-info.

  ls_info-main = p_progname.
  ls_info-uname = lv_uname.
  ls_info-datum = p_udat.
  ls_info-uzeit = p_utime.
  APPEND ls_info TO <ls_log_flow>-info.
  CLEAR ls_info.

ENDFORM.                    "frm_set_log_flow
*&---------------------------------------------------------------------*
*& Form frm_get_ddl
*&---------------------------------------------------------------------*
*&  获取 DDL
*&---------------------------------------------------------------------*
FORM frm_get_ddl .

  " ECC 可能不存在相关表，此处注释掉
*
*  TYPES: BEGIN OF ty_ddlsrc,
*          ddlname TYPE CHAR40, " ddddlsrc-ddlname,
*          as4user TYPE CHAR12, " ddddlsrc-as4user,
*          as4date TYPE DATUM,  " ddddlsrc-as4date,
*          as4time TYPE UZEIT,  " ddddlsrc-as4time,
*          source  TYPE STRING, " ddddlsrc-source,
*          ddtext  TYPE TEXT60, " ddddlsrct-ddtext,
*         END OF ty_ddlsrc.
*
*  DATA: lv_filename TYPE string.
*  DATA: lt_source  TYPE TABLE OF text1000 WITH DEFAULT KEY,
*        lv_source  TYPE string,
*        lv_xstring TYPE xstring.
*
*  DATA: lv_folder TYPE char2,
*        lv_max    TYPE i.
*
*  DATA: lt_ddlsrc TYPE TABLE OF ty_ddlsrc,
*        ls_ddlsrc TYPE ty_ddlsrc.
*
*  " 读取 DDL
*  SELECT
*    rc~ddlname
*    rc~as4user
*    rc~as4date
*    rc~as4time
*    rc~source
*    ct~ddtext
*    FROM ('ddddlsrc') AS rc
*    INNER JOIN tadir AS ta ON ta~obj_name = rc~ddlname AND object = 'DDLS' " 存在 STOB 结构化对象
*    LEFT JOIN ddddlsrct AS ct ON ct~ddlname = rc~ddlname
*                             AND ct~ddlanguage = '1'
*                               AND ct~as4local = rc~as4local
*    INTO TABLE lt_ddlsrc
*    WHERE rc~ddlname LIKE 'Z%'
*      AND rc~as4local = 'A'
*      AND ta~devclass IN gt_range_devclass.
*
*  SORT lt_ddlsrc BY ddlname.
*
*  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
*  READ TABLE gt_delt_log ASSIGNING field-symbol(<ls_delt_log>) WITH KEY object = 'DDLS' BINARY SEARCH.
*  IF sy-subrc <> 0.
*    ls_delt_log-object = 'DDLS'.
*    ls_delt_log-ddate = sy-datum.
*    ls_delt_log-dtime = sy-uzeit.
*    APPEND ls_delt_log TO gt_delt_log.
*    CLEAR ls_delt_log.
*  ELSE.
*    ls_delt_log = <ls_delt_log>.
*    <ls_delt_log>-ddate = sy-datum.
*    <ls_delt_log>-dtime = sy-uzeit.
*  ENDIF.
*
*  LOOP AT lt_ddlsrc INTO data(ls_ddl).
*    " 文件夹匹配 -> 文件名
*
*    PERFORM frm_get_folder_name USING 'D' ls_ddl-ddlname lv_folder.
*
*    IF lv_folder IS NOT INITIAL.
*      CONCATENATE lv_folder `/` ls_ddl-ddlname `.ddl` INTO lv_filename.
*    ELSE.
*      CONCATENATE ls_ddl-ddlname '.ddl' INTO lv_filename.
*    ENDIF.
*
*    " map 文件路径
*    PERFORM frm_set_map_file USING lv_filename ls_ddl-ddtext.
*
*    CONCATENATE gv_parent_folder lv_filename INTO lv_filename.
*
*    " 日志 生成
*    PERFORM frm_set_log_flow USING 'DDLS' ls_ddl-ddlname ls_ddl-as4user ls_ddl-as4date ls_ddl-as4time.
*
*    " 检查增量
*    IF p_delt = 'X'.
*      IF ls_delt_log-ddate > ls_ddl-as4date
*        OR ( ls_delt_log-ddate = ls_ddl-as4date AND ls_delt_log-dtime > ls_ddl-as4time ).
*        REFRESH lt_source.
*        CLEAR: lv_filename, lv_xstring.
*
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
*    " --> Begin 清除末尾注释
*    REPLACE FIRST OCCURRENCE OF REGEX `\/\*\+\[internal\].*\*\/` IN ls_ddl-source WITH ``.
*    " <--
*
*    " string -> xstring
*    gr_cover_out->convert(
*          EXPORTING data = ls_ddl-source
*          IMPORTING buffer = lv_xstring ).
*
*    " 添加到压缩包
*    gr_zip->add( name    = lv_filename
*                 content = lv_xstring ).
*
*    " 清除缓存
*    REFRESH lt_source.
*    CLEAR: lv_filename, lv_xstring.
*  ENDLOOP.
*
*  " map 文件
*  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.                    "frm_get_ddl
*&---------------------------------------------------------------------*
*& Form frm_get_more
*&---------------------------------------------------------------------*
*&  获取更多数据
*&---------------------------------------------------------------------*
FORM frm_get_more .

  IF s_pack[] IS INITIAL.
    " 更多 类

  ENDIF.
ENDFORM.                    "frm_get_more

*----------------------------------------------------------------------*
*       CLASS lcl_export_ddldict DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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
    TYPES: tty_foreignkey_eq TYPE TABLE OF ty_foreignkey_eq WITH DEFAULT KEY.
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
    TYPES: tty_fields TYPE TABLE OF ty_field WITH DEFAULT KEY.

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

ENDCLASS.                    "lcl_export_ddldict DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_pretty_json DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pretty_json DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: pretty IMPORTING json               TYPE string
                          RETURNING VALUE(pretty_json) TYPE string.
ENDCLASS.                    "lcl_pretty_json DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_export_ddldict IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_export_ddldict IMPLEMENTATION.

  METHOD: table_maintain.
    DATA: lv_string  TYPE string,
          lv_xstring TYPE xstring.
    DATA: ls_table LIKE LINE OF gt_tables.
    DATA: lv_str TYPE string.
    DATA: ls_field TYPE ty_field.
    FIELD-SYMBOLS: <ls_eq> TYPE ty_foreignkey_eq.

    FIELD-SYMBOLS <ls_ddldicts> LIKE LINE OF me->ddldicts.

    DATA: lv_max TYPE i.

    LOOP AT gt_tables INTO ls_table.
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
      lv_str = ls_table-name.
      TRANSLATE lv_str TO LOWER CASE.
      add_line( str = lv_str  type = me->types-define ).

      LOOP AT ls_table-fields INTO ls_field.
        IF ls_field-foreignkey-label IS NOT INITIAL.
          CONCATENATE `  @abapcatalog.foreignkey.label : '` ls_field-foreignkey-label `'` INTO lv_str RESPECTING BLANKS.
          APPEND lv_str TO me->ddldicts.
        ENDIF.
        IF ls_field-foreignkey-keytype IS NOT INITIAL.
          CASE ls_field-foreignkey-keytype.
            WHEN 'KEY'.
              APPEND `  @abapcatalog.foreignkey.keytype : #KEY` TO me->ddldicts.
            WHEN 'REF'.
              APPEND `  @abapcatalog.foreignkey.keytype : #NON_KEY` TO me->ddldicts.
            WHEN 'TEXT'.
              APPEND `  @abapcatalog.foreignkey.keytype : #TEXT_KEY` TO me->ddldicts.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.
        IF ls_field-foreignkey-screencheck IS NOT INITIAL.
          CONCATENATE `  @abapcatalog.foreignkey.screencheck : ` ls_field-foreignkey-screencheck INTO lv_str RESPECTING BLANKS.
          APPEND lv_str TO me->ddldicts.
        ENDIF.

        IF ls_field-key = 'X'.
          CONCATENATE `key ` ls_field-name INTO lv_string RESPECTING BLANKS.
        ELSE.
          lv_string = ls_field-name.
        ENDIF.

        IF ls_field-adddesc IS NOT INITIAL.
          CONCATENATE `  @endusertext.label : '` ls_field-adddesc `'` INTO lv_str RESPECTING BLANKS.
          APPEND lv_str TO me->ddldicts.
        ENDIF.

        IF ls_field-addtion IS NOT INITIAL.
          APPEND ls_field-addtion TO me->ddldicts.
        ENDIF.

        IF ls_field-foreignkey-checktable IS INITIAL.
          IF ls_field-name IS INITIAL.
            CONCATENATE `  ` ls_field-type INTO lv_str RESPECTING BLANKS.
            IF ls_field-nonull = 'X'.
              CONCATENATE lv_str ` not null` INTO lv_str RESPECTING BLANKS.
            ENDIF.
            CONCATENATE lv_str `;` INTO lv_str RESPECTING BLANKS.

            APPEND lv_str TO me->ddldicts.
          ELSE.
            CONCATENATE `  ` lv_string INTO lv_str RESPECTING BLANKS.
            lv_max = ls_table-field_max + 1 - strlen( lv_string ).
            DO lv_max TIMES.
              CONCATENATE lv_str ` ` INTO lv_str RESPECTING BLANKS.
            ENDDO.
            CONCATENATE lv_str `: ` ls_field-type INTO lv_str RESPECTING BLANKS.
            IF ls_field-nonull = 'X'.
              CONCATENATE lv_str ` not null` INTO lv_str RESPECTING BLANKS.
            ENDIF.
            CONCATENATE lv_str `;` INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          ENDIF.
        ELSE.
          IF ls_field-name IS INITIAL.
            CONCATENATE `  ` ls_field-type INTO lv_str RESPECTING BLANKS.
            IF ls_field-nonull = 'X'.
              CONCATENATE lv_str ` not null` INTO lv_str RESPECTING BLANKS.
            ENDIF.
            APPEND lv_str TO me->ddldicts.
          ELSE.
            CONCATENATE `  ` lv_string INTO lv_str RESPECTING BLANKS.
            lv_max = ls_table-field_max + 1 - strlen( lv_string ).
            DO lv_max TIMES.
              CONCATENATE lv_str ` ` INTO lv_str RESPECTING BLANKS.
            ENDDO.
            CONCATENATE lv_str `: ` ls_field-type INTO lv_str RESPECTING BLANKS.

            IF ls_field-nonull = 'X'.
              CONCATENATE lv_str ` not null` INTO lv_str RESPECTING BLANKS.
            ENDIF.
            APPEND lv_str TO me->ddldicts.
          ENDIF.

          IF ls_field-foreignkey-card = '' AND ls_field-foreignkey-cardleft = ''.
            CONCATENATE `    with foreign key ` ls_field-foreignkey-checktable INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          ELSE.
            lv_str = `    with foreign key [` .

            IF ls_field-foreignkey-card = '1'.
              CONCATENATE lv_str `1` INTO lv_str RESPECTING BLANKS.
            ELSEIF ls_field-foreignkey-card = 'C'.
              CONCATENATE lv_str `0..1` INTO lv_str RESPECTING BLANKS.
            ELSE.
              CONCATENATE lv_str `0..*` INTO lv_str RESPECTING BLANKS.
            ENDIF.
            CONCATENATE lv_str `,` INTO lv_str RESPECTING BLANKS.

            IF ls_field-foreignkey-cardleft = '1'.
              CONCATENATE lv_str `1` INTO lv_str RESPECTING BLANKS.
            ELSE.
              CONCATENATE lv_str `0..1` INTO lv_str RESPECTING BLANKS.
            ENDIF.
            CONCATENATE lv_str `] ` ls_field-foreignkey-checktable INTO lv_str RESPECTING BLANKS.

            APPEND lv_str TO me->ddldicts.
          ENDIF.
          LOOP AT ls_field-foreignkey-foreignkey_eq ASSIGNING <ls_eq>.
            AT FIRST.
              CONCATENATE `      where ` <ls_eq>-checkfield ` = ` INTO lv_str RESPECTING BLANKS.
              IF <ls_eq>-forkey IS INITIAL.
                CONCATENATE lv_str <ls_eq>-fortable INTO lv_str RESPECTING BLANKS.
              ELSE.
                lv_string = <ls_eq>-fortable.
                TRANSLATE lv_string TO LOWER CASE.
                CONCATENATE lv_str lv_string `.` <ls_eq>-forkey INTO lv_str RESPECTING BLANKS.
              ENDIF.

              APPEND lv_str TO me->ddldicts.
              CONTINUE.
            ENDAT.

            CONCATENATE `        and ` <ls_eq>-checkfield ` = ` INTO lv_str RESPECTING BLANKS.
            IF <ls_eq>-forkey IS INITIAL.
              CONCATENATE lv_str <ls_eq>-fortable INTO lv_str RESPECTING BLANKS.
            ELSE.
              lv_string = <ls_eq>-fortable.
              TRANSLATE lv_string TO LOWER CASE.
              CONCATENATE lv_str lv_string `.` <ls_eq>-forkey INTO lv_str RESPECTING BLANKS.
            ENDIF.
            APPEND lv_str TO me->ddldicts.
          ENDLOOP.
          IF sy-subrc = 0.
            DESCRIBE TABLE me->ddldicts LINES lv_max.
            READ TABLE me->ddldicts ASSIGNING <ls_ddldicts> INDEX lv_max.
            IF sy-subrc = 0.
              CONCATENATE <ls_ddldicts> `;` INTO <ls_ddldicts>.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " 结束
      add_line( str = '' type = '' ).
      add_line( str = '}' type = '' ).

      FIELD-SYMBOLS <ls_string> LIKE LINE OF me->gt_string.

      APPEND INITIAL LINE TO me->gt_string ASSIGNING <ls_string>.
      <ls_string>-table = ls_table-name.

      CONCATENATE LINES OF me->ddldicts INTO <ls_string>-string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      REFRESH me->ddldicts.
    ENDLOOP.

  ENDMETHOD.                    "table_maintain

  METHOD struct_maintain.
    DATA: lv_string  TYPE string.

    DATA: ls_table LIKE LINE OF gt_tables.
    DATA: lv_str TYPE string.
    DATA: ls_field LIKE LINE OF ls_table-fields.
    DATA: lv_max TYPE i.

    LOOP AT gt_tables INTO ls_table.
      " 描述
      add_line( str = ls_table-desc  type = me->types-label ).
      " 增强类别
      add_line( str = ls_table-encat type = me->types-encat ).

      " 表名称
      CONCATENATE `define structure ` lv_str ` {` INTO lv_str RESPECTING BLANKS.
      TRANSLATE lv_str TO LOWER CASE.
      add_line( str = lv_str type = '' ).

      LOOP AT ls_table-fields INTO ls_field.
        IF ls_field-adddesc IS NOT INITIAL.
          CONCATENATE `  @endusertext.label : '` ls_field-adddesc `'` INTO lv_str RESPECTING BLANKS.
          APPEND lv_str TO me->ddldicts.
        ENDIF.

        IF ls_field-addtion IS NOT INITIAL.
          APPEND ls_field-addtion TO me->ddldicts.
        ENDIF.

        IF ls_field-name IS INITIAL.
          CONCATENATE `  ` ls_field-type INTO lv_str RESPECTING BLANKS.
          IF ls_field-nonull = 'X'.
            CONCATENATE lv_str ` not null` INTO lv_str RESPECTING BLANKS.
          ENDIF.
          CONCATENATE lv_str `;` INTO lv_str RESPECTING BLANKS.
          APPEND lv_str TO me->ddldicts.
        ELSE.
          lv_max = ls_table-field_max + 1.
          CONCATENATE `  ` ls_field-name INTO lv_str RESPECTING BLANKS.
          DO lv_max TIMES.
            CONCATENATE lv_str ` ` INTO lv_str RESPECTING BLANKS.
          ENDDO.
          CONCATENATE lv_str `: ` ls_field-type INTO lv_str RESPECTING BLANKS.

          IF ls_field-nonull = 'X'.
            CONCATENATE lv_str ` not null` INTO lv_str RESPECTING BLANKS.
          ENDIF.

          CONCATENATE lv_str `;` INTO lv_str RESPECTING BLANKS.
          APPEND lv_str TO me->ddldicts.
        ENDIF.
        CLEAR lv_str.
      ENDLOOP.

      " 结束
      add_line( str = '' type = '' ).
      add_line( str = '}' type = '' ).

      FIELD-SYMBOLS: <ls_string> LIKE LINE OF me->gt_string.

      APPEND INITIAL LINE TO me->gt_string ASSIGNING <ls_string>.
      <ls_string>-table = ls_table-name.

      CONCATENATE LINES OF me->ddldicts INTO <ls_string>-string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      REFRESH me->ddldicts.
    ENDLOOP.

  ENDMETHOD.                    "struct_maintain

  METHOD: add_line.
    DATA: lv_str TYPE string.

    CASE type.
      WHEN me->types-label.
        " 描述
        CONCATENATE `@EndUserText.label : '` str `'` INTO lv_str RESPECTING BLANKS.
        APPEND lv_str TO me->ddldicts.
      WHEN me->types-encat.
        " 增强类别

        " #NOT_CLASSIFIED         - 未分类
        " #NOT_EXTENSIBLE         - 无法增强（扩展）
        " #EXTENSIBLE_CHARACTER   - 可以增强（扩展）并且类似于角色
        " #EXTENSIBLE_CHARACTER_NUMERIC - 可以增强（扩展），并且是类似字符或数字的
        " #EXTENSIBLE_ANY         - 可以以任何方式增强（扩展）
        lv_str = `@AbapCatalog.enhancementCategory : #`.
        CASE str.
          WHEN '0'.
            CONCATENATE lv_str 'NOT_CLASSIFIED' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN '1'.
            CONCATENATE lv_str 'NOT_EXTENSIBLE' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN '2'.
            CONCATENATE lv_str 'EXTENSIBLE_CHARACTER' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN '3'.
            CONCATENATE lv_str 'EXTENSIBLE_CHARACTER_NUMERIC' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN '4'.
            CONCATENATE lv_str 'EXTENSIBLE_ANY' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN OTHERS.
        ENDCASE.
      WHEN me->types-tabty.
        " 表类别

        " #TRANSPARENT            - 透明表
        " #GLOBAL_TEMPORARY       - 全局临时表 （GTT）
        lv_str = `@AbapCatalog.tableCategory : `.
        IF str IS INITIAL.
          CONCATENATE lv_str `#TRANSPARENT` INTO lv_str RESPECTING BLANKS.
        ELSE.
          CONCATENATE lv_str str INTO lv_str RESPECTING BLANKS.
        ENDIF.
        APPEND lv_str TO me->ddldicts.
      WHEN me->types-edits.
        " 维护方式

        " RESTRICTED   - 补充
        " #NOT_ALLOWED - 无显示/编辑
        " #LIMITED     - 有限的显示/编辑
        " #ALLOWED     - 允许显示/编辑
        CASE str.
          WHEN ''.
            CONCATENATE '@AbapCatalog.dataMaintenance : #' 'RESTRICTED' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN 'D'.
            CONCATENATE '@AbapCatalog.dataMaintenance : #' 'LIMITED' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN 'N'.
            CONCATENATE '@AbapCatalog.dataMaintenance : #' 'NOT_ALLOWED' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN 'X'.
            CONCATENATE '@AbapCatalog.dataMaintenance : #' 'ALLOWED' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
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
        lv_str = `@AbapCatalog.deliveryClass : #`.
        IF str IS INITIAL.
          CONCATENATE lv_str `A` INTO lv_str RESPECTING BLANKS.
        ELSE.
          CONCATENATE lv_str str INTO lv_str RESPECTING BLANKS.
        ENDIF.
        APPEND lv_str TO me->ddldicts.
      WHEN me->types-actty.
        " 激活类型

        " #NOT_CLASSIFIED             - 激活类型 00
        " #NAMETAB_GENERATION_OFFLINE - 激活类型 01
        " #ADAPT_C_STRUCTURES         - 激活类型 02
        " #INITIAL_TABLE_REQUIRED     - 激活类型 10
        CASE str.
          WHEN '00'.
            CONCATENATE '@AbapCatalog.activationType : #' 'NOT_CLASSIFIED' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN '01'.
            CONCATENATE '@AbapCatalog.activationType : #' 'NAMETAB_GENERATION_OFFLINE' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN '02'.
            CONCATENATE '@AbapCatalog.activationType : #' 'ADAPT_C_STRUCTURES' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN '10'.
            CONCATENATE '@AbapCatalog.activationType : #' 'INITIAL_TABLE_REQUIRED' INTO lv_str RESPECTING BLANKS.
            APPEND lv_str TO me->ddldicts.
          WHEN OTHERS.
        ENDCASE.
      WHEN me->types-repla.
        " 替换对象
        CONCATENATE `@AbapCatalog.replacementObject : ` str INTO lv_str RESPECTING BLANKS.
        APPEND lv_str TO me->ddldicts.
      WHEN me->types-define.
        " 表名
        CONCATENATE `define table ` str ` {` INTO lv_str RESPECTING BLANKS.
        APPEND lv_str TO me->ddldicts.
      WHEN OTHERS.
        APPEND str TO me->ddldicts.
    ENDCASE.
  ENDMETHOD.                    "add_line

ENDCLASS.                    "lcl_export_ddldict IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pretty_json IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pretty_json IMPLEMENTATION.
  METHOD pretty.
    " 匹配数量
    DATA: invalidfs TYPE i,
          invalidbs TYPE i.

    DATA: lv_loff TYPE i,  " 上次开始位置
          l_alen  TYPE i.  " 匹配开始位置

    " 匹配内容
    DATA: l_exec TYPE string,
          l_indx TYPE i,
          l_ftc  TYPE string.

    " 缩进计算
    DATA: keytimesf        TYPE i,
          keytimesb        TYPE i,
          indentationtimes TYPE i.

    " 统计
    DATA: lv_ob_times TYPE i, " {
          lv_cb_times TYPE i, " }
          lv_dq_times TYPE i, " "
          lv_loff_new TYPE i,
          lv_alen_new TYPE i.

    DATA: ls_result TYPE match_result,
          lt_result TYPE TABLE OF match_result.

    DATA: lv_ftc TYPE i.

    FIND ALL OCCURRENCES OF REGEX `\{|\}|,|:` IN json
      RESULTS lt_result.

    IF lt_result IS INITIAL.
      pretty_json = json.
      RETURN.
    ELSE.

      LOOP AT lt_result INTO ls_result.
        l_alen = ls_result-offset + ls_result-length - lv_loff.
        lv_alen_new = ls_result-offset + ls_result-length - lv_loff_new.

        l_exec = json+ls_result-offset(ls_result-length).

        l_indx = ls_result-offset + ls_result-length.

        " 匹配开头到当前字符串之间的字符串
        l_ftc = json+lv_loff_new(lv_alen_new).

        REPLACE ALL OCCURRENCES OF REGEX `\\"` IN l_ftc WITH ``.
        REPLACE ALL OCCURRENCES OF REGEX `[^"]` IN l_ftc WITH ``.

        lv_dq_times = lv_dq_times + strlen( l_ftc ).
        lv_ftc = lv_dq_times MOD 2.

        IF lv_ftc <> 0.

          IF l_exec+0(1) = `{`.
            invalidfs = invalidfs + 1.
          ELSEIF l_exec+0(1) = `}`.
            invalidbs = invalidbs + 1.
          ENDIF.
          lv_loff_new = ls_result-offset + ls_result-length.

          CONTINUE.
        ENDIF.

        " 匹配开头到当前字符串之间的字符串
        l_ftc = json+lv_loff_new(lv_alen_new).

        REPLACE ALL OCCURRENCES OF REGEX `[^{]` IN l_ftc WITH ``.
        lv_ob_times = lv_ob_times + strlen( l_ftc ).
        keytimesf = lv_ob_times - invalidfs.

        " 匹配开头到当前字符串之间的字符串
        l_ftc = json+lv_loff_new(lv_alen_new).

        REPLACE ALL OCCURRENCES OF REGEX `[^}]` IN l_ftc WITH ``.
        lv_cb_times = lv_cb_times + strlen( l_ftc ).
        keytimesb = lv_cb_times - invalidbs.

        indentationtimes = keytimesf - keytimesb.

        IF l_exec+0(1) = '{'.
          CONCATENATE pretty_json json+lv_loff(l_alen) cl_abap_char_utilities=>cr_lf INTO pretty_json RESPECTING BLANKS.
          DO indentationtimes TIMES.
            CONCATENATE pretty_json `  ` INTO pretty_json RESPECTING BLANKS.
          ENDDO.
        ELSEIF l_exec+0(1) = '}'.
          l_alen = l_alen - 1.
          CONCATENATE pretty_json json+lv_loff(l_alen) cl_abap_char_utilities=>cr_lf INTO pretty_json RESPECTING BLANKS.
          DO indentationtimes TIMES.
            CONCATENATE pretty_json `  ` INTO pretty_json RESPECTING BLANKS.
          ENDDO.

          CONCATENATE pretty_json '}' INTO pretty_json.
        ELSEIF l_exec+0(1) = ','.
          CONCATENATE pretty_json json+lv_loff(l_alen) cl_abap_char_utilities=>cr_lf INTO pretty_json RESPECTING BLANKS.
          DO indentationtimes TIMES.
            CONCATENATE pretty_json `  ` INTO pretty_json RESPECTING BLANKS.
          ENDDO.
        ELSEIF l_exec+0(1) = ':'.
          CONCATENATE pretty_json json+lv_loff(l_alen) ` ` INTO pretty_json RESPECTING BLANKS.
        ENDIF.

        lv_loff = ls_result-offset + ls_result-length.
        lv_loff_new = ls_result-offset + ls_result-length.

        CLEAR: invalidfs, invalidbs.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "pretty
ENDCLASS.                    "lcl_pretty_json IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  frm_get_tables_ddl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_ZIP        text
*      -->PR_COVER_OUT  text
*      -->P_FILENAME    text
*----------------------------------------------------------------------*
FORM frm_get_tables_ddl USING pr_zip TYPE REF TO cl_abap_zip
                              pr_cover_out TYPE REF TO cl_abap_conv_out_ce
                              p_filename TYPE string.
  CHECK pr_zip IS NOT INITIAL
    AND pr_cover_out IS NOT INITIAL.

  PERFORM frm_set_parent_folder USING p_filename.

  DATA: lr_ddl TYPE REF TO lcl_export_ddldict.

  CREATE OBJECT lr_ddl.

  PERFORM frm_get_xx_ddl USING 'TRANSP' lr_ddl.

  lr_ddl->table_maintain( ).
  DATA: lv_filename TYPE string,
        lv_xstring  TYPE xstring.

  DATA: ls_string LIKE LINE OF lr_ddl->gt_string.

  LOOP AT lr_ddl->gt_string INTO ls_string.

    CONCATENATE p_filename ls_string-table `.ddl` INTO lv_filename.

    pr_cover_out->convert(
          EXPORTING data = ls_string-string
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    pr_zip->add( name    = lv_filename
                 content = lv_xstring ).
  ENDLOOP.

ENDFORM.                    "frm_get_tables_ddl

*&---------------------------------------------------------------------*
*&      Form  frm_get_structs_ddl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_ZIP        text
*      -->PR_COVER_OUT  text
*      -->P_FILENAME    text
*----------------------------------------------------------------------*
FORM frm_get_structs_ddl USING pr_zip TYPE REF TO cl_abap_zip
                              pr_cover_out TYPE REF TO cl_abap_conv_out_ce
                              p_filename TYPE string.
  CHECK pr_zip IS NOT INITIAL
    AND pr_cover_out IS NOT INITIAL.

  DATA: lr_ddl TYPE REF TO lcl_export_ddldict.

  CREATE OBJECT lr_ddl.

  PERFORM frm_get_xx_ddl USING 'INTTAB' lr_ddl.

  lr_ddl->struct_maintain( ).
  DATA: lv_filename TYPE string,
        lv_xstring  TYPE xstring.

  DATA: ls_string LIKE LINE OF lr_ddl->gt_string.

  LOOP AT lr_ddl->gt_string INTO ls_string.

    CONCATENATE p_filename ls_string-table `.ddl` INTO lv_filename.

    pr_cover_out->convert(
          EXPORTING data = ls_string-string
          IMPORTING buffer = lv_xstring ).

    " 添加到压缩包
    pr_zip->add( name    = lv_filename
                 content = lv_xstring ).
  ENDLOOP.

ENDFORM.                    "frm_get_structs_ddl


*&---------------------------------------------------------------------*
*&      Form  frm_get_xx_ddl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TYPE     text
*      -->PR_DDL     text
*----------------------------------------------------------------------*
FORM frm_get_xx_ddl USING p_type TYPE tabclass pr_ddl TYPE REF TO lcl_export_ddldict.

  TYPES: BEGIN OF ty_dd02l,
           tabname   TYPE dd02l-tabname,
           contflag  TYPE dd02l-contflag,
           authclass TYPE dd02l-authclass,
           mainflag  TYPE dd02l-mainflag,
           exclass   TYPE dd02l-exclass,
           as4date   TYPE dd02l-as4date,
           as4time   TYPE dd02l-as4time,
         END OF ty_dd02l.

  TYPES: BEGIN OF ty_dd02t,
           tabname    TYPE dd02t-tabname,
           ddlanguage TYPE dd02t-ddlanguage,
           ddtext     TYPE dd02t-ddtext,
         END OF ty_dd02t.
  TYPES: BEGIN OF ty_dd03t,
           tabname    TYPE dd03t-tabname,
           fieldname  TYPE dd03t-fieldname,
           ddlanguage TYPE dd03t-ddlanguage,
           ddtext     TYPE dd03t-ddtext,
         END OF ty_dd03t.
  TYPES: BEGIN OF ty_dd05q,
           tabname    TYPE dd05q-tabname,
           fieldname  TYPE dd05q-fieldname,
           primpos    TYPE dd05q-primpos,
           fortable   TYPE dd05q-fortable,
           forkey     TYPE dd05q-forkey,
           checktable TYPE dd05q-checktable,
           checkfield TYPE dd05q-checkfield,
         END OF ty_dd05q.
  TYPES: BEGIN OF ty_dd08l,
           tabname    TYPE dd08l-tabname,
           fieldname  TYPE dd08l-fieldname,
           checktable TYPE dd08l-checktable,
           frkart     TYPE dd08l-frkart,
           cardleft   TYPE dd08l-cardleft,
           card       TYPE dd08l-card,
           ddlanguage TYPE dd08t-ddlanguage,
           ddtext     TYPE dd08t-ddtext,
         END OF ty_dd08l.

  DATA: lt_dd02l TYPE TABLE OF ty_dd02l,
        ls_dd02l TYPE ty_dd02l.
  DATA: lt_dd02t TYPE TABLE OF ty_dd02t,
        ls_dd02t TYPE ty_dd02t.
  DATA: lt_dd03l TYPE TABLE OF dd03l,
        ls_dd03l TYPE dd03l.
  DATA: lt_dd03t TYPE TABLE OF ty_dd03t,
        ls_dd03t TYPE ty_dd03t.
  DATA: lt_dd05q TYPE TABLE OF ty_dd05q,
        ls_dd05q TYPE ty_dd05q.
  DATA: lt_dd08l TYPE TABLE OF ty_dd08l,
        ls_dd08l TYPE ty_dd08l.

  DATA: lr_pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT lr_pb.

  " 查询基础数据
  SELECT
   dd~tabname
   dd~contflag
   dd~authclass
   dd~mainflag
   dd~exclass
   dd~as4date
   dd~as4time
   FROM dd02l AS dd
   INNER JOIN tadir AS ta ON dd~tabname = ta~obj_name
   INTO TABLE lt_dd02l
   WHERE dd~tabname LIKE 'Z%'
     AND dd~tabclass = p_type
     AND dd~as4local = 'A'
     AND ta~pgmid = 'R3TR'
     AND ta~object = 'TABL'
     AND ta~devclass IN gt_range_devclass.

  CHECK lt_dd02l IS NOT INITIAL.

  SELECT
    tabname
    ddlanguage
    ddtext
    FROM dd02t
    INTO TABLE lt_dd02t
    FOR ALL ENTRIES IN lt_dd02l
    WHERE tabname = lt_dd02l-tabname
      AND ddlanguage IN ('1','E').
  SORT lt_dd02t BY tabname ddlanguage.
  DELETE ADJACENT DUPLICATES FROM lt_dd02t COMPARING tabname.

  " 字段 与 字段描述
  SELECT
    *
    FROM dd03l AS 3l
    INTO TABLE lt_dd03l
    FOR ALL ENTRIES IN lt_dd02l
    WHERE tabname  = lt_dd02l-tabname
      AND adminfield = '0' " 仅处理第一级别的字段/结构 => 在函数里有效 直接查表需要下方的逻辑
      AND depth = '00'     " adminfield 针对结构有效 depth 针对结构套结构有效
      AND as4local   = 'A'.
  SORT lt_dd03l BY tabname position.

  SELECT
    tabname
    fieldname
    ddlanguage
    ddtext
    FROM dd03t AS 3t
    INTO TABLE lt_dd03t
    FOR ALL ENTRIES IN lt_dd02l
    WHERE tabname = lt_dd02l-tabname
      AND as4local = 'A'
      AND ddlanguage IN ('1','E').
  SORT lt_dd03t BY tabname fieldname ddlanguage.
  DELETE ADJACENT DUPLICATES FROM lt_dd03t COMPARING tabname fieldname.


  " --> 补充 外键逻辑
  IF p_type = 'TRANSP'.
    SELECT
      tabname
      fieldname
      primpos
      fortable
      forkey
      checktable
      checkfield
      FROM dd05q
      INTO TABLE lt_dd05q
      FOR ALL ENTRIES IN lt_dd02l
      WHERE tabname = lt_dd02l-tabname.
    SORT lt_dd05q BY tabname fieldname primpos.

    SELECT
      8l~tabname
      8l~fieldname
      8l~checktable
      8l~frkart
      8l~cardleft
      8l~card
      8t~ddlanguage
      8t~ddtext
      FROM dd08l AS 8l
      LEFT JOIN dd08t AS 8t ON 8t~tabname = 8l~tabname
                           AND 8t~fieldname = 8l~fieldname
                           AND 8t~as4local = 8l~as4local
                           AND 8t~as4vers = 8l~as4vers
                           AND 8t~ddlanguage = '1'
      INTO TABLE lt_dd08l
      FOR ALL ENTRIES IN lt_dd02l
      WHERE 8l~tabname = lt_dd02l-tabname.

    SELECT
      8l~tabname
      8l~fieldname
      8l~checktable
      8l~frkart
      8l~cardleft
      8l~card
      8t~ddlanguage
      8t~ddtext
      FROM dd08l AS 8l
      LEFT JOIN dd08t AS 8t ON 8t~tabname = 8l~tabname
                           AND 8t~fieldname = 8l~fieldname
                           AND 8t~as4local = 8l~as4local
                           AND 8t~as4vers = 8l~as4vers
                           AND 8t~ddlanguage = 'E'
      APPENDING TABLE lt_dd08l
      FOR ALL ENTRIES IN lt_dd02l
      WHERE 8l~tabname = lt_dd02l-tabname.
    SORT lt_dd08l BY tabname fieldname ddlanguage.
    DELETE ADJACENT DUPLICATES FROM lt_dd08l COMPARING tabname fieldname.
  ENDIF.
  " <--

  DATA: lv_gotstate TYPE dcobjif-gotstate.
  DATA: lt_dd03p TYPE TABLE OF dd03p,
        ls_dd02v TYPE dd02v.

  DATA: lv_tabix TYPE sytabix.


  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  FIELD-SYMBOLS: <ls_delt_log> LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = p_type+0(4) BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = p_type+0(4).
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  FIELD-SYMBOLS: <ls_table> LIKE LINE OF pr_ddl->gt_tables.
  FIELD-SYMBOLS <ls_field> LIKE LINE OF <ls_table>-fields.
  DATA: lv_fieldlen TYPE i.
  DATA: lv_len TYPE i.
  DATA: lv_str TYPE string.
  DATA: ls_eq LIKE LINE OF <ls_field>-foreignkey-foreignkey_eq.

  DESCRIBE TABLE lt_dd02l LINES lr_pb->count.
  CONCATENATE 'Process ' p_type ' & ' INTO lr_pb->base_desc RESPECTING BLANKS.

  LOOP AT lt_dd02l INTO ls_dd02l.
    lr_pb->add( i_desc = ls_dd02l-tabname ).

    " 检查增量
    IF p_delt = 'X'.
      IF ls_delt_log-ddate > ls_dd02l-as4date
        OR ( ls_delt_log-ddate = ls_dd02l-as4date AND ls_delt_log-dtime > ls_dd02l-as4time ).
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO pr_ddl->gt_tables ASSIGNING <ls_table>.

    " 表名
    <ls_table>-name = ls_dd02l-tabname.

    " 表描述
    READ TABLE lt_dd02t INTO ls_dd02t WITH KEY tabname = ls_dd02l-tabname BINARY SEARCH.
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

      LOOP AT lt_dd03l INTO ls_dd03l FROM lv_tabix.
        IF ls_dd03l-tabname <> ls_dd02l-tabname.
          EXIT.
        ENDIF.

        APPEND INITIAL LINE TO <ls_table>-fields ASSIGNING <ls_field>.

        " 字段名
        <ls_field>-name   = ls_dd03l-fieldname.
        TRANSLATE <ls_field>-name TO LOWER CASE.

        IF ls_dd03l-fieldname = '.INCLUDE' OR ls_dd03l-fieldname = '.INCLU--AP'.
          CLEAR <ls_field>-name.

          IF ls_dd03l-reffield IS NOT INITIAL.
            <ls_field>-name = ls_dd03l-reffield.
            TRANSLATE <ls_field>-name TO LOWER CASE.
          ENDIF.
        ENDIF.

        lv_fieldlen = strlen( <ls_field>-name ).

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
              CONCATENATE 'abap.' ls_dd03l-datatype INTO <ls_field>-type.
            WHEN 'CHAR' OR 'NUMC'.
              CONCATENATE 'abap.' ls_dd03l-datatype  ls_dd03l-leng INTO <ls_field>-type.
            WHEN 'CURR'.
              CONCATENATE 'abap.' ls_dd03l-datatype  ls_dd03l-leng ls_dd03l-decimals INTO <ls_field>-type.
            WHEN 'DEC'.
              CONCATENATE 'abap.' ls_dd03l-datatype  ls_dd03l-leng ls_dd03l-decimals INTO <ls_field>-type.
            WHEN 'QUAN'.
              CONCATENATE 'abap.' ls_dd03l-datatype  ls_dd03l-leng ls_dd03l-decimals INTO <ls_field>-type.
            WHEN 'STRG'.
              CONCATENATE `abap.string(` ls_dd03l-leng `)` INTO <ls_field>-type.
            WHEN 'INT4' OR 'INT1' OR 'INT2' OR 'INT8'.
              CONCATENATE 'abap.' ls_dd03l-datatype INTO <ls_field>-type.
            WHEN OTHERS.
              CONCATENATE ls_dd03l-datatype '-' ls_dd03l-leng INTO <ls_field>-type.
          ENDCASE.
          TRANSLATE <ls_field>-type TO LOWER CASE.
          " 补充描述
          READ TABLE lt_dd03t INTO ls_dd03t WITH KEY tabname = ls_dd03l-tabname fieldname = ls_dd03l-fieldname BINARY SEARCH.
          IF sy-subrc = 0.
            <ls_field>-adddesc = ls_dd03t-ddtext.
          ENDIF.
        ELSE.
          " 数据元素不为空

          IF ls_dd03l-fieldname = '.INCLUDE'.
            "<ls_field>-type = |include { ls_dd03l-rollname CASE = LOWER }|. " 函数有效
            CONCATENATE `include ` ls_dd03l-precfield INTO <ls_field>-type.
            TRANSLATE <ls_field>-type TO LOWER CASE. " 表有效
          ELSEIF ls_dd03l-fieldname = '.INCLU--AP'.
            " append 的类型
            CONCATENATE `append ` ls_dd03l-precfield ` /* append 类型在 ddl 不会有, 导入时需删除 */` INTO <ls_field>-type.
            TRANSLATE <ls_field>-type TO LOWER CASE.
          ELSE.
            <ls_field>-type = ls_dd03l-rollname.
            TRANSLATE <ls_field>-type TO LOWER CASE.
          ENDIF.

        ENDIF.

        CASE ls_dd03l-datatype.
          WHEN 'CURR'.
            <ls_field>-addtion = `  @SEMANTICS.amount.currencycode : '`.
            CONCATENATE ls_dd03l-reftable ls_dd03l-reffield INTO lv_str RESPECTING BLANKS.
            TRANSLATE lv_str TO LOWER CASE.
            CONCATENATE <ls_field>-addtion lv_str INTO <ls_field>-addtion RESPECTING BLANKS.
          WHEN 'QUAN'.
            <ls_field>-addtion = `  @SEMANTICS.quantity.unitofmeasure : '`.
            CONCATENATE ls_dd03l-reftable ls_dd03l-reffield INTO lv_str RESPECTING BLANKS.
            TRANSLATE lv_str TO LOWER CASE.
            CONCATENATE <ls_field>-addtion lv_str INTO <ls_field>-addtion RESPECTING BLANKS.
          WHEN 'LANG'.
            <ls_field>-addtion = `  @ABAPCATALOG.textlanguage`.
          WHEN OTHERS.
        ENDCASE.

        READ TABLE lt_dd08l INTO ls_dd08l WITH KEY tabname = ls_dd03l-tabname fieldname = ls_dd03l-fieldname BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_field>-foreignkey-label = ls_dd08l-ddtext.
          IF ls_dd08l-frkart IS INITIAL.
            <ls_field>-foreignkey-screencheck = 'false'.
          ELSE.
            <ls_field>-foreignkey-screencheck = 'true'.
          ENDIF.
          IF ls_dd08l-cardleft IS INITIAL
            AND ls_dd08l-card IS INITIAL.
            <ls_field>-foreignkey-screencheck = 'true'.
          ENDIF.

          <ls_field>-foreignkey-keytype  = ls_dd08l-frkart.
          <ls_field>-foreignkey-cardleft = ls_dd08l-cardleft.
          <ls_field>-foreignkey-card     = ls_dd08l-card.
          <ls_field>-foreignkey-checktable  = ls_dd08l-checktable.
          TRANSLATE <ls_field>-foreignkey-checktable TO LOWER CASE.
        ENDIF.

        READ TABLE lt_dd05q TRANSPORTING NO FIELDS WITH KEY tabname = ls_dd03l-tabname fieldname = ls_dd03l-fieldname BINARY SEARCH.
        IF sy-subrc = 0.
          lv_tabix = sy-tabix.

          LOOP AT lt_dd05q INTO ls_dd05q FROM lv_tabix.
            IF ls_dd05q-tabname <> ls_dd03l-tabname
            OR ls_dd05q-fieldname <> ls_dd03l-fieldname.
              EXIT.
            ENDIF.

            CHECK ls_dd05q-fortable <> '*'.

            ls_eq-fortable = ls_dd05q-fortable.
            ls_eq-forkey   = ls_dd05q-forkey.
            TRANSLATE ls_eq-forkey TO LOWER CASE.
            ls_eq-checkfield = ls_dd05q-checkfield.
            TRANSLATE ls_eq-checkfield TO LOWER CASE.
            APPEND ls_eq TO <ls_field>-foreignkey-foreignkey_eq.
            CLEAR ls_eq.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "frm_get_xx_ddl
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
  DATA: lt_split_name TYPE TABLE OF string,
        lv_split_name TYPE string.

  DATA: l_off TYPE i,
        l_len TYPE i.
  DATA: l_sta TYPE i.

  lv_name = p_name.

  CLEAR p_folder.

  DATA: lv_folder TYPE ty_folder-tag.

  CASE p_type+0(1).
    WHEN 'R'.
      " mg 结尾 或 R/B 结尾 或 M 结尾
      lv_regex = `(MG|[RB]|M)_`.

      SPLIT lv_name AT '_' INTO TABLE lt_split_name.
      " IF sy-subrc <> 0.
      "   RETURN.
      " ENDIF.

      IF lines( lt_split_name ) <> 1.
        READ TABLE lt_split_name INTO lv_split_name INDEX 1.
        CONCATENATE lv_split_name '_' INTO lv_split_name.

        FIND REGEX lv_regex IN lv_split_name
          MATCH OFFSET l_off
          MATCH LENGTH l_len.
        IF sy-subrc = 0.
          l_off = l_off - 1.
          lv_folder = lv_split_name+1(l_off).
        ELSE.
          lv_folder = lv_name+1(2).
        ENDIF.
      ELSE.
        " 无下划线
        lv_regex = `(MG|[RB]|M)$`.

        FIND REGEX lv_regex IN lv_name
          MATCH OFFSET l_off
          MATCH LENGTH l_len.
        IF sy-subrc = 0.
          l_off = l_off - 1.
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
      lv_regex = `ZFM_.+_|ZFM.+_`.

      FIND REGEX lv_regex IN lv_name
        MATCH OFFSET l_off
        MATCH LENGTH l_len.
      IF sy-subrc = 0.
*        l_sta = l_off - l_len.
*        lv_name = lv_name+l_sta(l_len).
        lv_name = lv_name+l_off(l_len).
        REPLACE REGEX `^ZFM` IN lv_name WITH ''.
        REPLACE ALL OCCURRENCES OF SUBSTRING '_' IN lv_name WITH ''.
        lv_folder = lv_name.
      ENDIF.

    WHEN 'T'.
      lv_regex = `ZT\w+`.

      FIND REGEX lv_regex IN lv_name
        MATCH OFFSET l_off
        MATCH LENGTH l_len.
      IF sy-subrc = 0.
        l_sta = l_off - l_len.
        lv_name = lv_name+l_sta(l_len).
        REPLACE REGEX `^ZT` IN lv_name WITH ''.
        lv_folder = lv_name.
      ENDIF.

    WHEN 'D'.
      lv_regex = `ZV.*_`.

      FIND REGEX lv_regex IN lv_name
        MATCH OFFSET l_off
        MATCH LENGTH l_len.
      IF sy-subrc = 0.
        l_sta = l_off - l_len.
        lv_name = lv_name+l_sta(l_len).
        REPLACE REGEX `^ZV` IN lv_name WITH ''.
        REPLACE SUBSTRING '_' IN lv_name WITH ''.
        lv_folder = lv_name.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

  DATA: ls_folder LIKE LINE OF gt_folder.

  READ TABLE gt_folder INTO ls_folder WITH KEY tag = lv_folder BINARY SEARCH.
  IF sy-subrc = 0.

    IF ls_folder-folder IS INITIAL.
      p_folder = ls_folder-tag.
    ELSE.
      p_folder = ls_folder-folder.
    ENDIF.

  ENDIF.

ENDFORM.                    "frm_get_folder_name
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

  TYPES: BEGIN OF ty_dd01l,
           domname TYPE dd01l-domname,
           as4user TYPE dd01l-as4user,
           as4date TYPE dd01l-as4date,
           as4time TYPE dd01l-as4time,
           ddtext  TYPE dd01t-ddtext,
         END OF ty_dd01l.
  TYPES: BEGIN OF ty_dd07,
           domname    TYPE dd07l-domname,
           valpos     TYPE dd07l-valpos,
           ddtext     TYPE dd07t-ddtext,
           domvalue_l TYPE dd07l-domvalue_l,
           domvalue_h TYPE dd07l-domvalue_h,
           appval     TYPE dd07l-appval,     " 附加
         END OF ty_dd07.

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

  DATA: lt_source  TYPE TABLE OF text1000 WITH DEFAULT KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: ls_doma  TYPE ty_doma,
        lv_tabix TYPE sytabix.

  DATA: lt_dd01l TYPE TABLE OF ty_dd01l,
        ls_dd01  TYPE ty_dd01l.
  DATA: lt_dd07 TYPE TABLE OF ty_dd07,
        ls_dd07 TYPE ty_dd07.

  FIELD-SYMBOLS: <ls_values> LIKE LINE OF ls_doma-values.

  DATA: lo_doma TYPE REF TO data.

  DATA: lr_pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT lr_pb.

  " 数据获取
  SELECT
    dd~domname
    dd~as4user
    dd~as4date
    dd~as4time
    dt~ddtext
    FROM dd01l AS dd
    INNER JOIN tadir AS ta ON dd~domname = ta~obj_name
    LEFT JOIN dd01t AS dt ON dt~domname = dd~domname AND dt~ddlanguage = sy-langu
                         AND dt~as4local = dd~as4local AND dt~as4vers = dd~as4vers
    INTO TABLE lt_dd01l
    WHERE ta~pgmid = 'R3TR'
      AND ta~object = 'DOMA'
      AND ta~devclass IN gt_range_devclass
      AND dd~as4local = 'A'
      AND dd~domname  LIKE 'Z%'
      AND dd~as4user <> 'SAP'.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  FIELD-SYMBOLS: <ls_delt_log> LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'DOMA' BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = 'DOMA'.
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  " 获取域值
  IF lt_dd01l IS NOT INITIAL.
    SELECT
      dl~domname
      dl~valpos
      dt~ddtext
      dl~domvalue_l
      dl~domvalue_h
      dl~appval     " 附加
      FROM dd07l AS dl
      LEFT JOIN dd07t AS dt ON dt~domname  = dl~domname
                           AND dt~valpos   = dl~valpos
                           AND dt~as4vers  = dl~as4vers
                           AND dt~ddlanguage = sy-langu
      INTO TABLE lt_dd07
      FOR ALL ENTRIES IN lt_dd01l
      WHERE dl~domname = lt_dd01l-domname.
    SORT lt_dd07 BY domname valpos.
  ENDIF.

  DESCRIBE TABLE lt_dd01l LINES lr_pb->count.
  lr_pb->base_desc = 'Process Domain & '.

  LOOP AT lt_dd01l INTO ls_dd01.
    lr_pb->add( i_desc = ls_dd01-domname ).

    CONCATENATE ls_dd01-domname '.json' INTO lv_filename.

    CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

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

      LOOP AT lt_dd07 INTO ls_dd07 FROM lv_tabix.
        IF ls_dd07-domname <> ls_dd01-domname.
          EXIT.
        ENDIF.

        APPEND INITIAL LINE TO ls_doma-values ASSIGNING <ls_values>.
        MOVE-CORRESPONDING ls_dd07 TO <ls_values>.
      ENDLOOP.
    ENDIF.

    GET REFERENCE OF ls_doma INTO lo_doma.

    lv_source = lcl_ui2_json=>serialize( data = lo_doma
                                  pretty_name = lcl_ui2_json=>pretty_mode-camel_case ).
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

ENDFORM.                    "frm_get_domain
*&---------------------------------------------------------------------*
*& Form frm_get_element
*&---------------------------------------------------------------------*
*&  获取数据元素
*&---------------------------------------------------------------------*
FORM frm_get_element .

  " 数据元素和数据域对应关系 DD04L

  TYPES: BEGIN OF ty_dd04l,
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
         END OF ty_dd04l.

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

  DATA: lt_source  TYPE TABLE OF text1000 WITH DEFAULT KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: ls_elem  TYPE ty_elem,
        lv_tabix TYPE sytabix.

  DATA: lt_dd04l TYPE TABLE OF ty_dd04l,
        ls_dd04  TYPE ty_dd04l.

  DATA: lo_elem TYPE REF TO data.

  DATA: lr_pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT lr_pb.

  " 数据获取
  SELECT
    dd~rollname
    dd~domname
    dd~as4user
    dd~as4date
    dd~as4time
    dd~datatype
    dd~leng
    dd~decimals
    dd~outputlen
    dd~lowercase
    dd~convexit
    dd~entitytab
    dd~refkind
    dt~ddtext
    dt~reptext
    dt~scrtext_s
    dt~scrtext_m
    dt~scrtext_l
    FROM dd04l AS dd
    INNER JOIN tadir AS ta ON dd~rollname = ta~obj_name
    LEFT JOIN dd04t AS dt ON dt~rollname = dd~rollname AND dt~ddlanguage = dd~dtelmaster
                         AND dt~as4local = dd~as4local AND dt~as4vers = dd~as4vers
    INTO TABLE lt_dd04l
    WHERE ta~pgmid = 'R3TR'
      AND ta~object = 'DTEL'
      AND ta~devclass IN gt_range_devclass
      AND dd~as4local = 'A'
      AND dd~rollname  LIKE 'Z%'
      AND dd~as4user <> 'SAP'.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  FIELD-SYMBOLS: <ls_delt_log> LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'DTEL' BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = 'DTEL'.
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  DESCRIBE TABLE lt_dd04l LINES lr_pb->count.
  lr_pb->base_desc = 'Process Element & '.

  LOOP AT lt_dd04l INTO ls_dd04.
    lr_pb->add( i_desc = ls_dd04-rollname ).

    CONCATENATE ls_dd04-rollname '.json' INTO lv_filename.

    CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

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

    GET REFERENCE OF ls_elem INTO lo_elem.

    lv_source = lcl_ui2_json=>serialize( data = lo_elem
                                  pretty_name = lcl_ui2_json=>pretty_mode-camel_case ).
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

ENDFORM.                    "frm_get_element
*&---------------------------------------------------------------------*
*& Form FRM_GET_TABLETYPES_DDL
*&---------------------------------------------------------------------*
*&  获取表类型
*&---------------------------------------------------------------------*
FORM frm_get_tabletypes.
  " 表类型     DD40L
  " 表类型文本 DD40T

  TYPES: BEGIN OF ty_dd40l,
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
         END OF ty_dd40l.

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

  DATA: lt_source  TYPE TABLE OF text1000 WITH DEFAULT KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: ls_ttyp  TYPE ty_ttyp,
        lv_tabix TYPE sytabix.

  DATA: lt_dd40l TYPE TABLE OF ty_dd40l,
        ls_dd40  TYPE ty_dd40l.

  DATA: lo_ttyp TYPE REF TO data.

  DATA: lr_pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT lr_pb.

  " 数据获取
  SELECT
    dd~typename
    dd~rowtype
    dd~rowkind
    dd~datatype
    dd~leng
    dd~decimals
    dd~accessmode
    dd~keydef
    dd~keykind
    dd~keyfdcount
    dd~generic
    dd~typelen
    dd~as4user
    dd~as4date
    dd~as4time
    dt~ddtext
    FROM dd40l AS dd
    INNER JOIN tadir AS ta ON dd~typename = ta~obj_name
    LEFT JOIN dd40t AS dt ON dt~typename = dd~typename AND dt~ddlanguage = sy-langu
    INTO TABLE lt_dd40l
    WHERE ta~pgmid = 'R3TR'
      AND ta~object = 'TTYP'
      AND ta~devclass IN gt_range_devclass
      AND dd~as4local = 'A'
      AND dd~typename  LIKE 'Z%'.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  FIELD-SYMBOLS: <ls_delt_log> LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'TTYP' BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = 'TTYP'.
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  DESCRIBE TABLE lt_dd40l LINES lr_pb->count.
  lr_pb->base_desc = 'Process TableType & '.

  LOOP AT lt_dd40l INTO ls_dd40.
    lr_pb->add( i_desc = ls_dd40-typename ).
    CONCATENATE ls_dd40-typename '.json' INTO lv_filename.

    CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

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

    GET REFERENCE OF ls_ttyp INTO lo_ttyp.

    lv_source = lcl_ui2_json=>serialize( data = lo_ttyp
                                  pretty_name = lcl_ui2_json=>pretty_mode-camel_case ).
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

ENDFORM.                    "frm_get_tabletypes
*&---------------------------------------------------------------------*
*& Form frm_get_others
*&---------------------------------------------------------------------*
*&  保存其他
*&---------------------------------------------------------------------*
FORM frm_get_others .

  IF p_smw0 = abap_true.
    PERFORM frm_set_parent_folder USING `SMW0/`.
    " 由于文件名包含中文，设置编码格式为 UTF-8
*    gr_zip->support_unicode_names = abap_true.

    PERFORM frm_get_smw0.

    " 还原设置
*    gr_zip->support_unicode_names = abap_false.
  ENDIF.

  IF p_tran = abap_true.
    PERFORM frm_set_parent_folder USING `TCODE/`.

    PERFORM frm_get_tcode.
  ENDIF.

  IF p_xslt = abap_true.
    PERFORM frm_set_parent_folder USING `STRANS/`.

    PERFORM frm_get_strans.
  ENDIF.

  PERFORM frm_get_ench IN PROGRAM zsltest18 IF FOUND USING gr_zip gr_cover_out `ENCH/`.

  " 结果存储
  gv_parent_folder = `logs/flow/`.
  PERFORM frm_get_logs.

  DATA: ls_blob TYPE demo_indx_table.

  IF p_delt = 'X'.
    ls_blob-userid = sy-uname.
    GET TIME STAMP FIELD ls_blob-timestamp.
    EXPORT gt_delt_log TO DATABASE demo_indx_table(zd) FROM ls_blob ID 'DeltaDownload'.
  ENDIF.

ENDFORM.                    "frm_get_others
*&---------------------------------------------------------------------*
*& Form frm_get_smw0
*&---------------------------------------------------------------------*
*&  获取 SMW0
*&---------------------------------------------------------------------*
FORM frm_get_smw0 .
  TYPES: BEGIN OF ty_smw0,
           relid  TYPE wwwdata-relid,
           objid  TYPE wwwdata-objid,
           chname TYPE wwwdata-chname,
           tdate  TYPE wwwdata-tdate,
           ttime  TYPE wwwdata-ttime,
           text   TYPE wwwdata-text,
         END OF ty_smw0.
  TYPES: BEGIN OF ty_param,
           objid TYPE wwwparams-objid,
           name  TYPE wwwparams-name,
           value TYPE wwwparams-value,
         END OF ty_param.
  DATA: lv_filename TYPE string.
  DATA: lt_mime TYPE TABLE OF w3mime.
  DATA: lv_xstring TYPE xstring,
        lv_size    TYPE i.

  DATA: lt_smw0 TYPE TABLE OF ty_smw0,
        ls_smw0 TYPE ty_smw0.
  DATA: lt_param      TYPE TABLE OF ty_param,
        ls_param      TYPE ty_param,
        ls_param_ext  TYPE ty_param,
        ls_param_size TYPE ty_param.

  DATA: lr_pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT lr_pb.

  SELECT
    relid
    objid
    chname
    tdate
    ttime
    text
    FROM wwwdata
    INTO TABLE lt_smw0
   WHERE srtf2 = 0
     AND relid = 'MI'
     AND objid LIKE 'Z%'.
  IF sy-subrc <> 0.
    " 未找到数据不进行下载
    RETURN.
  ENDIF.

  " 获取文件参数
  SELECT
    objid
    name
    value
    FROM wwwparams
    INTO TABLE lt_param
    WHERE relid = 'MI'
      AND objid LIKE 'Z%'.

  SORT lt_smw0 BY objid.
  SORT lt_param BY objid name.

  DATA: ls_delt_log LIKE LINE OF gt_delt_log.
  FIELD-SYMBOLS: <ls_delt_log> LIKE LINE OF gt_delt_log.
  READ TABLE gt_delt_log ASSIGNING <ls_delt_log> WITH KEY object = 'SMW0' BINARY SEARCH.
  IF sy-subrc <> 0.
    ls_delt_log-object = 'SMW0'.
    ls_delt_log-ddate = sy-datum.
    ls_delt_log-dtime = sy-uzeit.
    APPEND ls_delt_log TO gt_delt_log.
    CLEAR ls_delt_log.
  ELSE.
    ls_delt_log = <ls_delt_log>.
    <ls_delt_log>-ddate = sy-datum.
    <ls_delt_log>-dtime = sy-uzeit.
  ENDIF.

  DESCRIBE TABLE lt_smw0 LINES lr_pb->count.
  lr_pb->base_desc = 'Process SMW0 & '.

  LOOP AT lt_smw0 INTO ls_smw0.
    lr_pb->add( i_desc = ls_smw0-objid ).
    CONCATENATE ls_smw0-objid `_` ls_smw0-text INTO lv_filename.

    " 后缀名
    READ TABLE lt_param INTO ls_param_ext WITH KEY objid = ls_smw0-objid name = 'fileextension' BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE lv_filename ls_param_ext-value INTO lv_filename.
    ENDIF.

    " map 文件路径
    PERFORM frm_set_map_file USING lv_filename ls_smw0-text.

    CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

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
    READ TABLE lt_param INTO ls_param_size WITH KEY objid = ls_smw0-objid name = 'filesize' BINARY SEARCH.
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

ENDFORM.                    "frm_get_smw0
*&---------------------------------------------------------------------*
*& Form frm_get_tcode
*&---------------------------------------------------------------------*
*&  事务码
*&---------------------------------------------------------------------*
FORM frm_get_tcode .

  " 事务码 tstc

  TYPES: BEGIN OF ty_tstc,
           tcode TYPE tstc-tcode,
           pgmna TYPE tstc-pgmna,
           dypno TYPE tstc-dypno,
           menue TYPE tstc-menue,
           cinfo TYPE tstc-cinfo,
           arbgb TYPE tstc-arbgb,
           ttext TYPE tstct-ttext,
           param TYPE tstcp-param,
         END OF ty_tstc.

  " 事务码
  " 目的便于查看与还原
  TYPES: BEGIN OF ty_tran,
           tcode TYPE tstc-tcode,
           pgmna TYPE tstc-pgmna,
           dypno TYPE tstc-dypno,
           menue TYPE tstc-menue,
           cinfo TYPE tstc-cinfo,
           arbgb TYPE tstc-arbgb,
           ttext TYPE tstct-ttext,
           param TYPE tstcp-param,
         END OF ty_tran.

  DATA: lv_filename TYPE string.

  DATA: lt_source  TYPE TABLE OF text1000 WITH DEFAULT KEY,
        lv_source  TYPE string,
        lv_xstring TYPE xstring.
  DATA: ls_tran  TYPE ty_tran,
        lt_tran  TYPE TABLE OF ty_tran,
        lv_tabix TYPE sytabix.

  DATA: lt_tstc TYPE TABLE OF ty_tstc,
        ls_tstc TYPE ty_tstc.

  DATA: lo_tran TYPE REF TO data.

  " 数据获取
  SELECT
    tc~tcode
    tc~pgmna
    tc~dypno
    tc~menue
    tc~cinfo
    tc~arbgb
    tt~ttext
    tp~param
    FROM tstc AS tc
    INNER JOIN tadir AS ta ON tc~tcode = ta~obj_name
    LEFT JOIN tstcp AS tp ON tp~tcode = tc~tcode
    LEFT JOIN tstct AS tt ON tt~tcode = tc~tcode AND tt~sprsl = sy-langu
    INTO TABLE lt_tstc
    WHERE ta~pgmid = 'R3TR'
      AND ta~object = 'TRAN'
      AND ta~devclass IN gt_range_devclass
      AND tc~tcode LIKE 'Z%'.

  LOOP AT lt_tstc INTO ls_tstc.
    " 生成 json 文件
    MOVE-CORRESPONDING ls_tstc TO ls_tran.

    APPEND ls_tran TO lt_tran.
    CLEAR ls_tran.
  ENDLOOP.

  CONCATENATE `TCode` '.json' INTO lv_filename.

  CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

  " map 文件路径
  PERFORM frm_set_map_file USING lv_filename '事务码目录'.

  GET REFERENCE OF lt_tran INTO lo_tran.

  lv_source = lcl_ui2_json=>serialize( data = lo_tran
                                pretty_name = lcl_ui2_json=>pretty_mode-camel_case ).

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

  " map 文件
  PERFORM frm_add_map_file USING gv_parent_folder.

ENDFORM.                    "frm_get_tcode
*&---------------------------------------------------------------------*
*& Form frm_get_STRANS
*&---------------------------------------------------------------------*
*& 获取转换
*&---------------------------------------------------------------------*
FORM frm_get_strans .

  " 内容表 O2XSLTDESC
  " 描述   O2XSLTTEXT

  TYPES: BEGIN OF ty_xslt,
           xsltdesc TYPE o2xsltdesc-xsltdesc,
           srtf2    TYPE o2xsltdesc-srtf2,
           clustr   TYPE o2xsltdesc-clustr,
           clustd   TYPE o2xsltdesc-clustd,
         END OF ty_xslt.

  DATA: lv_xstr_xslt TYPE xstring.
  DATA: lt_tline TYPE TABLE OF abaptxt255.

  DATA: lv_source  TYPE string,
        lv_xstring TYPE xstring.

  DATA: lv_filename TYPE string.

  DATA: lt_xslt TYPE TABLE OF ty_xslt.

  SELECT
    o2~xsltdesc
    o2~srtf2
    o2~clustr
    o2~clustd
    FROM tadir AS ta
    INNER JOIN o2xsltdesc AS o2 ON ta~obj_name = o2~xsltdesc
    INTO TABLE lt_xslt
    WHERE ta~pgmid = 'R3TR'
      AND ta~object = 'XSLT'
      AND ta~obj_name LIKE 'Z%'
      AND ta~devclass IN gt_range_devclass
      AND o2~state = 'A'
      AND o2~relid = 'TR'.

  CHECK lt_xslt IS NOT INITIAL.

  SORT lt_xslt BY xsltdesc srtf2.

  FIELD-SYMBOLS: <ls_xslt> LIKE LINE OF lt_xslt.

  LOOP AT lt_xslt ASSIGNING <ls_xslt>.
    CONCATENATE lv_xstr_xslt <ls_xslt>-clustd INTO lv_xstr_xslt IN BYTE MODE.

    AT END OF xsltdesc.
      IMPORT xsltdesc = lt_tline FROM DATA BUFFER lv_xstr_xslt.

      IF lt_tline IS NOT INITIAL.

        CONCATENATE LINES OF lt_tline INTO lv_source SEPARATED BY gc_newline.

        CONCATENATE <ls_xslt>-xsltdesc '.xsd' INTO lv_filename.

        CONCATENATE gv_parent_folder lv_filename INTO lv_filename.

        " string -> xstring
        gr_cover_out->convert(
              EXPORTING data = lv_source
              IMPORTING buffer = lv_xstring ).

        " 添加到压缩包
        gr_zip->add( name    = lv_filename
                     content = lv_xstring ).

        " 清除缓存
        CLEAR: lv_filename, lv_xstring, lv_source.
      ENDIF.

      CLEAR: lt_tline[], lv_xstr_xslt.
    ENDAT.

  ENDLOOP.

ENDFORM.                    "frm_get_strans
*&---------------------------------------------------------------------*
*& Form frm_set_parent_folder
*&---------------------------------------------------------------------*
*& 设置父层目录
*&---------------------------------------------------------------------*
FORM frm_set_parent_folder  USING p_folder.

  gv_parent_folder = p_folder.

ENDFORM.                    "frm_set_parent_folder


DEFINE escape_json_inplace.
*  replace all occurrences of regex `[\\"]` in &1 with `\\$0`. <-- this is slower than 2 plain replaces
  REPLACE ALL OCCURRENCES OF `\` IN &1 WITH `\\`.
  REPLACE ALL OCCURRENCES OF `"` IN &1 WITH `\"`.
END-OF-DEFINITION.

DEFINE escape_json.
  &2 = &1.
  escape_json_inplace &2.
END-OF-DEFINITION.

DEFINE is_compressable.
  IF mv_extended IS INITIAL.
    &3 = abap_true.
  ELSE.
    &3 = is_compressable( type_descr = &1 name = &2 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE dump_type.
  IF mv_extended IS INITIAL.
    dump_type_int &1 &2 &3 &4.
  ELSE.
    &3 = dump_type( data = &1 type_descr = &2 convexit = &4 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE dump_type_int.

  IF &4 IS NOT INITIAL AND &1 IS NOT INITIAL.
    TRY.
        CALL FUNCTION &4
          EXPORTING
            input  = &1
          IMPORTING
            output = &3
          EXCEPTIONS
            OTHERS = 1.
        IF sy-subrc IS INITIAL.
          CONCATENATE `"` &3 `"` INTO &3.
        ENDIF.
      CATCH cx_root.                                    "#EC NO_HANDLER
    ENDTRY.
  ELSE.
    CASE &2->type_kind.
      WHEN cl_abap_typedescr=>typekind_float OR cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1 OR
           cl_abap_typedescr=>typekind_int2 OR cl_abap_typedescr=>typekind_packed OR `8`. " TYPEKIND_INT8 -> '8' only from 7.40.
        IF &2->type_kind EQ cl_abap_typedescr=>typekind_packed AND mv_ts_as_iso8601 EQ c_bool-true AND &2->absolute_name CP `\TYPE=TIMESTAMP*`.
          IF &1 IS INITIAL.
            &3 = `""`.
          ELSE.
            &3 = &1.
            IF &2->absolute_name EQ `\TYPE=TIMESTAMP`.
              CONCATENATE `"` &3(4) `-` &3+4(2) `-` &3+6(2) `T` &3+8(2) `:` &3+10(2) `:` &3+12(2) `.0000000Z"`  INTO &3.
            ELSEIF &2->absolute_name EQ `\TYPE=TIMESTAMPL`.
              CONCATENATE `"` &3(4) `-` &3+4(2) `-` &3+6(2) `T` &3+8(2) `:` &3+10(2) `:` &3+12(2) `.` &3+15(7) `Z"`  INTO &3.
            ENDIF.
          ENDIF.
        ELSEIF &1 IS INITIAL.
          &3 = `0`.
        ELSE.
          &3 = &1.
          IF &1 LT 0.
            IF &2->type_kind <> cl_abap_typedescr=>typekind_float. "float: sign is already at the beginning
              SHIFT &3 RIGHT CIRCULAR.
            ENDIF.
          ELSE.
            CONDENSE &3.
          ENDIF.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_num.
        IF mv_numc_as_string EQ abap_true.
          IF &1 IS INITIAL.
            &3 = `""`.
          ELSE.
            CONCATENATE `"` &1 `"` INTO &3.
          ENDIF.
        ELSE.
          &3 = &1.
          SHIFT &3 LEFT DELETING LEADING ` 0`.
          IF &3 IS INITIAL.
            &3 = `0`.
          ENDIF.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_clike.
        IF &1 IS INITIAL.
          &3 = `""`.
        ELSEIF &2->absolute_name EQ mc_json_type.
          &3 = &1.
        ELSE.
          escape_json &1 &3.
          CONCATENATE `"` &3 `"` INTO &3.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
        IF &1 IS INITIAL.
          &3 = `""`.
        ELSE.
          &3 = xstring_to_string( &1 ).
          escape_json_inplace &3.
          CONCATENATE `"` &3 `"` INTO &3.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_char.
        IF &2->output_length EQ 1 AND mc_bool_types CS &2->absolute_name.
          IF &1 EQ c_bool-true.
            &3 = `true`.                                    "#EC NOTEXT
          ELSEIF mc_bool_3state CS &2->absolute_name AND &1 IS INITIAL.
            &3 = `null`.                                    "#EC NOTEXT
          ELSE.
            &3 = `false`.                                   "#EC NOTEXT
          ENDIF.
        ELSE.
          escape_json &1 &3.
          CONCATENATE `"` &3 `"` INTO &3.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_date.
        CONCATENATE `"` &1(4) `-` &1+4(2) `-` &1+6(2) `"` INTO &3.
      WHEN cl_abap_typedescr=>typekind_time.
        CONCATENATE `"` &1(2) `:` &1+2(2) `:` &1+4(2) `"` INTO &3.
      WHEN `k`. " cl_abap_typedescr=>typekind_enum
        &3 = &1.
        CONCATENATE `"` &3 `"` INTO &3.
      WHEN OTHERS.
        IF &1 IS INITIAL.
          &3 = `null`.                                      "#EC NOTEXT
        ELSE.
          &3 = &1.
        ENDIF.
    ENDCASE.
  ENDIF.

END-OF-DEFINITION.

DEFINE format_name.
  CASE &2.
    WHEN pretty_mode-camel_case.
      &3 = pretty_name( &1 ).
    WHEN pretty_mode-extended.
      &3 = pretty_name_ex( &1 ).
    WHEN pretty_mode-user_low_case.
      READ TABLE mt_name_mappings WITH TABLE KEY abap = &1 ASSIGNING <cache>. "#EC WARNOK
      IF sy-subrc IS INITIAL.
        &3 = <cache>-json.
      ELSE.
        &3 = &1.
        TRANSLATE &3 TO LOWER CASE.                       "#EC SYNTCHAR
      ENDIF.
    WHEN pretty_mode-user.
      READ TABLE mt_name_mappings WITH TABLE KEY abap = &1 ASSIGNING <cache>. "#EC WARNOK
      IF sy-subrc IS INITIAL.
        &3 = <cache>-json.
      ELSE.
        &3 = &1.
      ENDIF.
    WHEN pretty_mode-low_case.
      &3 = &1.
      TRANSLATE &3 TO LOWER CASE.                         "#EC SYNTCHAR
    WHEN OTHERS.
      &3 = &1.
  ENDCASE.
END-OF-DEFINITION.

DEFINE throw_error.
  RAISE EXCEPTION TYPE cx_sy_move_cast_error.
END-OF-DEFINITION.

DEFINE while_offset_cs.
*  >= 7.02 alternative
*  pos = find_any_not_of( val = json sub = &1 off = offset ).
*  if pos eq -1. offset = length.
*  else. offset = pos. endif.

* < 7.02
  WHILE offset < length.
    FIND FIRST OCCURRENCE OF json+offset(1) IN &1.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
    offset = offset + 1.
  ENDWHILE.
* < 7.02

END-OF-DEFINITION.

DEFINE while_offset_not_cs.
  WHILE offset < length.
    FIND FIRST OCCURRENCE OF &2+offset(1) IN &1.
    IF sy-subrc IS INITIAL.
      EXIT.
    ENDIF.
    offset = offset + 1.
  ENDWHILE.
END-OF-DEFINITION.

DEFINE eat_white.
  while_offset_cs sv_white_space.
  IF offset GE length.
    throw_error.
  ENDIF.
END-OF-DEFINITION.

DEFINE eat_name.
  IF json+offset(1) EQ `"`.
    mark   = offset + 1.
    offset = mark.
    FIND FIRST OCCURRENCE OF `"` IN SECTION OFFSET offset OF json MATCH OFFSET offset.
    IF sy-subrc IS NOT INITIAL.
      throw_error.
    ENDIF.
    match = offset - mark.
    &1 = json+mark(match).
    offset = offset + 1.
  ELSE.
    throw_error.
  ENDIF.
END-OF-DEFINITION.

DEFINE eat_string.
  IF json+offset(1) EQ `"`.
    mark   = offset + 1.
    offset = mark.
    DO.
      FIND FIRST OCCURRENCE OF `"` IN SECTION OFFSET offset OF json MATCH OFFSET pos.
      IF sy-subrc IS NOT INITIAL.
        throw_error.
      ENDIF.
      offset = pos.
      pos = pos - 1.
      " if escaped search further
      WHILE pos GE 0 AND json+pos(1) EQ `\`.
        pos = pos - 1.
      ENDWHILE.
      match = ( offset - pos ) MOD 2.
      IF match NE 0.
        EXIT.
      ENDIF.
      offset = offset + 1.
    ENDDO.
    match = offset - mark.
    &1 = json+mark(match).
    " unescaped singe characters, e.g \\, \", \/ etc,
    " BUT ONLY if someone really need the data
    IF type_descr IS NOT INITIAL.
      &1 = unescape( &1 ).
    ENDIF.
    offset = offset + 1.
  ELSE.
    throw_error.
  ENDIF.
END-OF-DEFINITION.

DEFINE eat_number.
  mark   = offset.
  while_offset_cs `0123456789+-eE.`.                        "#EC NOTEXT
  match = offset - mark.
  &1 = json+mark(match).
END-OF-DEFINITION.

DEFINE eat_bool.
  mark   = offset.
  while_offset_cs `aeflnrstu`.                              "#EC NOTEXT
  match = offset - mark.
  IF json+mark(match) EQ `true`.                            "#EC NOTEXT
    &1 = c_bool-true.
  ELSEIF json+mark(match) EQ `false`.                       "#EC NOTEXT
    IF type_descr IS BOUND AND mc_bool_3state CS type_descr->absolute_name.
      &1 = c_tribool-false.
    ELSE.
      &1 = c_bool-false.
    ENDIF.
  ELSEIF json+mark(match) EQ `null`.                        "#EC NOTEXT
    CLEAR &1.
  ENDIF.
END-OF-DEFINITION.

DEFINE eat_char.
  IF offset < length AND json+offset(1) EQ &1.
    offset = offset + 1.
  ELSE.
    throw_error.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_ui2_json IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_ui2_json IMPLEMENTATION.


  METHOD xstring_to_string.

    DATA: lv_xstring TYPE xstring.

    " let us fix data conversion issues here
    lv_xstring = in.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata = lv_xstring
      IMPORTING
        b64data = out
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc IS NOT INITIAL.
      out = in.
    ENDIF.

  ENDMETHOD.                    "xstring_to_string


  METHOD unescape.

    DATA: lv_offset          TYPE i,
          lv_match           TYPE i,
          lv_delta           TYPE i,
          lv_length          TYPE i,
          lv_offset_e        TYPE i,
          lv_length_e        TYPE i,
          lv_unicode_symb    TYPE c,
          lv_unicode_escaped TYPE string,
          lt_matches         TYPE match_result_tab.

    FIELD-SYMBOLS: <match> LIKE LINE OF lt_matches.

    " see reference for escaping rules in JSON RFC
    " https://www.ietf.org/rfc/rfc4627.txt

    unescaped = escaped.

    lv_length = strlen( unescaped ).

    FIND FIRST OCCURRENCE OF REGEX `\\[rntfbu]` IN unescaped RESPECTING CASE.
    IF sy-subrc IS INITIAL.
      FIND ALL OCCURRENCES OF REGEX `\\.` IN unescaped RESULTS lt_matches RESPECTING CASE.
      LOOP AT lt_matches ASSIGNING <match>.
        lv_match  = <match>-offset - lv_delta.
        lv_offset = lv_match + 1.
        CASE unescaped+lv_offset(1).
          WHEN `r`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>cr_lf(1).
            lv_delta = lv_delta + 1.
          WHEN `n`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>newline.
            lv_delta = lv_delta + 1.
          WHEN `t`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>horizontal_tab.
            lv_delta = lv_delta + 1.
          WHEN `f`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>form_feed.
            lv_delta = lv_delta + 1.
          WHEN `b`.
            REPLACE SECTION OFFSET lv_match LENGTH 2 OF unescaped WITH cl_abap_char_utilities=>backspace.
            lv_delta = lv_delta + 1.
          WHEN `u`.
            lv_offset   = lv_offset + 1.
            lv_offset_e = lv_offset + 4.
            lv_length_e = lv_length + lv_delta.
            IF lv_offset_e LE lv_length_e.
              lv_unicode_escaped = unescaped+lv_offset(4).
              TRANSLATE lv_unicode_escaped TO UPPER CASE.
              lv_unicode_symb = cl_abap_conv_in_ce=>uccp( lv_unicode_escaped ).
              IF lv_unicode_symb NE mc_cov_error.
                REPLACE SECTION OFFSET lv_match LENGTH 6 OF unescaped WITH lv_unicode_symb.
                lv_delta = lv_delta + 5.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    " based on RFC mentioned above, _any_ character can be escaped, and so shall be enscaped
    " the only exception is Unicode symbols, that shall be kept untouched, while serializer does not handle them
    " unescaped singe characters, e.g \\, \", \/ etc
    REPLACE ALL OCCURRENCES OF REGEX `\\(.)` IN unescaped WITH `$1` RESPECTING CASE.

  ENDMETHOD.                    "unescape


  METHOD tribool_to_bool.
    IF iv_tribool EQ c_tribool-true.
      rv_bool = c_bool-true.
    ELSEIF iv_tribool EQ c_tribool-undefined.
      rv_bool = abap_undefined. " fall back to abap_undefined
    ENDIF.
  ENDMETHOD.                    "TRIBOOL_TO_BOOL


  METHOD string_to_xstring.

    DATA: lv_xstring TYPE xstring.

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = in
      IMPORTING
        bindata = lv_xstring
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc IS INITIAL.
      out = lv_xstring.
    ELSE.
      out = in.
    ENDIF.

  ENDMETHOD.                    "string_to_xstring


  METHOD string_to_raw.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text     = iv_string
        encoding = iv_encoding
      IMPORTING
        buffer   = rv_xstring
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc IS NOT INITIAL.
      CLEAR rv_xstring.
    ENDIF.

  ENDMETHOD.                    "string_to_raw


  METHOD serialize_int.

    " **********************************************************************
    " Usage examples and documentation can be found on SCN:
    " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
    " **********************************************************************  "

    DATA: lo_descr      TYPE REF TO cl_abap_typedescr,
          lo_elem_descr TYPE REF TO cl_abap_elemdescr,
          lv_convexit   TYPE string.

    IF type_descr IS INITIAL.
      lo_descr = cl_abap_typedescr=>describe_by_data( data ).
    ELSE.
      lo_descr = type_descr.
    ENDIF.

    IF mv_conversion_exits EQ abap_true AND lo_descr->kind EQ cl_abap_typedescr=>kind_elem.
      lo_elem_descr ?= lo_descr.
      lv_convexit = get_convexit_func( elem_descr = lo_elem_descr input = abap_false ).
    ENDIF.

    r_json = dump_int( data = data type_descr = lo_descr convexit = lv_convexit ).

    " we do not do escaping of every single string value for white space characters,
    " but we do it on top, to replace multiple calls by 3 only, while we do not serialize
    " outlined/formatted JSON this shall not produce any harm
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN r_json WITH `\r\n`.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN r_json WITH `\n`.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN r_json WITH `\t`.
* replace all occurrences of cl_abap_char_utilities=>form_feed      in r_json with `\f`.
* replace all occurrences of cl_abap_char_utilities=>backspace      in r_json with `\b`.

    IF name IS NOT INITIAL AND ( mv_compress IS INITIAL OR r_json IS NOT INITIAL ).
      CONCATENATE `"` name `":` r_json INTO r_json.
    ENDIF.

  ENDMETHOD.                    "serialize


  METHOD serialize.

    " **********************************************************************
    " Usage examples and documentation can be found on SCN:
    " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
    " **********************************************************************  "

    DATA: lo_json  TYPE REF TO lcl_ui2_json.

    CREATE OBJECT lo_json
      EXPORTING
        compress         = compress
        pretty_name      = pretty_name
        name_mappings    = name_mappings
        assoc_arrays     = assoc_arrays
        assoc_arrays_opt = assoc_arrays_opt
        expand_includes  = expand_includes
        numc_as_string   = numc_as_string
        conversion_exits = conversion_exits
        ts_as_iso8601    = ts_as_iso8601.

    r_json = lo_json->serialize_int( name = name data = data type_descr = type_descr ).

  ENDMETHOD.                    "serialize


  METHOD restore_type.

    DATA: mark        LIKE offset,
          match       LIKE offset,
          sdummy      TYPE string,                          "#EC NEEDED
          rdummy      TYPE REF TO data,                     "#EC NEEDED
          pos         LIKE offset,
          line        TYPE REF TO data,
          key_ref     TYPE REF TO data,
          data_ref    TYPE REF TO data,
          key_name    TYPE string,
          key_value   TYPE string,
          lt_fields   LIKE field_cache,
          lt_symbols  TYPE t_t_symbol,
          lv_ticks    TYPE string,
          lv_offset   TYPE string,
          lv_convexit LIKE convexit,
          lo_exp      TYPE REF TO cx_root,
          elem_descr  TYPE REF TO cl_abap_elemdescr,
          table_descr TYPE REF TO cl_abap_tabledescr,
          data_descr  TYPE REF TO cl_abap_datadescr.

    FIELD-SYMBOLS: <line>      TYPE any,
                   <value>     TYPE any,
                   <data>      TYPE data,
                   <field>     LIKE LINE OF lt_fields,
                   <table>     TYPE ANY TABLE,
                   <value_sym> LIKE LINE OF lt_symbols.

    lv_convexit = convexit.

    IF type_descr IS INITIAL AND data IS SUPPLIED.
      type_descr = cl_abap_typedescr=>describe_by_data( data ).
      IF mv_conversion_exits EQ abap_true AND lv_convexit IS INITIAL AND type_descr->kind EQ cl_abap_typedescr=>kind_elem.
        elem_descr ?= type_descr.
        lv_convexit = get_convexit_func( elem_descr = elem_descr input = abap_true ).
      ENDIF.
    ENDIF.

    eat_white.

    TRY .
        IF type_descr IS NOT INITIAL AND type_descr->absolute_name EQ mc_json_type.
          " skip deserialization
          mark = offset.
          restore_type( EXPORTING json = json length = length CHANGING offset = offset ).
          match = offset - mark.
          data = json+mark(match).
        ENDIF.

        CASE json+offset(1).
          WHEN `{`. " object
            IF type_descr IS NOT INITIAL.
              IF mv_assoc_arrays EQ c_bool-true AND type_descr->kind EQ cl_abap_typedescr=>kind_table.
                table_descr ?= type_descr.
                data_descr = table_descr->get_table_line_type( ).
                IF table_descr->has_unique_key IS NOT INITIAL.
                  eat_char `{`.
                  eat_white.
                  IF json+offset(1) NE `}`.
                    ASSIGN data TO <table>.
                    CLEAR <table>.
                    CREATE DATA line LIKE LINE OF <table>.
                    ASSIGN line->* TO <line>.
                    lt_fields = get_fields( type_descr = data_descr data = line ).
                    IF table_descr->key_defkind EQ table_descr->keydefkind_user AND lines( table_descr->key ) EQ 1.
                      READ TABLE table_descr->key INDEX 1 INTO key_name.
                      READ TABLE lt_fields WITH TABLE KEY name = key_name ASSIGNING <field>.
                      key_ref = <field>-value.
                      IF mv_assoc_arrays_opt EQ c_bool-true.
                        lt_symbols = get_symbols( type_descr = data_descr data = line ).
                        DELETE lt_symbols WHERE name EQ key_name.
                        IF lines( lt_symbols ) EQ 1.
                          READ TABLE lt_symbols INDEX 1 ASSIGNING <value_sym>.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                    eat_white.
                    WHILE offset < length AND json+offset(1) NE `}`.
                      CLEAR <line>.
                      eat_name key_value.
                      eat_white.
                      eat_char `:`.
                      eat_white.
                      IF <value_sym> IS ASSIGNED.
                        ASSIGN <value_sym>-value->* TO <value>.
                        restore_type( EXPORTING json = json length = length type_descr = <value_sym>-type convexit = <value_sym>-convexit_in
                                      CHANGING data = <value> offset = offset ).
                      ELSE.
                        restore_type( EXPORTING json = json length = length type_descr = data_descr field_cache = lt_fields
                                      CHANGING data = <line> offset = offset ).
                      ENDIF.
                      IF table_descr->key_defkind EQ table_descr->keydefkind_user.
                        IF key_ref IS BOUND.
                          ASSIGN key_ref->* TO <value>.
                          IF <value> IS INITIAL.
                            <value> = key_value.
                          ENDIF.
                        ENDIF.
                      ELSEIF <line> IS INITIAL.
                        <line> = key_value.
                      ENDIF.

                      INSERT <line> INTO TABLE <table>.
                      eat_white.
                      IF offset < length AND json+offset(1) NE `}`.
                        eat_char `,`.
                        eat_white.
                      ELSE.
                        EXIT.
                      ENDIF.
                    ENDWHILE.
                  ELSE.
                    CLEAR data.
                  ENDIF.
                  eat_char `}`.
                ELSE.
                  restore( EXPORTING json = json length = length CHANGING  offset = offset ).
                ENDIF.
              ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                IF data IS INITIAL.
                  generate_int_ex( EXPORTING json = json length = length CHANGING offset = offset data = data ).
                ELSE.
                  data_ref ?= data.
                  type_descr = cl_abap_typedescr=>describe_by_data_ref( data_ref ).
                  ASSIGN data_ref->* TO <data>.
                  restore_type( EXPORTING json = json length = length type_descr = type_descr CHANGING data = <data> offset = offset ).
                ENDIF.
              ELSE.
                restore( EXPORTING json = json length = length type_descr = type_descr field_cache = field_cache
                         CHANGING data = data offset = offset ).
              ENDIF.
            ELSE.
              restore( EXPORTING json = json length = length CHANGING  offset = offset ).
            ENDIF.
          WHEN `[`. " array
            IF type_descr IS NOT INITIAL AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
              IF data IS INITIAL.
                generate_int_ex( EXPORTING json = json length = length CHANGING offset = offset data = data ).
              ELSE.
                data_ref ?= data.
                type_descr = cl_abap_typedescr=>describe_by_data_ref( data_ref ).
                ASSIGN data_ref->* TO <data>.
                restore_type( EXPORTING json = json length = length type_descr = type_descr CHANGING data = <data> offset = offset ).
              ENDIF.
            ELSE.
              eat_char `[`.
              eat_white.
              IF json+offset(1) NE `]`.
                IF type_descr IS NOT INITIAL AND type_descr->kind EQ cl_abap_typedescr=>kind_table.
                  table_descr ?= type_descr.
                  data_descr = table_descr->get_table_line_type( ).
                  ASSIGN data TO <table>.
                  CLEAR <table>.
                  CREATE DATA line LIKE LINE OF <table>.
                  ASSIGN line->* TO <line>.
                  lt_fields = get_fields( type_descr = data_descr data = line ).
                  WHILE offset < length AND json+offset(1) NE `]`.
                    CLEAR <line>.
                    restore_type( EXPORTING json = json length = length type_descr = data_descr field_cache = lt_fields
                                  CHANGING data = <line> offset = offset ).
                    INSERT <line> INTO TABLE <table>.
                    eat_white.
                    IF offset < length AND json+offset(1) NE `]`.
                      eat_char `,`.
                      eat_white.
                    ELSE.
                      EXIT.
                    ENDIF.
                  ENDWHILE.
                ELSE.
                  " skip array
                  eat_white.
                  WHILE offset < length AND json+offset(1) NE `]`.
                    restore_type( EXPORTING json = json length = length CHANGING offset = offset ).
                    eat_white.
                    IF offset < length AND json+offset(1) NE `]`.
                      eat_char `,`.
                      eat_white.
                    ELSE.
                      EXIT.
                    ENDIF.
                  ENDWHILE.
                  IF type_descr IS NOT INITIAL.
                    eat_char `]`.
                    throw_error.
                  ENDIF.
                ENDIF.
              ELSE.
                CLEAR data.
              ENDIF.
              eat_char `]`.
            ENDIF.
          WHEN `"`. " string
            eat_string sdummy.
            IF type_descr IS NOT INITIAL.
              " unescape string
              IF sdummy IS NOT INITIAL.
                IF type_descr->kind EQ cl_abap_typedescr=>kind_elem.
                  elem_descr ?= type_descr.

                  IF lv_convexit IS NOT INITIAL.
                    TRY .
                        CALL FUNCTION lv_convexit
                          EXPORTING
                            input         = sdummy
                          IMPORTING
                            output        = data
                          EXCEPTIONS
                            error_message = 2
                            OTHERS        = 1.
                        IF sy-subrc IS INITIAL.
                          RETURN.
                        ENDIF.
                      CATCH cx_root.                    "#EC NO_HANDLER
                    ENDTRY.
                  ENDIF.

                  CASE elem_descr->type_kind.
                    WHEN cl_abap_typedescr=>typekind_char.
                      IF elem_descr->output_length EQ 1 AND mc_bool_types CS elem_descr->absolute_name.
                        IF sdummy(1) CA `XxTt1`.
                          data = c_bool-true.
                        ELSE.
                          data = c_bool-false.
                        ENDIF.
                        RETURN.
                      ENDIF.
                    WHEN cl_abap_typedescr=>typekind_xstring.
                      string_to_xstring( EXPORTING in = sdummy CHANGING out = data ).
                      RETURN.
                    WHEN cl_abap_typedescr=>typekind_hex.
                      " support for Edm.Guid
                      REPLACE FIRST OCCURRENCE OF REGEX `^([0-9A-F]{8})-([0-9A-F]{4})-([0-9A-F]{4})-([0-9A-F]{4})-([0-9A-F]{12})$` IN sdummy
                      WITH `$1$2$3$4$5` REPLACEMENT LENGTH match. "#EC NOTEXT
                      IF sy-subrc EQ 0.
                        data = sdummy(match).
                      ELSE.
                        string_to_xstring( EXPORTING in = sdummy CHANGING out = data ).
                      ENDIF.
                      RETURN.
                    WHEN cl_abap_typedescr=>typekind_date.
                      " support for ISO8601 => https://en.wikipedia.org/wiki/ISO_8601
                      REPLACE FIRST OCCURRENCE OF REGEX `^(\d{4})-(\d{2})-(\d{2})` IN sdummy WITH `$1$2$3`
                      REPLACEMENT LENGTH match.             "#EC NOTEXT
                      IF sy-subrc EQ 0.
                        sdummy = sdummy(match).
                      ELSE.
                        " support for Edm.DateTime => http://www.odata.org/documentation/odata-version-2-0/json-format/
                        FIND FIRST OCCURRENCE OF REGEX `^\/Date\((-?\d+)([+-]\d{1,4})?\)\/` IN sdummy SUBMATCHES lv_ticks lv_offset IGNORING CASE. "#EC NOTEXT
                        IF sy-subrc EQ 0.
                          sdummy = edm_datetime_to_ts( ticks = lv_ticks offset = lv_offset typekind = elem_descr->type_kind ).
                        ELSE.
                          " support for Edm.Time => https://www.w3.org/TR/xmlschema11-2/#nt-durationRep
                          REPLACE FIRST OCCURRENCE OF REGEX `^-?P(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)(?:\.(\d+))?S)?)?` IN sdummy WITH `$1$2$3`
                          REPLACEMENT LENGTH match.         "#EC NOTEXT
                          IF sy-subrc EQ 0.
                            sdummy = sdummy(match).
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    WHEN cl_abap_typedescr=>typekind_time.
                      " support for ISO8601 => https://en.wikipedia.org/wiki/ISO_8601
                      REPLACE FIRST OCCURRENCE OF REGEX `^(\d{2}):(\d{2}):(\d{2})` IN sdummy WITH `$1$2$3`
                      REPLACEMENT LENGTH match.             "#EC NOTEXT
                      IF sy-subrc EQ 0.
                        sdummy = sdummy(match).
                      ELSE.
                        " support for Edm.DateTime => http://www.odata.org/documentation/odata-version-2-0/json-format/
                        FIND FIRST OCCURRENCE OF REGEX '^\/Date\((-?\d+)([+-]\d{1,4})?\)\/' IN sdummy SUBMATCHES lv_ticks lv_offset IGNORING CASE. "#EC NOTEXT
                        IF sy-subrc EQ 0.
                          sdummy = edm_datetime_to_ts( ticks = lv_ticks offset = lv_offset typekind = elem_descr->type_kind ).
                        ELSE.
                          " support for Edm.Time => https://www.w3.org/TR/xmlschema11-2/#nt-durationRep
                          REPLACE FIRST OCCURRENCE OF REGEX `^-?P(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)(?:\.(\d+))?S)?)?` IN sdummy WITH `$4$5$6`
                          REPLACEMENT LENGTH match.         "#EC NOTEXT
                          IF sy-subrc EQ 0.
                            sdummy = sdummy(match).
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    WHEN cl_abap_typedescr=>typekind_packed.
                      REPLACE FIRST OCCURRENCE OF REGEX `^(\d{4})-?(\d{2})-?(\d{2})T(\d{2}):?(\d{2}):?(\d{2})(?:[\.,](\d{0,7}))?Z?` IN sdummy WITH `$1$2$3$4$5$6.$7`
                      REPLACEMENT LENGTH match.             "#EC NOTEXT
                      IF sy-subrc EQ 0.
                        sdummy = sdummy(match).
                      ELSE.
                        FIND FIRST OCCURRENCE OF REGEX '^\/Date\((-?\d+)([+-]\d{1,4})?\)\/' IN sdummy SUBMATCHES lv_ticks lv_offset IGNORING CASE. "#EC NOTEXT
                        IF sy-subrc EQ 0.
                          sdummy = edm_datetime_to_ts( ticks = lv_ticks offset = lv_offset typekind = elem_descr->type_kind ).
                        ELSE.
                          " support for Edm.Time => https://www.w3.org/TR/xmlschema11-2/#nt-durationRep
                          REPLACE FIRST OCCURRENCE OF REGEX `^-?P(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)(?:\.(\d+))?S)?)?` IN sdummy WITH `$1$2$3$4$5$6.$7`
                          REPLACEMENT LENGTH match.         "#EC NOTEXT
                          IF sy-subrc EQ 0.
                            sdummy = sdummy(match).
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    WHEN `k`. "cl_abap_typedescr=>typekind_enum
                      TRY.
                          CALL METHOD ('CL_ABAP_XSD')=>('TO_VALUE')
                            EXPORTING
                              cs  = sdummy
                            CHANGING
                              val = data.
                          RETURN.
                        CATCH cx_sy_dyn_call_error.
                          throw_error. " Deserialization of enums is not supported
                      ENDTRY.
                  ENDCASE.
                ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                  CREATE DATA rdummy TYPE string.
                  ASSIGN rdummy->* TO <data>.
                  <data> = sdummy.
                  data ?= rdummy.
                  RETURN.
                ELSE.
                  throw_error. " Other wise dumps with OBJECTS_MOVE_NOT_SUPPORTED
                ENDIF.
                data = sdummy.
              ELSEIF type_descr->kind EQ cl_abap_typedescr=>kind_elem.
                CLEAR data.
              ELSE.
                throw_error. " Other wise dumps with OBJECTS_MOVE_NOT_SUPPORTED
              ENDIF.
            ENDIF.
          WHEN `-` OR `0` OR `1` OR `2` OR `3` OR `4` OR `5` OR `6` OR `7` OR `8` OR `9`. " number
            IF type_descr IS NOT INITIAL.
              IF type_descr->kind EQ type_descr->kind_ref AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                eat_number sdummy.                          "#EC NOTEXT
                match = strlen( sdummy ).
                IF sdummy CS '.'. " float.
                  CREATE DATA rdummy TYPE f.
                ELSEIF match GT 9. " packed
                  CREATE DATA rdummy TYPE p.
                ELSE. " integer
                  CREATE DATA rdummy TYPE i.
                ENDIF.
                ASSIGN rdummy->* TO <data>.
                <data> = sdummy.
                data ?= rdummy.
              ELSEIF type_descr->kind EQ type_descr->kind_elem.
                IF lv_convexit IS NOT INITIAL.
                  TRY .
                      eat_number sdummy.                    "#EC NOTEXT
                      CALL FUNCTION lv_convexit
                        EXPORTING
                          input         = sdummy
                        IMPORTING
                          output        = data
                        EXCEPTIONS
                          error_message = 2
                          OTHERS        = 1.
                      IF sy-subrc IS INITIAL.
                        RETURN.
                      ENDIF.
                    CATCH cx_root.                      "#EC NO_HANDLER
                  ENDTRY.
                ENDIF.
                eat_number data.                            "#EC NOTEXT
              ELSE.
                eat_number sdummy.                          "#EC NOTEXT
                throw_error.
              ENDIF.
            ELSE.
              eat_number sdummy.                            "#EC NOTEXT
            ENDIF.
          WHEN OTHERS. " boolean, e.g true/false/null
            IF type_descr IS NOT INITIAL.
              IF type_descr->kind EQ type_descr->kind_ref AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                CREATE DATA rdummy TYPE bool.
                ASSIGN rdummy->* TO <data>.
                eat_bool <data>.                            "#EC NOTEXT
                data ?= rdummy.
              ELSEIF type_descr->kind EQ type_descr->kind_elem.
                eat_bool data.                              "#EC NOTEXT
              ELSE.
                eat_bool sdummy.                            "#EC NOTEXT
                throw_error.
              ENDIF.
            ELSE.
              eat_bool sdummy.                              "#EC NOTEXT
            ENDIF.
        ENDCASE.
      CATCH cx_sy_move_cast_error cx_sy_conversion_no_number cx_sy_conversion_overflow INTO lo_exp.
        CLEAR data.
        IF mv_strict_mode EQ abap_true.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error EXPORTING previous = lo_exp.
        ENDIF.
    ENDTRY.

  ENDMETHOD.                    "restore_type


  METHOD restore.

    DATA: mark       LIKE offset,
          match      LIKE offset,
          ref_descr  TYPE REF TO cl_abap_refdescr,
          data_descr TYPE REF TO cl_abap_datadescr,
          data_ref   TYPE REF TO data,
          object_ref TYPE REF TO object,
          fields     LIKE field_cache,
          name_json  TYPE string.

    FIELD-SYMBOLS: <value>       TYPE any,
                   <field_cache> LIKE LINE OF field_cache.

    fields = field_cache.

    IF type_descr IS NOT INITIAL AND type_descr->kind EQ type_descr->kind_ref.
      ref_descr ?= type_descr.
      type_descr = ref_descr->get_referenced_type( ).
      IF ref_descr->type_kind EQ ref_descr->typekind_oref.
        IF data IS INITIAL.
          " can fire an exception, if type is abstract or constructor protected
          CREATE OBJECT data
            TYPE
              (type_descr->absolute_name).
        ENDIF.
        object_ref ?= data.
        fields = get_fields( type_descr = type_descr object = object_ref ).
      ELSEIF ref_descr->type_kind EQ ref_descr->typekind_dref.
        IF data IS INITIAL.
          data_descr ?= type_descr.
          CREATE DATA data TYPE HANDLE data_descr.
        ENDIF.
        data_ref ?= data.
        ASSIGN data_ref->* TO <value>.
        fields = get_fields( type_descr = type_descr data = data_ref ).
        restore( EXPORTING json = json length = length type_descr = type_descr field_cache = fields
                   CHANGING data = <value> offset = offset ).
        RETURN.
      ENDIF.
    ENDIF.

    IF fields IS INITIAL AND type_descr IS NOT INITIAL AND type_descr->kind EQ type_descr->kind_struct.
      GET REFERENCE OF data INTO data_ref.
      fields = get_fields( type_descr = type_descr data = data_ref ).
    ENDIF.

    eat_white.
    eat_char `{`.
    eat_white.

    WHILE offset < length AND json+offset(1) NE `}`.

      eat_name name_json.
      eat_white.
      eat_char `:`.
      eat_white.

      READ TABLE fields WITH TABLE KEY name = name_json ASSIGNING <field_cache>.
      IF sy-subrc IS NOT INITIAL.
        TRANSLATE name_json TO UPPER CASE.
        READ TABLE fields WITH TABLE KEY name = name_json ASSIGNING <field_cache>.
      ENDIF.

      IF sy-subrc IS INITIAL.
        ASSIGN <field_cache>-value->* TO <value>.
        restore_type( EXPORTING json = json length = length type_descr = <field_cache>-type convexit = <field_cache>-convexit_in CHANGING data = <value> offset = offset ).
      ELSE.
        restore_type( EXPORTING json = json length = length CHANGING offset = offset ).
      ENDIF.

      eat_white.

      IF offset < length AND json+offset(1) NE `}`.
        eat_char `,`.
        eat_white.
      ELSE.
        EXIT.
      ENDIF.

    ENDWHILE.

    eat_char `}`.

  ENDMETHOD.                    "restore


  METHOD raw_to_string.

    DATA: lv_output_length TYPE i,
          lt_binary_tab    TYPE STANDARD TABLE OF sdokcntbin.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_xstring
      IMPORTING
        output_length = lv_output_length
      TABLES
        binary_tab    = lt_binary_tab.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length  = lv_output_length
        encoding      = iv_encoding
      IMPORTING
        text_buffer   = rv_string
        output_length = lv_output_length
      TABLES
        binary_tab    = lt_binary_tab.

  ENDMETHOD.                    "raw_to_string


  METHOD pretty_name_ex.

    DATA: tokens TYPE TABLE OF char128,
          cache  LIKE LINE OF mt_name_mappings.

    FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                   <cache> LIKE LINE OF mt_name_mappings.

    READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
    IF sy-subrc IS INITIAL.
      out = <cache>-json.
    ELSE.
      out = in.


      TRANSLATE out TO LOWER CASE.
      TRANSLATE out USING `/_:_~_`.

      REPLACE ALL OCCURRENCES OF `__e__` IN out WITH `!`.
      REPLACE ALL OCCURRENCES OF `__n__` IN out WITH `#`.
      REPLACE ALL OCCURRENCES OF `__d__` IN out WITH `$`.
      REPLACE ALL OCCURRENCES OF `__p__` IN out WITH `%`.
      REPLACE ALL OCCURRENCES OF `__m__` IN out WITH `&`.
      REPLACE ALL OCCURRENCES OF `__s__` IN out WITH `*`.
      REPLACE ALL OCCURRENCES OF `__h__` IN out WITH `-`.
      REPLACE ALL OCCURRENCES OF `__t__` IN out WITH `~`.
      REPLACE ALL OCCURRENCES OF `__l__` IN out WITH `/`.
      REPLACE ALL OCCURRENCES OF `__c__` IN out WITH `:`.
      REPLACE ALL OCCURRENCES OF `__v__` IN out WITH `|`.
      REPLACE ALL OCCURRENCES OF `__a__` IN out WITH `@`.
      REPLACE ALL OCCURRENCES OF `__o__` IN out WITH `.`.
      REPLACE ALL OCCURRENCES OF `___`   IN out WITH `.`.

      REPLACE ALL OCCURRENCES OF `__` IN out WITH `"`.

      SPLIT out AT `_` INTO TABLE tokens.
      LOOP AT tokens ASSIGNING <token> FROM 2.
        TRANSLATE <token>(1) TO UPPER CASE.
      ENDLOOP.

      CONCATENATE LINES OF tokens INTO out.
      REPLACE ALL OCCURRENCES OF `"` IN out WITH `_`.

      cache-abap  = in.
      cache-json = out.
      INSERT cache INTO TABLE mt_name_mappings.
      INSERT cache INTO TABLE mt_name_mappings_ex.
    ENDIF.

  ENDMETHOD.                    "pretty_name_ex


  METHOD pretty_name.

    DATA: tokens TYPE TABLE OF char128,
          cache  LIKE LINE OF mt_name_mappings.

    FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                   <cache> LIKE LINE OF mt_name_mappings.

    READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
    IF sy-subrc IS INITIAL.
      out = <cache>-json.
    ELSE.
      out = in.

      REPLACE ALL OCCURRENCES OF `__` IN out WITH `*`.

      TRANSLATE out TO LOWER CASE.
      TRANSLATE out USING `/_:_~_`.
      SPLIT out AT `_` INTO TABLE tokens.
      LOOP AT tokens ASSIGNING <token> FROM 2.
        TRANSLATE <token>(1) TO UPPER CASE.
      ENDLOOP.

      CONCATENATE LINES OF tokens INTO out.
      REPLACE ALL OCCURRENCES OF `*` IN out WITH `_`.

      cache-abap  = in.
      cache-json = out.
      INSERT cache INTO TABLE mt_name_mappings.
      INSERT cache INTO TABLE mt_name_mappings_ex.
    ENDIF.

  ENDMETHOD.                    "pretty_name


  METHOD is_compressable.
    rv_compress = abap_true.
  ENDMETHOD.                    "is_compressable


  METHOD get_symbols.

    DATA: comp_tab     TYPE cl_abap_structdescr=>component_table,
          symb_tab     LIKE result,
          symb         LIKE LINE OF symb_tab,
          elem_descr   TYPE REF TO cl_abap_elemdescr,
          class_descr  TYPE REF TO cl_abap_classdescr,
          struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <comp>  LIKE LINE OF comp_tab,
                   <attr>  LIKE LINE OF cl_abap_objectdescr=>attributes,
                   <cache> LIKE LINE OF mt_name_mappings,
                   <field> TYPE any.

    IF type_descr->kind EQ cl_abap_typedescr=>kind_struct.

      struct_descr ?= type_descr.
      comp_tab = struct_descr->get_components( ).

      LOOP AT comp_tab ASSIGNING <comp>.
        IF <comp>-name IS NOT INITIAL AND
          ( <comp>-as_include EQ abap_false OR include_aliases EQ abap_true OR mv_expand_includes EQ abap_false ).
          symb-name = <comp>-name.
          symb-type = <comp>-type.
          IF data IS BOUND.
            IF mv_conversion_exits EQ abap_true AND symb-type->kind EQ cl_abap_typedescr=>kind_elem.
              elem_descr ?= symb-type.
              symb-convexit_in = get_convexit_func( elem_descr = elem_descr input = abap_true ).
              symb-convexit_out = get_convexit_func( elem_descr = elem_descr input = abap_false ).
            ENDIF.
            is_compressable symb-type symb-name symb-compressable.
            ASSIGN data->(symb-name) TO <field>.
            GET REFERENCE OF <field> INTO symb-value.
            format_name symb-name mv_pretty_name symb-header.
            CONCATENATE `"` symb-header  `":` INTO symb-header.
          ENDIF.
          APPEND symb TO result.
        ENDIF.
        IF <comp>-as_include EQ abap_true AND mv_expand_includes EQ abap_true.
          struct_descr ?= <comp>-type.
          symb_tab = get_symbols( type_descr = struct_descr include_aliases = include_aliases ).
          LOOP AT symb_tab INTO symb.
            CONCATENATE symb-name <comp>-suffix INTO symb-name.
            IF data IS BOUND.
              IF mv_conversion_exits EQ abap_true AND symb-type->kind EQ cl_abap_typedescr=>kind_elem.
                elem_descr ?= symb-type.
                symb-convexit_in = get_convexit_func( elem_descr = elem_descr input = abap_true ).
                symb-convexit_out = get_convexit_func( elem_descr = elem_descr input = abap_false ).
              ENDIF.
              is_compressable symb-type symb-name symb-compressable.
              ASSIGN data->(symb-name) TO <field>.
              GET REFERENCE OF <field> INTO symb-value.
              format_name symb-name mv_pretty_name symb-header.
              CONCATENATE `"` symb-header  `":` INTO symb-header.
            ENDIF.
            APPEND symb TO result.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

    ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_class.

      class_descr ?= type_descr.
      LOOP AT class_descr->attributes ASSIGNING <attr> WHERE is_constant IS INITIAL AND alias_for IS INITIAL AND
        ( is_interface IS INITIAL OR type_kind NE cl_abap_typedescr=>typekind_oref ).
        ASSIGN object->(<attr>-name) TO <field>.
        CHECK sy-subrc IS INITIAL. " we can only assign to public attributes
        symb-name = <attr>-name.
        symb-read_only = <attr>-is_read_only.
        symb-type = class_descr->get_attribute_type( <attr>-name ).
        IF mv_conversion_exits EQ abap_true AND symb-type->kind EQ cl_abap_typedescr=>kind_elem.
          elem_descr ?= symb-type.
          symb-convexit_in = get_convexit_func( elem_descr = elem_descr input = abap_true ).
          symb-convexit_out = get_convexit_func( elem_descr = elem_descr input = abap_false ).
        ENDIF.
        is_compressable symb-type symb-name symb-compressable.
        GET REFERENCE OF <field> INTO symb-value.
        format_name symb-name mv_pretty_name symb-header.
        CONCATENATE `"` symb-header  `":` INTO symb-header.
        APPEND symb TO result.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "GET_SYMBOLS


  METHOD get_fields.

    DATA: lt_symbols TYPE t_t_symbol,
          lv_name    TYPE char128,
          ls_field   LIKE LINE OF rt_fields.

    FIELD-SYMBOLS: <sym>   LIKE LINE OF lt_symbols,
                   <cache> LIKE LINE OF mt_name_mappings.

    lt_symbols = get_symbols( type_descr = type_descr data = data object = object include_aliases = abap_true ).

    LOOP AT lt_symbols ASSIGNING <sym> WHERE read_only EQ abap_false.
      MOVE-CORRESPONDING <sym> TO ls_field.

      " insert as UPPER CASE
      INSERT ls_field INTO TABLE rt_fields.

      " insert as lower case
      TRANSLATE ls_field-name TO LOWER CASE.
      INSERT ls_field INTO TABLE rt_fields.

      " as pretty printed
      IF mv_pretty_name NE pretty_mode-none AND mv_pretty_name NE pretty_mode-low_case.
        format_name <sym>-name mv_pretty_name ls_field-name.
        INSERT ls_field INTO TABLE rt_fields.
        " let us check for not well formed canelCase to be compatible with old logic
        lv_name = ls_field-name.
        TRANSLATE lv_name(1) TO UPPER CASE.
        ls_field-name = lv_name.
        INSERT ls_field INTO TABLE rt_fields.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "get_fields


  METHOD get_convexit_func.

    DATA: ls_dfies     TYPE dfies.

    elem_descr->get_ddic_field(
      RECEIVING
        p_flddescr   = ls_dfies    " Field Description
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3
    ).
    IF sy-subrc IS INITIAL AND ls_dfies-convexit IS NOT INITIAL.
      IF input EQ abap_true.
        CONCATENATE 'CONVERSION_EXIT_' ls_dfies-convexit '_INPUT' INTO rv_func.
      ELSE.
        CONCATENATE 'CONVERSION_EXIT_' ls_dfies-convexit '_OUTPUT' INTO rv_func.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_convexit_func


  METHOD generate_int_ex.

    DATA: lv_assoc_arrays     LIKE mv_assoc_arrays,
          lv_assoc_arrays_opt LIKE mv_assoc_arrays_opt,
          lv_mark             LIKE offset,
          lv_match            LIKE lv_mark,
          lv_json             TYPE lcl_ui2_json=>json.

    lv_mark = offset.
    restore_type( EXPORTING json = json length = length CHANGING offset = offset ).
    lv_match = offset - lv_mark.
    lv_json = json+lv_mark(lv_match).

    lv_assoc_arrays     = mv_assoc_arrays.
    lv_assoc_arrays_opt = mv_assoc_arrays_opt.

    mv_assoc_arrays     = abap_true.
    mv_assoc_arrays_opt = abap_true.

    data = generate_int( lv_json ).

    mv_assoc_arrays = lv_assoc_arrays.
    mv_assoc_arrays_opt = lv_assoc_arrays_opt.

  ENDMETHOD.                    "generate_int_ex


  METHOD generate_int.

    TYPES: BEGIN OF ts_field,
             name  TYPE string,
             value TYPE json,
           END OF ts_field.

    DATA: offset TYPE i.

    DATA: lt_json      TYPE STANDARD TABLE OF json WITH DEFAULT KEY,
          lv_comp_name TYPE abap_compname,
          lt_fields    TYPE SORTED TABLE OF ts_field WITH UNIQUE KEY name,
          lo_type      TYPE REF TO cl_abap_datadescr,
          lt_comp      TYPE abap_component_tab,
          lt_names     TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
          cache        LIKE LINE OF mt_name_mappings_ex,
          ls_comp      LIKE LINE OF lt_comp.

    FIELD-SYMBOLS: <data>   TYPE any,
                   <struct> TYPE any,
                   <json>   LIKE LINE OF lt_json,
                   <field>  LIKE LINE OF lt_fields,
                   <table>  TYPE STANDARD TABLE,
                   <cache>  LIKE LINE OF mt_name_mappings_ex.

    IF length IS NOT SUPPLIED.
      length = strlen( json ).
    ENDIF.

    eat_white.

    CASE json+offset(1).
      WHEN `{`."result must be a structure
        restore_type( EXPORTING json = json length = length CHANGING  data = lt_fields ).
        IF lt_fields IS NOT INITIAL.
          ls_comp-type = cl_abap_refdescr=>get_ref_to_data( ).
          LOOP AT lt_fields ASSIGNING <field>.
            READ TABLE mt_name_mappings_ex WITH TABLE KEY json = <field>-name ASSIGNING <cache>.
            IF sy-subrc IS INITIAL.
              ls_comp-name = <cache>-abap.
            ELSE.
              cache-json = ls_comp-name = <field>-name.
              " remove characters not allowed in component names
              TRANSLATE ls_comp-name USING ` _/_\_:_~_._-_=_>_<_(_)_@_+_*_?_&_$_#_%_^_`.
              IF mv_pretty_name EQ pretty_mode-camel_case OR mv_pretty_name EQ pretty_mode-extended.
                REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN ls_comp-name WITH `$1_$2`. "#EC NOTEXT
              ENDIF.
              TRANSLATE ls_comp-name TO UPPER CASE.
              cache-abap = ls_comp-name = lv_comp_name = ls_comp-name. " truncate by allowed field name length
              INSERT cache INTO TABLE mt_name_mappings_ex.
            ENDIF.
            INSERT ls_comp-name INTO TABLE lt_names.
            IF sy-subrc IS INITIAL.
              APPEND ls_comp TO lt_comp.
            ELSE.
              DELETE lt_fields.
            ENDIF.
          ENDLOOP.
          TRY.
              lo_type = cl_abap_structdescr=>create( p_components = lt_comp p_strict = c_bool-false ).
              CREATE DATA rr_data TYPE HANDLE lo_type.
              ASSIGN rr_data->* TO <struct>.
              LOOP AT lt_fields ASSIGNING <field>.
                ASSIGN COMPONENT sy-tabix OF STRUCTURE <struct> TO <data>.
                <data> = generate_int( <field>-value ).
              ENDLOOP.
            CATCH cx_sy_create_data_error cx_sy_struct_creation. "#EC NO_HANDLER
          ENDTRY.
        ENDIF.
        RETURN.
      WHEN `[`."result must be a table of ref
        restore_type( EXPORTING json = json length = length CHANGING  data = lt_json ).
        CREATE DATA rr_data TYPE TABLE OF REF TO data.
        ASSIGN rr_data->* TO <table>.
        LOOP AT lt_json ASSIGNING <json>.
          APPEND INITIAL LINE TO <table> ASSIGNING <data>.
          <data> = generate_int( <json> ).
        ENDLOOP.
        RETURN.
      WHEN `"`."string
        CREATE DATA rr_data TYPE string.
      WHEN `-` OR `0` OR `1` OR `2` OR `3` OR `4` OR `5` OR `6` OR `7` OR `8` OR `9`. " number
        IF json+offset CS '.'.
          CREATE DATA rr_data TYPE f.
        ELSEIF length GT 9.
          CREATE DATA rr_data TYPE p.
        ELSE.
          CREATE DATA rr_data TYPE i.
        ENDIF.
      WHEN OTHERS.
        IF json+offset EQ `true` OR json+offset EQ `false`. "#EC NOTEXT
          CREATE DATA rr_data TYPE abap_bool.
        ENDIF.
    ENDCASE.

    IF rr_data IS BOUND.
      ASSIGN rr_data->* TO <data>.
      restore_type( EXPORTING json = json length = length CHANGING  data = <data> ).
    ENDIF.

  ENDMETHOD.                    "generate_int


  METHOD generate.

    DATA: lo_json TYPE REF TO lcl_ui2_json,
          offset  TYPE i,
          length  TYPE i,
          lv_json LIKE json.

    " skip leading BOM signs
    length = strlen( json ).
    while_offset_not_cs `"{[` json.
    lv_json = json+offset.
    length  = length - offset.

    CREATE OBJECT lo_json
      EXPORTING
        pretty_name      = pretty_name
        name_mappings    = name_mappings
        assoc_arrays     = c_bool-true
        assoc_arrays_opt = c_bool-true.

    TRY .
        rr_data = lo_json->generate_int( json = lv_json length = length ).
      CATCH cx_sy_move_cast_error.                      "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "generate


  METHOD escape.

    out = in.

    REPLACE ALL OCCURRENCES OF `\` IN out WITH `\\`.
    REPLACE ALL OCCURRENCES OF `"` IN out WITH `\"`.

  ENDMETHOD.                    "escape


  METHOD edm_datetime_to_ts.

    CONSTANTS: lc_epochs TYPE string VALUE `19700101000000`.

    DATA: lv_ticks      TYPE p,
          lv_seconds    TYPE p,
          lv_subsec     TYPE p,
          lv_timestamps TYPE string,
          lv_timestamp  TYPE timestampl VALUE `19700101000000.0000000`.

    lv_ticks     = ticks.
    lv_seconds   = lv_ticks / 1000. " in seconds
    lv_subsec    = lv_ticks MOD 1000. " in subsec
    IF lv_subsec GT 0.
      lv_timestamps = lv_subsec.
      CONCATENATE lc_epochs `.` lv_timestamps INTO lv_timestamps.
      lv_timestamp = lv_timestamps.
    ENDIF.
    lv_timestamp = cl_abap_tstmp=>add( tstmp = lv_timestamp secs = lv_seconds ).

    IF offset IS NOT INITIAL.
      lv_ticks = offset+1.
      lv_ticks = lv_ticks * 60. "offset is in minutes
      IF offset(1) = '+'.
        lv_timestamp = cl_abap_tstmp=>subtractsecs( tstmp = lv_timestamp secs = lv_ticks ).
      ELSE.
        lv_timestamp = cl_abap_tstmp=>add( tstmp = lv_timestamp secs = lv_ticks ).
      ENDIF.
    ENDIF.

    CASE typekind.
      WHEN cl_abap_typedescr=>typekind_time.
        r_data = lv_timestamp.
        r_data = r_data+8(6).
      WHEN cl_abap_typedescr=>typekind_date.
        r_data = lv_timestamp.
        r_data = r_data(8).
      WHEN cl_abap_typedescr=>typekind_packed.
        r_data = lv_timestamp.
    ENDCASE.

  ENDMETHOD.                    "edm_datetime_to_ts


  METHOD dump_type_ex.

    DATA: lo_descr    TYPE REF TO cl_abap_elemdescr,
          lv_convexit TYPE string.

    lo_descr ?= cl_abap_typedescr=>describe_by_data( data ).

    IF mv_conversion_exits EQ abap_true.
      lv_convexit = get_convexit_func( elem_descr = lo_descr input = abap_false ).
    ENDIF.

    r_json = dump_type( data = data type_descr = lo_descr convexit = lv_convexit ).

  ENDMETHOD.                    "DUMP_TYPE_EX


  METHOD dump_type.

    CONSTANTS: lc_typekind_utclong TYPE abap_typekind VALUE 'p', " CL_ABAP_TYPEDESCR=>TYPEKIND_UTCLONG -> 'p' only from 7.60
               lc_typekind_int8    TYPE abap_typekind VALUE '8'.  " TYPEKIND_INT8 -> '8' only from 7.40

    IF convexit IS NOT INITIAL AND data IS NOT INITIAL.
      TRY.
          CALL FUNCTION convexit
            EXPORTING
              input  = data
            IMPORTING
              output = r_json
            EXCEPTIONS
              OTHERS = 1.
          IF sy-subrc IS INITIAL.
            CONCATENATE `"` r_json `"` INTO r_json.
          ENDIF.
        CATCH cx_root.                                  "#EC NO_HANDLER
      ENDTRY.
    ELSE.
      CASE type_descr->type_kind.
        WHEN cl_abap_typedescr=>typekind_float OR cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1 OR
             cl_abap_typedescr=>typekind_int2 OR cl_abap_typedescr=>typekind_packed OR lc_typekind_utclong OR lc_typekind_int8.

          IF mv_ts_as_iso8601 EQ c_bool-true AND
            ( type_descr->type_kind EQ lc_typekind_utclong OR
            ( type_descr->type_kind EQ cl_abap_typedescr=>typekind_packed AND type_descr->absolute_name CP `\TYPE=TIMESTAMP*` ) ).
            IF data IS INITIAL.
              r_json = `""`.
            ELSE.
              r_json = data.
              IF type_descr->absolute_name EQ `\TYPE=TIMESTAMP`.
                CONCATENATE `"` r_json(4) `-` r_json+4(2) `-` r_json+6(2) `T` r_json+8(2) `:` r_json+10(2) `:` r_json+12(2) `.0000000Z"`  INTO r_json.
              ELSEIF type_descr->absolute_name EQ `\TYPE=TIMESTAMPL`.
                CONCATENATE `"` r_json(4) `-` r_json+4(2) `-` r_json+6(2) `T` r_json+8(2) `:` r_json+10(2) `:` r_json+12(2) `.` r_json+15(7) `Z"`  INTO r_json.
              ENDIF.
            ENDIF.
          ELSEIF data IS INITIAL.
            r_json = `0`.
          ELSE.
            r_json = data.
            IF data LT 0.
              IF type_descr->type_kind <> cl_abap_typedescr=>typekind_float. "float: sign is already at the beginning
                SHIFT r_json RIGHT CIRCULAR.
              ENDIF.
            ELSE.
              CONDENSE r_json.
            ENDIF.
          ENDIF.
        WHEN cl_abap_typedescr=>typekind_num.
          IF mv_numc_as_string EQ abap_true.
            IF data IS INITIAL.
              r_json = `""`.
            ELSE.
              CONCATENATE `"` data `"` INTO r_json.
            ENDIF.
          ELSE.
            r_json = data.
            SHIFT r_json LEFT DELETING LEADING ` 0`.
            IF r_json IS INITIAL.
              r_json = `0`.
            ENDIF.
          ENDIF.
        WHEN cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_clike.
          IF data IS INITIAL.
            r_json = `""`.
          ELSEIF type_descr->absolute_name EQ mc_json_type.
            r_json = data.
          ELSE.
            r_json = escape( data ).
            CONCATENATE `"` r_json `"` INTO r_json.
          ENDIF.
        WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
          IF data IS INITIAL.
            r_json = `""`.
          ELSE.
            r_json = xstring_to_string( data ).
            r_json = escape( r_json ).
            CONCATENATE `"` r_json `"` INTO r_json.
          ENDIF.
        WHEN cl_abap_typedescr=>typekind_char.
          IF type_descr->output_length EQ 1 AND mc_bool_types CS type_descr->absolute_name.
            IF data EQ c_bool-true.
              r_json = `true`.                              "#EC NOTEXT
            ELSEIF mc_bool_3state CS type_descr->absolute_name AND data IS INITIAL.
              r_json = `null`.                              "#EC NOTEXT
            ELSE.
              r_json = `false`.                             "#EC NOTEXT
            ENDIF.
          ELSE.
            r_json = escape( data ).
            CONCATENATE `"` r_json `"` INTO r_json.
          ENDIF.
        WHEN cl_abap_typedescr=>typekind_date.
          CONCATENATE `"` data(4) `-` data+4(2) `-` data+6(2) `"` INTO r_json.
        WHEN cl_abap_typedescr=>typekind_time.
          CONCATENATE `"` data(2) `:` data+2(2) `:` data+4(2) `"` INTO r_json.
        WHEN 'k'. " cl_abap_typedescr=>typekind_enum
          r_json = data.
          CONCATENATE `"` r_json `"` INTO r_json.
        WHEN OTHERS.
          IF data IS INITIAL.
            r_json = `null`.                                "#EC NOTEXT
          ELSE.
            r_json = data.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDMETHOD.                    "dump_type


  METHOD dump_symbols.

    DATA: lv_properties TYPE STANDARD TABLE OF string,
          lv_itemval    TYPE string.

    FIELD-SYMBOLS: <value>  TYPE any,
                   <symbol> LIKE LINE OF it_symbols.

    LOOP AT it_symbols ASSIGNING <symbol>.
      ASSIGN <symbol>-value->* TO <value>.
      IF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
        lv_itemval = dump_int( data = <value> type_descr = <symbol>-type convexit = <symbol>-convexit_out ).
        CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
        APPEND lv_itemval TO lv_properties.
      ENDIF.
    ENDLOOP.

    CONCATENATE LINES OF lv_properties INTO r_json SEPARATED BY `,`.
    CONCATENATE `{` r_json `}` INTO r_json.

  ENDMETHOD.                    "dump_symbols


  METHOD dump_int.

    DATA: lo_typedesc   TYPE REF TO cl_abap_typedescr,
          lo_elem_descr TYPE REF TO cl_abap_elemdescr,
          lo_classdesc  TYPE REF TO cl_abap_classdescr,
          lo_structdesc TYPE REF TO cl_abap_structdescr,
          lo_tabledescr TYPE REF TO cl_abap_tabledescr,
          lt_symbols    TYPE t_t_symbol,
          lt_keys       LIKE lt_symbols,
          lt_properties TYPE STANDARD TABLE OF string,
          lt_fields     TYPE STANDARD TABLE OF string,
          lo_obj_ref    TYPE REF TO object,
          lo_data_ref   TYPE REF TO data,
          ls_skip_key   TYPE LINE OF abap_keydescr_tab,
          lv_array_opt  TYPE abap_bool,
          lv_prop_name  TYPE string,
          lv_keyval     TYPE string,
          lv_itemval    TYPE string.

    FIELD-SYMBOLS: <line>   TYPE any,
                   <value>  TYPE any,
                   <data>   TYPE data,
                   <key>    TYPE LINE OF abap_keydescr_tab,
                   <symbol> LIKE LINE OF lt_symbols,
                   <table>  TYPE ANY TABLE.

    " we need here macro instead of method calls because of the performance reasons.
    " based on SAT measurements.

    CASE type_descr->kind.
      WHEN cl_abap_typedescr=>kind_ref.

        IF data IS INITIAL.
          r_json = `null`.                                  "#EC NOTEXT
        ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
          lo_data_ref ?= data.
          lo_typedesc = cl_abap_typedescr=>describe_by_data_ref( lo_data_ref ).
          ASSIGN lo_data_ref->* TO <data>.
          r_json = dump_int( data = <data> type_descr = lo_typedesc ).
        ELSE.
          lo_obj_ref ?= data.
          lo_classdesc ?= cl_abap_typedescr=>describe_by_object_ref( lo_obj_ref ).
          lt_symbols = get_symbols( type_descr = lo_classdesc object = lo_obj_ref ).
          r_json = dump_symbols( lt_symbols ).
        ENDIF.

      WHEN cl_abap_typedescr=>kind_elem.
        lo_elem_descr ?= type_descr.
        dump_type data lo_elem_descr r_json convexit.

      WHEN cl_abap_typedescr=>kind_struct.

        lo_structdesc ?= type_descr.
        GET REFERENCE OF data INTO lo_data_ref.
        lt_symbols = get_symbols( type_descr = lo_structdesc data = lo_data_ref ).
        r_json = dump_symbols( lt_symbols ).

      WHEN cl_abap_typedescr=>kind_table.

        lo_tabledescr ?= type_descr.
        lo_typedesc = lo_tabledescr->get_table_line_type( ).

        ASSIGN data TO <table>.

        " optimization for structured tables
        IF lo_typedesc->kind EQ cl_abap_typedescr=>kind_struct.
          lo_structdesc ?= lo_typedesc.
          CREATE DATA lo_data_ref LIKE LINE OF <table>.
          ASSIGN lo_data_ref->* TO <line>.
          lt_symbols = get_symbols( type_descr = lo_structdesc data = lo_data_ref ).

          " here we have differentiation of output of simple table to JSON array
          " and sorted or hashed table with unique key into JSON associative array
          IF lo_tabledescr->has_unique_key IS NOT INITIAL AND mv_assoc_arrays IS NOT INITIAL.

            IF lo_tabledescr->key_defkind EQ lo_tabledescr->keydefkind_user.
              LOOP AT lo_tabledescr->key ASSIGNING <key>.
                READ TABLE lt_symbols WITH KEY name = <key>-name ASSIGNING <symbol>.
                APPEND <symbol> TO lt_keys.
              ENDLOOP.
            ENDIF.

            IF lines( lo_tabledescr->key ) EQ 1.
              READ TABLE lo_tabledescr->key INDEX 1 INTO ls_skip_key.
              DELETE lt_symbols WHERE name EQ ls_skip_key-name.
              " remove object wrapping for simple name-value tables
              IF mv_assoc_arrays_opt EQ abap_true AND lines( lt_symbols ) EQ 1.
                lv_array_opt = abap_true.
              ENDIF.
            ENDIF.

            LOOP AT <table> INTO <line>.
              CLEAR: lt_fields, lv_prop_name.
              LOOP AT lt_symbols ASSIGNING <symbol>.
                ASSIGN <symbol>-value->* TO <value>.
                IF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
                  IF <symbol>-type->kind EQ cl_abap_typedescr=>kind_elem.
                    lo_elem_descr ?= <symbol>-type.
                    dump_type <value> lo_elem_descr lv_itemval <symbol>-convexit_out.
                  ELSE.
                    lv_itemval = dump_int( data = <value> type_descr = <symbol>-type convexit = <symbol>-convexit_out ).
                  ENDIF.
                  IF lv_array_opt EQ abap_false.
                    CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
                  ENDIF.
                  APPEND lv_itemval TO lt_fields.
                ENDIF.
              ENDLOOP.

              IF lo_tabledescr->key_defkind EQ lo_tabledescr->keydefkind_user.
                LOOP AT lt_keys ASSIGNING <symbol>.
                  ASSIGN <symbol>-value->* TO <value>.
                  lv_keyval = <value>.
                  CONDENSE lv_keyval.
                  IF lv_prop_name IS NOT INITIAL.
                    CONCATENATE lv_prop_name mc_key_separator lv_keyval INTO lv_prop_name.
                  ELSE.
                    lv_prop_name = lv_keyval.
                  ENDIF.
                ENDLOOP.
              ELSE.
                LOOP AT lt_symbols ASSIGNING <symbol>.
                  ASSIGN <symbol>-value->* TO <value>.
                  lv_keyval = <value>.
                  CONDENSE lv_keyval.
                  IF lv_prop_name IS NOT INITIAL.
                    CONCATENATE lv_prop_name mc_key_separator lv_keyval INTO lv_prop_name.
                  ELSE.
                    lv_prop_name = lv_keyval.
                  ENDIF.
                ENDLOOP.
              ENDIF.

              CONCATENATE LINES OF lt_fields INTO lv_itemval SEPARATED BY `,`.
              IF lv_array_opt EQ abap_false.
                CONCATENATE `"` lv_prop_name `":{` lv_itemval `}` INTO lv_itemval.
              ELSE.
                CONCATENATE `"` lv_prop_name `":` lv_itemval `` INTO lv_itemval.
              ENDIF.
              APPEND lv_itemval TO lt_properties.

            ENDLOOP.

            CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
            CONCATENATE `{` r_json `}` INTO r_json.

          ELSE.

            LOOP AT <table> INTO <line>.
              CLEAR lt_fields.
              LOOP AT lt_symbols ASSIGNING <symbol>.
                ASSIGN <symbol>-value->* TO <value>.
                IF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
                  IF <symbol>-type->kind EQ cl_abap_typedescr=>kind_elem.
                    lo_elem_descr ?= <symbol>-type.
                    dump_type <value> lo_elem_descr lv_itemval <symbol>-convexit_out.
                  ELSE.
                    lv_itemval = dump_int( data = <value> type_descr = <symbol>-type convexit = <symbol>-convexit_out ).
                  ENDIF.
                  CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
                  APPEND lv_itemval TO lt_fields.
                ENDIF.
              ENDLOOP.

              CONCATENATE LINES OF lt_fields INTO lv_itemval SEPARATED BY `,`.
              CONCATENATE `{` lv_itemval `}` INTO lv_itemval.
              APPEND lv_itemval TO lt_properties.
            ENDLOOP.

            CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
            CONCATENATE `[` r_json `]` INTO r_json.

          ENDIF.
        ELSE.
          LOOP AT <table> ASSIGNING <value>.
            lv_itemval = dump_int( data = <value> type_descr = lo_typedesc ).
            APPEND lv_itemval TO lt_properties.
          ENDLOOP.

          CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
          CONCATENATE `[` r_json `]` INTO r_json.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "dump


  METHOD dump.

    DATA: lo_json TYPE REF TO lcl_ui2_json.

    CREATE OBJECT lo_json
      EXPORTING
        compress      = compress
        pretty_name   = pretty_name
        assoc_arrays  = assoc_arrays
        ts_as_iso8601 = ts_as_iso8601.

    r_json = lo_json->dump_int( data = data type_descr = type_descr ).

  ENDMETHOD.                    "dump


  METHOD deserialize_int.

    DATA: length    TYPE i,
          offset    TYPE i,
          unescaped LIKE json.

    " **********************************************************************
    " Usage examples and documentation can be found on SCN:
    " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
    " **********************************************************************  "

    IF json IS NOT INITIAL OR jsonx IS NOT INITIAL.

      IF jsonx IS NOT INITIAL.
        unescaped = raw_to_string( jsonx ).
      ELSE.
        unescaped = json.
      ENDIF.

      " skip leading BOM signs
      length = strlen( unescaped ).
      while_offset_not_cs `"{[` unescaped.
      unescaped = unescaped+offset.
      length = length - offset.
      restore_type( EXPORTING json = unescaped length = length CHANGING data = data ).

    ENDIF.

  ENDMETHOD.                    "deserialize


  METHOD deserialize.

    DATA: lo_json TYPE REF TO lcl_ui2_json.

    " **********************************************************************
    " Usage examples and documentation can be found on SCN:
    " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
    " **********************************************************************  "

    IF json IS NOT INITIAL OR jsonx IS NOT INITIAL.

      CREATE OBJECT lo_json
        EXPORTING
          pretty_name      = pretty_name
          name_mappings    = name_mappings
          assoc_arrays     = assoc_arrays
          conversion_exits = conversion_exits
          assoc_arrays_opt = assoc_arrays_opt.

      TRY .
          lo_json->deserialize_int( EXPORTING json = json jsonx = jsonx CHANGING data = data ).
        CATCH cx_sy_move_cast_error.                    "#EC NO_HANDLER
      ENDTRY.

    ENDIF.

  ENDMETHOD.                    "deserialize


  METHOD constructor.

    DATA: rtti TYPE REF TO cl_abap_classdescr,
          pair LIKE LINE OF name_mappings.

    mv_compress         = compress.
    mv_pretty_name      = pretty_name.
    mv_assoc_arrays     = assoc_arrays.
    mv_ts_as_iso8601    = ts_as_iso8601.
    mv_expand_includes  = expand_includes.
    mv_assoc_arrays_opt = assoc_arrays_opt.
    mv_strict_mode      = strict_mode.
    mv_numc_as_string   = numc_as_string.
    mv_conversion_exits = conversion_exits.

    LOOP AT name_mappings INTO pair.
      TRANSLATE pair-abap TO UPPER CASE.
      INSERT pair INTO TABLE mt_name_mappings.
    ENDLOOP.

    " if it dumps here, you have passed ambiguous mapping to the API
    " please check your code for duplicates, pairs ABAP - JSON shall be unique
    INSERT LINES OF mt_name_mappings INTO TABLE mt_name_mappings_ex.

    IF mt_name_mappings IS NOT INITIAL.
      IF mv_pretty_name EQ pretty_mode-none.
        mv_pretty_name = pretty_mode-user.
      ELSEIF pretty_name EQ pretty_mode-low_case.
        mv_pretty_name = pretty_mode-user_low_case.
      ENDIF.
    ENDIF.

    rtti ?= cl_abap_classdescr=>describe_by_object_ref( me ).
    IF rtti->absolute_name NE mc_me_type.
      mv_extended = c_bool-true.
    ENDIF.

  ENDMETHOD.                    "constructor


  METHOD class_constructor.

    DATA: lo_bool_type_descr    TYPE REF TO cl_abap_typedescr,
          lo_tribool_type_descr TYPE REF TO cl_abap_typedescr,
          lo_json_type_descr    TYPE REF TO cl_abap_typedescr,
          lv_pos                LIKE sy-fdpos,
          lv_json_string        TYPE json.

    lo_bool_type_descr    = cl_abap_typedescr=>describe_by_data( c_bool-true ).
    lo_tribool_type_descr = cl_abap_typedescr=>describe_by_data( c_tribool-true ).
    lo_json_type_descr    = cl_abap_typedescr=>describe_by_data( lv_json_string ).

    CONCATENATE mc_bool_types lo_bool_type_descr->absolute_name lo_tribool_type_descr->absolute_name INTO mc_bool_types.
    CONCATENATE mc_bool_3state lo_tribool_type_descr->absolute_name INTO mc_bool_3state.
    CONCATENATE mc_json_type lo_json_type_descr->absolute_name INTO mc_json_type.

    FIND FIRST OCCURRENCE OF `\TYPE=` IN lo_json_type_descr->absolute_name MATCH OFFSET lv_pos.
    IF sy-subrc IS INITIAL.
      mc_me_type = lo_json_type_descr->absolute_name(lv_pos).
    ENDIF.

    sv_white_space = cl_abap_char_utilities=>get_simple_spaces_for_cur_cp( ).

    mc_cov_error = cl_abap_conv_in_ce=>uccp( '0000' ).

  ENDMETHOD.                    "class_constructor


  METHOD bool_to_tribool.
    IF iv_bool EQ c_bool-true.
      rv_tribool = c_tribool-true.
    ELSEIF iv_bool EQ abap_undefined. " fall back for abap _bool
      rv_tribool = c_tribool-undefined.
    ELSE.
      rv_tribool = c_tribool-false.
    ENDIF.
  ENDMETHOD.                    "bool_to_tribool
ENDCLASS.                    "LCL_UI2_JSON IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_progress_bar IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_progress_bar IMPLEMENTATION.

  METHOD constructor.
    me->count = i_count.
    me->base_desc = i_base_desc.
  ENDMETHOD.

  METHOD add.

    me->curr = me->curr + i_add.

    me->percent = me->curr / me->count * 100.

    me->display( i_desc ).

  ENDMETHOD.

  METHOD display.
    DATA: lv_text TYPE string,
          lv_num1 TYPE string,
          lv_num2 TYPE string.
    lv_num1 = me->curr.
    lv_num2 = me->count.

    CONDENSE lv_num1 NO-GAPS.
    CONDENSE lv_num2 NO-GAPS.

    CONCATENATE '[' lv_num1 '/' lv_num2 '] ' me->base_desc INTO lv_text.

    REPLACE FIRST OCCURRENCE OF '&' IN lv_text WITH desc.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = me->percent
        text       = lv_text.
  ENDMETHOD.
ENDCLASS.