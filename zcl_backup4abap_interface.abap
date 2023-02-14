CLASS zcl_backup4abap_interface DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_req,
             checkflag TYPE char12, " 检查字段 固定值 BACKUP4ABAP
             deltaflag TYPE char1,  " 增量标识
           END OF ty_req.

    TYPES: BEGIN OF ty_res,
             code    TYPE numc3,  " 返回编码
             message TYPE string, " 返回文本
           END OF ty_res.

    INTERFACES if_http_extension .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_BACKUP4ABAP_INTERFACE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    DATA: lv_req_json TYPE string,
          ls_req      TYPE ty_req.
    DATA: lv_res_json TYPE string,
          ls_res      TYPE ty_res.

    DATA: lv_xlen     TYPE i,
          lv_zip_xstr TYPE xstring.

    DATA: lv_brun TYPE char30.

    CONSTANTS: lv_backup_prog TYPE progname VALUE 'ZBACKUP4ABAP'.

    " 检查请求方式
    DATA(lv_method) = server->request->get_method( ).

    IF lv_method <> 'POST'.
      server->response->set_status( code = 404 reason = `Useless Method` ).
      EXIT.
    ENDIF.

    " 获取请求数据
    lv_req_json = server->request->get_cdata( ).

    " 解析报文
    /ui2/cl_json=>deserialize(
      EXPORTING json = lv_req_json
       CHANGING data = ls_req ).

    IF ls_req-checkflag <> 'BACKUP4ABAP'.
      server->response->set_status( code = 401 reason = `Bad Request` ).
      EXIT.
    ENDIF.

    " 加锁
    CALL FUNCTION 'ENQUEUE_ES_PROG'
      EXPORTING
        mode_trdir     = 'E'
        name           = lv_backup_prog
        x_name         = ' '
        _scope         = '2'
        _wait          = ''
        _collect       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      IF sy-msgid = 'MC' AND sy-msgty = 'E' AND sy-msgno = '601'.
        MESSAGE ID 'ZXX01' TYPE sy-msgty NUMBER 005
          WITH sy-msgv1 lv_backup_prog INTO ls_res-message.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_res-message.
      ENDIF.
      ls_res-code = '500'.
      ls_res-message = `Lock fail ` && ls_res-message.
    ENDIF.

    DATA: lv_delta TYPE abap_bool.

    IF ls_res-code IS INITIAL.
      " 文件名
      GET TIME.
      lv_brun = sy-datum && sy-uzeit.

      IF ls_req-deltaflag = 'X'.
        lv_delta = 'X'.
      ENDIF.

      " 执行程序存储 ZIP
      SUBMIT (lv_backup_prog) WITH p_brun = lv_brun
                              WITH p_delt = lv_delta AND RETURN.

      " 获取 ZIP 数据
      IMPORT xlen = lv_xlen
             zip  = lv_zip_xstr FROM DATABASE demo_indx_blob(zb) ID lv_brun.

      " 删除数据
      DELETE FROM demo_indx_blob WHERE relid = 'ZB'
                                   AND id    = lv_brun.
      COMMIT WORK.

      " 设置返回文件名

      server->response->set_content_type( `application/zip` ).
      server->response->set_header_field( name = `content-disposition`
                                         value = |attachment; filename={ lv_brun }.zip| ). " 文件名
      server->response->set_status( code = 200 reason = 'Ok' ).

      server->response->set_data( data = lv_zip_xstr ).

    ELSE.

      GET REFERENCE OF ls_res INTO DATA(lo_res).

      lv_res_json = /ui2/cl_json=>serialize( data = lo_res
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      " 设置接口成功调用的的状态
      server->response->set_status( code = '200' reason = 'Request Error' ).

      " 设置接口返回的数据
      server->response->set_content_type( `application/json` ).
      server->response->set_cdata( data = lv_res_json ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.