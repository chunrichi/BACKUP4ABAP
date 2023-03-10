*&---------------------------------------------------------------------*
*& Report prettyjson_speed_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT prettyjson_speed_test.

" 性能优化

" 在 ecc 中出现运行速度较慢的情况，考虑优化性能
" 01 prettyn 通过减少每次正则匹配内容的长度


CLASS lcl_pretty_json DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: pretty IMPORTING json               TYPE string
                          RETURNING VALUE(pretty_json) TYPE string.

    CLASS-METHODS: prettyn IMPORTING json               TYPE string
                           RETURNING VALUE(pretty_json) TYPE string.
ENDCLASS.

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

        l_exec = json+ls_result-offset(ls_result-length).

        l_indx = ls_result-offset + ls_result-length.

        " 匹配开头到当前字符串之间的字符串
        l_ftc = json+0(l_indx).

        REPLACE ALL OCCURRENCES OF REGEX `\\"` IN l_ftc WITH ``.
        REPLACE ALL OCCURRENCES OF REGEX `[^"]` IN l_ftc WITH ``.

        lv_ftc = strlen( l_ftc ) MOD 2.

        IF lv_ftc <> 0.

          IF l_exec+0(1) = `{`.
            invalidfs = invalidfs + 1.
          ELSEIF l_exec+0(1) = `}`.
            invalidbs = invalidbs + 1.
          ENDIF.

          CONTINUE.
        ENDIF.

        " 匹配开头到当前字符串之间的字符串
        l_ftc = json+0(l_indx).

        REPLACE ALL OCCURRENCES OF REGEX `[^{]` IN l_ftc WITH ``.
        keytimesf = strlen( l_ftc ) - invalidfs.

        " 匹配开头到当前字符串之间的字符串
        l_ftc = json+0(l_indx).

        REPLACE ALL OCCURRENCES OF REGEX `[^}]` IN l_ftc WITH ``.
        keytimesb = strlen( l_ftc ) - invalidbs.

        indentationtimes = keytimesf - keytimesb.

        IF l_exec+0(1) = '{'.
          CONCATENATE pretty_json json+lv_loff(l_alen) cl_abap_char_utilities=>cr_lf INTO pretty_json RESPECTING BLANKS.
          DO indentationtimes TIMES.
            CONCATENATE pretty_json `  ` INTO pretty_json RESPECTING BLANKS.
          ENDDO.
        ELSEIF l_exec+0(1) = '}'.
          l_alen = l_alen - 1.
          CONCATENATE pretty_json json+lv_loff(l_alen) cl_abap_char_utilities=>cr_lf INTO pretty_json RESPECTING BLANKS.
          " CONCATENATE pretty_json json+lv_loff(l_alen) INTO pretty_json RESPECTING BLANKS.
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

      ENDLOOP.
      IF sy-subrc = 0.
*        lv_loff -= 1.
*
*        CONCATENATE pretty_json json+lv_loff INTO pretty_json.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "pretty

  METHOD prettyn.
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
ENDCLASS.

START-OF-SELECTION.
  CONSTANTS: lc_loop_times TYPE i VALUE 1000.

  DATA: lv_used1 TYPE timestampl,
        lv_used2 TYPE timestampl,
        lv_begin TYPE timestampl,
        lv_end   TYPE timestampl.

  DATA: lv_json TYPE string.

  DATA: lv_rjson TYPE string.

  DO lc_loop_times TIMES.

    lv_json = `{"uu{{{{{{::}},}uc\\\"u":"kkkk","oooooo":{},"pp2":{"662":{"****":9},"k":88888},"c___9":true,"*":3}`.

    GET TIME STAMP FIELD lv_begin.

    lv_rjson = lcl_pretty_json=>pretty( lv_json ).

    GET TIME STAMP FIELD lv_end.

    lv_used1 = lv_used1 + cl_abap_tstmp=>subtract( tstmp1 = lv_end tstmp2 = lv_begin ).

  ENDDO.

  cl_demo_output=>write( lv_rjson ).
  cl_demo_output=>write( lv_used1 ).

  DO lc_loop_times TIMES.

    lv_json = `{"uu{{{{{{::}},}uc\\\"u":"kkkk","oooooo":{},"pp2":{"662":{"****":9},"k":88888},"c___9":true,"*":3}`.

    GET TIME STAMP FIELD lv_begin.

    lv_rjson = lcl_pretty_json=>prettyn( lv_json ).

    GET TIME STAMP FIELD lv_end.

    lv_used2 = lv_used2 + cl_abap_tstmp=>subtract( tstmp1 = lv_end tstmp2 = lv_begin ).

  ENDDO.

  cl_demo_output=>write( lv_rjson ).
  cl_demo_output=>display( lv_used2 ).