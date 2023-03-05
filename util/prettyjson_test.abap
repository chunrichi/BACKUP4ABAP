*&---------------------------------------------------------------------*
*& Report prettyjson_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT prettyjson_test.

" 参考 https://juejin.cn/post/6844904084852457486

CLASS lcl_pretty_json DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS: pretty IMPORTING json               TYPE string
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

    DATA: ls_result TYPE match_result.

    FIND ALL OCCURRENCES OF REGEX `\{|\}|,|:` IN json
      RESULTS DATA(results).

    IF results IS INITIAL.
      pretty_json = json.
      RETURN.
    ELSE.

      LOOP AT results INTO ls_result.
        l_alen = ls_result-offset + ls_result-length - lv_loff.

        l_exec = json+ls_result-offset(ls_result-length).

        l_indx = ls_result-offset + ls_result-length.

        " 匹配开头到当前字符串之间的字符串
        l_ftc = json+0(l_indx).

        REPLACE ALL OCCURRENCES OF REGEX `\\"` IN l_ftc WITH ``.
        REPLACE ALL OCCURRENCES OF REGEX `[^"]` IN l_ftc WITH ``.
        IF strlen( l_ftc ) MOD 2 <> 0.

          IF l_exec+0(1) = `{`.
            invalidfs += 1.
          ELSEIF l_exec+0(1) = `}`.
            invalidbs += 1.
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

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: lv_json TYPE string.

*lv_json = `{"rollname":"ZZED_AGENTID","domname":"TEXT12","as4user":"22443","as4date":` &&
*             `"2022-10-29","as4time":"13:52:41","datatype":"CHAR","leng":12,"decimals":0,` &&
*             `"outputlen":12,"lowercase":"X","convexit":"","entitytab":"","refkind":"D",` &&
*             `"ddtext":"AgentId","reptext":"AgentId","scrtextS":"AgentId","scrtextM":"AgentId","scrtextL":"AgentId"}`.

*  lv_json = `{ "as4time":"13:52:41" }`.

  lv_json = `{"uu{{{{{{::}},}uc\\\"u":"kkkk","oooooo":{},"pp2":{"662":{"****":9},"k":88888},"c___9":true,"*":3}`.

  DATA: lv_rjson TYPE string.

  lv_rjson = lcl_pretty_json=>pretty( lv_json ).

  cl_demo_output=>display( lv_rjson ).