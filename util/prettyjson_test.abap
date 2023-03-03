*&---------------------------------------------------------------------*
*& Report prettyjson_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT prettyjson_test.

DATA: json TYPE string.

*json = `{"rollname":"ZZED_AGENTID","domname":"TEXT12","as4user":"22443","as4date":` &&
*             `"2022-10-29","as4time":"13:52:41","datatype":"CHAR","leng":12,"decimals":0,` &&
*             `"outputlen":12,"lowercase":"X","convexit":"","entitytab":"","refkind":"D",` &&
*             `"ddtext":"AgentId","reptext":"AgentId","scrtextS":"AgentId","scrtextM":"AgentId","scrtextL":"AgentId"}`.

json = `{ "as4time":"13:52:41" }`.

DATA: rjson TYPE string.

" 参考 https://juejin.cn/post/6844904084852457486

DATA(invalidfs) = 0.
DATA(invalidbs) = 0.

DATA: lv_loff TYPE i, " 上次开始位置
      lv_alen TYPE i. " 匹配开始位置


" TABLE OF MATCH_RESULT

FIND ALL OCCURRENCES OF REGEX `\{|\}|,|:` IN json
  RESULTS DATA(results).


LOOP AT results INTO DATA(ls_result).
  DATA(l_off) = ls_result-offset.
  DATA(l_len) = ls_result-length.

  DATA(lv_exec) = json+l_off(l_len).

  DATA(l_indx) = l_off + l_len.

  " 匹配开头到当前字符串之间的字符串
  DATA(ftc) = json+0(l_indx).

  "
*  REPLACE ALL OCCURRENCES OF REGEX `"` IN ftc WITH ``.
  REPLACE ALL OCCURRENCES OF REGEX `[^"]` IN ftc WITH ``.
  IF strlen( ftc ) MOD 2 <> 0.

    IF lv_exec+0(1) = `{`.
      invalidfs += 1.
    ELSEIF lv_exec+0(1) = `}`.
      invalidbs += 1.
    ELSE.
      CONTINUE.
    ENDIF.

  ENDIF.

  " 计算缩进
  DATA(keytimesf) = 0.
  DATA(keytimesb) = 0.

  REPLACE ALL OCCURRENCES OF REGEX `[^{]` IN ftc WITH ``.
  keytimesf = strlen( ftc ) - invalidfs.

  REPLACE ALL OCCURRENCES OF REGEX `[^}]` IN ftc WITH ``.
  keytimesb = strlen( ftc ) - invalidbs.

  DATA(indentationtimes) = keytimesf - keytimesb.

  DATA(l_alen) = l_off + l_len - lv_loff.

  DATA: lv_json TYPE string.

  IF lv_exec+0(1) = '{'.
*    l_alen += 1.
    rjson &&= json+lv_loff(l_alen) && cl_abap_char_utilities=>cr_lf.
    DO indentationtimes TIMES.
      CONCATENATE rjson `  ` INTO rjson RESPECTING BLANKS.
    ENDDO.
    "lv_json &&= json+l_alen.
  ELSEIF lv_exec+0(1) = '}'.
    l_alen -= 1.
    rjson &&= json+lv_loff(l_alen) && cl_abap_char_utilities=>cr_lf.
    DO indentationtimes TIMES.
      CONCATENATE rjson `  ` INTO rjson RESPECTING BLANKS.
    ENDDO.
    "lv_json &&= json+l_alen.
  ELSEIF lv_exec+0(1) = ','.
*    l_alen += 1.
    rjson &&= json+lv_loff(l_alen) && cl_abap_char_utilities=>cr_lf.
    DO indentationtimes TIMES.
      CONCATENATE rjson `  ` INTO rjson RESPECTING BLANKS.
    ENDDO.
    "lv_json &&= json+l_alen.
  ELSEIF lv_exec+0(1) = ':'.
    rjson &&= json+lv_loff(l_alen) && ` `." && json+l_alen.
  ENDIF.

  lv_loff = l_off + l_len.
ENDLOOP.
IF sy-subrc = 0.
  lv_loff -= 1.
  rjson &&= json+lv_loff.
ENDIF.


cl_demo_output=>display( rjson ).