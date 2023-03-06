*&---------------------------------------------------------------------*
*& Report progressbar_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT progressbar_test.

CLASS lcl_progress_bar DEFINITION.
  PUBLIC SECTION.
    " 公有 便于多进度条调整
    DATA: count     TYPE i,
          base_desc TYPE string,
          curr      TYPE i.

    METHODS constructor IMPORTING i_count     TYPE i
                                  i_base_desc TYPE string.

    METHODS add IMPORTING i_add  TYPE i DEFAULT 1
                          i_desc TYPE string OPTIONAL.
  PRIVATE SECTION.
    DATA: percent TYPE p DECIMALS 2 LENGTH 5.
    METHODS display IMPORTING desc TYPE string.
ENDCLASS.

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
    DATA: lv_text TYPE string.

    lv_text = me->base_desc.

    REPLACE FIRST OCCURRENCE OF '&' IN lv_text WITH desc.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = me->percent
        text       = lv_text.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: pb TYPE REF TO lcl_progress_bar.

  CREATE OBJECT pb
    EXPORTING
      i_count     = 100
      i_base_desc = '测试 &'.

  DO 5 TIMES.
    WAIT UP TO '0.5' SECONDS.

    CALL METHOD pb->add
      EXPORTING
        i_add  = 20
        i_desc = 'abc'.
  ENDDO.