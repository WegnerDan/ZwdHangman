*&---------------------------------------------------------------------*
*& Report  Z_HANGMAN
*&
*&---------------------------------------------------------------------*
*& 20170803 Daniel Wegner
*&
*&---------------------------------------------------------------------*
REPORT z_hangman.
CLASS lcl_dd_document DEFINITION CREATE PUBLIC INHERITING FROM cl_dd_document.
  PUBLIC SECTION.
    CONSTANTS courier TYPE sdydo_attribute VALUE 'COURIER'.
    METHODS add_text_courier IMPORTING iv_text TYPE clike.
  PROTECTED SECTION.
    METHODS convert_attribute REDEFINITION.
ENDCLASS.

* --------------------------------------------------------------------------------------------------
CLASS lcl_hangman DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS: difficulty_easy   TYPE i VALUE 1,
               difficulty_medium TYPE i VALUE 2,
               difficulty_hard   TYPE i VALUE 3,
               length_5to10      TYPE i VALUE 1,
               length_10to15     TYPE i VALUE 2,
               length_15to30     TYPE i VALUE 3.
    METHODS:
      constructor,
      mod_pbo_2000,
      mod_pai_2000.
  PRIVATE SECTION.
    CONSTANTS: state_active TYPE i VALUE 0,
               state_won    TYPE i VALUE 1,
               state_lost   TYPE i VALUE 2.
    DATA: mv_difficulty      TYPE i,
          mv_word_length     TYPE i,
          mo_container1      TYPE REF TO cl_gui_custom_container,
          mo_container2      TYPE REF TO cl_gui_custom_container,
          mo_doc_man         TYPE REF TO lcl_dd_document,
          mo_doc_btn         TYPE REF TO lcl_dd_document,
          mv_word            TYPE string,
          mv_w_len           TYPE i,
          mv_errors          TYPE n LENGTH 1,
          mt_letters         TYPE SORTED TABLE OF c WITH UNIQUE KEY table_line,
          mt_letters_pressed LIKE mt_letters,
          mv_state           TYPE i.
    CLASS-DATA mv_nbsp TYPE c LENGTH 1.
    METHODS:
      generate_letters,
      generate_man,
      generate_word_text,
      adjust_letters_for_difficulty,
      get_random_word RETURNING VALUE(rv_word) TYPE string,
      handle_letter IMPORTING iv_letter TYPE clike,
      letter_is_active IMPORTING iv_letter           TYPE csequence
                       RETURNING VALUE(rv_is_active) TYPE abap_bool,
      letter_is_pressed IMPORTING iv_letter            TYPE any
                        RETURNING VALUE(rv_is_pressed) TYPE abap_bool,
      replace_spaces IMPORTING iv_text        TYPE clike
                     RETURNING VALUE(rv_text) TYPE string,
      save_score,
      show_highscores.
ENDCLASS.

* --------------------------------------------------------------------------------------------------
DATA go_hangman TYPE REF TO lcl_hangman.
DATA gv_ucomm   TYPE sy-ucomm.

* --------------------------------------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE b1_tit.
PARAMETERS: p_dif_e TYPE abap_bool RADIOBUTTON GROUP dif DEFAULT 'X',
            p_dif_m TYPE abap_bool RADIOBUTTON GROUP dif,
            p_dif_h TYPE abap_bool RADIOBUTTON GROUP dif.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE b2_tit.
PARAMETERS: p_5to10  TYPE abap_bool RADIOBUTTON GROUP wlen,
            p_10to15 TYPE abap_bool RADIOBUTTON GROUP wlen DEFAULT 'X',
            p_15to30 TYPE abap_bool RADIOBUTTON GROUP wlen.
SELECTION-SCREEN END OF BLOCK b2.

* --------------------------------------------------------------------------------------------------
INITIALIZATION.
  b1_tit = 'Difficulty'(001).
  b2_tit = 'Word Length'(002).

* --------------------------------------------------------------------------------------------------
START-OF-SELECTION.
  go_hangman = NEW #( ).
  CALL SCREEN 2000.


MODULE status_2000 OUTPUT.
* --------------------------------------------------------------------------------------------------
  go_hangman->mod_pbo_2000( ).

* --------------------------------------------------------------------------------------------------
ENDMODULE.

MODULE user_command_2000 INPUT.
* --------------------------------------------------------------------------------------------------
  go_hangman->mod_pai_2000( ).

* --------------------------------------------------------------------------------------------------
ENDMODULE.

CLASS lcl_dd_document IMPLEMENTATION.

  METHOD convert_attribute.
* --------------------------------------------------------------------------------------------------
    super->convert_attribute( EXPORTING sap_attribute         = sap_attribute
                              IMPORTING html_attribute        = html_attribute
                                        change_foreground_col = change_foreground_col ).

* --------------------------------------------------------------------------------------------------
    IF sap_attribute = lcl_dd_document=>courier.
      html_attribute = ' COURIER'.
    ENDIF.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD add_text_courier.
* --------------------------------------------------------------------------------------------------
    DATA lv_text TYPE sdydo_text_element.

* --------------------------------------------------------------------------------------------------
    lv_text = iv_text.

* --------------------------------------------------------------------------------------------------
    add_text( text          = lv_text
              sap_fontsize  = cl_dd_document=>large
              sap_fontstyle = 'COURIER'             ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.

CLASS lcl_hangman IMPLEMENTATION.

  METHOD constructor.
* --------------------------------------------------------------------------------------------------
    CASE abap_true.
      WHEN p_dif_e.
        mv_difficulty = difficulty_easy.
      WHEN p_dif_m.
        mv_difficulty = difficulty_medium.
      WHEN p_dif_h.
        mv_difficulty = difficulty_hard.
      WHEN OTHERS.
        mv_difficulty = difficulty_easy.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
    CASE abap_true.
      WHEN p_5to10.
        mv_word_length = length_5to10.
      WHEN p_10to15.
        mv_word_length = length_10to15.
      WHEN p_15to30.
        mv_word_length = length_15to30.
      WHEN OTHERS.
        mv_word_length = length_10to15.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
    generate_letters( ).
    mv_word = get_random_word( ).
    IF mv_word IS INITIAL.
      MESSAGE 'Could not find word!' TYPE 'E'.
    ENDIF.
    adjust_letters_for_difficulty( ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD adjust_letters_for_difficulty.
* --------------------------------------------------------------------------------------------------
    DATA:
      lv_nr_remove TYPE i,
      lv_letter    TYPE c LENGTH 1,
      lt_letters   LIKE STANDARD TABLE OF lv_letter WITH DEFAULT KEY,
      lv_index     TYPE sy-index.

* --------------------------------------------------------------------------------------------------
    CASE mv_difficulty.
      WHEN difficulty_easy.
        lv_nr_remove = mv_w_len - 5.
      WHEN difficulty_medium.
        lv_nr_remove = mv_w_len - 10.
      WHEN difficulty_hard.
        lv_nr_remove = mv_w_len - 15.
    ENDCASE.
    CALL FUNCTION 'GENERAL_GET_RANDOM_INT'
      EXPORTING
        range  = lv_nr_remove
      IMPORTING
        random = lv_nr_remove.

* --------------------------------------------------------------------------------------------------
    DO mv_w_len TIMES.
      DATA(lv_offset) = sy-index - 1.
      lv_letter = mv_word+lv_offset(1).
      TRANSLATE lv_letter TO UPPER CASE.
      READ TABLE lt_letters TRANSPORTING NO FIELDS WITH KEY table_line = lv_letter.
      IF sy-subrc <> 0.
        INSERT lv_letter INTO TABLE lt_letters.
      ENDIF.
    ENDDO.

* --------------------------------------------------------------------------------------------------
    DO lv_nr_remove TIMES.
      CALL FUNCTION 'GENERAL_GET_RANDOM_INT'
        EXPORTING
          range  = lines( lt_letters )
        IMPORTING
          random = lv_index.
      READ TABLE lt_letters INTO lv_letter INDEX lv_index.
      IF sy-subrc = 0.
        handle_letter( lv_letter ).
      ENDIF.
    ENDDO.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD generate_letters.
* --------------------------------------------------------------------------------------------------
    FREE mt_letters.
    DO 26 TIMES.
      DATA(lv_offset) = sy-index - 1.
      INSERT sy-abcde+lv_offset(1) INTO TABLE mt_letters.
    ENDDO.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD replace_spaces.
* --------------------------------------------------------------------------------------------------
    CONSTANTS lc_nbsp_hex TYPE syhex02 VALUE '00A0'.
    DATA lv_trl TYPE c LENGTH 2.

* --------------------------------------------------------------------------------------------------
    IF mv_nbsp IS INITIAL.
      mv_nbsp = cl_abap_conv_in_ce=>uccp( lc_nbsp_hex ).
    ENDIF.
    lv_trl+1 = mv_nbsp.

* --------------------------------------------------------------------------------------------------
    rv_text = iv_text.

* --------------------------------------------------------------------------------------------------
    TRANSLATE rv_text USING lv_trl.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD mod_pbo_2000.
* --------------------------------------------------------------------------------------------------
    SET PF-STATUS 'STATUS_2000'.
    SET TITLEBAR 'TITLE_2000'.

* --------------------------------------------------------------------------------------------------
    mo_container1 = NEW #( container_name = 'CONTROL1' ).
    mo_container2 = NEW #( container_name = 'CONTROL2' ).

* --------------------------------------------------------------------------------------------------
    generate_man( ).
    generate_word_text( ).

* --------------------------------------------------------------------------------------------------
    LOOP AT SCREEN.
      IF screen-group1 = 'LTR'.
        CASE mv_state.
          WHEN state_active.
            CASE letter_is_pressed( screen-group2 ).
              WHEN abap_true.
                screen-input = 1.
              WHEN abap_false.
                screen-input = 0.
            ENDCASE.
          WHEN OTHERS.
            screen-input = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

* --------------------------------------------------------------------------------------------------
    CASE mv_state.
      WHEN state_active.
      WHEN state_won.
        MESSAGE 'You did it!' TYPE 'S'.
      WHEN state_lost.
        MESSAGE 'You lost.' TYPE 'S' DISPLAY LIKE 'E'.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD mod_pai_2000.
* --------------------------------------------------------------------------------------------------
    CASE gv_ucomm(7).
      WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
        LEAVE TO SCREEN 0.
      WHEN 'LETTER_'.
        handle_letter( gv_ucomm+7(1) ).
      WHEN 'RESTART'.
*        LEAVE TO SCREEN 0.
        go_hangman = NEW #( ).
        CALL SCREEN 2000.
      WHEN 'SHOWSCO'.
        IF gv_ucomm = 'SHOWSCORE'.
          show_highscores( ).
        ENDIF.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD handle_letter.
* --------------------------------------------------------------------------------------------------
    DATA lv_letter TYPE c LENGTH 1.
    DATA lv_word_up TYPE string.

* --------------------------------------------------------------------------------------------------
    IF mv_state <> state_active.
      RETURN.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    lv_letter = iv_letter.
    lv_word_up = mv_word.
    TRANSLATE: lv_letter TO UPPER CASE, lv_word_up TO UPPER CASE.

* --------------------------------------------------------------------------------------------------
    INSERT lv_letter INTO TABLE mt_letters_pressed.
    IF lv_word_up CA lv_letter.
      DELETE mt_letters WHERE table_line = lv_letter.
    ELSE.
      mv_errors = mv_errors + 1.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    IF mv_errors > 5.
      mv_state = state_lost.
      FREE mt_letters.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    IF mv_state = state_active.
      mv_state = state_won.
      DO mv_w_len TIMES.
        DATA(lv_offset) = sy-index - 1.
        lv_letter = mv_word+lv_offset(1).
        IF letter_is_active( lv_letter ).
          mv_state = state_active.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    CASE mv_state.
      WHEN state_won OR state_lost.
        save_score( ).
      WHEN OTHERS.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD generate_word_text.
* --------------------------------------------------------------------------------------------------
    DATA:
      lv_len    TYPE i,
      lv_letter TYPE c LENGTH 1,
      lv_text   TYPE string.

* --------------------------------------------------------------------------------------------------
    IF mo_doc_btn IS NOT BOUND.
      mo_doc_btn = NEW #( ).
    ENDIF.
    mo_doc_btn->initialize_document( ).

* --------------------------------------------------------------------------------------------------
    mo_doc_btn->new_line( ).
    DO mv_w_len TIMES.
      DATA(lv_offset) = sy-index - 1.
      lv_letter = mv_word+lv_offset(1).
      CASE letter_is_active( lv_letter ).
        WHEN abap_true.
          lv_text = lv_text && ` ` && '_'.
        WHEN abap_false.
          lv_text = lv_text && ` ` && lv_letter.
      ENDCASE.
    ENDDO.
    mo_doc_btn->add_text_courier( lv_text ).

* --------------------------------------------------------------------------------------------------
    mo_doc_btn->display_document( reuse_control = abap_true
                                  parent        = mo_container2 ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD letter_is_active.
* --------------------------------------------------------------------------------------------------
    DATA lv_letter TYPE c LENGTH 1.

* --------------------------------------------------------------------------------------------------
    lv_letter = iv_letter.
    TRANSLATE lv_letter TO UPPER CASE.

* --------------------------------------------------------------------------------------------------
    READ TABLE mt_letters TRANSPORTING NO FIELDS WITH KEY table_line = lv_letter BINARY SEARCH.
    CASE sy-subrc.
      WHEN 0.
        rv_is_active = abap_true.
      WHEN OTHERS.
        rv_is_active = abap_false.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD letter_is_pressed.
* --------------------------------------------------------------------------------------------------
    DATA lv_letter TYPE c LENGTH 1.

* --------------------------------------------------------------------------------------------------
    lv_letter = iv_letter.
    TRANSLATE lv_letter TO UPPER CASE.

* --------------------------------------------------------------------------------------------------
    READ TABLE mt_letters_pressed TRANSPORTING NO FIELDS WITH KEY table_line = lv_letter BINARY SEARCH.
    CASE sy-subrc.
      WHEN 0.
        rv_is_pressed = abap_false.
      WHEN OTHERS.
        rv_is_pressed = abap_true.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD generate_man.
* --------------------------------------------------------------------------------------------------
    CONSTANTS:
      lc_top       TYPE string VALUE `     _________ `,
      lc_rope      TYPE string VALUE `    |         |`,
      lc_head      TYPE string VALUE `    0         |`,
      lc_body      TYPE string VALUE `   /|\        |`,
      lc_legs      TYPE string VALUE `   / \        |`,
      lc_base      TYPE string VALUE `              |`,
      lc_bare_body TYPE string VALUE `    |         |`,
      lc_left_arm  TYPE string VALUE `   /|         |`,
      lc_left_leg  TYPE string VALUE `   /          |`.
    DATA:
      lv_line TYPE string.

* --------------------------------------------------------------------------------------------------
    IF mo_doc_man IS NOT BOUND.
      mo_doc_man = NEW #(  ).
    ENDIF.
    mo_doc_man->initialize_document( ).

* --------------------------------------------------------------------------------------------------
    mo_doc_man->add_text_courier( replace_spaces( lc_top ) ).
    mo_doc_man->new_line( ).
    mo_doc_man->add_text_courier( replace_spaces( lc_rope ) ).
    mo_doc_man->new_line( ).
    CASE mv_errors.
      WHEN 0.
        DO 4 TIMES.
          mo_doc_man->add_text_courier( replace_spaces( lc_base ) ).
          mo_doc_man->new_line( ).
        ENDDO.
      WHEN 1.
        mo_doc_man->add_text_courier( replace_spaces( lc_head ) ).
        mo_doc_man->new_line( ).
        DO 3 TIMES.
          mo_doc_man->add_text_courier( replace_spaces( lc_base ) ).
          mo_doc_man->new_line( ).
        ENDDO.
      WHEN 2.
        mo_doc_man->add_text_courier( replace_spaces( lc_head ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_bare_body ) ).
        mo_doc_man->new_line( ).
        DO 2 TIMES.
          mo_doc_man->add_text_courier( replace_spaces( lc_base ) ).
          mo_doc_man->new_line( ).
        ENDDO.
      WHEN 3.
        mo_doc_man->add_text_courier( replace_spaces( lc_head ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_left_arm ) ).
        mo_doc_man->new_line( ).
        DO 2 TIMES.
          mo_doc_man->add_text_courier( replace_spaces( lc_base ) ).
          mo_doc_man->new_line( ).
        ENDDO.
      WHEN 4.
        mo_doc_man->add_text_courier( replace_spaces( lc_head ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_body ) ).
        mo_doc_man->new_line( ).
        DO 2 TIMES.
          mo_doc_man->add_text_courier( replace_spaces( lc_base ) ).
          mo_doc_man->new_line( ).
        ENDDO.
      WHEN 5.
        mo_doc_man->add_text_courier( replace_spaces( lc_head ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_body ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_left_leg ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_base ) ).
        mo_doc_man->new_line( ).
      WHEN OTHERS.
        mo_doc_man->add_text_courier( replace_spaces( lc_head ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_body ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_legs ) ).
        mo_doc_man->new_line( ).
        mo_doc_man->add_text_courier( replace_spaces( lc_base ) ).
        mo_doc_man->new_line( ).
    ENDCASE.
    mo_doc_man->add_text_courier( replace_spaces( lc_base ) ).
    mo_doc_man->new_line( ).

* --------------------------------------------------------------------------------------------------
    mo_doc_man->display_document( reuse_control = abap_true
                                  parent        = mo_container1 ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_random_word.
* --------------------------------------------------------------------------------------------------
    DATA:
      ls_dd04t   TYPE dd04t,
      lt_dd04t   TYPE STANDARD TABLE OF dd04t WITH DEFAULT KEY,
      lv_tabsize TYPE i,
      lv_tabpos  TYPE i,
      lv_word_up TYPE string,
      lv_word    TYPE c LENGTH 200,
      lv_min_len TYPE i,
      lv_max_len TYPE i.

* --------------------------------------------------------------------------------------------------
    CASE mv_word_length.
      WHEN length_5to10.
        lv_min_len = 5.
        lv_max_len = 10.
      WHEN length_10to15.
        lv_min_len = 10.
        lv_max_len = 15.
      WHEN length_15to30.
        lv_min_len = 15.
        lv_max_len = 30.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Finding random word...'(003).

* --------------------------------------------------------------------------------------------------
    SELECT COUNT(*) FROM dd04t INTO lv_tabsize
    WHERE ddlanguage = sy-langu
    AND   as4local   = 'A'
    AND   as4vers    = space.

* --------------------------------------------------------------------------------------------------
    lv_tabsize = lv_tabsize / 4.

* --------------------------------------------------------------------------------------------------
    DO 4 TIMES.
      lv_tabsize = lv_tabsize * sy-index.
      CALL FUNCTION 'GENERAL_GET_RANDOM_INT'
        EXPORTING
          range  = lv_tabsize
        IMPORTING
          random = lv_tabpos.

* --------------------------------------------------------------------------------------------------
      FREE lt_dd04t.
      SELECT * FROM dd04t INTO ls_dd04t UP TO lv_tabpos ROWS
      WHERE ddlanguage = sy-langu
      AND   as4local   = 'A'
      AND   as4vers    = space.
        " get 20 last entries of select result
        IF sy-dbcnt > ( lv_tabpos - 20 ).
          APPEND ls_dd04t TO lt_dd04t.
        ENDIF.
      ENDSELECT.

* --------------------------------------------------------------------------------------------------
      LOOP AT lt_dd04t INTO ls_dd04t.
        FIELD-SYMBOLS <lv_text> TYPE clike.
        DO 3 TIMES.
          CASE sy-index.
            WHEN 1.
              ASSIGN ls_dd04t-ddtext TO <lv_text>.
            WHEN 2.
              ASSIGN ls_dd04t-reptext TO <lv_text>.
            WHEN 3.
              ASSIGN ls_dd04t-scrtext_l TO <lv_text>.
          ENDCASE.
          SPLIT <lv_text> AT space INTO TABLE DATA(lt_words).
          TRY.
              lv_word_up = rv_word = lt_words[ lines( lt_words ) ].
              TRANSLATE lv_word_up TO UPPER CASE.
              mv_w_len = strlen( rv_word ).
              IF mv_w_len <  lv_min_len
              OR mv_w_len >= lv_max_len
              OR lv_word_up CN sy-abcde. " filter any non alphabetic words
                FREE rv_word.
                CONTINUE.
              ENDIF.
            CATCH cx_sy_itab_line_not_found.
              FREE rv_word.
              CONTINUE.
          ENDTRY.
          IF rv_word IS NOT INITIAL. EXIT. ENDIF.
        ENDDO.
        IF rv_word IS NOT INITIAL. EXIT. ENDIF.
      ENDLOOP.
      IF rv_word IS NOT INITIAL. EXIT. ENDIF.
    ENDDO.

* --------------------------------------------------------------------------------------------------
    lv_word = rv_word.
    TRANSLATE lv_word TO LOWER CASE.
    TRANSLATE lv_word(1) TO UPPER CASE.
    rv_word = lv_word.

* --------------------------------------------------------------------------------------------------
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.


  METHOD save_score.
* --------------------------------------------------------------------------------------------------
    DATA:
      ls_score TYPE zwd_hangm_score.

* --------------------------------------------------------------------------------------------------
    SELECT SINGLE * FROM zwd_hangm_score INTO ls_score WHERE username = sy-uname.
    CASE sy-subrc.
      WHEN 0.
        DELETE FROM zwd_hangm_score WHERE username = sy-uname.
      WHEN OTHERS.
        ls_score-username = sy-uname.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
    CASE mv_state.
      WHEN state_won.
        ls_score-wins   = ls_score-wins   + 1.
      WHEN state_lost.
        ls_score-losses = ls_score-losses + 1.
      WHEN OTHERS.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
    INSERT INTO zwd_hangm_score VALUES ls_score.

* --------------------------------------------------------------------------------------------------
    COMMIT WORK.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD show_highscores.
* --------------------------------------------------------------------------------------------------
    TYPES:
      BEGIN OF lty_data.
            INCLUDE TYPE zwd_hangm_score.
    TYPES:
      ratio TYPE p LENGTH 3 DECIMALS 2,
      END OF lty_data,
      lty_t_data TYPE STANDARD TABLE OF lty_data WITH DEFAULT KEY.
    DATA:
      lt_scores TYPE lty_t_data,
      lo_alv    TYPE REF TO cl_salv_table.

* --------------------------------------------------------------------------------------------------
    SELECT *
    FROM zwd_hangm_score
    INTO CORRESPONDING FIELDS OF TABLE lt_scores.

* --------------------------------------------------------------------------------------------------
    LOOP AT lt_scores ASSIGNING FIELD-SYMBOL(<ls_score>).
      <ls_score>-ratio = <ls_score>-wins / ( <ls_score>-wins + <ls_score>-losses ).
    ENDLOOP.
    SORT lt_scores BY ratio DESCENDING.

* --------------------------------------------------------------------------------------------------
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = lt_scores ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

* --------------------------------------------------------------------------------------------------
    lo_alv->display( ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
