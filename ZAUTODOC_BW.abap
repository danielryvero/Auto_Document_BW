*&---------------------------------------------------------------------*
*& Report ZAUTODOC_BW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& What it does:
*  This program lets you choose from the presented Objects in the POPUP window
*  by entering one of the letters referring to the InfoObjects and will provide
*  you with a .CSV file containing the information for analyzing them.
******************
*& How to use it:
* 1.  Go to line 343 and 344 and follow instructions of step 2.
* 2.  For this program to work on one Object at a time, line 343, that contains
*     "nopopup = 'X'." must be commented and line 344, that states "nopopup = ''."
*     must be uncommented. So change that accordingly.
* 3.  Go to line 337 and set variable LV_ROOT to your system's path for
*     output files (E.g. LV_ROOT = '\\WINSDGBPC\sapmnt\trans\'.)
* 4.  Go to line 339 and 340 and set variables LANGUAGE to your default language wich is
*     normally English (E.g. LANGUAGE = 'E'.) and SECOND_LANG with your country's language.
* 5.  Execute this program (F8) and follow POPUP instructions to select Object to document
* 6.  Do /OAL11 and check output files, they are named:
*
*     InfoSets_with_IOs.csv
*     InfoSource_with_IOBs.csv
*     List_Of_Active_ADSO_With_IOBs.csv
*     List_Of_Active_ADSOs.csv
*     List_Of_Active_Cubes.csv
*     List_Of_Cubes_With_IOBs.csv
*     List_Of_Active_DTPs.csv
*     List_Of_Active_DataSources.csv
*     List_Of_Active_InfoObjects.csv
*     List_Of_Active_InfoSources.csv
*     List_Of_Active_Transformations.csv
*     List_Of_Transformation_Code.csv
*     Queries_1_List_Of_All_Queries.csv
*     Queries_2_First_Lvl_Elem.csv
*     Queries_3_All_Elem_By_Query.csv
*     Queries_4_Detail_Of_Elem_By_ID.csv
*     Queries_5_Restricted_Key_Fig.csv
*     Queries_6_Calculated_Key_Fig.csv
*************************************
*& Note that this program can be launched completely, inlcuding all Object
*  instances, by going to program ZTEST_OBJS_X, reading use instructions
*  and executing it accordingly.
************************************************************************
*&---------------------------------------------------------------------*
REPORT ZAUTODOC_BW.

PARAMETERS: obj_type(1).

TYPES:
       BEGIN OF ty_export_rszcalc,
         elementid              TYPE rszcalc-eltuid,
         formula_stage          TYPE rszcalc-stepnr,
         form_operator          TYPE rszcalc-opera,
         type_operand           TYPE rszcalc-o1flg,
         desc_type_operand      TYPE string,
         form_operand           TYPE rszcalc-oper1,
         type_operand_2         TYPE rszcalc-o2flg,
         desc_type_operand_2    TYPE string,
         form_operand_2         TYPE rszcalc-oper2,
         calculate              TYPE rszcalc-callate,
         simple_formula         TYPE rszcalc-simpl,
         non_comulative_flag    TYPE rszcalc-ncumfl,
         desc_non_cum_flag      TYPE string,
         aggregation            TYPE rszcalc-aggrgen,
         desc_aggregation       TYPE string,
         exception_aggregation  TYPE rszcalc-aggrexc,
         desc_exc_aggregation   TYPE string,
         ref_chrctic_exc_agg    TYPE rszcalc-aggrcha,
         ref_chrctic_exc_agg_2  TYPE rszcalc-aggrcha2,
         ref_chrctic_exc_agg_3  TYPE rszcalc-aggrcha3,
         ref_chrctic_exc_agg_4  TYPE rszcalc-aggrcha4,
         ref_chrctic_exc_agg_5  TYPE rszcalc-aggrcha5,
         txtlg                  TYPE rszelttxt-txtlg,
       END OF ty_export_rszcalc,

       BEGIN OF ty_export_rszrange,
         elementid              TYPE rszrange-eltuid,
         info_object            TYPE rszrange-iobjnm,
         info_object_detail     TYPE rszrange-faciobjnm,
         base_info_object       TYPE rszrange-attrinm,
         sequence_num           TYPE rszrange-enum,
         purpose_selection      TYPE rszrange-sotp,
         desc_purpose           TYPE string,
         factor_type            TYPE rszrange-factortype,
         desc_factor_type       TYPE string,
         selection_type         TYPE rszrange-seltp,
         desc_selection_type    TYPE string,
         select_sign            TYPE rszrange-sign,
         desc_select_sign       TYPE string,
         operator               TYPE rszrange-opt,
         desc_operator          TYPE string,
         lower_value            TYPE rszrange-low,
         higher_value           TYPE rszrange-high,
         shift_low              TYPE rszrange-shiftlow,
         lower_flag             TYPE rszrange-lowflag,
         desc_lower_flag        TYPE string,
         shift_high             TYPE rszrange-shifthigh,
         higher_flag            TYPE rszrange-highflag,
         desc_higher_flag       TYPE string,
         hierarchy_name         TYPE rszrange-hienm,
         alert_level            TYPE rszrange-alertlevel,
         desc_alert_level       TYPE string,
         txtlg                  TYPE rszelttxt-txtlg,
       END OF ty_export_rszrange,

       BEGIN OF ty_export_rszeltdir,
         elementid              TYPE rszeltdir-eltuid,
         deftp                  TYPE rszeltdir-deftp,
         desc_deftp             TYPE string,
         subdeftp               TYPE rszeltdir-subdeftp,
         desc_subdeftp          TYPE string,
         technical_name         TYPE rszeltdir-mapname,
         additional_info        TYPE rszeltdir-defaulthint,
         info_flag              TYPE rszeltdir-defaulthintflag,
         desc_flag              TYPE string,
         txtlg                  TYPE rszelttxt-txtlg,
       END OF ty_export_rszeltdir,

       BEGIN OF ty_export,
         elementid              TYPE rszeltxref-seltuid,
         teltuid                TYPE rszeltxref-teltuid,
         objvers                TYPE rszeltxref-objvers,
         laytp                  TYPE rszeltxref-laytp,
         laytp_desc             TYPE string,
         posn                   TYPE rszeltxref-posn,
         infocube               TYPE rszeltxref-infocube,
         technical_name         TYPE rszeltdir-mapname,
         optional               TYPE rszeltxref-optional,
         flat_posn              TYPE rszeltxref-flat_posn,
         txtlg                  TYPE rszelttxt-txtlg,
       END OF ty_export,

       BEGIN OF ty_query_main,
         compuid                TYPE rsrrepdir-compuid,
         infocube               TYPE rsrrepdir-infocube,
         mapname                TYPE rszeltdir-mapname,
         txtlg                  TYPE rszelttxt-txtlg,
       END OF ty_query_main,
************Queries until here

       BEGIN OF ty_adso,
          adso_name TYPE rsdodso-ODSOBJECT,
          adso_text TYPE rsdodsot-TXTLG,
          char_name TYPE RSDIOBJFIELDNM,
          char_text TYPE rstxtlg,
          key_field TYPE string,
       END OF ty_adso,

       BEGIN OF ty_cube,
          cube_name TYPE RSDCUBE-INFOCUBE,
          infoarea  TYPE RSDCUBE-INFOAREA,
          cube_text TYPE rstxtlg,
          char_name TYPE RSDIOBJFIELDNM,
          char_text TYPE rstxtlg,
          txtlg     TYPE RSDCUBET-TXTLG,
       END OF ty_cube,

       BEGIN OF ty_isource,
          isource_name TYPE RSIST-isource,
          isource_text TYPE RSIST-txtlg,
          char_name TYPE RSDIOBJFIELDNM,
          char_text TYPE rstxtlg,
       END OF ty_isource,

       BEGIN OF ty_infoset,
         infoset_name TYPE RSQINFOSET,
         infoset_text TYPE rstxtlg,
         char_name TYPE RSDIOBJFIELDNM,
         char_text TYPE rstxtlg,
       END OF ty_infoset,

       BEGIN OF ty_query,
         q_info TYPE RSINFOCUBE,
         q_name TYPE RSZCOMPID,
         q_text TYPE rstxtlg,
       END OF ty_query,

       BEGIN OF ty_CodeTransformationField,
         trans_id TYPE RSTRANID,
         code_id  TYPE RSCODEID,
         line_no(6),
         line     TYPE EDPLINE,
       END OF   ty_CodeTransformationField.

*Variables
DATA:
      "Queries
      lt_texts_rszcalc        TYPE TABLE OF rszcalc,
      wa_texts_rszcalc        TYPE rszcalc,
      lt_export_rszcalc       TYPE TABLE OF ty_export_rszcalc,
      wa_export_rszcalc       TYPE ty_export_rszcalc,
      lt_valid_ids_rszcalc    TYPE TABLE OF rszcalc,
      wa_valid_rszcalc        TYPE rszcalc,
      lt_texts_rszrange       TYPE TABLE OF rszrange,
      wa_texts_rszrange       TYPE rszrange,
      lt_export_rszrange      TYPE TABLE OF ty_export_rszrange,
      wa_export_rszrange      TYPE ty_export_rszrange,
      lt_valid_ids_rszrange   TYPE TABLE OF rszrange,
      wa_valid_rszrange       TYPE rszrange,
      lt_texts_rszelttxt      TYPE TABLE OF rszelttxt,
      wa_texts_rszelttxt      TYPE rszelttxt,
      lt_texts_rszeltdir      TYPE TABLE OF rszeltdir,
      wa_texts_rszeltdir      TYPE rszeltdir,
      lt_export_rszeltdir     TYPE TABLE OF ty_export_rszeltdir,
      wa_export_rszeltdir     TYPE ty_export_rszeltdir,
      lt_export_final         TYPE TABLE OF ty_export,
      wa_export_final         TYPE ty_export,
      lt_children             TYPE TABLE OF rszeltxref,
      wa_child                TYPE rszeltxref,
      lt_query_main           TYPE TABLE OF ty_query_main,
      wa_query_main           TYPE ty_query_main,
      lt_query_elements       TYPE TABLE OF rszeltxref,
      wa_query_elements       TYPE rszeltxref,
      lt_query_elements_ext   TYPE TABLE OF ty_export,
      wa_query_elements_ext   TYPE ty_export,
      lt_current_level        TYPE TABLE OF ty_export,
      lt_next_level           TYPE TABLE OF ty_export,
      lt_all_processed        TYPE TABLE OF ty_export,
      lt_processed_check      TYPE TABLE OF ty_export,
      wa_new_link             TYPE ty_export,
      wa_process              TYPE ty_export,
      lv_posn_text            TYPE char10,
      lv_flat_posn_text       TYPE char10,
      lv_lines_before         TYPE i,
      lv_lines_after          TYPE i,
      lv_new_records          TYPE i,
      lv_level_counter        TYPE i VALUE 1,
      "DS
      lt_rsds  TYPE TABLE OF RSDS,
      wa_rsds  TYPE RSDS,
      lt_rsds_text  TYPE TABLE OF RSDST,
      ls_rsds_text TYPE RSDSt,
      "ADSOs
      lt_adsos_raw TYPE TABLE OF rsdodso,
      wa_adsos_raw TYPE rsdodso,
      lt_txtlg TYPE TABLE OF RSDODSOT,
      ls_txtlg TYPE RSDODSOT,
      lt_adsos TYPE TABLE OF ty_adso,
      wa_adsos  TYPE ty_adso,
      lt_adsos2 TYPE TABLE OF ty_adso,
      ls_adso2  TYPE ty_adso,
      lt_characters2 TYPE TABLE OF RSDODSOIOBJ,
      ls_characters2 TYPE RSDODSOIOBJ,
      "CUBE
      lt_rsdcube    TYPE TABLE OF RSDCUBE,
      wa_rsdcube    TYPE RSDCUBE,
      lt_export_rsdcube   TYPE TABLE OF ty_cube,
      wa_export_rsdcube   TYPE ty_cube,
      lt_txtcube    TYPE TABLE OF RSDCUBET,
      ls_txtcube    TYPE RSDCUBET,
      lt_cube TYPE TABLE OF ty_cube,
      wa_cube  TYPE ty_cube,
      lt_characters TYPE TABLE OF rsdcubeiobj,
      ls_characters TYPE rsdcubeiobj,
      lt_txt_rsdiobjt TYPE TABLE OF RSDIOBJT,
      wa_txt_rsdiobjt TYPE RSDIOBJT,
      "Transformations
      lt_transformations TYPE TABLE OF rstran,
      wa_transformation  TYPE rstran,
      lt_rstranrule  TYPE TABLE OF RSTRANRULE,
      wa_rstranrule  TYPE RSTRANRULE,
      lt_CodeTransformationField TYPE TABLE OF ty_CodeTransformationField,
      ls_CodeTransformationField TYPE ty_CodeTransformationField,
      lt_rstransteprout TYPE TABLE OF  RSTRANSTEPROUT,
      ls_rstransteprout TYPE RSTRANSTEPROUT,
      lt_rsaabap  TYPE TABLE OF RSAABAP,
      ls_rsaabap  TYPE RSAABAP,
      "InfoObjects
      lt_infoobj  TYPE TABLE OF RSDIOBJ,
      wa_infoobj  TYPE RSDIOBJ,
      lt_txtiob   TYPE TABLE OF RSDIOBJT,
      ls_txtiob   TYPE RSDIOBJT,
      lv_path TYPE  string,
      lv_data  TYPE string,
      lv_string TYPE string,
      "InfoSource
      lt_infosource_raw TYPE TABLE OF RSIS,
      wa_infosource_raw TYPE RSIS,
      lt_infosource TYPE TABLE OF ty_isource,
      wa_infosource TYPE ty_isource,
      lt_isource TYPE TABLE OF ty_isource,
      ls_isource TYPE ty_isource,
      lt_istxt   TYPE TABLE OF RSIST,
      ls_istxt   TYPE RSIST,
      lt_characters3 TYPE TABLE OF RSISFIELD,
      ls_characters3 TYPE RSISFIELD,
      "InfoSet
      lt_infoset1 TYPE TABLE OF RSQISET,
      wa_infoset1 TYPE RSQISET,
      lt_infoset  TYPE TABLE OF ty_infoset,
      ls_infoset  TYPE ty_infoset,
      lt_infoset_t TYPE TABLE OF RSQFOBJT,
      ls_infoset_t TYPE RSQFOBJT,
      lt_cha_infoset TYPE TABLE OF RSQFOBJ,
      ls_cha_infoset TYPE RSQFOBJ,
      "Query
      lt_query1 TYPE TABLE OF RSRREPDIR,
      wa_query  TYPE RSRREPDIR,
      lt_query  TYPE TABLE OF ty_query,
      ls_query  TYPE ty_query,
      lt_query_t TYPE TABLE OF RSZELTTXT,
      ls_query_t TYPE RSZELTTXT,
      lt_query_g TYPE TABLE OF RSZELTXREF,
      wa_query_g  TYPE RSZELTXREF,
      lt_query_dir2 TYPE TABLE OF RSZELTDIR,
      wa_query_dir2  TYPE RSZELTDIR,
      lt_query_ran TYPE TABLE OF RSZRANGE,
      wa_query_ran  TYPE RSZRANGE,
      lt_query_cal TYPE TABLE OF RSZCALC,
      wa_query_cal  TYPE RSZCALC,
      lt_query_prop TYPE TABLE OF RSZELTPROP,
      wa_query_prop  TYPE RSZELTPROP,
      "DTP
      lt_dtp TYPE TABLE OF RSBKDTP,
      wa_dtp TYPE RSBKDTP,
      lt_dtp_t TYPE TABLE OF RSBKDTPT,
      ls_dtp_t TYPE RSBKDTPT,
      "Others
      lv_text TYPE rstxtlg,
      lv_char_text TYPE rstxtlg,
      lv_count  TYPE i,
      lv_file_name  TYPE string,
      lv_root  TYPE string,
      nopopup  TYPE C LENGTH 1,
      LANGUAGE TYPE C LENGTH 1,
      SECOND_LANG TYPE C LENGTH 1.

INITIALIZATION.

IF sy-batch = 'X'.
  nopopup = 'X'.
ENDIF.

***** GLOBAL PARAMETERS *****
LV_ROOT = '\\WINSDGBPC\sapmnt\trans\'.

LANGUAGE = 'E'.
SECOND_LANG = 'S'.

*" Select and uncomment one of the options for NOPOPUP to be active
nopopup = 'X'.
*nopopup = ''.

*****************************
IF nopopup <> 'X'.
  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel   = ' Options for the Extraction Menu '
      txt1    = 'S -> Datasources  / X -> ADSOs  / B -> ADSOs with chars / I -> Infosources'
      txt2    = 'C -> Cubes         / D -> Cubes with chars          / Y -> Transformations'
      txt3    = 'F -> Trnsf coding / P -> Infosources with IOs     / Z -> InfoObjects'
      txt4    = 'N -> DTPs          / E -> Infosets with chars       / Q -> Queries'
    EXCEPTIONS
      OTHERS  = 0.
ENDIF.

START-OF-SELECTION.

CASE obj_type.

***** DataSources ************************************************************************************************************************

WHEN 'S'.
  SELECT * FROM RSDS
    INTO TABLE lt_rsds
    WHERE OBJVERS = 'A'.

  SELECT * FROM RSDST
    INTO TABLE lt_rsds_text
    WHERE OBJVERS = 'A' AND LANGU = LANGUAGE.

  SORT lt_rsds BY DATASOURCE ASCENDING.
  lv_count = lines( lt_rsds ).

  lv_file_name = 'List_Of_Active_DataSources.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'DataSource Technical Name' 'Desc DataSource'
                'Source System' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

    "List
    LOOP AT lt_rsds INTO wa_rsds.
      READ TABLE lt_rsds_text INTO ls_rsds_text WITH KEY DATASOURCE = wa_rsds-DATASOURCE.
      CONCATENATE
        wa_rsds-DATASOURCE
        ls_rsds_text-TXTLG
        wa_rsds-LOGSYS
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.

***** DSOs ************************************************************************************************************************

WHEN 'X'.
  SELECT * FROM rsdodso
    INTO TABLE lt_adsos_raw
    WHERE objvers = 'A'.

CLEAR LT_TXTLG.
  SELECT * FROM RSDODSOT
    INTO TABLE LT_TXTLG
    WHERE OBJVERS = 'A'
      AND LANGU = LANGUAGE.

CLEAR wa_adsos_raw.
LOOP AT lt_adsos_raw INTO wa_adsos_raw.
  CLEAR wa_adsos.
      wa_adsos-adso_name = wa_adsos_raw-ODSOBJECT.
  APPEND wa_adsos TO lt_adsos.
ENDLOOP.

CLEAR wa_adsos.
LOOP AT lt_adsos INTO wa_adsos.
    READ TABLE LT_TXTLG INTO LS_TXTLG
      WITH KEY ODSOBJECT = wa_adsos-adso_name.
    IF sy-subrc = 0.
      wa_adsos-adso_text = LS_TXTLG-txtlg.
    ENDIF.
  MODIFY lt_adsos FROM wa_adsos.
ENDLOOP.

CLEAR LT_TXTLG.
  SELECT * FROM RSDODSOT
    INTO TABLE LT_TXTLG
    WHERE OBJVERS = 'A'
      AND LANGU = SECOND_LANG.

CLEAR wa_adsos.
LOOP AT lt_adsos INTO wa_adsos.
  IF wa_adsos-adso_text IS INITIAL.
    READ TABLE LT_TXTLG INTO LS_TXTLG
      WITH KEY ODSOBJECT = wa_adsos-adso_name.
    IF sy-subrc = 0.
      wa_adsos-adso_text = LS_TXTLG-txtlg.
    ENDIF.
  MODIFY lt_adsos FROM wa_adsos.
  ENDIF.
ENDLOOP.

  SORT LT_ADSOS BY adso_name ASCENDING.
  lv_count = lines( lt_adsos ).

CLEAR LV_COUNT.
  lv_file_name = 'List_Of_Active_ADSOs.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'ADSO Technical Name'
                'Desc. ADSO' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

    "List
    LOOP AT lt_adsos INTO wa_adsos.
      CONCATENATE
        wa_adsos-adso_name
        wa_adsos-adso_text
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.

***** ADSOs with CHARS *********************************************************************************************************************

WHEN 'B'.
  SELECT *
  FROM rsdodso
  INTO TABLE lt_adsos_raw
  WHERE objvers = 'A'.

  LOOP AT lt_adsos_raw INTO wa_adsos_raw.
    SELECT SINGLE txtlg
      INTO lv_text
      FROM RSDODSOT
      WHERE odsobject = wa_adsos_raw-ODSOBJECT
       AND OBJVERS = 'A'
       AND LANGU = LANGUAGE.

     SELECT *
      INTO TABLE lt_characters2
      FROM RSDODSOIOBJ
      WHERE ODSOBJECT = wa_adsos_raw-ODSOBJECT.

     LOOP AT lt_characters2 INTO ls_characters2.
       CLEAR: lv_char_text.
       SELECT SINGLE txtlg
        INTO lv_char_text
        FROM RSDIOBJT
        WHERE iobjnm = ls_characters2-IOBJNM
        AND OBJVERS  = 'A' AND LANGU = LANGUAGE.

       ls_adso2-adso_name = wa_adsos_raw-ODSOBJECT.
       ls_adso2-adso_text = lv_text.
       ls_adso2-char_name = ls_characters2-IOBJNM.
       ls_adso2-char_text = lv_char_text.
       ls_adso2-key_field = ls_characters2-KEYFLAG.

       APPEND ls_adso2 TO lt_adsos2.
     ENDLOOP.
  ENDLOOP.

LOOP AT lt_adsos2 INTO ls_adso2.
SELECT *
  INTO TABLE lt_txtlg
  FROM RSDODSOT
  WHERE odsobject = ls_adso2-adso_name
       AND OBJVERS = 'A'
       AND LANGU = SECOND_LANG.
ENDLOOP.

CLEAR ls_adso2.
  LOOP AT lt_adsos2 INTO ls_adso2.
    IF ls_adso2-adso_text IS INITIAL.
      READ TABLE lt_txtlg INTO ls_txtlg
        WITH KEY odsobject = ls_adso2-adso_name.
          IF sy-subrc = 0.
            ls_adso2-adso_text = ls_txtlg-txtlg.
          ENDIF.
        MODIFY lt_adsos2 FROM ls_adso2.
    ENDIF.
  ENDLOOP.

  SORT LT_ADSOS2 BY adso_name ASCENDING key_field DESCENDING char_name ASCENDING.

CLEAR LV_COUNT.
  lv_file_name = 'List_Of_Active_ADSO_With_IOBs.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'ADSO Technical Name' 'Desc. ADSO' 'Characteristic Technical Name'
                'Desc. Characteristic' 'Key field' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

    "List
    LOOP AT lt_adsos2 INTO ls_adso2.
      CONCATENATE
        ls_adso2-adso_name
        ls_adso2-adso_text
        ls_adso2-char_name
        ls_adso2-char_text
        ls_adso2-key_field
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.

***** Cubes ************************************************************************************************************************

WHEN 'C'.
CLEAR lt_rsdcube.
  SELECT * FROM RSDCUBE
    INTO TABLE lt_rsdcube
    WHERE OBJVERS = 'A'.

  SELECT * FROM RSDCUBET
    INTO TABLE lt_txtcube
    WHERE OBJVERS = 'A' AND LANGU = LANGUAGE.

LOOP AT lt_rsdcube INTO wa_rsdcube.
  CLEAR wa_export_rsdcube.
  wa_export_rsdcube-cube_name = wa_rsdcube-infocube.
  wa_export_rsdcube-infoarea  = wa_rsdcube-infoarea.
  APPEND wa_export_rsdcube TO lt_export_rsdcube.
ENDLOOP.

LOOP AT lt_export_rsdcube INTO wa_export_rsdcube.
    READ TABLE lt_txtcube INTO ls_txtcube
      WITH KEY infocube = wa_export_rsdcube-cube_name.
    IF sy-subrc = 0.
      wa_export_rsdcube-txtlg = ls_txtcube-txtlg.
    ENDIF.
  MODIFY lt_export_rsdcube FROM wa_export_rsdcube.
ENDLOOP.

CLEAR lt_txtcube.
SELECT * FROM RSDCUBET
  INTO TABLE lt_txtcube
  WHERE OBJVERS = 'A' AND LANGU = SECOND_LANG.

CLEAR wa_export_rsdcube.
LOOP AT lt_export_rsdcube INTO wa_export_rsdcube.
 IF wa_export_rsdcube-txtlg IS INITIAL.
    READ TABLE lt_txtcube INTO ls_txtcube
      WITH KEY infocube = wa_export_rsdcube-cube_name.
    IF sy-subrc = 0.
      wa_export_rsdcube-txtlg = ls_txtcube-txtlg.
    ENDIF.
  MODIFY lt_export_rsdcube FROM wa_export_rsdcube.
  ENDIF.
ENDLOOP.

SORT lt_export_rsdcube BY cube_name infoarea txtlg.
DELETE ADJACENT DUPLICATES FROM lt_export_rsdcube COMPARING cube_name infoarea txtlg.

CLEAR LV_COUNT.
  lv_file_name = 'List_Of_Active_Cubes.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

      "Header
      CONCATENATE 'Cube Technical Name' 'InfoArea'
                  'Desc. Cube' INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.

CLEAR wa_export_rsdcube.
      "List
    LOOP AT lt_export_rsdcube INTO wa_export_rsdcube.
      CONCATENATE
        wa_export_rsdcube-cube_name
        wa_export_rsdcube-INFOAREA
        wa_export_rsdcube-TXTLG
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.

***** Cubes with chars ************************************************************************************************************************

WHEN 'D'.
  SELECT *
  FROM RSDCUBE
  INTO TABLE lt_rsdcube
  WHERE objvers = 'A'.

  LOOP AT lt_rsdcube INTO wa_rsdcube.
    SELECT SINGLE txtlg
      INTO lv_text
      FROM RSDCUBET
      WHERE INFOCUBE = wa_rsdcube-INFOCUBE
       AND LANGU = LANGUAGE.
     SELECT *
      INTO TABLE lt_characters
      FROM rsdcubeiobj
      WHERE infocube = wa_rsdcube-INFOCUBE
       AND OBJVERS = 'A'.

     LOOP AT lt_characters INTO ls_characters.
       CLEAR: lv_char_text.
       SELECT SINGLE txtlg
        INTO lv_char_text
        FROM RSDIOBJT
        WHERE iobjnm = ls_characters-IOBJNM
        AND OBJVERS = 'A' AND LANGU = LANGUAGE.

       wa_cube-cube_name = wa_rsdcube-INFOCUBE.
       wa_cube-cube_text = lv_text.
       wa_cube-char_name = ls_characters-IOBJNM.
       wa_cube-char_text = lv_char_text.
       APPEND wa_cube TO lt_cube.
     ENDLOOP.
  ENDLOOP.
  SORT lt_cube BY cube_name char_name ASCENDING.

CLEAR wa_cube.
CLEAR lt_txtcube.
LOOP AT lt_cube INTO wa_cube.
  SELECT *
    INTO TABLE lt_txtcube
    FROM RSDCUBET
    WHERE INFOCUBE = wa_cube-cube_name
       AND OBJVERS = 'A'
       AND LANGU = SECOND_LANG.
ENDLOOP.

CLEAR wa_cube.
   LOOP AT lt_cube INTO wa_cube.
     IF wa_cube-cube_text IS INITIAL.
       READ TABLE lt_txtcube INTO ls_txtcube
        WITH KEY INFOCUBE = wa_cube-cube_name.
         IF sy-subrc = 0.
           wa_cube-cube_text = ls_txtcube-txtlg.
         ENDIF.
         MODIFY lt_cube FROM wa_cube.
     ENDIF.
   ENDLOOP.

CLEAR LV_COUNT.
  lv_file_name = 'List_Of_Cubes_With_IOBs.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'Cube Technical Name' 'Desc. Cube' 'Characteristic Technical Name'
                'Desc. Characteristic' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

    "List
    LOOP AT lt_cube INTO wa_cube.
      CONCATENATE
        wa_cube-cube_name
        wa_cube-cube_text
        wa_cube-char_name
        wa_cube-char_text
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.


***** Transformations ************************************************************************************************************************

WHEN 'Y'.
  SELECT * FROM rstran
  INTO TABLE lt_transformations
  WHERE objvers = 'A'.

  SORT lt_transformations BY tranid sourcename targetname ASCENDING.

CLEAR LV_COUNT.
  lv_file_name = 'List_Of_Active_Transformations.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'Transformation ID' 'Origin Type' 'Origin Name' 'Destination Type' 'Destination Name'
                'Start Routine ID' 'End Routine ID' 'Expert Routine ID' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

    "List
    CLEAR wa_transformation.
    LOOP AT lt_transformations INTO wa_transformation.
      CONCATENATE
        wa_transformation-tranid
        wa_transformation-sourcetype
        wa_transformation-sourcename
        wa_transformation-targettype
        wa_transformation-targetname
        wa_transformation-STARTROUTINE
        wa_transformation-ENDROUTINE
        wa_transformation-EXPERT
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.

***** Transformation field code ************************************************************************************************************************

WHEN 'F'.

CLEAR lt_transformations.
" Get all Active transformations
SELECT * FROM rstran
  INTO TABLE lt_transformations
  WHERE objvers = 'A'.

" Get all Routines from the Routines table
SELECT * FROM RSTRANRULE
  INTO TABLE lt_rstranrule
  WHERE RULETYPE = 'ROUTINE'
    AND ABAP = 'X'
    AND OBJVERS = 'A'.

LOOP AT lt_rstranrule INTO wa_rstranrule.

  CLEAR lt_rstransteprout.

  SELECT * FROM RSTRANSTEPROUT
   INTO TABLE lt_rstransteprout
   WHERE
       OBJVERS = 'A'
       AND TRANID = wa_rstranrule-TRANID
       AND RULEID = wa_rstranrule-RULEID.

  LOOP AT lt_rstransteprout INTO ls_rstransteprout.

    CLEAR lt_rsaabap.
    SELECT * FROM RSAABAP
      INTO TABLE lt_rsaabap
      WHERE OBJVERS = 'A'
        AND CODEID = ls_rstransteprout-CODEID.

    LOOP AT lt_rsaabap INTO ls_rsaabap.
      ls_CodeTransformationField-TRANS_ID = wa_rstranrule-TRANID.
      ls_CodeTransformationField-CODE_ID = ls_rstransteprout-CODEID.
      ls_CodeTransformationField-LINE_NO = ls_rsaabap-LINE_NO.
      ls_CodeTransformationField-LINE = ls_rsaabap-LINE.
      APPEND ls_CodeTransformationField TO lt_CodeTransformationField.
    ENDLOOP.
  ENDLOOP.
ENDLOOP.

" Get all ROUTINES in the tranformations
LOOP AT lt_transformations INTO wa_transformation.

  " STARTROUTINE
  IF NOT wa_transformation-STARTROUTINE IS INITIAL.
    CLEAR lt_rsaabap.
    SELECT * FROM RSAABAP
      INTO TABLE lt_rsaabap
      WHERE OBJVERS = 'A'
        AND CODEID = wa_transformation-STARTROUTINE.
      CLEAR ls_rsaabap.
    LOOP AT lt_rsaabap INTO ls_rsaabap.
      CLEAR ls_CodeTransformationField.
      ls_CodeTransformationField-TRANS_ID = wa_transformation-TRANID.
      ls_CodeTransformationField-CODE_ID  = wa_transformation-STARTROUTINE.
      ls_CodeTransformationField-LINE_NO  = ls_rsaabap-LINE_NO.
      ls_CodeTransformationField-LINE     = ls_rsaabap-LINE.
      APPEND ls_CodeTransformationField TO lt_CodeTransformationField.
    ENDLOOP.
  ENDIF.

  " ENDROUTINE
  IF NOT wa_transformation-ENDROUTINE IS INITIAL.
    CLEAR lt_rsaabap.
    SELECT * FROM RSAABAP
      INTO TABLE lt_rsaabap
      WHERE OBJVERS = 'A'
        AND CODEID = wa_transformation-ENDROUTINE.
    LOOP AT lt_rsaabap INTO ls_rsaabap.
      CLEAR ls_CodeTransformationField.
      ls_CodeTransformationField-TRANS_ID = wa_transformation-TRANID.
      ls_CodeTransformationField-CODE_ID  = wa_transformation-ENDROUTINE.
      ls_CodeTransformationField-LINE_NO  = ls_rsaabap-LINE_NO.
      ls_CodeTransformationField-LINE     = ls_rsaabap-LINE.
      APPEND ls_CodeTransformationField TO lt_CodeTransformationField.
    ENDLOOP.
  ENDIF.

  " EXPERT
  IF NOT wa_transformation-EXPERT IS INITIAL.
    CLEAR lt_rsaabap.
    SELECT * FROM RSAABAP
      INTO TABLE lt_rsaabap
      WHERE OBJVERS = 'A'
        AND CODEID = wa_transformation-EXPERT.
    LOOP AT lt_rsaabap INTO ls_rsaabap.
      CLEAR ls_CodeTransformationField.
      ls_CodeTransformationField-TRANS_ID = wa_transformation-TRANID.
      ls_CodeTransformationField-CODE_ID  = wa_transformation-EXPERT.
      ls_CodeTransformationField-LINE_NO  = ls_rsaabap-LINE_NO.
      ls_CodeTransformationField-LINE     = ls_rsaabap-LINE.
      APPEND ls_CodeTransformationField TO lt_CodeTransformationField.
    ENDLOOP.
  ENDIF.

ENDLOOP.

SORT lt_CodeTransformationField BY TRANS_ID CODE_ID LINE_NO ASCENDING.

CLEAR LV_COUNT.
  lv_file_name = 'List_Of_Transformation_Code.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'Transformation ID' 'Code ID'
                'Line No' 'Line' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

    "List
    LOOP AT lt_CodeTransformationField INTO ls_CodeTransformationField.
      CONCATENATE
        ls_CodeTransformationField-TRANS_ID
        ls_CodeTransformationField-CODE_ID
        ls_CodeTransformationField-LINE_NO
        ls_CodeTransformationField-LINE
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.


***** InfoSources ************************************************************************************************************************

WHEN 'I'.
  SELECT * FROM RSIS
    INTO TABLE lt_infosource_raw
    WHERE OBJVERS = 'A'.

  SELECT * FROM RSIST
    INTO TABLE lt_istxt
    WHERE OBJVERS = 'A'
      AND LANGU = LANGUAGE.

LOOP AT lt_infosource_raw INTO wa_infosource_raw.
  CLEAR wa_infosource.
      wa_infosource-isource_name = wa_infosource_raw-isource.
  APPEND wa_infosource TO lt_infosource.
ENDLOOP.

CLEAR wa_infosource.
LOOP AT lt_infosource INTO wa_infosource.
    READ TABLE lt_istxt INTO ls_istxt
      WITH KEY isource = wa_infosource-isource_name.
    IF sy-subrc = 0.
      wa_infosource-isource_text = ls_istxt-txtlg.
    ENDIF.
  MODIFY lt_infosource FROM wa_infosource.
ENDLOOP.

CLEAR lt_istxt.
  SELECT * FROM RSIST
    INTO TABLE lt_istxt
    WHERE OBJVERS = 'A'
      AND LANGU = SECOND_LANG.

CLEAR wa_infosource.
LOOP AT lt_infosource INTO wa_infosource.
  IF wa_infosource-isource_text IS INITIAL.
    READ TABLE lt_istxt INTO ls_istxt
      WITH KEY isource = wa_infosource-isource_name.
    IF sy-subrc = 0.
      wa_infosource-isource_text = ls_istxt-txtlg.
    ENDIF.
  MODIFY lt_infosource FROM wa_infosource.
  ENDIF.
ENDLOOP.

  SORT lt_infosource BY isource_name isource_text ASCENDING.

CLEAR LV_COUNT.
  lv_file_name = 'List_Of_Active_InfoSources.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'InfoSource Technical Name'
                'InfoSource Description' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

CLEAR wa_infosource.
    "List
    LOOP AT lt_infosource INTO wa_infosource.
      CONCATENATE
        wa_infosource-isource_name
        wa_infosource-isource_text
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.


***** InfoObjets ************************************************************************************************************************

WHEN 'Z'.
  SELECT * FROM RSDIOBJ
    INTO TABLE  LT_INFOOBJ
    WHERE objvers = 'A'.

    SELECT * FROM RSDIOBJT
      INTO TABLE LT_TXTIOB
      WHERE OBJVERS = 'A' AND LANGU = LANGUAGE.

  SORT LT_INFOOBJ BY IOBJTP IOBJNM ASCENDING.
  SORT LT_TXTIOB BY IOBJNM TXTLG ASCENDING.

CLEAR LV_COUNT.
  lv_file_name = 'List_Of_Active_InfoObjects.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
    OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc = 0.

      "Header
      CONCATENATE 'InfoObject Type' 'InfoObject Name'
                  'Desc. InfoObject' INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.

      "List
      LOOP AT lt_infoobj INTO wa_infoobj.
        READ TABLE LT_TXTIOB INTO LS_TXTIOB WITH KEY IOBJNM = WA_INFOOBJ-IOBJNM.
        CONCATENATE
          WA_INFOOBJ-IOBJTP
          WA_INFOOBJ-IOBJNM
          LS_TXTIOB-TXTLG
          INTO lv_string SEPARATED BY ';'.
        TRANSFER lv_string TO lv_path.
        LV_COUNT = LV_COUNT + 1.
      ENDLOOP.

      CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.


***** InfoSource with InfoObjects **********************************************************************************************************

WHEN 'P'.
  SELECT * FROM RSIS
    INTO TABLE lt_infosource_raw
    WHERE OBJVERS = 'A'.


  LOOP AT lt_infosource_raw INTO wa_infosource_raw.
     SELECT SINGLE txtlg
      INTO lv_text
      FROM RSIST
      WHERE ISOURCE = wa_infosource_raw-ISOURCE
      AND OBJVERS = 'A' AND LANGU = LANGUAGE.

      SELECT *
      INTO TABLE lt_characters3
      FROM RSISFIELD
      WHERE ISOURCE = wa_infosource_raw-ISOURCE AND OBJVERS = 'A'.


      LOOP AT lt_characters3 INTO ls_characters3.
       CLEAR: lv_char_text.
       SELECT SINGLE txtlg
        INTO lv_char_text
        FROM RSDIOBJT
        WHERE iobjnm = ls_characters3-IOBJNM
        AND OBJVERS = 'A' AND LANGU = LANGUAGE.

        ls_isource-isource_NAME = wa_infosource_raw-ISOURCE.
        ls_isource-isource_TEXT = lv_text.
        ls_isource-CHAR_NAME = ls_characters3-IOBJNM.
        ls_isource-CHAR_TEXT = lv_char_text.

        APPEND ls_isource TO lt_isource.
      ENDLOOP.
  ENDLOOP.

  SORT LT_ISOURCE BY ISOURCE_NAME CHAR_NAME ASCENDING.

CLEAR LV_COUNT.
  lv_file_name = 'InfoSource_with_IOBs.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'InfoSource Technical Name' 'Desc InfoSource'
                'Characteristic Technical Name' 'Desc. Characteristic' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

    "List
    LOOP AT lt_isource INTO ls_isource.
      CONCATENATE
        ls_isource-ISOURCE_NAME
        ls_isource-ISOURCE_TEXT
        ls_isource-CHAR_NAME
        ls_isource-CHAR_TEXT
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.


***** DTPs ************************************************************************************************************************

WHEN 'N'.
  SELECT * FROM RSBKDTP
    INTO TABLE lt_dtp
    WHERE OBJVERS = 'A'.

  SELECT * FROM RSBKDTPT
    INTO TABLE lt_dtp_t
    WHERE OBJVERS = 'A' AND LANGU = LANGUAGE.

  SORT LT_DTP BY DTP SRCTP TGTTP ASCENDING.
  SORT LT_DTP_T BY DTP TXTLG ASCENDING.

  lv_file_name = 'List_Of_Active_DTPs.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'DTP' 'Desc. DTP' 'Origin Type' 'Origin Name' 'Destination Type'
                'Destination Name' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.
    LV_COUNT = 1.

    "List
    LOOP AT lt_dtp INTO wa_dtp.
      READ TABLE lt_dtp_t INTO ls_dtp_t WITH KEY DTP = wa_dtp-DTP.
      CONCATENATE
        wa_dtp-DTP
        ls_dtp_t-TXTLG
        wa_dtp-SRCTP
        wa_dtp-SRC
        wa_dtp-TGTTP
        wa_dtp-TGT
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.


***** Infosets with chars ************************************************************************************************************************

WHEN 'E'.
  SELECT *
    FROM RSQISET
    INTO TABLE lt_infoset1
    WHERE OBJVERS = 'A'.

  SELECT * FROM RSQFOBJT
    INTO TABLE lt_infoset_t
    WHERE OBJVERS = 'A' AND LANGU = LANGUAGE.

  LOOP AT lt_infoset1 INTO wa_infoset1.

    READ TABLE lt_infoset_t INTO ls_infoset_t
    WITH KEY INFOSET = wa_infoset1-INFOSET.
    lv_text = ls_infoset_t-TXTLG.

    SELECT *
      INTO TABLE lt_cha_infoset
      FROM RSQFOBJ
      WHERE INFOSET = wa_infoset1-INFOSET and OBJVERS = 'A'.

     LOOP AT lt_cha_infoset INTO ls_cha_infoset.
       CLEAR: lv_char_text.
       SELECT SINGLE txtlg
        INTO lv_char_text
        FROM RSDIOBJT
        WHERE iobjnm = ls_cha_infoset-FNAME
        AND OBJVERS  = 'A' AND LANGU = LANGUAGE.

       ls_infoset-INFOSET_NAME = wa_infoset1-INFOSET.
       ls_infoset-INFOSET_TEXT = lv_text.
       ls_infoset-CHAR_NAME = ls_cha_infoset-FNAME.
       ls_infoset-CHAR_TEXT = lv_char_text.

       APPEND ls_infoset TO lt_infoset.
     ENDLOOP.
  ENDLOOP.

  SORT LT_INFOSET BY INFOSET_NAME CHAR_NAME ASCENDING.

  lv_file_name = 'InfoSets_with_IOs.csv'.
  CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'InfoSet Tech. Name' 'InfoSet Description' 'Characteristic Technical Name'
                'Desc. Characteristic' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.
    LV_COUNT = 1.

    "List
    LOOP AT lt_infoset INTO ls_infoset.
      CONCATENATE
        ls_infoset-INFOSET_NAME
        ls_infoset-INFOSET_TEXT
        ls_infoset-CHAR_NAME
        ls_infoset-CHAR_TEXT
        INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.


***** Queries **************************************************************************

WHEN 'Q'.

SELECT a~compuid,
       a~infocube,
       b~mapname,
       c~txtlg
  FROM RSRREPDIR AS a
  INNER JOIN RSZELTDIR AS b
    ON a~objvers = b~objvers
   AND a~comptype = b~subdeftp
   AND a~compuid = b~eltuid
  LEFT OUTER JOIN RSZELTTXT AS c
    ON b~objvers = c~objvers
   AND b~eltuid = c~eltuid
  WHERE b~objvers = 'A'
    AND b~subdeftp = 'REP'
  INTO TABLE @lt_query_main.

  SELECT *
   FROM
    RSZELTXREF
    FOR ALL ENTRIES IN @lt_query_main
   WHERE
      SELTUID = @lt_query_main-compuid and objvers = 'A'
  INTO TABLE @lt_query_elements.

SORT lt_query_elements BY seltuid teltuid.
DELETE ADJACENT DUPLICATES FROM lt_query_elements COMPARING seltuid teltuid.

SORT lt_query_main BY compuid infocube.
DELETE ADJACENT DUPLICATES FROM lt_query_main COMPARING compuid infocube.

*********OUTPUT FILE 1**********
CLEAR LV_COUNT.
  lv_file_name = 'Queries_1_List_Of_All_Queries.csv'.
  CONCATENATE LV_ROOT lv_file_name INTO lv_path.
  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    "Header
    CONCATENATE 'Query ID' 'InfoProvider' 'Technical Name'
                'Desc. Query' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.
    LOOP AT lt_query_main INTO wa_query_main.
      CONCATENATE wa_query_main-COMPUID
                  wa_query_main-INFOCUBE
                  wa_query_main-MAPNAME
                  wa_query_main-TXTLG
                  INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.
    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.

*********************************
*********************************
*************FILE 2**************
*********************************
LOOP AT lt_query_elements INTO wa_query_elements.
  CLEAR wa_query_elements_ext.

  " Look for corresponding record in lt_query_main where SELTUID = COMPUID
  READ TABLE lt_query_main INTO wa_query_main
    WITH KEY compuid = wa_query_elements-seltuid.
  IF sy-subrc = 0.
    wa_query_elements_ext-technical_name = wa_query_main-mapname.
  ELSE.
    CLEAR wa_query_elements_ext-technical_name.
  ENDIF.

  " Put values into extended table
  wa_query_elements_ext-elementid      = wa_query_elements-seltuid.
  wa_query_elements_ext-objvers        = wa_query_elements-objvers.
  wa_query_elements_ext-teltuid        = wa_query_elements-teltuid.
  wa_query_elements_ext-laytp          = wa_query_elements-laytp.
  wa_query_elements_ext-infocube       = wa_query_elements-infocube.
  wa_query_elements_ext-technical_name = wa_query_main-mapname.
  wa_query_elements_ext-optional       = wa_query_elements-optional.
  wa_query_elements_ext-posn           = wa_query_elements-posn.
  wa_query_elements_ext-flat_posn      = wa_query_elements-flat_posn.
  APPEND wa_query_elements_ext TO lt_query_elements_ext.
ENDLOOP.

CLEAR wa_query_elements_ext.
LOOP AT lt_query_elements_ext INTO wa_query_elements_ext.
  CASE wa_query_elements_ext-laytp.
    WHEN 'VAR'.
      wa_query_elements_ext-laytp_desc = 'Variable Sequence'.
    WHEN 'NIL'.
      wa_query_elements_ext-laytp_desc = 'No Layout'.
    WHEN 'SHT'.
      wa_query_elements_ext-laytp_desc = 'Query Sheet'.
    WHEN 'SOB'.
      wa_query_elements_ext-laytp_desc = 'Selection Object'.
    WHEN 'AGG'.
      wa_query_elements_ext-laytp_desc = 'Aggregated'.
    WHEN 'ROW'.
      wa_query_elements_ext-laytp_desc = 'Row Details'.
    WHEN 'COL'.
      wa_query_elements_ext-laytp_desc = 'Column Details'.
    WHEN 'CELL'.
      wa_query_elements_ext-laytp_desc = 'Cell References'.
    WHEN 'NAV'.
      wa_query_elements_ext-laytp_desc = 'Navigation'.
    WHEN 'FIX'.
      wa_query_elements_ext-laytp_desc = 'Filter'.
    WHEN 'MBR'.
      wa_query_elements_ext-laytp_desc = 'Structure element'.
    WHEN 'OPD'.
      wa_query_elements_ext-laytp_desc = 'Operand'.
    WHEN 'RNG'.
      wa_query_elements_ext-laytp_desc = 'Area'.
    WHEN 'REP'.
      wa_query_elements_ext-laytp_desc = 'Internal Use'.
    WHEN 'FLT'.
      wa_query_elements_ext-laytp_desc = 'Formatted Reporting - Order'.
    WHEN 'ATR'.
      wa_query_elements_ext-laytp_desc = 'Order of Attributes'.
    WHEN 'QVR'.
      wa_query_elements_ext-laytp_desc = 'Variable Squence of Query Variables'.
    WHEN OTHERS.
      wa_query_elements_ext-laytp_desc = 'Unknown type'.
  ENDCASE.

  MODIFY lt_query_elements_ext FROM wa_query_elements_ext.
ENDLOOP.

 " Order and delete duplicates
SORT lt_query_elements_ext BY elementid teltuid.
DELETE ADJACENT DUPLICATES FROM lt_query_elements_ext COMPARING elementid teltuid technical_name.

*********OUTPUT FILE 2**********
CLEAR LV_COUNT.
 lv_file_name = 'Queries_2_First_Lvl_Elem.csv'.
 CONCATENATE LV_ROOT LV_FILE_NAME INTO LV_PATH.
 OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
 IF sy-subrc = 0.

    "Header
    CONCATENATE 'Element ID' 'Subelement ID' 'Layout type' 'Desc. Layout type' 'InfoProvider' 'Query Technical Name'
                'Optional' 'Position' 'Flat Position' INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.

    LOOP AT lt_query_elements_ext INTO wa_query_elements_ext.
      lv_posn_text = wa_query_elements_ext-posn.
      lv_flat_posn_text = wa_query_elements_ext-flat_posn.
      CONCATENATE
        wa_query_elements_ext-elementid
        wa_query_elements_ext-teltuid
        wa_query_elements_ext-laytp
        wa_query_elements_ext-laytp_desc
        wa_query_elements_ext-infocube
        wa_query_elements_ext-technical_name
        wa_query_elements_ext-optional
        lv_posn_text
        lv_flat_posn_text
      INTO lv_string SEPARATED BY ';'.
      TRANSFER lv_string TO lv_path.
      LV_COUNT = LV_COUNT + 1.
    ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.

*********************************
*********************************
**************FILE 3*************
*********************************
" 1. Initialize with elements from latestoutput
lt_current_level = CORRESPONDING #( lt_query_elements_ext ).

" 2. Add elements to processed base table
LOOP AT lt_current_level INTO wa_new_link.
  INSERT wa_new_link INTO TABLE lt_processed_check.
  APPEND wa_new_link TO lt_all_processed.
ENDLOOP.

" 3. Main Loop: carries on while there are still elements to process
WHILE lt_current_level IS NOT INITIAL.

  " Count lines before processing the level
  DESCRIBE TABLE lt_all_processed LINES lv_lines_before.
  CLEAR lt_next_level.

  " Process each element in current level
  LOOP AT lt_current_level INTO wa_process.

    " Search for children for the element
    SELECT * FROM rszeltxref
      WHERE seltuid = @wa_process-elementid
        AND objvers = 'A'
      INTO TABLE @lt_children.

    " Process each child
    LOOP AT lt_children INTO wa_child.
      CLEAR wa_new_link.

      wa_new_link-teltuid        = wa_process-elementid.        " Parend
      wa_new_link-elementid      = wa_child-teltuid.          " Child
      wa_new_link-objvers        = wa_child-objvers.
      wa_new_link-laytp          = wa_child-laytp.
      wa_new_link-posn           = wa_child-posn.
      wa_new_link-infocube       = wa_child-infocube.
      wa_new_link-optional       = wa_child-optional.
      wa_new_link-flat_posn      = wa_child-flat_posn.
      wa_new_link-technical_name = wa_process-technical_name. " Parent's technical_name

      READ TABLE lt_processed_check WITH KEY
        teltuid = wa_new_link-teltuid
        elementid = wa_new_link-elementid
        technical_name = wa_new_link-technical_name
        laytp = wa_new_link-laytp
        posn = wa_new_link-posn
        flat_posn = wa_new_link-flat_posn
      TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        APPEND wa_new_link TO lt_processed_check.
        APPEND wa_new_link TO lt_all_processed.
        APPEND wa_new_link TO lt_next_level.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

  " Count lines after processing current level
  DESCRIBE TABLE lt_all_processed LINES lv_lines_after.

  " Calculate new found records
  lv_new_records = lv_lines_after - lv_lines_before.

  " Exit if no new records were found
  IF lv_new_records = 0.
    EXIT.
  ENDIF.

  " Prepare next level
  lt_current_level = lt_next_level.
  lv_level_counter = lv_level_counter + 1.

  " Security: avoid infinite loops
  IF lv_level_counter > 20.
    EXIT.
  ENDIF.

ENDWHILE.

" 4. All processed records go into output table
CLEAR lt_export_final.
lt_export_final = lt_all_processed.

" 5. Order and delete duplicates
SORT lt_export_final BY teltuid elementid technical_name.
DELETE ADJACENT DUPLICATES FROM lt_export_final COMPARING teltuid elementid technical_name laytp posn flat_posn.
DELETE lt_export_final WHERE flat_posn = 0 AND posn = 0 AND laytp IS INITIAL AND infocube IS INITIAL.


********Include texts and descriptions for FILE 3
SELECT *
  FROM rszelttxt
  INTO TABLE lt_texts_rszelttxt
  FOR ALL ENTRIES IN lt_export_final
  WHERE eltuid  = lt_export_final-elementid
    AND objvers = 'A'
    AND langu   = LANGUAGE.

LOOP AT lt_export_final INTO wa_export_final.
  READ TABLE lt_texts_rszelttxt INTO wa_texts_rszelttxt
    WITH KEY eltuid = wa_export_final-elementid.
  IF sy-subrc = 0.
    REPLACE ALL OCCURRENCES OF ';'
        IN wa_texts_rszelttxt-txtlg WITH ', '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN wa_texts_rszelttxt-txtlg WITH ' '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
        IN wa_texts_rszelttxt-txtlg WITH ' '.
    wa_export_final-txtlg = wa_texts_rszelttxt-txtlg.
  ENDIF.
  MODIFY lt_export_final FROM wa_export_final.
ENDLOOP.

 " Order and delete duplicates
SORT lt_export_final BY elementid laytp infocube technical_name.
DELETE ADJACENT DUPLICATES FROM lt_export_final COMPARING elementid laytp infocube technical_name.

" 6. Descriptions
LOOP AT lt_export_final INTO wa_export_final.
  CASE wa_export_final-laytp.
    WHEN 'VAR'.
      wa_export_final-laytp_desc = 'Variable Sequence'.
    WHEN 'NIL'.
      wa_export_final-laytp_desc = 'No Layout'.
    WHEN 'SHT'.
      wa_export_final-laytp_desc = 'Query Sheet'.
    WHEN 'SOB'.
      wa_export_final-laytp_desc = 'Selection Object'.
    WHEN 'AGG'.
      wa_export_final-laytp_desc = 'Aggregated'.
    WHEN 'ROW'.
      wa_export_final-laytp_desc = 'Row Details'.
    WHEN 'COL'.
      wa_export_final-laytp_desc = 'Column Details'.
    WHEN 'CELL'.
      wa_export_final-laytp_desc = 'Cell References'.
    WHEN 'NAV'.
      wa_export_final-laytp_desc = 'Navigation'.
    WHEN 'FIX'.
      wa_export_final-laytp_desc = 'Filter'.
    WHEN 'MBR'.
      wa_export_final-laytp_desc = 'Structure element'.
    WHEN 'OPD'.
      wa_export_final-laytp_desc = 'Operand'.
    WHEN 'RNG'.
      wa_export_final-laytp_desc = 'Area'.
    WHEN 'REP'.
      wa_export_final-laytp_desc = 'Internal Use'.
    WHEN 'FLT'.
      wa_export_final-laytp_desc = 'Formatted Reporting - Order'.
    WHEN 'ATR'.
      wa_export_final-laytp_desc = 'Order of Attributes'.
    WHEN 'QVR'.
      wa_export_final-laytp_desc = 'Variable Squence of Query Variables'.
    WHEN OTHERS.
      wa_export_final-laytp_desc = 'Unknown type'.
  ENDCASE.

  MODIFY lt_export_final FROM wa_export_final.
ENDLOOP.

*********OUTPUT FILE 3**********
CLEAR LV_COUNT.
lv_file_name = 'Queries_3_All_Elem_By_Query.csv'.
CONCATENATE LV_ROOT lv_file_name INTO lv_path.
OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
IF sy-subrc = 0.

  "Header
  CONCATENATE  'Element ID' 'Layout type' 'Desc. Layout type' 'InfoProvider' 'Query Technical Name'
                'Optional' 'Position' 'Flat Position' 'Desc. Element' INTO lv_string SEPARATED BY ';'.
  TRANSFER lv_string TO lv_path.

  LOOP AT lt_export_final INTO wa_export_final.
      lv_posn_text = wa_export_final-POSN.
      lv_flat_posn_text = wa_export_final-FLAT_POSN.
    CONCATENATE wa_export_final-elementid
                wa_export_final-laytp
                wa_export_final-laytp_desc
                wa_export_final-infocube
                wa_export_final-technical_name
                wa_export_final-optional
                lv_posn_text
                lv_flat_posn_text
                wa_export_final-txtlg
                INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.
    LV_COUNT = LV_COUNT + 1.
  ENDLOOP.

    CLOSE DATASET lv_path.
      WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

  ELSE.
      WRITE: / 'Error in file generation'.
  ENDIF.

*********************************
*********************************
**********FILE 4*****************
*********************************
" 1. Select * from rszeltdir since previous file had filters applied
LOOP AT lt_export_final INTO wa_export_final.
  CLEAR wa_export_rszeltdir.
  wa_export_rszeltdir-elementid = wa_export_final-elementid.

  APPEND wa_export_rszeltdir TO lt_export_rszeltdir.
ENDLOOP.

SELECT *
  FROM rszelttxt
  INTO TABLE lt_texts_rszelttxt
  FOR ALL ENTRIES IN lt_export_rszeltdir
  WHERE eltuid  = lt_export_rszeltdir-elementid
    AND objvers = 'A'
    AND langu   = LANGUAGE.

SELECT *
  FROM rszeltdir
  INTO TABLE lt_texts_rszeltdir
  FOR ALL ENTRIES IN lt_export_rszeltdir
  WHERE eltuid  = lt_export_rszeltdir-elementid
    AND objvers = 'A'.

" 2. Joins
LOOP AT lt_export_rszeltdir INTO wa_export_rszeltdir.
  READ TABLE lt_texts_rszelttxt INTO wa_texts_rszelttxt
    WITH KEY eltuid = wa_export_rszeltdir-elementid.
  IF sy-subrc = 0.
    REPLACE ALL OCCURRENCES OF ';'
        IN wa_texts_rszelttxt-txtlg WITH ', '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN wa_texts_rszelttxt-txtlg WITH ' '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
        IN wa_texts_rszelttxt-txtlg WITH ' '.
    wa_export_rszeltdir-txtlg = wa_texts_rszelttxt-txtlg.
  ENDIF.
  MODIFY lt_export_rszeltdir FROM wa_export_rszeltdir.
ENDLOOP.

LOOP AT lt_export_rszeltdir INTO wa_export_rszeltdir.
  READ TABLE lt_texts_rszeltdir INTO wa_texts_rszeltdir
    WITH KEY eltuid = wa_export_rszeltdir-elementid.
  IF sy-subrc = 0.
    wa_export_rszeltdir-deftp           = wa_texts_rszeltdir-deftp.
    wa_export_rszeltdir-subdeftp        = wa_texts_rszeltdir-subdeftp.
    wa_export_rszeltdir-additional_info = wa_texts_rszeltdir-defaulthint.
    wa_export_rszeltdir-technical_name  = wa_texts_rszeltdir-mapname.
  ENDIF.
  MODIFY lt_export_rszeltdir FROM wa_export_rszeltdir.
ENDLOOP.

" 3. Descriptions
LOOP AT lt_export_rszeltdir INTO wa_export_rszeltdir.
  CASE wa_export_rszeltdir-deftp.
    WHEN 'VAR'.
      wa_export_rszeltdir-desc_deftp = 'Variable'.
    WHEN 'NIL'.
      wa_export_rszeltdir-desc_deftp = 'No definition'.
    WHEN 'SOB'.
      wa_export_rszeltdir-desc_deftp = 'Filter'.
    WHEN 'REP'.
      wa_export_rszeltdir-desc_deftp = 'Query'.
    WHEN 'STR'.
      wa_export_rszeltdir-desc_deftp = 'Structure'.
    WHEN 'SEL'.
      wa_export_rszeltdir-desc_deftp = 'Restricted Key Figure'.
    WHEN 'CKF'.
      wa_export_rszeltdir-desc_deftp = 'Calculated key figure'.
    WHEN 'FML'.
      wa_export_rszeltdir-desc_deftp = 'Formula'.
    WHEN 'SHT'.
      wa_export_rszeltdir-desc_deftp = 'Worksheet'.
    WHEN 'CEL'.
      wa_export_rszeltdir-desc_deftp  = 'Cell'.
    WHEN 'ATR'.
      wa_export_rszeltdir-desc_deftp  = 'Attribute'.
    WHEN OTHERS.
      wa_export_rszeltdir-desc_deftp = 'Unknown type'.
  ENDCASE.

  CASE wa_export_rszeltdir-subdeftp.
    WHEN 'RKF'.
      wa_export_rszeltdir-desc_subdeftp  = 'Restricted Key Figure'.
    WHEN 'CON'.
      wa_export_rszeltdir-desc_subdeftp  = 'Condition'.
    WHEN 'EXC'.
      wa_export_rszeltdir-desc_subdeftp  = 'Exception'.
    WHEN 'SOB'.
      wa_export_rszeltdir-desc_subdeftp  = 'Filters'.
    WHEN 'CHA'.
      wa_export_rszeltdir-desc_subdeftp  = 'Restricted Characteristic'.
    WHEN 'CEL'.
      wa_export_rszeltdir-desc_subdeftp  = 'Cell'.
    WHEN 'STM'.
      wa_export_rszeltdir-desc_subdeftp  = 'Structure Element'.
    WHEN 'STI'.
      wa_export_rszeltdir-desc_subdeftp  = 'Structure Element as Inverse Formula'.
    WHEN 'CEI'.
      wa_export_rszeltdir-desc_subdeftp  = 'Cell as Inverse Formula'.
    WHEN 'STR'.
      wa_export_rszeltdir-desc_subdeftp  = 'Structure'.
    WHEN 'SHT'.
      wa_export_rszeltdir-desc_subdeftp  = 'Worksheet'.
    WHEN 'CKF'.
      wa_export_rszeltdir-desc_subdeftp  = 'Calculated key figure'.
    WHEN 'VAR'.
      wa_export_rszeltdir-desc_subdeftp  = 'Variable'.
    WHEN 'ATR'.
      wa_export_rszeltdir-desc_subdeftp  = 'Attribute'.
    WHEN 'VAR'.
      wa_export_rszeltdir-desc_subdeftp = 'Variable'.
    WHEN 'CKF'.
      wa_export_rszeltdir-desc_subdeftp = 'Calculated key figure'.
    WHEN OTHERS.
      wa_export_rszeltdir-desc_subdeftp  = 'Unknown type'.
  ENDCASE.

  CASE wa_export_rszeltdir-info_flag.
    WHEN '0'.
      wa_export_rszeltdir-desc_flag = 'Blank'.
    WHEN '1'.
      wa_export_rszeltdir-desc_flag = 'Value'.
    WHEN '2'.
      wa_export_rszeltdir-desc_flag = 'CIN link'.
    WHEN '3'.
      wa_export_rszeltdir-desc_flag = 'Variable CIN'.
    WHEN '4'.
      wa_export_rszeltdir-desc_flag = 'InfoObject'.
    WHEN '5'.
      wa_export_rszeltdir-desc_flag = 'Constant'.
    WHEN '6'.
      wa_export_rszeltdir-desc_flag = 'Exit variable name (screen filter)'.
    WHEN OTHERS.
      wa_export_rszeltdir-desc_flag = 'Unknown type'.
  ENDCASE.

  MODIFY lt_export_rszeltdir FROM wa_export_rszeltdir.
ENDLOOP.

 " 4. Order and delete duplicates
SORT lt_export_rszeltdir BY elementid deftp subdeftp.
DELETE ADJACENT DUPLICATES FROM lt_export_rszeltdir COMPARING elementid deftp subdeftp.

*********OUTPUT FILE 4**********
CLEAR LV_COUNT.
lv_file_name = 'Queries_4_Detail_Of_Elem_By_ID.csv'.
CONCATENATE LV_ROOT lv_file_name INTO lv_path.
OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
IF sy-subrc = 0.

  "Header
  CONCATENATE 'Element ID' 'Definition' 'Desc. Definition' 'Subdefinition' 'Desc. Subdefinition'
              'Technical Name' 'Additional Info' 'Flag Info' 'Desc. Flag' 'Desc. Element' INTO lv_string SEPARATED BY ';'.
  TRANSFER lv_string TO lv_path.

  LOOP AT lt_export_rszeltdir INTO wa_export_rszeltdir.
    CONCATENATE wa_export_rszeltdir-elementid
                wa_export_rszeltdir-deftp
                wa_export_rszeltdir-desc_deftp
                wa_export_rszeltdir-subdeftp
                wa_export_rszeltdir-desc_subdeftp
                wa_export_rszeltdir-technical_name
                wa_export_rszeltdir-additional_info
                wa_export_rszeltdir-info_flag
                wa_export_rszeltdir-desc_flag
                wa_export_rszeltdir-txtlg
                INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.
    LV_COUNT = LV_COUNT + 1.
  ENDLOOP.

  CLOSE DATASET lv_path.
    WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

ELSE.
    WRITE: / 'Error in file generation'.
ENDIF.

*********************************
*********************************
**********FILE 5*****************
*********************************
" 1. Get all IDs from lt_export_rszeltdir
CLEAR lt_texts_rszeltdir.
CLEAR wa_texts_rszeltdir.
SELECT *
  FROM rszeltdir
  INTO TABLE lt_texts_rszeltdir
  WHERE objvers = 'A'.

CLEAR lt_valid_ids_rszrange.
IF lt_texts_rszeltdir IS NOT INITIAL.
  SELECT *
    FROM rszrange
    INTO TABLE lt_valid_ids_rszrange
    FOR ALL ENTRIES IN lt_texts_rszeltdir
    WHERE eltuid = lt_texts_rszeltdir-eltuid
      AND objvers = 'A'.
ENDIF.

" 2. Build lt_export_rszrange using extracted IDs
CLEAR lt_export_rszrange.
LOOP AT lt_valid_ids_rszrange INTO wa_valid_rszrange.
  CLEAR wa_export_rszrange.
  wa_export_rszrange-elementid    = wa_valid_rszrange-eltuid.
  wa_export_rszrange-info_object  = wa_valid_rszrange-iobjnm.
  wa_export_rszrange-sequence_num = wa_valid_rszrange-enum.
  APPEND wa_export_rszrange TO lt_export_rszrange.
ENDLOOP.

" 3. Texts
IF lt_export_rszrange IS NOT INITIAL.
  CLEAR lt_texts_rszelttxt.
  SELECT *
    FROM rszelttxt
    INTO TABLE lt_texts_rszelttxt
    FOR ALL ENTRIES IN lt_export_rszrange
    WHERE eltuid  = lt_export_rszrange-elementid
      AND objvers = 'A'
      AND langu   = LANGUAGE.

  CLEAR lt_texts_rszrange.
  SELECT *
    FROM rszrange
    INTO TABLE lt_texts_rszrange
    FOR ALL ENTRIES IN lt_export_rszrange
    WHERE eltuid  = lt_export_rszrange-elementid
      AND objvers = 'A'.
ENDIF.

" 4. Joins
CLEAR wa_texts_rszelttxt.
LOOP AT lt_export_rszrange INTO wa_export_rszrange.
  READ TABLE lt_texts_rszelttxt INTO wa_texts_rszelttxt
       WITH KEY eltuid = wa_export_rszrange-elementid.
  IF sy-subrc = 0.
    REPLACE ALL OCCURRENCES OF ';'
        IN wa_texts_rszelttxt-txtlg WITH ', '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN wa_texts_rszelttxt-txtlg WITH ' '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
        IN wa_texts_rszelttxt-txtlg WITH ' '.
        wa_export_rszrange-txtlg = wa_texts_rszelttxt-txtlg.
  ENDIF.
  MODIFY lt_export_rszrange FROM wa_export_rszrange.
ENDLOOP.

LOOP AT lt_export_rszrange INTO wa_export_rszrange.
  READ TABLE lt_texts_rszrange INTO wa_texts_rszrange
       WITH KEY eltuid = wa_export_rszrange-elementid
                iobjnm = wa_export_rszrange-info_object
                enum   = wa_export_rszrange-sequence_num.
  IF sy-subrc = 0.
    wa_export_rszrange-info_object_detail = wa_texts_rszrange-faciobjnm.
    wa_export_rszrange-base_info_object   = wa_texts_rszrange-attrinm.
    wa_export_rszrange-purpose_selection  = wa_texts_rszrange-sotp.
    wa_export_rszrange-factor_type        = wa_texts_rszrange-factortype.
    wa_export_rszrange-selection_type     = wa_texts_rszrange-seltp.
    wa_export_rszrange-select_sign        = wa_texts_rszrange-sign.
    wa_export_rszrange-operator           = wa_texts_rszrange-opt.
    wa_export_rszrange-lower_value        = wa_texts_rszrange-low.
    wa_export_rszrange-higher_value       = wa_texts_rszrange-high.
    wa_export_rszrange-lower_flag         = wa_texts_rszrange-lowflag.
    wa_export_rszrange-higher_flag        = wa_texts_rszrange-highflag.
    wa_export_rszrange-hierarchy_name     = wa_texts_rszrange-hienm.
    wa_export_rszrange-alert_level        = wa_texts_rszrange-alertlevel.
  ENDIF.
  MODIFY lt_export_rszrange FROM wa_export_rszrange.
ENDLOOP.

" 5. Descriptions
LOOP AT lt_export_rszrange INTO wa_export_rszrange.
 IF wa_export_rszrange-purpose_selection IS INITIAL.
    wa_export_rszrange-desc_purpose = ' '.
  ELSE.
    CASE wa_export_rszrange-purpose_selection.
      WHEN '0'.
        wa_export_rszrange-desc_purpose = 'Default from Releases < Apollo (Corres. Selection Area)'.
      WHEN '1'.
        wa_export_rszrange-desc_purpose = 'Selection Area (Filter)'.
      WHEN '2'.
        wa_export_rszrange-desc_purpose = 'Start Value'.
      WHEN '3'.
        wa_export_rszrange-desc_purpose = 'Both (1 and 2, Only in RSZSELECT)'.
      WHEN OTHERS.
        wa_export_rszrange-desc_purpose = 'Unknown type'.
    ENDCASE.
 ENDIF.

 IF wa_export_rszrange-factor_type IS INITIAL OR wa_export_rszrange-factor_type = '0'.
    wa_export_rszrange-desc_factor_type = 'Numerical expression   (Key figure <|>|.. numerical Literal)'.
  ELSE.
    CASE wa_export_rszrange-factor_type.
      WHEN '1'.
        wa_export_rszrange-desc_factor_type  = 'Characteristic Value'.
      WHEN '2'.
        wa_export_rszrange-desc_factor_type  = 'Structure Member'.
      WHEN '3'.
        wa_export_rszrange-desc_factor_type  = 'Attrib. Value'.
      WHEN OTHERS.
        wa_export_rszrange-desc_factor_type  = 'Unknown type'.
    ENDCASE.
 ENDIF.

 IF wa_export_rszrange-selection_type IS INITIAL.
    wa_export_rszrange-desc_selection_type = ' '.
  ELSE.
    CASE wa_export_rszrange-selection_type.
      WHEN '1'.
        wa_export_rszrange-desc_selection_type = 'Area Selection'.
      WHEN '2'.
        wa_export_rszrange-desc_selection_type = 'Nodes'.
      WHEN '3'.
        wa_export_rszrange-desc_selection_type = 'Compounded Value'.
      WHEN '4'.
        wa_export_rszrange-desc_selection_type = 'Selection Option'.
      WHEN '5'.
        wa_export_rszrange-desc_selection_type = 'Calculated Key Figure'.
      WHEN '6'.
        wa_export_rszrange-desc_selection_type = 'Basic Key Figure'.
      WHEN '7'.
        wa_export_rszrange-desc_selection_type = 'Restricted Key Figure'.
      WHEN '8'.
        wa_export_rszrange-desc_selection_type = 'Query Variable'.
      WHEN OTHERS.
        wa_export_rszrange-desc_selection_type = 'Unknown type'.
    ENDCASE.
 ENDIF.

    CASE wa_export_rszrange-select_sign.
      WHEN 'I'.
        wa_export_rszrange-desc_select_sign = 'Include'.
      WHEN 'E'.
        wa_export_rszrange-desc_select_sign = 'Exclude'.
      WHEN OTHERS.
        wa_export_rszrange-desc_select_sign = 'Unknown type'.
    ENDCASE.

    CASE wa_export_rszrange-operator.
      WHEN 'EQ'.
        wa_export_rszrange-desc_operator = 'Equal: Single Value'.
      WHEN 'NE'.
        wa_export_rszrange-desc_operator = 'Not Equal: Everything Apart from the Specified Single Value'.
      WHEN 'BT'.
        wa_export_rszrange-desc_operator = 'Between: Range of Values'.
      WHEN 'NB'.
        wa_export_rszrange-desc_operator = 'Not Between: Everything Outside the Range'.
      WHEN 'LE'.
        wa_export_rszrange-desc_operator = 'Less or Equal: Everything <= Value in Field LOW'.
      WHEN 'GT'.
        wa_export_rszrange-desc_operator = 'Greater Than: Everything > Value in Field LOW'.
      WHEN 'GE'.
        wa_export_rszrange-desc_operator = 'Greater or Equal: Everything >= Value in Field LOW'.
      WHEN 'LT'.
        wa_export_rszrange-desc_operator = 'Less Than: Everything < Value in Field LOW'.
      WHEN 'CP'.
        wa_export_rszrange-desc_operator = 'Contains Pattern: Masked Input: Find Pattern'.
      WHEN 'NP'.
        wa_export_rszrange-desc_operator = 'Not Contains Pattern: Masked Input: Reject Pattern'.
      WHEN OTHERS.
        wa_export_rszrange-desc_operator = 'Unknown type'.
    ENDCASE.

    CASE wa_export_rszrange-lower_flag.
      WHEN '0'.
        wa_export_rszrange-desc_lower_flag = 'Blank'.
      WHEN '1'.
        wa_export_rszrange-desc_lower_flag = 'Value'.
      WHEN '2'.
        wa_export_rszrange-desc_lower_flag = 'CIN Link'.
      WHEN '3'.
        wa_export_rszrange-desc_lower_flag = 'Variable CIN'.
      WHEN '4'.
        wa_export_rszrange-desc_lower_flag = 'InfoObject'.
      WHEN '5'.
        wa_export_rszrange-desc_lower_flag = 'Constant'.
      WHEN '6'.
        wa_export_rszrange-desc_lower_flag = 'Exit variable name (screen filter)'.
      WHEN OTHERS.
        wa_export_rszrange-desc_lower_flag = 'Unknown type'.
    ENDCASE.

    CASE wa_export_rszrange-higher_flag.
      WHEN '0'.
        wa_export_rszrange-desc_higher_flag = 'Blank'.
      WHEN '1'.
        wa_export_rszrange-desc_higher_flag = 'Value'.
      WHEN '2'.
        wa_export_rszrange-desc_higher_flag = 'CIN Link'.
      WHEN '3'.
        wa_export_rszrange-desc_higher_flag = 'Variable CIN'.
      WHEN '4'.
        wa_export_rszrange-desc_higher_flag = 'InfoObject'.
      WHEN '5'.
        wa_export_rszrange-desc_higher_flag = 'Constant'.
      WHEN '6'.
        wa_export_rszrange-desc_higher_flag = 'Exit variable name (screen filter)'.
      WHEN OTHERS.
        wa_export_rszrange-desc_higher_flag = 'Unknown type'.
    ENDCASE.

    CASE wa_export_rszrange-alert_level.
      WHEN '00' OR '0'.
        wa_export_rszrange-desc_alert_level = 'No Alert'.
      WHEN '01' OR '1'.
        wa_export_rszrange-desc_alert_level = 'Good 1'.
      WHEN '02' OR '2'.
        wa_export_rszrange-desc_alert_level = 'Good 2'.
      WHEN '03' OR '3'.
        wa_export_rszrange-desc_alert_level = 'Good 3'.
      WHEN '04' OR '4'.
        wa_export_rszrange-desc_alert_level = 'Critical 1'.
      WHEN '05' OR '5'.
        wa_export_rszrange-desc_alert_level = 'Critical 2'.
      WHEN '06' OR '6'.
        wa_export_rszrange-desc_alert_level = 'Critical 3'.
      WHEN '07' OR '7'.
        wa_export_rszrange-desc_alert_level = 'Bad 1'.
      WHEN '08' OR '8'.
        wa_export_rszrange-desc_alert_level = 'Bad 2'.
      WHEN '09' OR '9'.
        wa_export_rszrange-desc_alert_level = 'Bad 3'.
      WHEN OTHERS.
        wa_export_rszrange-desc_alert_level = 'Unknown type'.
    ENDCASE.

  MODIFY lt_export_rszrange FROM wa_export_rszrange.
ENDLOOP.

 " 6. Order and delete duplicates
SORT lt_export_rszrange BY elementid info_object sequence_num.
DELETE ADJACENT DUPLICATES FROM lt_export_rszrange COMPARING elementid info_object sequence_num.

*********OUTPUT FILE 5**********
CLEAR LV_COUNT.
lv_file_name = 'Queries_5_Restricted_Key_Fig.csv'.
CONCATENATE LV_ROOT lv_file_name INTO lv_path.
OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
IF sy-subrc = 0.

  "Header
  CONCATENATE 'Element ID' 'InfoObject' 'Detail InfoObject' 'Base InfoObject' 'Sequence Number' 'Purpose Selection' 'Desc. Purpose'
              'Factor Type' 'Desc. Factor Type' 'Selection Type' 'Desc. Selection Type' 'Select Sign' 'Desc. Select Sign' 'Operator' 'Desc. Operator'
              'Lower Value' 'Higher Value' 'Lower Flag' 'Desc Lower Flag' 'Higher Flag' 'Desc. Higher Flag' 'Hierarchy Name' 'Alert Level'
              'Desc. Alert Level' 'Desc. Element' INTO lv_string SEPARATED BY ';'.
  TRANSFER lv_string TO lv_path.

  LOOP AT lt_export_rszrange INTO wa_export_rszrange.
    CONCATENATE wa_export_rszrange-elementid
                wa_export_rszrange-info_object
                wa_export_rszrange-info_object_detail
                wa_export_rszrange-base_info_object
                wa_export_rszrange-sequence_num
                wa_export_rszrange-purpose_selection
                wa_export_rszrange-desc_purpose
                wa_export_rszrange-factor_type
                wa_export_rszrange-desc_factor_type
                wa_export_rszrange-selection_type
                wa_export_rszrange-desc_selection_type
                wa_export_rszrange-select_sign
                wa_export_rszrange-desc_select_sign
                wa_export_rszrange-operator
                wa_export_rszrange-desc_operator
                wa_export_rszrange-lower_value
                wa_export_rszrange-higher_value
                wa_export_rszrange-lower_flag
                wa_export_rszrange-desc_lower_flag
                wa_export_rszrange-higher_flag
                wa_export_rszrange-desc_higher_flag
                wa_export_rszrange-hierarchy_name
                wa_export_rszrange-alert_level
                wa_export_rszrange-desc_alert_level
                wa_export_rszrange-txtlg
                INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.
    LV_COUNT = LV_COUNT + 1.
  ENDLOOP.

  CLOSE DATASET lv_path.
    WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

ELSE.
    WRITE: / 'Error in file generation'.
ENDIF.

*********************************
*********************************
**********FILE 6*****************
*********************************
" 1 Get all valid IDs
CLEAR lt_texts_rszeltdir.
CLEAR wa_texts_rszeltdir.
SELECT *
  FROM rszeltdir
  INTO TABLE lt_texts_rszeltdir
  WHERE objvers = 'A'.

CLEAR lt_valid_ids_rszcalc.
IF lt_texts_rszeltdir IS NOT INITIAL.
  SELECT *
    FROM rszcalc
    INTO TABLE lt_valid_ids_rszcalc
    FOR ALL ENTRIES IN lt_texts_rszeltdir
    WHERE eltuid = lt_texts_rszeltdir-eltuid
      AND objvers = 'A'.
ENDIF.

" 2. Build lt_export_rszcalc using those IDs
CLEAR lt_export_rszcalc.
LOOP AT lt_valid_ids_rszcalc INTO wa_valid_rszcalc.
  CLEAR wa_export_rszcalc.
  wa_export_rszcalc-elementid     = wa_valid_rszcalc-eltuid.
  wa_export_rszcalc-formula_stage = wa_valid_rszcalc-stepnr.
  APPEND wa_export_rszcalc TO lt_export_rszcalc.
ENDLOOP.

" 3. Texts
IF lt_export_rszcalc IS NOT INITIAL.
  CLEAR lt_texts_rszelttxt.
  SELECT *
    FROM rszelttxt
    INTO TABLE lt_texts_rszelttxt
    FOR ALL ENTRIES IN lt_export_rszcalc
    WHERE eltuid  = lt_export_rszcalc-elementid
      AND objvers = 'A'
      AND langu   = LANGUAGE.

  CLEAR lt_texts_rszcalc.
  SELECT *
    FROM rszcalc
    INTO TABLE lt_texts_rszcalc
    FOR ALL ENTRIES IN lt_export_rszcalc
    WHERE eltuid  = lt_export_rszcalc-elementid
      AND objvers = 'A'.
ENDIF.

" 4. Joins
CLEAR wa_texts_rszelttxt.
LOOP AT lt_export_rszcalc INTO wa_export_rszcalc.
  READ TABLE lt_texts_rszelttxt INTO wa_texts_rszelttxt
       WITH KEY eltuid = wa_export_rszcalc-elementid.
  IF sy-subrc = 0.
    REPLACE ALL OCCURRENCES OF ';'
        IN wa_texts_rszelttxt-txtlg WITH ', '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN wa_texts_rszelttxt-txtlg WITH ' '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
        IN wa_texts_rszelttxt-txtlg WITH ' '.
        wa_export_rszcalc-txtlg = wa_texts_rszelttxt-txtlg.
  ENDIF.
  MODIFY lt_export_rszcalc FROM wa_export_rszcalc.
ENDLOOP.

LOOP AT lt_export_rszcalc INTO wa_export_rszcalc.
  READ TABLE lt_texts_rszcalc INTO wa_texts_rszcalc
       WITH KEY eltuid = wa_export_rszcalc-elementid
                stepnr = wa_export_rszcalc-formula_stage.
  IF sy-subrc = 0.
    wa_export_rszcalc-form_operator           = wa_texts_rszcalc-opera.
    wa_export_rszcalc-type_operand            = wa_texts_rszcalc-o1flg.
    wa_export_rszcalc-form_operand            = wa_texts_rszcalc-oper1.
    wa_export_rszcalc-type_operand_2          = wa_texts_rszcalc-o2flg.
    wa_export_rszcalc-form_operand_2          = wa_texts_rszcalc-oper2.
    wa_export_rszcalc-calculate               = wa_texts_rszcalc-callate.
    wa_export_rszcalc-simple_formula          = wa_texts_rszcalc-simpl.
    wa_export_rszcalc-non_comulative_flag     = wa_texts_rszcalc-ncumfl.
    wa_export_rszcalc-aggregation             = wa_texts_rszcalc-aggrgen.
    wa_export_rszcalc-exception_aggregation   = wa_texts_rszcalc-aggrexc.
    wa_export_rszcalc-ref_chrctic_exc_agg     = wa_texts_rszcalc-aggrcha.
    wa_export_rszcalc-ref_chrctic_exc_agg_2   = wa_texts_rszcalc-aggrcha2.
    wa_export_rszcalc-ref_chrctic_exc_agg_3   = wa_texts_rszcalc-aggrcha3.
    wa_export_rszcalc-ref_chrctic_exc_agg_4   = wa_texts_rszcalc-aggrcha4.
    wa_export_rszcalc-ref_chrctic_exc_agg_5   = wa_texts_rszcalc-aggrcha5.
  ENDIF.
  MODIFY lt_export_rszcalc FROM wa_export_rszcalc.
ENDLOOP.

" 5. Descriptions
LOOP AT lt_export_rszcalc INTO wa_export_rszcalc.
 IF wa_export_rszcalc-type_operand IS INITIAL OR wa_export_rszcalc-type_operand  = ' '.
    wa_export_rszcalc-desc_type_operand  = 'Blank'.
  ELSE.
    CASE wa_export_rszcalc-type_operand.
      WHEN 'B'.
        wa_export_rszcalc-desc_type_operand  = 'Basis'.
      WHEN 'H'.
        wa_export_rszcalc-desc_type_operand  = 'Formula stage'.
      WHEN 'C'.
        wa_export_rszcalc-desc_type_operand  = 'Constant'.
      WHEN 'V'.
        wa_export_rszcalc-desc_type_operand  = 'Variable'.
      WHEN 'I'.
        wa_export_rszcalc-desc_type_operand  = 'InfoObject'.
      WHEN 'K'.
        wa_export_rszcalc-desc_type_operand  = 'Formula'.
      WHEN OTHERS.
        wa_export_rszcalc-desc_type_operand  = 'Unknown type'.
    ENDCASE.
 ENDIF.

 IF wa_export_rszcalc-type_operand_2 IS INITIAL OR wa_export_rszcalc-type_operand_2  = ' '.
    wa_export_rszcalc-desc_type_operand_2  = 'Blank'.
  ELSE.
    CASE wa_export_rszcalc-type_operand_2.
      WHEN 'B'.
        wa_export_rszcalc-desc_type_operand_2  = 'Basis'.
      WHEN 'H'.
        wa_export_rszcalc-desc_type_operand_2  = 'Formula stage'.
      WHEN 'C'.
        wa_export_rszcalc-desc_type_operand_2  = 'Constant'.
      WHEN 'V'.
        wa_export_rszcalc-desc_type_operand_2  = 'Variable'.
      WHEN 'I'.
        wa_export_rszcalc-desc_type_operand_2  = 'InfoObject'.
      WHEN 'K'.
        wa_export_rszcalc-desc_type_operand_2  = 'Formula'.
      WHEN OTHERS.
        wa_export_rszcalc-desc_type_operand_2  = 'Unknown type'.
    ENDCASE.
 ENDIF.

  IF wa_export_rszcalc-non_comulative_flag IS INITIAL OR wa_export_rszcalc-non_comulative_flag  = ' '.
    wa_export_rszcalc-desc_non_cum_flag  = 'Cumulative value'.
  ELSE.
    CASE wa_export_rszcalc-non_comulative_flag.
      WHEN '1'.
        wa_export_rszcalc-desc_non_cum_flag  = 'Stock with change involving stocks'.
      WHEN '2'.
        wa_export_rszcalc-desc_non_cum_flag  = 'Stock with inward and outward movement'.
      WHEN OTHERS.
        wa_export_rszcalc-desc_non_cum_flag  = 'Unknown type'.
    ENDCASE.
 ENDIF.

  IF wa_export_rszcalc-aggregation IS INITIAL.
    wa_export_rszcalc-desc_aggregation  = ' '.
  ELSE.
    CASE wa_export_rszcalc-aggregation.
      WHEN 'MAX'.
        wa_export_rszcalc-desc_aggregation  = 'Maximum'.
      WHEN 'MIN'.
        wa_export_rszcalc-desc_aggregation  = 'Minimum'.
      WHEN 'NGA'.
        wa_export_rszcalc-desc_aggregation  = 'No Hierarchy Aggregation (Only to be Used Internally)'.
      WHEN 'NHA'.
        wa_export_rszcalc-desc_aggregation  = 'No Hierarchy Aggregation (Only to be Used Internally)'.
      WHEN 'NO1'.
        wa_export_rszcalc-desc_aggregation  = 'No Aggregation (X, If More Than One Record Occurs)'.
      WHEN 'NO2'.
        wa_export_rszcalc-desc_aggregation  = 'No Aggregation (X, If More Than One Value Occurs)'.
      WHEN 'NOP'.
        wa_export_rszcalc-desc_aggregation  = 'No Aggregation (X, If More Than One Value Uneq. to 0 Occurs)'.
      WHEN 'SUM'.
        wa_export_rszcalc-desc_aggregation  = 'Sum'.
      WHEN OTHERS.
        wa_export_rszcalc-desc_aggregation  = 'Unknown type'.
    ENDCASE.
 ENDIF.

   IF wa_export_rszcalc-exception_aggregation IS INITIAL.
    wa_export_rszcalc-desc_aggregation  = ' '.
  ELSE.
    CASE wa_export_rszcalc-exception_aggregation.
      WHEN 'AV0'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Average (values not equal to zero)'.
      WHEN 'AV1'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Average (weighted with the number of days)'.
      WHEN 'AV2'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Average (Weighted with No. of Working Days; Factory Calcul.)'.
      WHEN 'AVG'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Average (all values)'.
      WHEN 'CN0'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Counter (Values Unequal to Zero )'.
      WHEN 'CNT'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Counter (all values)'.
      WHEN 'FIR'.
        wa_export_rszcalc-desc_exc_aggregation  = 'First Value'.
      WHEN 'LAS'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Last Value'.
      WHEN 'MAX'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Maximum'.
      WHEN 'MIN'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Minimum'.
      WHEN 'NO1'.
        wa_export_rszcalc-desc_exc_aggregation  = 'No Aggregation (X, If More Than One Record Occurs)'.
      WHEN 'NO2'.
        wa_export_rszcalc-desc_exc_aggregation  = 'No Aggregation (X, If More Than One Value Occurs)'.
      WHEN 'NOP'.
        wa_export_rszcalc-desc_exc_aggregation  = 'No Aggregation (X, If More Than One Value Uneq. to 0 Occurs)'.
      WHEN 'STD'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Standard Deviation'.
      WHEN 'SUM'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Summation'.
      WHEN 'VAR'.
        wa_export_rszcalc-desc_exc_aggregation  = 'Variance'.
      WHEN OTHERS.
        wa_export_rszcalc-desc_exc_aggregation  = 'Unknown type'.
    ENDCASE.
 ENDIF.

  MODIFY lt_export_rszcalc FROM wa_export_rszcalc.
ENDLOOP.

 "6. Order and delete duplicates
SORT lt_export_rszcalc BY elementid formula_stage.
DELETE ADJACENT DUPLICATES FROM lt_export_rszcalc COMPARING elementid formula_stage.

*********OUTPUT FILE 6**********
CLEAR LV_COUNT.
lv_file_name = 'Queries_6_Calculated_Key_Fig.csv'.
CONCATENATE LV_ROOT lv_file_name INTO lv_path.
OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
IF sy-subrc = 0.

  "Header
  CONCATENATE 'Element ID' 'Formula Stage' 'Formula Operator' 'Type Operand' 'Desc. Type Operand' 'Formula Operand' 'Type Operand 2'
              'Desc. Type Operand 2' 'Formula Operand 2' 'Calculate' 'Simple Formula' 'Non Cumulative Flag' 'Desc. Non Cumulative Flag'
              'Aggregation' 'Desc. Aggregation' 'Exception Aggregation' 'Desc. Exception Aggregation'
              'Ref. Charact. Exception Aggregation' 'Ref. Charact. Exception Aggregation 2' 'Ref. Charact. Exception Aggregation 3'
              'Ref. Charact. Exception Aggregation 4' 'Ref. Charact. Exception Aggregation 5' 'Desc. Element' INTO lv_string SEPARATED BY ';'.
  TRANSFER lv_string TO lv_path.

  LOOP AT lt_export_rszcalc INTO wa_export_rszcalc.
    CONCATENATE wa_export_rszcalc-elementid
                wa_export_rszcalc-formula_stage
                wa_export_rszcalc-form_operator
                wa_export_rszcalc-type_operand
                wa_export_rszcalc-desc_type_operand
                wa_export_rszcalc-form_operand
                wa_export_rszcalc-type_operand_2
                wa_export_rszcalc-desc_type_operand_2
                wa_export_rszcalc-form_operand_2
                wa_export_rszcalc-calculate
                wa_export_rszcalc-simple_formula
                wa_export_rszcalc-non_comulative_flag
                wa_export_rszcalc-desc_non_cum_flag
                wa_export_rszcalc-aggregation
                wa_export_rszcalc-desc_aggregation
                wa_export_rszcalc-exception_aggregation
                wa_export_rszcalc-desc_exc_aggregation
                wa_export_rszcalc-ref_chrctic_exc_agg
                wa_export_rszcalc-ref_chrctic_exc_agg_2
                wa_export_rszcalc-ref_chrctic_exc_agg_3
                wa_export_rszcalc-ref_chrctic_exc_agg_4
                wa_export_rszcalc-ref_chrctic_exc_agg_5
                wa_export_rszcalc-txtlg
                INTO lv_string SEPARATED BY ';'.
    TRANSFER lv_string TO lv_path.
    LV_COUNT = LV_COUNT + 1.
  ENDLOOP.

  CLOSE DATASET lv_path.
    WRITE: / 'The file named', lv_file_name, 'has been generated correctly with', lv_count, 'lines'.

ELSE.
    WRITE: / 'Error in file generation'.
ENDIF.

****************************************************************************************************************************

WHEN OTHERS.
  WRITE: / 'Incorrect option'.

ENDCASE.


*IMPROVEMENT AREAS
*& In "Datasources":
* - Declare a table that contains the output fields and make a logic that in the case of the
*     TXTLG being empty, it picks up the text from field TXTSH or the description in second language.
*
*&  In "InfoObjects":
* - Declare a table with the structure of the output fields for consistency's sake
*     and read TXTSH field or second language variant in the case that TXTLG is empty.
*
*& In "InfoSource with InfoObjects"
* - Implement the logic for reading the second language description in case that
*     there is no description for the object  in the main language.
*
*& In "DTPs":
* - Declare a table with the structure of the output fields for consistency's sake
*     and read TXTSH field or second language variant in the case that TXTLG is empty.
*
*& In "Infosets with chars":
* - Implement the logic for reading the second language description in case that
*    there is no description for the object  in the main language.
