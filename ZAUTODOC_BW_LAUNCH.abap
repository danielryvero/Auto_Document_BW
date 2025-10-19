*&---------------------------------------------------------------------*
*& Report ZAUTODOC_BW_LAUNCH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*& What it does:
* This program launches the ZAUTODOC_BW program in a background Job.
******************
*& How to use it:
* 1.  Go to ZAUTODOC_BW program, to line 343 and 344.
* 2.  For this program to work properly, line 343, that contains "nopopup = 'X'."
*     must be uncommented and line 344, that states "nopopup = ''."
*     must be commented. So change that accordingly.
* 3.  Execute this program (F8) and follow instructions to see output comments
* 4.  Do /OAL11 and check output files, they are named:
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
************************************************************************

REPORT ZAUTODOC_BW_LAUNCH.

TYPES: ty_code TYPE c LENGTH 1.

DATA: lt_values  TYPE STANDARD TABLE OF ty_code WITH EMPTY KEY,
      lv_value   TYPE ty_code,
      lv_jobname TYPE tbtcjob-jobname VALUE 'ZAUTODOC_BW_BATCH',
      lv_jobcount TYPE tbtcjob-jobcount.

*--- Fill out with codes present in ZAUTODOC_BW
lt_values = VALUE #(
  ( 'S' ) ( 'X' ) ( 'B' ) ( 'C' ) ( 'D' ) ( 'Y' )
  ( 'F' ) ( 'I' ) ( 'Z' ) ( 'P' ) ( 'N' ) ( 'E' ) ( 'Q' )
).

*--- 1. Open background job
CALL FUNCTION 'JOB_OPEN'
  EXPORTING
    jobname  = lv_jobname
    jobclass = 'C'
  IMPORTING
    jobcount = lv_jobcount
  EXCEPTIONS
    OTHERS   = 1.

IF sy-subrc <> 0.
  MESSAGE 'Job batch could not be created' TYPE 'E'.
  EXIT.
ENDIF.

*--- 2. Add a step for each value
LOOP AT lt_values INTO lv_value.
  SUBMIT ztest_objs
         WITH obj_type = lv_value
         WITH nopopup  = 'X'
         VIA JOB lv_jobname
         NUMBER lv_jobcount
         AND RETURN.
ENDLOOP.

*--- 3. Close and kick off the job
CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
    jobname   = lv_jobname
    jobcount  = lv_jobcount
    strtimmed = 'X'     "immediate
  EXCEPTIONS
    OTHERS    = 1.

IF sy-subrc = 0.
  WRITE: / |Job { lv_jobname } ({ lv_jobcount }) kicked off correctly|.
ELSE.
  WRITE: / 'Error in closing/kicking off the batch job'.
ENDIF.

*--- 4. Message at the end of the program
WRITE: /.
WRITE: / 'Program ZAUTODOC_BW_LAUNCH has finished.',
       / 'Follow steps below if you want to check output comments'.
WRITE: /.
WRITE: / '1. Now go to transaction /OSM37.',
       / '2. Paste the job name ZAUTODOC_BW_BATCH and hit Execute (F8).',
       / '3. Double-click the icon in the Spool column of the row whose Start Time',
       / 'matches the job you just executed.',
       / '4. Select the program you want to check and click the Spool icon above',
       / '    the table to view the comment.',
       / '5. In the column "Type", you can click on the icon to see the output comments'.
WRITE: /.
WRITE: / 'Do /OAL11 and check output files, they are named:',
       /  '--------------------------------------',
       /  'InfoSets_with_IOs.csv',
       /  'InfoSource_with_IOBs.csv',
       /  'List_Of_Active_ADSO_With_IOBs.csv',
       /  'List_Of_Active_ADSOs.csv',
       /  'List_Of_Active_Cubes.csv',
       /  'List_Of_Cubes_With_IOBs.csv',
       /  'List_Of_Active_DTPs.csv',
       /  'List_Of_Active_DataSources.csv',
       /  'List_Of_Active_InfoObjects.csv',
       /  'List_Of_Active_InfoSources.csv',
       /  'List_Of_Active_Transformations.csv',
       /  'List_Of_Transformation_Code.csv',
       /  'Queries_1_List_Of_All_Queries.csv',
       /  'Queries_2_First_Lvl_Elem.csv',
       /  'Queries_3_All_Elem_By_Query.csv',
       /  'Queries_4_Detail_Of_Elem_By_ID.csv',
       /  'Queries_5_Restricted_Key_Fig.csv',
       /  'Queries_6_Calculated_Key_Fig.csv'.
