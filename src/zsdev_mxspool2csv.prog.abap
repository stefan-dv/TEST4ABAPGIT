REPORT  zsdev_mxspool2csv.
*
*&—————————————————————————————————————————————————————————————————————*
*& Report  ZMXSPOOL2CSV                                                *
*&                                                                     *
*&—————————————————————————————————————————————————————————————————————*
*& Converts spool request into csv document and emails it to           *
*& recipicant.                                                         *
*&                                                                     *
*& Execution                                                           *
*&—————————————————————————————————————————————————————————————————————*
*& This program must be run as a background job in-order for the write *
*& commands to create a Spool request rather than be displayed on      *
*& screen                                                              *
*&—————————————————————————————————————————————————————————————————————*

TABLES: tsp01.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-bl1
                                    NO INTERVALS.
PARAMETER: p_job    LIKE tbtcp-jobname.
PARAMETER: p_job_al AS CHECKBOX.         "Mail spools of all job steps
PARAMETER: p_step   LIKE tbtcp-stepcount.
PARAMETER: p_spono  LIKE tsp01-rqident.
PARAMETER: p_delspl  AS CHECKBOX.
PARAMETER: p_otf     AS CHECKBOX.
PARAMETER: p_mlnout  AS CHECKBOX.  "Send mail when no spool output
PARAMETER: p_mlnoui  LIKE solisti1-line. "Inform. text when no output
PARAMETER: p_skphdr  AS CHECKBOX.
PARAMETER: p_skip    TYPE i.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-bl2
                                    NO INTERVALS.
PARAMETER: p_email0 LIKE somlreci1-receiver.
*            DEFAULT 'user@umicore.com'.
PARAMETER: p_email1 LIKE somlreci1-receiver.
PARAMETER: p_email2 LIKE somlreci1-receiver.
PARAMETER: p_email3 LIKE somlreci1-receiver.
PARAMETER: p_email4 LIKE somlreci1-receiver.
PARAMETER: p_email5 LIKE somlreci1-receiver.
PARAMETER: p_email6 LIKE somlreci1-receiver.
PARAMETER: p_email7 LIKE somlreci1-receiver.
PARAMETER: p_email8 LIKE somlreci1-receiver.
PARAMETER: p_email9 LIKE somlreci1-receiver.
PARAMETER: p_sender LIKE somlreci1-receiver.

PARAMETER: p_subj   LIKE sodocchgi1-obj_descr
            DEFAULT 'Subject'.
PARAMETER: p_body1  LIKE solisti1-line
            DEFAULT 'Message Body text, line 1'.
PARAMETER: p_body2  LIKE solisti1-line
            DEFAULT 'Message Body text, line 2'.
PARAMETER: p_body3  LIKE solisti1-line
            DEFAULT 'Message Body text, line 3'.
PARAMETER: p_atdesc LIKE solisti1
            DEFAULT 'Example.csv document attachment'.
PARAMETER: p_atname LIKE solisti1
            DEFAULT 'Attachname'.
SELECTION-SCREEN END OF BLOCK bl2.


*DATA DECLARATION
DATA: gd_recsize TYPE i.

* Spool IDs
TYPES: BEGIN OF t_tbtcp.
    INCLUDE STRUCTURE tbtcp.
TYPES: END OF t_tbtcp.
DATA: it_tbtcp TYPE STANDARD TABLE OF t_tbtcp INITIAL SIZE 0,
      wa_tbtcp TYPE t_tbtcp.
DATA: w_spool_nr LIKE tsp01-rqident.

* Job Runtime Parameters
DATA: gd_eventid                 LIKE tbtcm-eventid,
      gd_eventparm               LIKE tbtcm-eventparm,
      gd_external_program_active LIKE tbtcm-xpgactive,
      gd_jobcount                LIKE tbtcm-jobcount,
      gd_jobname                 LIKE tbtcm-jobname,
      gd_stepcount               LIKE tbtcm-stepcount,
      gd_jobname_current         LIKE tbtcm-jobname,
      gd_stepcount_current       LIKE tbtcm-stepcount,
      gd_error                   TYPE sy-subrc,
      gd_reciever                TYPE sy-subrc.

DATA:  w_recsize TYPE i.

DATA: tab_lines LIKE sy-tabix.
DATA: tab_lines1 LIKE sy-tabix.

DATA: gd_subject         LIKE sodocchgi1-obj_descr,
      it_mess_bod        LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      it_mess_att        LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      gd_sender_type     LIKE soextreci1-adr_typ,
      gd_attachment_desc TYPE so_obj_nam,
      gd_attachment_name TYPE so_obj_des.

DATA: BEGIN OF it_attach OCCURS 0,
        spool_id    LIKE tsp01-rqident,
        content_bin LIKE solisti1 OCCURS 0.
DATA: END   OF it_attach.


*Printer params
DATA: params   LIKE pri_params,
      days(1)  TYPE n VALUE 2,
      count(3) TYPE n VALUE 1,
      valid    TYPE c.

DATA: gd_objtype LIKE rststype-type.

* Spool to csv conversions
DATA: gd_spool_nr    LIKE tsp01-rqident,
      gd_destination LIKE rlgrap-filename,
      gd_bytecount   LIKE tst01-dsize,
      gd_buffer      TYPE string.

DATA: BEGIN OF it_spools OCCURS 0,
        spool_id LIKE tsp01-rqident,
      END   OF it_spools.

* Binary store for csv
DATA: BEGIN OF it_csv_output OCCURS 0.
    INCLUDE STRUCTURE tline.
DATA: END OF it_csv_output.

RANGES: rgd_stepcount FOR tbtcp-stepcount.

CONSTANTS: c_dev       LIKE  sy-sysid VALUE 'DEV',
           c_no(1)     TYPE c   VALUE ' ',
           c_device(4) TYPE c   VALUE 'LOCL'.
************************************************************************
*START-OF-SELECTION.
START-OF-SELECTION.

  IF sy-batch EQ 'X'.
    IF p_spono IS INITIAL.
* Spool number is primary
      IF p_job IS INITIAL.
* Current Job
        PERFORM get_job_details.
        IF p_step IS INITIAL.
          IF NOT p_job_al IS INITIAL.
* Previous step
            CLEAR rgd_stepcount.
            rgd_stepcount-sign = 'I'.
            rgd_stepcount-option = 'LT'.
            rgd_stepcount-low = gd_stepcount.
            APPEND rgd_stepcount.
          ELSE.
*          gd_stepcount = gd_stepcount - 1.
            CLEAR rgd_stepcount.
            rgd_stepcount-sign = 'I'.
            rgd_stepcount-option = 'EQ'.
            rgd_stepcount-low = gd_stepcount - 1.
            APPEND rgd_stepcount.
          ENDIF.
        ELSE.
*          gd_stepcount = p_step.
          CLEAR rgd_stepcount.
          rgd_stepcount-sign = 'I'.
          rgd_stepcount-option = 'EQ'.
          rgd_stepcount-low = p_step.
          APPEND rgd_stepcount.
        ENDIF.
      ELSE.
        gd_jobname = p_job.
        IF p_step IS INITIAL.
          IF NOT p_job_al IS INITIAL.
            REFRESH: rgd_stepcount.
          ELSE.
* Both step and job are required
            SKIP.
            WRITE:/ 'Program needs job & step in-order for spool determination'
                                                                               .
          ENDIF.
          STOP.
        ELSE.
*          gd_stepcount = p_step.
          CLEAR rgd_stepcount.
          rgd_stepcount-sign = 'I'.
          rgd_stepcount-option = 'EQ'.
          rgd_stepcount-low = p_step.
          APPEND rgd_stepcount.
        ENDIF.

        PERFORM get_other_job_details.

      ENDIF.

      PERFORM obtain_spool_id.
* for debugging => Uncomment next statements
      SKIP.
      WRITE:/ 'Job Name     :', gd_jobname.
      WRITE:/ 'Step Number  :', gd_stepcount.
      WRITE:/ 'Job Number   :', gd_jobcount.
      WRITE:/ 'Spool Number :', gd_spool_nr.
*      IF gd_spool_nr IS INITIAL.
      IF it_spools[] IS INITIAL and
         p_mlnout    IS INITIAL.
***********************************
** If batch spool number must be found.
************************************
        SKIP.
        WRITE:/ 'No spool found'.
        STOP.
      ENDIF.
    ELSE.
*      gd_spool_nr = p_spono.
      it_spools-spool_id = p_spono.
      APPEND it_spools.
    ENDIF.
  ELSE.
    IF p_spono IS INITIAL.
***********************************
** If interactive spool number must be filled in.
************************************
      SKIP.
      WRITE:/ 'Program must be executed in background  OR '.
      WRITE:/ '  spool-number must be filled in.'.
      STOP.
    ELSE.
*      gd_spool_nr = p_spono.
      it_spools-spool_id = p_spono.
      APPEND it_spools.
    ENDIF.
  ENDIF.

  REFRESH: it_attach.
  LOOP AT it_spools.

    REFRESH: it_mess_att.

    gd_spool_nr = it_spools-spool_id.

    SELECT SINGLE * FROM tsp01 WHERE rqident = gd_spool_nr.
    CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
      EXPORTING
        name    = tsp01-rqo1name
        part    = 1
      IMPORTING
        objtype = gd_objtype.

    IF gd_objtype(3) = 'OTF'
    OR not p_otf is initial.
      PERFORM convert_otf_spool_to_csv.
    ELSE.
      PERFORM convert_abab_spool_to_csv.
    ENDIF.

    CHECK NOT it_mess_att[] IS INITIAL.
    it_attach-spool_id      = gd_spool_nr.
    it_attach-content_bin[] = it_mess_att[].
    APPEND it_attach.

  ENDLOOP.

  PERFORM process_email.

  IF p_delspl EQ 'X'.
    PERFORM delete_spool.
  ENDIF.

  IF sy-sysid = c_dev.
    WAIT UP TO 5 SECONDS.
    SUBMIT rsconn01 WITH mode   = 'INT'
    WITH output = 'X'
    AND RETURN.
  ENDIF.

END-OF-SELECTION.


*———————————————————————*
*  FORM obtain_spool_id *
*———————————————————————*
FORM obtain_spool_id.
  CHECK NOT ( gd_jobname IS INITIAL ).
*  CHECK NOT ( gd_jobcount IS INITIAL ).

  SELECT * FROM  tbtcp
  INTO TABLE it_tbtcp
  WHERE      jobname     = gd_jobname
  AND        jobcount    = gd_jobcount
*  AND        stepcount   = gd_stepcount
  AND        stepcount   IN rgd_stepcount
  AND        listident   <> '0000000000'
  ORDER BY  jobname
            jobcount
            stepcount.

*  READ TABLE it_tbtcp INTO wa_tbtcp INDEX 1.
*  IF sy-subrc = 0.
*    MESSAGE s004(zdd) WITH gd_spool_nr.
*    gd_spool_nr = wa_tbtcp-listident.
*    MESSAGE s004(zdd) WITH gd_spool_nr.
*  ELSE.
*    MESSAGE s005(zdd).
*  ENDIF.
  LOOP AT it_tbtcp INTO wa_tbtcp.
    it_spools-spool_id = wa_tbtcp-listident.
    APPEND it_spools.
  ENDLOOP.

ENDFORM.

*———————————————————————*
*  FORM get_job_details *
*———————————————————————*
FORM get_job_details.
* Get current job details
  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      eventid                 = gd_eventid
      eventparm               = gd_eventparm
      external_program_active = gd_external_program_active
      jobcount                = gd_jobcount
      jobname                 = gd_jobname
      stepcount               = gd_stepcount
    EXCEPTIONS
      no_runtime_info         = 1
      OTHERS                  = 2.
ENDFORM.

*———————————————————————————————*
*  FORM get_other_job_details   *
*———————————————————————————————*
FORM get_other_job_details.
* Get info of other job
  CHECK NOT ( gd_jobname IS INITIAL ).
*  CHECK NOT ( gd_stepcount IS INITIAL ).
* Get current job details
  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      eventid                 = gd_eventid
      eventparm               = gd_eventparm
      external_program_active = gd_external_program_active
      jobcount                = gd_jobcount
      jobname                 = gd_jobname_current
      stepcount               = gd_stepcount_current
    EXCEPTIONS
      no_runtime_info         = 1
      OTHERS                  = 2.
* If jobname is equal to current take this one
* else
* Get info of other job TAKE LAST SCHEDULE
  IF NOT gd_jobname_current = gd_jobname.
    SELECT * FROM  tbtcp
    INTO TABLE it_tbtcp
    WHERE      jobname     = gd_jobname
*    AND        stepcount   = gd_stepcount
    AND        stepcount   IN rgd_stepcount
    AND        listident   <> '0000000000'
    ORDER BY  sdldate DESCENDING
              sdltime DESCENDING.

    READ TABLE it_tbtcp INTO wa_tbtcp INDEX 1.
    IF sy-subrc = 0.
      gd_jobcount = wa_tbtcp-jobcount.
    ENDIF.
  ENDIF.
ENDFORM.

*—————————————————————————————————*
*  FORM convert_abab_spool_to_csv *
*—————————————————————————————————*
FORM convert_abab_spool_to_csv.

  DATA it_spool_csv type TEXT2048 OCCURS 0 WITH HEADER LINE.

*FM called that returns the Spool Request Number data into and internal table
  CALL FUNCTION 'RSPO_RETURN_ABAP_SPOOLJOB'
    EXPORTING
      rqident              = gd_spool_nr         "Spool Request Number
      first_line           = 1
    TABLES
      buffer               = it_spool_csv        "Internal table that will have the Spool Request No data
    EXCEPTIONS
      no_such_job          = 1
      not_abap_list        = 2
      job_contains_no_data = 3
      selection_empty      = 4
      no_permission        = 5
      can_not_access       = 6
      read_error           = 7
      OTHERS               = 8.

*To convert the spool data into excel format
  constants:
    con_tab  type c value cl_abap_char_utilities=>HORIZONTAL_TAB,
    con_cret type c value cl_abap_char_utilities=>CR_LF.
  Data: it_string type string,
        it_header type string.
  Data: len    type i,
        split  type i,
        offset type i.
  clear: it_header , it_string.
  LOOP AT it_spool_csv into it_string.
    if not it_string is initial.
      if not p_skphdr is initial and not it_string(1) = '|'.
        clear it_string.
      endif.
    endif.
    if p_skip is initial.
      if not it_string is initial.
        if not it_string+1(10) = '----------'.
          if not it_string = it_header.
            if it_header is initial.
              it_header = it_string.
              it_string = it_string+1.
* replace all ; by : to avoid field-separator issue
              REPLACE ALL OCCURRENCES OF ';' IN it_string WITH ':'.
              REPLACE ALL OCCURRENCES OF '|' IN it_string WITH ';'.
            else.
* replace all ; by : to avoid field-separator issue
              REPLACE ALL OCCURRENCES OF ';' IN it_string WITH ':'.
              REPLACE ALL OCCURRENCES OF '|' IN it_string WITH ';'.
              CONCATENATE con_cret it_string+1 INTO it_string.
            endif.
            len = STRLEN( it_string ).
            split = len div 255 + 1.
            clear offset.
            do split times.
              if len < 255.
                APPEND  it_string+offset(len) to it_mess_att.
              else.
                APPEND  it_string+offset(255) to it_mess_att.
              endif.
              len = len - 255.
              offset = offset + 255.
            enddo.
          endif.
        endif.
      endif.
    else.
      p_skip = p_skip - 1.
    endif.
  ENDLOOP.

ENDFORM.
	
*—————————————————————————————————*
*  FORM convert_otf_spool_to_csv *
*—————————————————————————————————*
FORM convert_otf_spool_to_csv.
  DATA it_spool_csv type soli OCCURS 0 WITH HEADER LINE.
  DATA it_spool_raw type soli OCCURS 0 WITH HEADER LINE.

*FM called that returns the Spool Request Number data into and internal table
  CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
    EXPORTING
      rqident              = gd_spool_nr       "Spool Request Number
      first_line           = 1
      desired_type         = 'RAW'
    TABLES
      buffer               = it_spool_raw      "Internal table that will have the Spool Request No data
    EXCEPTIONS
      no_such_job          = 1
      not_abap_list        = 2
      job_contains_no_data = 3
      selection_empty      = 4
      no_permission        = 5
      can_not_access       = 6
      read_error           = 7
      OTHERS               = 8.

  Data: it_string type string,
        it_header type string.

  LOOP AT it_spool_raw into it_string.
    if not it_string is initial.
      if not p_skphdr is initial and not it_string(1) = '|'.
        clear it_string.
      endif.
    endif.
    if p_skip is initial.
      if not it_string is initial.
        if not it_string+1(10) = '----------'.
          if not it_string = it_header.
            if it_header is initial.
              it_header = it_string.
              it_string = it_string+1.
* replace all ; by : to avoid field-separator issue
              REPLACE ALL OCCURRENCES OF ';' IN it_string WITH ':'.
              REPLACE ALL OCCURRENCES OF '|' IN it_string WITH ';'.
              APPEND  it_string+1 to it_spool_csv.
            else.
* replace all ; by : to avoid field-separator issue
              REPLACE ALL OCCURRENCES OF ';' IN it_string WITH ':'.
              REPLACE ALL OCCURRENCES OF '|' IN it_string WITH ';'.
              APPEND  it_string+1 to it_spool_csv.
            endif.
          endif.
        endif.
      endif.
    else.
      p_skip = p_skip - 1.
    endif.
  ENDLOOP.

*To convert the spool data into excel format
  CALL FUNCTION 'SO_RAW_TO_RTF'
    TABLES
      objcont_old = it_spool_csv   "Internal table having spool data
      objcont_new = it_mess_att.   "Int table having Excel format data converted from Spool data

ENDFORM.

*———————————————————————*
*FORM process_email                                            *
*———————————————————————*
FORM process_email.
*  DESCRIBE TABLE it_mess_att LINES gd_recsize.
*  CHECK gd_recsize > 0.
  IF p_mlnout IS INITIAL.
    CHECK NOT it_attach[] IS INITIAL.
  ENDIF.
  IF p_email0 IS INITIAL.
* Get E-mail of current user
    SELECT SINGLE smtp_addr INTO p_email0
    FROM adr6 AS a
    JOIN usr21 AS u ON
       u~persnumber = a~persnumber AND
       u~addrnumber = a~addrnumber
    WHERE u~bname = sy-uname.
  ENDIF.
  CHECK NOT p_email0 IS INITIAL
     OR NOT p_email1 IS INITIAL
     OR NOT p_email2 IS INITIAL
     OR NOT p_email3 IS INITIAL
     OR NOT p_email4 IS INITIAL
     OR NOT p_email5 IS INITIAL
     OR NOT p_email6 IS INITIAL
     OR NOT p_email7 IS INITIAL
     OR NOT p_email8 IS INITIAL
     OR NOT p_email9 IS INITIAL.

  PERFORM send_email.

ENDFORM.

*———————————————————————*
*       FORM send_email *
*———————————————————————*
*  –>  p_email          *
*———————————————————————*
FORM send_email.

  REFRESH it_mess_bod.

* Default subject matter
  gd_subject         = p_subj.
  IF it_attach[] IS INITIAL.
    it_mess_bod        = p_mlnoui.
    APPEND it_mess_bod.
  ELSE.
    gd_attachment_desc = p_atdesc.
    gd_attachment_name = p_atname.
    IF NOT ( p_body1 IS INITIAL ).
      it_mess_bod        = p_body1.
      APPEND it_mess_bod.
    ELSE.
      it_mess_bod        = 'Copy of spool in attachment'.
      APPEND it_mess_bod.
    ENDIF.
    IF NOT ( p_body2 IS INITIAL ).
      it_mess_bod        = p_body2.
      APPEND it_mess_bod.
    ENDIF.
    IF NOT ( p_body3 IS INITIAL ).
      it_mess_bod        = p_body3.
      APPEND it_mess_bod.
    ENDIF.
  ENDIF.

* If no sender specified - default blank
  IF p_sender EQ space.
    gd_sender_type  = space.
  ELSE.
    gd_sender_type  = 'INT'.
  ENDIF.

* Send file by email as .csv speadsheet
  PERFORM send_file_as_email_attachment
    TABLES it_mess_bod
*    it_mess_att
    USING
    gd_subject
    'csv'
    gd_attachment_desc
    gd_attachment_name
    p_sender
    gd_sender_type
    CHANGING gd_error.
ENDFORM.

*———————————————————————*
*     FORM delete_spool *
*———————————————————————*
FORM delete_spool.
  DATA: ld_spool_nr TYPE tsp01_sp0r-rqid_char.

  ld_spool_nr = gd_spool_nr.

  CHECK p_delspl <> c_no.
  CALL FUNCTION 'RSPO_R_RDELETE_SPOOLREQ'
    EXPORTING
      spoolid = ld_spool_nr.
ENDFORM.

*&———————————————————————*
*&      Form  SEND_FILE_AS_EMAIL_ATTACHMENT
*&———————————————————————*
*       Send email
*———————————————————————-*
FORM send_file_as_email_attachment TABLES it_message
*it_attach
USING
p_mtitle
p_format
p_filename
p_attdescription
p_sender_address
p_sender_addres_type
CHANGING p_error.

  DATA: ld_error               TYPE sy-subrc,
        ld_reciever            TYPE sy-subrc,
        ld_mtitle              LIKE sodocchgi1-obj_descr,
        ld_format              TYPE  so_obj_tp,
        ld_attdescription      TYPE  so_obj_des,
        ld_attfilename         TYPE  so_obj_des,
        ld_sender_address      LIKE  soextreci1-receiver,
        ld_sender_address_type LIKE  soextreci1-adr_typ,
        ld_receiver            LIKE  sy-subrc,
        ld_att_nbr             TYPE n.

  DATA: t_packing_list  LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        t_contents      LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        t_receivers     LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
*          t_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        t_object_header LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        t_obj_bin       LIKE solisti1 OCCURS 10 WITH HEADER LINE,
        w_cnt           TYPE i,
        w_sent_all(1)   TYPE c,
        w_doc_data      LIKE sodocchgi1.

  ld_mtitle = p_mtitle.
  ld_format              = p_format.
  ld_attdescription      = p_attdescription.
  ld_attfilename         = p_filename.
  ld_sender_address      = p_sender_address.
  ld_sender_address_type = p_sender_addres_type.

* Fill the document data.
  w_doc_data-doc_size = 1.

* Fill the document data and get size of attachment
  CLEAR w_doc_data.
  READ TABLE it_message INDEX w_cnt.
  w_doc_data-doc_size =
  ( w_cnt - 1 ) * 255 + STRLEN( it_message ).
  w_doc_data-obj_langu  = sy-langu.
  w_doc_data-obj_name   = 'SAPRPT'.
  w_doc_data-obj_descr  = ld_mtitle.
  w_doc_data-sensitivty = 'F'.

*  CLEAR t_attachment.
*  REFRESH t_attachment.
*  t_attachment[] = it_attach[].

* Describe the body of the message
  CLEAR t_packing_list.
  REFRESH t_packing_list.
  t_packing_list-transf_bin = space.
  t_packing_list-head_start = 1.
  t_packing_list-head_num = 0.
  t_packing_list-body_start = 1.
  DESCRIBE TABLE it_message LINES t_packing_list-body_num.
  t_packing_list-doc_type = 'RAW'.
  APPEND t_packing_list.

  DESCRIBE TABLE it_attach LINES w_cnt.
  LOOP AT it_attach.

    ld_att_nbr = sy-tabix.

    DESCRIBE TABLE t_obj_bin LINES tab_lines.

* Create attachment notification
    t_packing_list-transf_bin = 'X'.
    t_packing_list-head_start = 1.
    t_packing_list-head_num   = 1.
*    t_packing_list-body_start = 1.
    t_packing_list-body_start = tab_lines + 1.

*    DESCRIBE TABLE t_attachment LINES t_packing_list-body_num.
    APPEND LINES OF it_attach-content_bin[] TO t_obj_bin[].
    DESCRIBE TABLE it_attach-content_bin LINES tab_lines1.
    t_packing_list-body_num = tab_lines1.

    t_packing_list-doc_type   =  ld_format.
    IF w_cnt = 1.
      t_packing_list-obj_descr  =  ld_attdescription.
      t_packing_list-obj_name   =  ld_attfilename.
    ELSE.
      IF NOT ld_attdescription IS INITIAL.
        CONCATENATE ld_attdescription ld_att_nbr
               INTO t_packing_list-obj_descr
          SEPARATED BY '_'.
      ELSE.
        CONCATENATE 'ATT_' ld_att_nbr INTO t_packing_list-obj_descr.
      ENDIF.
    ENDIF.

    REPLACE '&DATE' WITH sy-datum INTO t_packing_list-obj_descr.
    REPLACE '&TIME' WITH sy-uzeit INTO t_packing_list-obj_descr.
    REPLACE '&USER' WITH sy-uname INTO t_packing_list-obj_descr.
    CONDENSE t_packing_list-obj_descr NO-GAPS.
    REPLACE '&JOBNAME' WITH gd_jobname(20)
                                  INTO t_packing_list-obj_descr.
    CONDENSE t_packing_list-obj_descr NO-GAPS.

    t_packing_list-doc_size   =  t_packing_list-body_num * 255.
    APPEND t_packing_list.

  ENDLOOP.

* Add the recipients email address
  CLEAR t_receivers.
  REFRESH t_receivers.

  t_receivers-rec_type = 'U'.
  t_receivers-com_type = 'INT'.
  t_receivers-notif_del = 'X'.
  t_receivers-notif_ndel = 'X'.
  IF NOT p_email0 IS INITIAL.
    t_receivers-receiver = p_email0.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email1 IS INITIAL.
    t_receivers-receiver = p_email1.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email2 IS INITIAL.
    t_receivers-receiver = p_email2.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email3 IS INITIAL.
    t_receivers-receiver = p_email3.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email4 IS INITIAL.
    t_receivers-receiver = p_email4.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email5 IS INITIAL.
    t_receivers-receiver = p_email5.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email6 IS INITIAL.
    t_receivers-receiver = p_email6.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email7 IS INITIAL.
    t_receivers-receiver = p_email7.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email8 IS INITIAL.
    t_receivers-receiver = p_email8.
    APPEND t_receivers.
  ENDIF.
  IF NOT p_email9 IS INITIAL.
    t_receivers-receiver = p_email9.
    APPEND t_receivers.
  ENDIF.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = w_doc_data
      put_in_outbox              = 'X'
      sender_address             = ld_sender_address
      sender_address_type        = ld_sender_address_type
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = w_sent_all
    TABLES
      packing_list               = t_packing_list
*     contents_bin               = t_attachment
      contents_bin               = t_obj_bin
      contents_txt               = it_message
      receivers                  = t_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

* Populate zerror return code
  ld_error = sy-subrc.

* Populate zreceiver return code
  LOOP AT t_receivers.
    ld_receiver = t_receivers-retrn_code.
  ENDLOOP.
ENDFORM.
