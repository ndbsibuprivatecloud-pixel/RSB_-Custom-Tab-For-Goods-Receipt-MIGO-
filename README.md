# RSB_-Custom-Tab-For-Goods-Receipt-MIGO-
Custom Tab For Goods Receipt (MIGO)

Classes 

class ZCL_IM_BI_CUST_TAB_GRECP definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .

  types:
    BEGIN OF ty_field,
             name TYPE string,
           END OF ty_field .

  data DELNOTE type GOHEAD-LFSNR .
  data:
    lt_fields TYPE STANDARD TABLE OF ty_field .
  data LS_FIELD type TY_FIELD .
  data MVTTYP type BWART .
protected section.
private section.

  data Y_DISPLAY type XFELD .
  constants Y_CLASS_ID type MIGO_CLASS_ID value 'ZBI_CUST_TAB_GRECP' ##NO_TEXT.
  data Y_DISPLAY_501 type XFELD .
ENDCLASS.



CLASS ZCL_IM_BI_CUST_TAB_GRECP IMPLEMENTATION.


  METHOD if_ex_mb_migo_badi~check_header.


  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~CHECK_ITEM.
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_DELETE.
  endmethod.


  METHOD if_ex_mb_migo_badi~hold_data_load.

  ENDMETHOD.


  METHOD if_ex_mb_migo_badi~hold_data_save.

  ENDMETHOD.


  METHOD if_ex_mb_migo_badi~init.
    IF sy-mandt <> 120.
      APPEND y_class_id TO ct_init.
    ENDIF.
  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~LINE_DELETE.
  endmethod.


  method IF_EX_MB_MIGO_BADI~LINE_MODIFY.
  endmethod.


  method IF_EX_MB_MIGO_BADI~MAA_LINE_ID_ADJUST.
  endmethod.


  METHOD if_ex_mb_migo_badi~mode_set.

*    CLEAR: y_display, y_display_501.
    DATA: lv_gui_action(40)  TYPE c VALUE '(SAPLMIGO)GODEFAULT_TV-BWART',
          lv_invno(40)       TYPE c VALUE '(SAPLMIGO)GODYNPRO-MAT_DOC',
          lv_pono(50)        TYPE c VALUE '(SAPLMIGO)GODYNPRO-PO_NUMBER',
          lv_outbounddel(50) TYPE c VALUE '(SAPLMIGO)GODYNPRO-OUTBOUND_DELIVERY',
          lv_vendorinv       TYPE char10,
          lv_purchaseno      TYPE char10,
          lv_action          TYPE char3,
          lv_delivery        TYPE vbeln.

    " fetching the screen field values
    ASSIGN (lv_invno) TO FIELD-SYMBOL(<fs_invno>).
    IF <fs_invno> IS ASSIGNED.
      lv_vendorinv = <fs_invno>.
    ENDIF.

    ASSIGN (lv_outbounddel) TO FIELD-SYMBOL(<fs_outbounddel>).
    IF <fs_outbounddel> IS ASSIGNED.
      lv_delivery = <fs_outbounddel>.
    ENDIF.

    ASSIGN (lv_gui_action) TO FIELD-SYMBOL(<fs_gui_action>).
    IF <fs_gui_action> IS ASSIGNED.
      lv_action = <fs_gui_action>.
    ENDIF.

    ASSIGN (lv_pono) TO FIELD-SYMBOL(<fs_pono>).
    IF <fs_pono> IS ASSIGNED.
      lv_purchaseno = <fs_pono>.
    ENDIF.

    IF i_action = 'A01' AND i_refdoc = 'R01' AND lv_purchaseno IS NOT INITIAL.
      IF lv_action = '101' OR lv_action = '501'.
        y_display = 'X'.
      ELSE.
        CLEAR y_display.
      ENDIF.
    ELSEIF i_action = 'A01' AND i_refdoc = 'R05' AND lv_delivery IS NOT INITIAL.
      IF lv_action = '101'.
        y_display = 'X'.
      ELSE.
        CLEAR y_display.
      ENDIF.
**    ELSEIF  i_action = 'A01' AND i_refdoc = 'R10'. "OR ( lv_action = '101' OR lv_action = '501' ).
***      IF lv_action = '501'.
**        y_display_501 = 'X'.
***      ENDIF.
***    ELSEIF  i_action = 'A04' AND i_refdoc = 'R02'.
***      IF lv_vendorinv IS INITIAL.
***        CLEAR y_display.
***      ELSE.
***        y_display = 'X'.
***      ENDIF.
    ELSEIF  i_action = 'A12' AND i_refdoc = 'R02'.
      IF lv_vendorinv IS INITIAL.
        CLEAR y_display.
      ELSE.
        y_display = 'X'.
      ENDIF.
**    ELSE.
**      CLEAR y_display.
      "Added the conditions to display the inward gatepass tab
      "SOC on 02.12.2025 by saiteja as suggested by pawar mahesh
    ELSEIF i_action = 'A03' AND i_refdoc = 'R02'.
      IF mvttyp = '541'.
        y_display = 'X'.
      ELSE.
        CLEAR y_display.
      ENDIF.

    ELSEIF i_action = 'A01' AND i_refdoc = 'R02'.
      IF mvttyp = '101'.
        y_display = 'X'.
      ELSE.
        CLEAR y_display.
      ENDIF.
      "EOC on 02.12.2025
    ENDIF.

  ENDMETHOD.


  METHOD if_ex_mb_migo_badi~pai_detail.
  ENDMETHOD.


  METHOD if_ex_mb_migo_badi~pai_header.

    DATA: lv_gui_action(40) TYPE c VALUE '(SAPLMIGO)GODEFAULT_TV-BWART',
          lv_mvttyp         TYPE char3.

    DATA: lv_action(40)      TYPE c VALUE '(SAPLMIGO)GODYNPRO-ACTION',
          lv_ref(40)         TYPE c VALUE '(SAPLMIGO)GODYNPRO-REFDOC',
          lv_matdocno(50)    TYPE c VALUE '(SAPLMIGO)GODYNPRO-MAT_DOC',
          lv_pono(50)        TYPE c VALUE '(SAPLMIGO)GODYNPRO-PO_NUMBER',
          lv_outbounddel(50) TYPE c VALUE '(SAPLMIGO)GODYNPRO-OUTBOUND_DELIVERY',
          lv_delivery        TYPE vbeln,
          lv_purchaseno      TYPE ebeln,
          matdocno           TYPE mblnr,
          action             TYPE goaction,
          ref                TYPE refdoc.

    DATA: lv_bwart TYPE matdoc-bwart.

    ASSIGN (lv_action) TO FIELD-SYMBOL(<fs_action>).
    IF <fs_action> IS ASSIGNED.
      action = <fs_action>.
    ENDIF.

    ASSIGN (lv_ref) TO FIELD-SYMBOL(<fs_ref>).
    IF <fs_ref> IS ASSIGNED.
      ref = <fs_ref>.
    ENDIF.

    ASSIGN (lv_matdocno) TO FIELD-SYMBOL(<fs_matdoc>).
    IF <fs_matdoc> IS ASSIGNED.
      matdocno = <fs_matdoc>.
      UNASSIGN <fs_matdoc>.
    ENDIF.

    ASSIGN (lv_pono) TO FIELD-SYMBOL(<fs_pono>).
    IF <fs_pono> IS ASSIGNED.
      lv_purchaseno = <fs_pono>.
    ENDIF.

    SELECT SINGLE FROM matdoc
                LEFT OUTER JOIN zgtpassitem    AS gtitem    ON matdoc~mblnr      = gtitem~grnno
                LEFT OUTER JOIN zgatepassheader AS gtheader ON gtitem~headeruuid = gtheader~headeruuid
              FIELDS
                  gtheader~gatepassno,
                  gtheader~gatepassdatetime,
                  gtitem~invoiceno,
                  gtitem~invoicedate,
                  gtitem~challanno,
                  gtheader~vechilecnumber,
                  gtheader~vechiletype,
                  gtheader~vechilecapacity,
                  gtheader~lrnumber,
                  gtheader~lrdate,
                  gtitem~irnnumber,
                  gtitem~irndate,
                  gtitem~odnno,
                  gtitem~ponumber,
                  gtitem~ewaybillno,
                  gtitem~ewaybilldate,
                  gtitem~incoterms,
                  gtitem~shipfrmloc,
                  gtitem~grnno,
                  matdoc~bwart
               WHERE gtitem~grnno  = @matdocno
               INTO @DATA(matdocdata).
    IF sy-subrc = 0.
    ELSE.
    ENDIF.

    SELECT SINGLE bwart FROM matdoc
           INTO @mvttyp
           WHERE mblnr = @matdocno.

*    mvttyp = COND #( WHEN matdocdata-bwart IS NOT INITIAL
*                     THEN matdocdata-bwart ).

    ASSIGN (lv_gui_action) TO FIELD-SYMBOL(<fs_gui_action>).
    IF <fs_gui_action> IS ASSIGNED.
      lv_mvttyp = <fs_gui_action>.
      UNASSIGN <fs_gui_action>.
    ENDIF.

    ASSIGN (lv_outbounddel) TO FIELD-SYMBOL(<fs_outbounddel>).
    IF <fs_outbounddel> IS ASSIGNED.
      lv_delivery = <fs_outbounddel>.
    ENDIF.


    IF action = 'A01' AND ref = 'R10' AND lv_mvttyp = '501'.
      y_display_501 = abap_true.
*    ELSEIF action = 'A01' AND ref = 'R01' AND lv_purchaseno IS NOT INITIAL.
*      y_display = abap_true.
*    ELSEIF action = 'A01' AND ref = 'R05' AND lv_delivery IS NOT INITIAL.
*      y_display = abap_true.
    ELSEIF matdocdata-challanno IS NOT INITIAL.
      IF ( action = 'A12' OR action = 'A04' )  AND ref = 'R02'.
        y_display_501 = abap_true.
      ENDIF.
    ELSEIF matdocdata-invoiceno IS NOT INITIAL.
      IF ( action = 'A12' OR action = 'A04' ) AND ref = 'R02'.
        y_display = abap_true.
      ENDIF.
    ELSEIF action = 'A04' AND ref = 'R02'.
      IF mvttyp = '541' OR mvttyp = '542'.
        y_display = 'X'.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD if_ex_mb_migo_badi~pbo_detail.
  ENDMETHOD.


  METHOD if_ex_mb_migo_badi~pbo_header.
    IF sy-mandt <> 120.
      DATA: lv_action(40)   TYPE c VALUE '(SAPLMIGO)GODYNPRO-ACTION',
            lv_ref(40)      TYPE c VALUE '(SAPLMIGO)GODYNPRO-REFDOC',
            lv_matdocno(50) TYPE c VALUE '(SAPLMIGO)GODYNPRO-MAT_DOC',
            lv_invno(50)    TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASS-VENDORINVNO',
            action          TYPE goaction,
            ref             TYPE refdoc,
            matdocno        TYPE mblnr,
            invno           TYPE zgtpassitem-invoiceno.

      CHECK i_class_id = y_class_id.
      " Calling Quality Check tab in Migo screen
      IF y_display = abap_true.
        e_cprog   = 'ZMM_R010_MIGO_GRECEIPT'.
        e_dynnr   = '0100'.
        e_heading = 'Inward Gate Pass'.
      ENDIF.

      IF y_display_501 = abap_true.
        e_cprog   = 'ZMM_R010_MIGO_GRECEIPT'.
        e_dynnr   = '0101'.
        e_heading = 'Inward Gate Pass'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_ex_mb_migo_badi~post_document.
    WAIT UP TO 1 SECONDS.
    "Type declaration for importing the data from gatepass report('ZMM_R010_MIGO_GRECEIPT')
    TYPES: BEGIN OF ty_gatepassdata,
             gatepassno       TYPE zgatepassheader-gatepassno,
             gatepassdatetime TYPE zgatepassheader-gatepassdatetime,
             invoiceno        TYPE zgtpassitem-invoiceno,
             invoicedate      TYPE zgtpassitem-invoicedate,
             vechilecnumber   TYPE zgatepassheader-vechilecnumber,
             vechiletype      TYPE zgatepassheader-vechiletype,
             vechilecapacity  TYPE zgatepassheader-vechilecapacity,
             lrnumber         TYPE zgatepassheader-lrnumber,
             lrdate           TYPE zgatepassheader-lrdate,
             irnnumber        TYPE zgtpassitem-irnnumber,
             irndate          TYPE zgtpassitem-irndate,
             odnno            TYPE zgtpassitem-odnno,
             ponumber         TYPE zgtpassitem-ponumber,
             ewaybillno       TYPE zgtpassitem-ewaybillno,
             ewaybilldate     TYPE zgtpassitem-ewaybilldate,
             incoterms        TYPE zgtpassitem-incoterms,
             shipfrmloc       TYPE zgtpassitem-shipfrmloc,
             grnno            TYPE zgtpassitem-grnno,
           END OF ty_gatepassdata,
           tt_gatepassdata TYPE TABLE OF ty_gatepassdata.

    "Type declaration for importing the data from migo screen custom tab fields('ZMM_R010_MIGO_GRECEIPT')
    TYPES: BEGIN OF ty_gatepass,
             gatepassno    TYPE zgatepassheader-gatepassno,
             gatepassdate  TYPE string,
             vendorinvno   TYPE zgtpassitem-invoiceno,
             vendorinvdate TYPE sy-datum,
             vechileno     TYPE zgatepassheader-vechilecnumber,
             vechiletyp    TYPE zgatepassheader-vechiletype,
             vechilecap    TYPE zgatepassheader-vechilecapacity,
             lrno          TYPE zgatepassheader-lrnumber,
             lrdate        TYPE zgatepassheader-lrdate,
             irnno         TYPE zgtpassitem-irnnumber,
             irdate        TYPE zgtpassitem-irndate,
             odn           TYPE string,
             ponumber      TYPE zgtpassitem-ponumber,
             ewaybillno    TYPE zgtpassitem-ewaybillno,
             ewaybilldate  TYPE zgtpassitem-ewaybilldate,
           END OF ty_gatepass,
           tt_gatepass TYPE TABLE OF ty_gatepass.

    "Type declaration for "inward gate passs MASOP" data.
    TYPES: BEGIN OF ty_gatepassmasop,
             gatepassno       TYPE zgatepassheader-gatepassno,
             gatepassdatetime TYPE zgatepassheader-gatepassdatetime,
             challanno        TYPE zgtpassitem-challanno,
             challandate      TYPE zgtpassitem-challandate,
             vechilecnumber   TYPE zgatepassheader-vechilecnumber,
             vechiletype      TYPE zgatepassheader-vechiletype,
             vechilecapacity  TYPE zgatepassheader-vechilecapacity,
             lrnumber         TYPE zgatepassheader-lrnumber,
             lrdate           TYPE zgatepassheader-lrdate,
             customerpono     TYPE zgtpassitem-custponumber,
             grnno            TYPE zgtpassitem-grnno,
             ewaybillno       TYPE zgtpassitem-ewaybillno,
             ewaybilldate     TYPE zgtpassitem-ewaybilldate,
           END OF ty_gatepassmasop.

    "Type declaration for "inward gate passs MASOP" data migo screen details.
    TYPES: BEGIN OF ty_gatepassmigomasop,
             gatepassno       TYPE zgatepassheader-gatepassno,
             gatepassdatetime TYPE string,
             challanno        TYPE zgtpassitem-challanno,
             challandate      TYPE zgtpassitem-challandate,
             vechilecnumber   TYPE zgatepassheader-vechilecnumber,
             vechiletype      TYPE zgatepassheader-vechiletype,
             vechilecapacity  TYPE zgatepassheader-vechilecapacity,
             lrnumber         TYPE zgatepassheader-lrnumber,
             lrdate           TYPE zgatepassheader-lrdate,
             ponumber         TYPE zgtpassitem-ponumber,
             customerpono     TYPE zgtpassitem-custponumber,                 "Added by NTT_ABAP9 on 08.09.2025
             grnno            TYPE zgtpassitem-grnno,                        "Added by NTT_ABAP9 on 08.09.2025
             ewaybillno       TYPE zgtpassitem-ewaybillno,
             ewaybilldate     TYPE zgtpassitem-ewaybilldate,
*             customerpono     TYPE zgtpassitem-custponumber,                "Commented by NTT_ABAP9 on 08.09.2025
           END OF ty_gatepassmigomasop.

    "Data declarations
    DATA: gatepass          TYPE ty_gatepass,
          gatepasstab       TYPE tt_gatepass,
          gatepassdetails   TYPE ty_gatepassdata,
          gatepassdetilstab TYPE tt_gatepassdata,
          lv_irnno(50)      TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASS-VENDORINVNO',
          ponumber(40)      TYPE c VALUE '(SAPLMIGO)GOITEM-EBELN',
          invno             TYPE zgtpassitem-invoiceno,
          pono              TYPE zgtpassitem-ponumber,
          storageloc(40)    TYPE c VALUE '(SAPLMIGO)GOITEM-LGOBE)',
          itemcheck(50)     TYPE c VALUE '(SAPLMIGO)GODYNPRO-DETAIL_TAKE',
          check             TYPE mb_take_it,
          strloc            TYPE lgobe.
    "Data declaration for "inward gate passs MASOP" MIGO
    DATA: gatepassmasop     TYPE ty_gatepassmasop,
          gatepassmigomasop TYPE ty_gatepassmigomasop.

    DATA: action(40) TYPE c VALUE '(SAPLMIGO)GODYNPRO-ACTION',
          ref(40)    TYPE c VALUE '(SAPLMIGO)GODYNPRO-REFDOC'.

    ASSIGN (action) TO FIELD-SYMBOL(<fs_action>).
    IF <fs_action> IS ASSIGNED.
      action = <fs_action>.
      UNASSIGN <fs_action>.
    ENDIF.

    ASSIGN (ref) TO FIELD-SYMBOL(<fs_ref>).
    IF <fs_ref> IS ASSIGNED.
      ref = <fs_ref>.
      UNASSIGN <fs_ref>.
    ENDIF.


    "Importing the data from report ZMM_R010_MIGO_GRECEIPT
    IMPORT gatepass TO gatepass FROM MEMORY ID 'ZGATEPASS'.
    IF gatepass IS NOT INITIAL.
      FREE MEMORY ID 'ZGATEPASS'.
    ENDIF.

    "Importing the data from report
    IMPORT gatepassdetails TO gatepassdetails FROM MEMORY ID 'ZGATEPASSDATA'.
    IF gatepassdetails IS NOT INITIAL.
      FREE MEMORY ID 'ZGATEPASSDATA'.
    ENDIF.

    IMPORT gatepassmasop TO gatepassmasop FROM MEMORY ID 'ZGATEPASSMASOP'.
    IF gatepassmasop IS NOT INITIAL.
      FREE MEMORY ID 'ZGATEPASSMASOP'.
    ENDIF.

    IMPORT gatepassmigomasop TO gatepassmigomasop FROM MEMORY ID 'ZMIGOMASOP'.
    IF gatepassmigomasop IS NOT INITIAL.
      FREE MEMORY ID 'ZMIGOMASOP'.
    ENDIF.


    "Fetching the material document no.
    DATA(matdocno) = COND #( WHEN is_mkpf-mblnr IS NOT INITIAL
                             THEN is_mkpf-mblnr ).


    ASSIGN (lv_irnno) TO FIELD-SYMBOL(<fs_invno>).
    IF <fs_invno> IS ASSIGNED.
      invno  = <fs_invno>.
      UNASSIGN <fs_invno>.
    ENDIF.

    ASSIGN (storageloc) TO FIELD-SYMBOL(<fs_strloc>).
    IF <fs_strloc> IS ASSIGNED.
      strloc  = <fs_strloc>.
      UNASSIGN <fs_strloc>.
    ENDIF.

    ASSIGN (itemcheck) TO FIELD-SYMBOL(<fs_check>).
    IF <fs_check> IS ASSIGNED.
      check  = <fs_check>.
      UNASSIGN <fs_check>.
    ENDIF.

    ASSIGN (ponumber) TO FIELD-SYMBOL(<fs_po>).
    IF <fs_po> IS ASSIGNED.
      pono  = <fs_po>.
      UNASSIGN <fs_po>.
    ENDIF.

    IF action = 'A01' AND ref = 'R10'.
      "Fetching the invoiceno and ponumber for validataion
      SELECT SINGLE challanno FROM zgtpassitem
                              WHERE challanno = @gatepassmasop-challanno
                              INTO @DATA(challandetails).

      "Fetching the ZGTPASSITEM data for modifying.
      SELECT * FROM zgtpassitem INTO TABLE @DATA(masopitem)
                                WHERE challanno = @challandetails.

      "Fetching the ZGATEPASSHEADER data for modifying.
      SELECT * FROM zgatepassheader INTO TABLE @DATA(masopheader)
                                    WHERE gatepassno = @gatepassmigomasop-gatepassno.

      "Modifying the ZGATEPASSHEADER and ZGTPASSITEM table with the MIGO Inward gate pass tab fields
      IF sy-subrc = 0 AND check IS NOT INITIAL.
        MODIFY masopheader FROM VALUE #( vechilecnumber  = gatepassmigomasop-vechilecnumber
                                         vechiletype     = gatepassmigomasop-vechiletype
                                         vechilecapacity = gatepassmigomasop-vechilecapacity
                                         lrnumber        = gatepassmigomasop-lrnumber
                                         lrdate          = gatepassmigomasop-lrdate )
*                 TRANSPORTING vechilecnumber vechilecnumber vechilecapacity lrnumber lrdate      "Commented by NTT_ABAP9 on 04.09.2025
                 TRANSPORTING vechilecnumber vechiletype vechilecapacity lrnumber lrdate          "Added by NTT_ABAP9 on 04.09.2025
                 WHERE gatepassno = gatepassmigomasop-gatepassno.
        IF sy-subrc = 0.
          MODIFY zgatepassheader FROM TABLE masopheader.
          IF sy-subrc EQ 0.                                                                       "Added by NTT_ABAP9 on 05.09.2025
**              COMMIT WORK.
**              WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDIF.
      ENDIF.
      IF masopitem IS NOT INITIAL AND check IS NOT INITIAL..
        MODIFY masopitem FROM VALUE #( challanno    = gatepassmigomasop-challanno
                                       challandate  = gatepassmigomasop-challandate
                                       ewaybillno   = gatepassmigomasop-ewaybillno
                                       ewaybilldate = gatepassmigomasop-ewaybilldate
                                       grnno        = matdocno )
                       TRANSPORTING challanno challandate grnno
                       WHERE challanno  = gatepassmasop-challanno.
        IF sy-subrc  = 0.
          MODIFY zgtpassitem FROM TABLE masopitem.
          IF sy-subrc EQ 0.                                                                       "Added by NTT_ABAP9 on 05.09.2025
**              COMMIT WORK.
**              WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      "Fetching the invoiceno and ponumber for validataion
      SELECT SINGLE invoiceno, ponumber FROM zgtpassitem
                                        WHERE invoiceno = @gatepassdetails-invoiceno
                                        INTO @DATA(ls_invoicedetails).

      "Fetching the ZGTPASSITEM data for modifying.
      SELECT * FROM zgtpassitem INTO TABLE @DATA(gatepassitem)
                                 WHERE invoiceno  = @ls_invoicedetails-invoiceno.

      "Fetching the ZGATEPASSHEADER data for modifying.
      SELECT * FROM zgatepassheader INTO TABLE @DATA(gatepassheader)
        WHERE gatepassno = @gatepass-gatepassno.

      "Modifying the ZGATEPASSHEADER and ZGTPASSITEM table with the MIGO Inward gate pass tab fields
      IF sy-subrc = 0 AND check IS NOT INITIAL.
        MODIFY gatepassheader FROM VALUE #( vechilecnumber  = gatepass-vechileno
                                            vechiletype     = gatepass-vechiletyp
                                            vechilecapacity = gatepass-vechilecap
                                            lrnumber        = gatepass-lrno
                                            lrdate          = gatepass-lrdate )
*                 TRANSPORTING vechilecnumber vechilecnumber vechilecapacity lrnumber lrdate    "Commented by NTT_ABAP9 on 04.09.2025
                  TRANSPORTING vechilecnumber vechiletype vechilecapacity lrnumber lrdate       "Added by NTT_ABAP9 on 04.09.2025

                 WHERE gatepassno = gatepassdetails-gatepassno.
        IF sy-subrc = 0.
          MODIFY zgatepassheader FROM TABLE gatepassheader.
          IF sy-subrc EQ 0.                                                                       "Added by NTT_ABAP9 on 05.09.2025
**              COMMIT WORK.
**              WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDIF.

        MODIFY gatepassitem FROM VALUE #( invoiceno    = gatepass-vendorinvno
                                          invoicedate  = gatepass-vendorinvdate
                                          irnnumber    = gatepass-irnno
                                          irndate      = gatepass-irdate
                                          ewaybillno   = gatepass-ewaybillno
                                          ewaybilldate = gatepass-ewaybilldate
                                          odnno        = gatepass-odn
                                          grnno        = matdocno )
                       TRANSPORTING invoiceno invoicedate irnnumber irndate grnno odnno
                       WHERE invoiceno  = gatepassdetails-invoiceno.
        IF sy-subrc  = 0.
          MODIFY zgtpassitem FROM TABLE gatepassitem.
          IF sy-subrc EQ 0.                                                                       "Added by NTT_ABAP9 on 05.09.2025
**              COMMIT WORK.
**              WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

if sy-uname = 'NTT_ABAP6'.
break ntt_abap6.
**    CALL FUNCTION 'ZFM_RECONCILIATION_SUBCONTRACT' IN BACKGROUND TASK "IN BACKGROUND TASK AS SEPARATE UNIT
**      EXPORTING
**        i_mblnr = is_mkpf-mblnr
**        i_gjahr = is_mkpf-mjahr.
ENDIF.

  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PUBLISH_MATERIAL_ITEM.
  endmethod.


  method IF_EX_MB_MIGO_BADI~RESET.
  endmethod.


  METHOD if_ex_mb_migo_badi~status_and_header.

  ENDMETHOD.
ENDCLASS.
*****************************************************************************************************

class ZCL_IM_BI_DOCUMENT_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_BI_DOCUMENT_BADI IMPLEMENTATION.


  method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE.
  endmethod.


  METHOD if_ex_mb_document_badi~mb_document_update.
    DATA: lt_mkpf TYPE ty_t_mkpf.
    BREAK ntt_abap7.
    lt_mkpf = xmkpf[].
  ENDMETHOD.
ENDCLASS.
*************************************************************************

class ZCL_IM_MB_DOCUMENT_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MB_DOCUMENT_BADI IMPLEMENTATION.


  method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE.

**BREAK ntt_abap6.
**        SUBMIT ZHUNNDI_FULL
**    WITH p_mblnr = '5000000677'
**    WITH p_gjahr = '2026'
**    AND RETURN.

  endmethod.


  METHOD if_ex_mb_document_badi~mb_document_update.

    DATA new_insplot TYPE qals.

    IF ( VALUE #( xmseg[ bwart = '321' ]-mblnr OPTIONAL ) ) IS NOT INITIAL .
      CALL FUNCTION 'ZMM_GRN_POST_MAT_DOC' IN BACKGROUND TASK ##RFC_PERFORMANCE_OK
        EXPORTING
          new_insplot = new_insplot                 " Inspection lot record
          mseg        = xmseg.
    ENDIF.

**    IF sy-uname = 'NTT_ABAP6'.
**      BREAK ntt_abap6.
***      IF (   VALUE #( xmseg[ bwart = '101' ]-mblnr OPTIONAL ) ) IS NOT INITIAL .
****      CALL FUNCTION 'ZFM_RECONCILIATION_SUBCONTRACT' IN BACKGROUND TASK "IN BACKGROUND TASK AS SEPARATE UNIT
****        EXPORTING
****          i_mblnr = '5000000672'
****          i_gjahr = '2026'.
**
**data : lv_mblnr TYPE mblnr,
**       lv_gjahr TYPE gjahr.
**
***lv_mblnr  =
**

**BREAK ntt_abap6.
**        SUBMIT ZHUNNDI_FULL
**    WITH p_mblnr = '5000000677'
**    WITH p_gjahr = '2026'
**    AND RETURN.
**    ENDIF.


**BREAK ntt_abap6.
**    CALL FUNCTION 'ZFM_RECONCILIATION_SUBCONTRACT' in BACKGROUND TASK "IN BACKGROUND TASK AS SEPARATE UNIT
**      EXPORTING
**        i_mblnr = '5000000685'
**        i_gjahr = '2026'.



  ENDMETHOD.
ENDCLASS.

************************************************************************************************************

Includes

*&---------------------------------------------------------------------*
*& Include          ZMM_I010_TOPINCLUDE
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_gatepass,
         gatepassno    TYPE zgatepassheader-gatepassno,
         gatepassdate  TYPE string,
         vendorinvno   TYPE zgtpassitem-invoiceno,
         vendorinvdate TYPE sy-datum,
         vechileno     TYPE zgatepassheader-vechilecnumber,
         vechiletyp    TYPE zgatepassheader-vechiletype,
         vechilecap    TYPE zgatepassheader-vechilecapacity,
         lrno          TYPE zgatepassheader-lrnumber,
         lrdate        TYPE zgatepassheader-lrdate,
         irnno         TYPE zgtpassitem-irnnumber,
         irdate        TYPE zgtpassitem-irndate,
         odn           TYPE string,
         ponumber      TYPE zgtpassitem-ponumber,
         ewaybillno    TYPE zgtpassitem-ewaybillno,
         ewaybilldate  TYPE zgtpassitem-ewaybilldate,
       END OF ty_gatepass,
       tt_gatepass TYPE TABLE OF ty_gatepass.

TYPES: BEGIN OF ty_gatepassdata,
         gatepassno       TYPE zgatepassheader-gatepassno,
         gatepassdatetime TYPE zgatepassheader-gatepassdatetime,
         invoiceno        TYPE zgtpassitem-invoiceno,
         invoicedate      TYPE zgtpassitem-invoicedate,
         vechilecnumber   TYPE zgatepassheader-vechilecnumber,
         vechiletype      TYPE zgatepassheader-vechiletype,
         vechilecapacity  TYPE zgatepassheader-vechilecapacity,
         lrnumber         TYPE zgatepassheader-lrnumber,
         lrdate           TYPE zgatepassheader-lrdate,
         irnnumber        TYPE zgtpassitem-irnnumber,
         irndate          TYPE zgtpassitem-irndate,
         odnno            TYPE zgtpassitem-odnno,
         ponumber         TYPE zgtpassitem-ponumber,
         ewaybillno       TYPE zgtpassitem-ewaybillno,
         ewaybilldate     TYPE zgtpassitem-ewaybilldate,
         incoterms        TYPE zgtpassitem-incoterms,
         shipfrmloc       TYPE zgtpassitem-shipfrmloc,
         grnno            TYPE zgtpassitem-grnno,
       END OF ty_gatepassdata,
       tt_gatepassdata TYPE TABLE OF ty_gatepassdata.

TYPES: BEGIN OF ty_gatepassmasop,
         gatepassno       TYPE zgatepassheader-gatepassno,
         gatepassdatetime TYPE zgatepassheader-gatepassdatetime,
         challanno        TYPE zgtpassitem-challanno,
         challandate      TYPE zgtpassitem-challandate,
         vechilecnumber   TYPE zgatepassheader-vechilecnumber,
         vechiletype      TYPE zgatepassheader-vechiletype,
         vechilecapacity  TYPE zgatepassheader-vechilecapacity,
         lrnumber         TYPE zgatepassheader-lrnumber,
         lrdate           TYPE zgatepassheader-lrdate,
         customerpono     TYPE zgtpassitem-custponumber,
         grnno            TYPE zgtpassitem-grnno,
         ewaybillno    TYPE zgtpassitem-ewaybillno,
         ewaybilldate  TYPE zgtpassitem-ewaybilldate,
       END OF ty_gatepassmasop.

TYPES: BEGIN OF ty_gatepassmigomasop,
         gatepassno       TYPE zgatepassheader-gatepassno,
         gatepassdatetime TYPE string,
         challanno        TYPE zgtpassitem-challanno,
         challandate      TYPE zgtpassitem-challandate,
         vechilecnumber   TYPE zgatepassheader-vechilecnumber,
         vechiletype      TYPE zgatepassheader-vechiletype,
         vechilecapacity  TYPE zgatepassheader-vechilecapacity,
         lrnumber         TYPE zgatepassheader-lrnumber,
         lrdate           TYPE zgatepassheader-lrdate,
         ponumber         TYPE zgtpassitem-ponumber,                 "Added by NTT_ABAP9 on 08.09.2025
         custponumber     TYPE zgtpassitem-custponumber,
         grnno            TYPE zgtpassitem-grnno,
         ewaybillno       TYPE zgtpassitem-ewaybillno,
         ewaybilldate     TYPE zgtpassitem-ewaybilldate,
       END OF ty_gatepassmigomasop.

*DATA: gatepassmasop     TYPE ty_gatepassmasop.
DATA: gatepassmasop     TYPE ty_gatepassmasop,
      gatepassmigomasop TYPE ty_gatepassmigomasop.

DATA: ls_gatepass     TYPE ty_gatepassdata,
      gatepassdetails TYPE ty_gatepassdata.


DATA: gatepass    TYPE ty_gatepass,
      gatepasstab TYPE tt_gatepass.

DATA: getdatetime TYPE string,
      lvtimest    TYPE zgatepassheader-gatepassdatetime,
      dttime      TYPE string.

DATA: lv_bol(40)           TYPE c VALUE '(SAPLMIGO)GOHEAD-FRBNR'.
DATA: lv_delnote(40)       TYPE c VALUE '(SAPLMIGO)GOHEAD-XBLNR',
      lv_boe(40)           TYPE c VALUE '(SAPLMIGO)GOHEAD-FRBNR',
      lv_irnno(40)         TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASS-IRNNO',
      lv_invno(50)         TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASS-VENDORINVNO',
      lv_gatepassno(50)    TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASS-GATEPASSNO',
      lv_lrno(50)          TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASS-LRNO',
      lv_check(50)         TYPE c VALUE '(SAPLMIGO)GODYNPRO-DETAIL_TAKE',
      lv_delnotechange(40) TYPE c VALUE '(SAPLMIGO)GOHEAD-XBLNR',
      lv_outbounddel(50)   TYPE c VALUE '(SAPLMIGO)GODYNPRO-OUTBOUND_DELIVERY',
      lv_delivery          TYPE vbeln,
      lv_ponumber(40)      TYPE c VALUE '(SAPLMIGO)GOITEM-EBELN',
      lv_pono              TYPE zgtpassitem-ponumber.


DATA: lv_action(40)   TYPE c VALUE '(SAPLMIGO)GODYNPRO-ACTION',
      lv_ref(40)      TYPE c VALUE '(SAPLMIGO)GODYNPRO-REFDOC',
      lv_matdocno(50) TYPE c VALUE '(SAPLMIGO)GODYNPRO-MAT_DOC',
      action          TYPE goaction,
      ref             TYPE refdoc,
      matdocno        TYPE mblnr,
      outbounddelno   TYPE mblnr.

DATA: modifiedinv   TYPE zgtpassitem-invoiceno,
      modifiedboe   TYPE zgatepassheader-lrnumber,
      modifiedirnno TYPE zgtpassitem-irnnumber,
      modifiedlrnno TYPE zgatepassheader-lrnumber,
      changedinvno  TYPE zgtpassitem-invoiceno,
      lv_invoiceno  TYPE zgtpassitem-invoiceno.


DATA: modifieddelnote TYPE zgtpassitem-invoiceno,
      modifiedvinvno  TYPE zgtpassitem-invoiceno.

*******************************************************************************************************

Program 

*&---------------------------------------------------------------------*
*& Report ZMM_R010_MIGO_GRECEIPT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_r010_migo_greceipt.

INCLUDE zmm_i010_topinclude.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*MODULE status_0100 OUTPUT.
*
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GATEPASSNO_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gatepassno_f4 INPUT.
  "SOC on 02.12.2025 by saiteja as suggested by pawar mahesh
  IF action = 'A03' AND ref = 'R02'.
    SELECT FROM zgatepassheader AS header
           INNER JOIN zgtpassitem  AS item ON item~headeruuid = header~headeruuid
   FIELDS
    gatepassno
   WHERE header~withoutpo <> 'X' AND header~withmatdoc = 'X'
         AND item~materialdocno = @matdocno
   INTO TABLE @DATA(gatepassvh).

  ELSE.
    DATA: ponumber(40) TYPE c VALUE '(SAPLMIGO)GOITEM-EBELN',
          gatepasspono TYPE zgtpassitem-ponumber.
    ASSIGN (ponumber) TO FIELD-SYMBOL(<fs_ponumber>).
    IF <fs_ponumber> IS ASSIGNED.
      gatepasspono  = <fs_ponumber>.
      UNASSIGN <fs_ponumber>.
    ENDIF.
    SELECT FROM zgatepassheader AS header
        INNER JOIN zgtpassitem  AS item ON item~headeruuid = header~headeruuid
     FIELDS
      gatepassno
     WHERE header~withoutpo <> 'X' AND header~withmatdoc <> 'X'
           AND item~ponumber = @gatepasspono
           AND item~grnno IS INITIAL
     INTO TABLE @gatepassvh.
  ENDIF.
  "EOC on 02.12.2025.

  "Start of comment on 02.12.2025 by saiteja as suggested by pawar mahesh
**  SELECT FROM zgatepassheader
**     FIELDS gatepassno
**     WHERE withoutpo <> 'X'
**     INTO TABLE @DATA(gatepassvh).                      "#EC CI_NOWHERE
  "End of comment on 02.12.2025.

  IF sy-subrc = 0.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'GATEPASSNO'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'GATEPASS-GATEPASSNO'
        value_org       = 'S'
      TABLES
        value_tab       = gatepassvh
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
    ELSE.
* Implement suitable error handling here
    ENDIF.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  INVOICENO_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE invoiceno_f4 INPUT.

  DATA: lt_dynpfields     TYPE TABLE OF dynpread,
        ls_dynpfields     TYPE dynpread,
        lv_gatepass       TYPE zgatepassheader-gatepassno,
        lt_invoice        TYPE TABLE OF zgtpassitem-invoiceno,  " or use line type with just invoice no
        lt_return         TYPE TABLE OF ddshretval,
        lv_selected_value TYPE zgatepassheader-gatepassno.

  " Read the entered GATEPASSNO from screen
  CLEAR ls_dynpfields.
  ls_dynpfields-fieldname = 'GATEPASS-GATEPASSNO'.  " Use your screen field name
  APPEND ls_dynpfields TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpfields.

  READ TABLE lt_dynpfields INTO ls_dynpfields INDEX 1.
  IF sy-subrc = 0.
    lv_gatepass = ls_dynpfields-fieldvalue.
  ENDIF.

  IF lv_gatepass IS NOT INITIAL.
    SELECT FROM zgatepassheader
      INNER JOIN zgtpassitem ON zgatepassheader~headeruuid = zgtpassitem~headeruuid
      FIELDS invoiceno
      WHERE zgatepassheader~gatepassno = @lv_gatepass
      INTO TABLE @DATA(invoicevh).

    IF sy-subrc = 0.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'INVOICENO'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          dynprofield     = 'GATEPASS-VENDORINVNO'
          value_org       = 'S'
        TABLES
          value_tab       = invoicevh
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        IF invoicevh IS INITIAL.
          MESSAGE 'Please select an invoice number from the list.' TYPE 'E'.
        ELSE.
          READ TABLE invoicevh INTO DATA(ls_return_tab) INDEX 1.
          IF sy-subrc = 0.
            lv_selected_value = ls_return_tab-invoiceno.
            " Do something with lv_selected_value
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE 'Error displaying invoice list.' TYPE 'E'.
      ENDIF.

    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: initialinvno TYPE zgtpassitem-invoiceno,
        delnote      TYPE zgtpassitem-invoiceno,
        gatepassno   TYPE zgatepassheader-gatepassno,
        invoiceno    TYPE zgtpassitem-invoiceno.

  ASSIGN (lv_gatepassno) TO FIELD-SYMBOL(<fs_gatepass>).
  IF <fs_gatepass> IS ASSIGNED.
    gatepassno = <fs_gatepass>.
    UNASSIGN <fs_gatepass>.
  ENDIF.

  ASSIGN (lv_invno) TO FIELD-SYMBOL(<fs_invno>).
  IF <fs_invno> IS ASSIGNED .
    invoiceno = <fs_invno>.
    UNASSIGN <fs_invno>.
  ENDIF.

  ASSIGN (lv_delnotechange) TO FIELD-SYMBOL(<fs_delnotechange>).
  IF <fs_delnotechange> IS ASSIGNED AND initialinvno IS INITIAL.
    delnote = <fs_delnotechange>.
    initialinvno = <fs_delnotechange>.
  ENDIF.

  SELECT FROM zgatepassheader
     FIELDS gatepassno
     WHERE withoutpo <> 'X' AND gatepassno = @gatepassno
     INTO TABLE @DATA(gatepassvaluehelp).
  IF sy-subrc <> 0.
    MESSAGE |No Matching gatepass { gatepassno }| TYPE 'E'.
  ELSE.
**    SELECT FROM zgatepassheader
**      INNER JOIN zgtpassitem ON zgatepassheader~headeruuid = zgtpassitem~headeruuid
**      FIELDS invoiceno
***      WHERE zgatepassheader~gatepassno = @gatepassno
**      WHERE zgtpassitem~invoiceno = @invoiceno
**      INTO TABLE @DATA(invoicevaluehelp).
**    IF sy-subrc <> 0.
**      MESSAGE |No Matching invoice { invoiceno }| TYPE 'E'.
**    ENDIF.
  ENDIF.


  SELECT SINGLE FROM zgatepassheader AS gtheader
         LEFT OUTER JOIN zgtpassitem AS gtitem ON gtitem~headeruuid = gtheader~headeruuid
          FIELDS
                gtheader~gatepassno,
                gtheader~gatepassdatetime,
                gtitem~invoiceno,
                gtitem~invoicedate,
                gtheader~vechilecnumber,
                gtheader~vechiletype,
                gtheader~vechilecapacity,
                gtheader~lrnumber,
                gtheader~lrdate,
                gtitem~irnnumber,
                gtitem~irndate,
                gtitem~odnno,
                gtitem~ponumber,
                gtitem~ewaybillno,
                gtitem~ewaybilldate,
                gtitem~incoterms,
                gtitem~shipfrmloc,
                gtitem~grnno
             WHERE gtheader~gatepassno = @gatepass-gatepassno
             AND   ( gtitem~invoiceno  = @gatepass-vendorinvno OR gtitem~invoiceno = @initialinvno )
             INTO @ls_gatepass.
  IF ls_gatepass IS NOT INITIAL.
    gatepassdetails = ls_gatepass.
    EXPORT gatepassdetails FROM gatepassdetails TO MEMORY ID 'ZGATEPASSDATA'.
  ENDIF.

  CASE sy-ucomm.

    WHEN 'OK_GO'.
      ASSIGN (lv_check) TO FIELD-SYMBOL(<fs_check>).

      DATA(pono)  = ls_gatepass-ponumber.
      DATA(invno) = ls_gatepass-invoiceno.

      ASSIGN (lv_delnote) TO FIELD-SYMBOL(<fs_del_note>).
      ASSIGN (lv_invno)   TO <fs_invno>.

      IF action = 'A01' AND ref = 'R01'.
        IF <fs_invno> IS ASSIGNED AND <fs_del_note> IS ASSIGNED.
          IF <fs_del_note> IS INITIAL.
            <fs_del_note> = ls_gatepass-invoiceno.
          ELSEIF <fs_invno> <> <fs_del_note>.
            <fs_del_note> = <fs_invno>.
            modifiedinv = <fs_invno>.
          ENDIF.
        ENDIF.
      ENDIF.

      ASSIGN (lv_bol) TO FIELD-SYMBOL(<fs_billof_lading>).
      ASSIGN (lv_lrno) TO FIELD-SYMBOL(<fs_lrno>).

      IF <fs_billof_lading> IS ASSIGNED AND <fs_lrno> IS ASSIGNED.
        IF <fs_billof_lading> IS INITIAL AND ls_gatepass-lrnumber IS NOT INITIAL.
          <fs_billof_lading> = ls_gatepass-lrnumber.
        ELSEIF <fs_billof_lading> IS INITIAL AND ls_gatepass-lrnumber IS INITIAL.
          <fs_billof_lading> = <fs_lrno>.
        ELSEIF <fs_billof_lading> <> <fs_lrno>.
          <fs_billof_lading> = <fs_lrno>.
          modifiedlrnno = <fs_lrno>.
        ENDIF.
      ENDIF.

      ASSIGN (lv_irnno) TO FIELD-SYMBOL(<fs_irnno>).
      IF <fs_irnno> IS ASSIGNED AND modifiedirnno IS INITIAL.
        modifiedirnno = <fs_irnno>.
        UNASSIGN <fs_irnno>.
      ENDIF.

      dttime = ls_gatepass-gatepassdatetime.
      TRY.
          getdatetime = |{ dttime+6(2) }.{ dttime+4(2) }.{ dttime+0(4) } { dttime+8(2) }:{ dttime+10(2) }|.
        CATCH cx_sy_range_out_of_bounds.
      ENDTRY.
      IF ( action <> 'A12' AND ref <> 'R02' ) OR ( action = 'A03' AND ref = 'R02' ).

        IF gatepass-gatepassno IS NOT INITIAL AND gatepass-vendorinvno IS NOT INITIAL.
          gatepass-gatepassno    = ls_gatepass-gatepassno. "gatepass-gatepassno.
          gatepass-gatepassdate  = getdatetime.
          gatepass-vendorinvno   = COND #( WHEN changedinvno IS NOT INITIAL
                                           THEN changedinvno
                                           WHEN modifiedinv IS NOT INITIAL
                                           THEN modifiedinv
                                           ELSE ls_gatepass-invoiceno ).
          gatepass-vendorinvdate = ls_gatepass-invoicedate.
          gatepass-vechileno     = ls_gatepass-vechilecnumber.
          gatepass-vechilecap    = ls_gatepass-vechilecapacity.
          gatepass-vechiletyp    = ls_gatepass-vechiletype.
          gatepass-lrno          = COND #( WHEN modifiedlrnno IS NOT INITIAL
                                           THEN modifiedlrnno
                                           ELSE ls_gatepass-lrnumber ).
          gatepass-lrdate        = COND #( WHEN gatepass-lrdate IS NOT INITIAL AND gatepass-lrdate <> ls_gatepass-lrdate
                                           THEN gatepass-lrdate
                                           ELSE ls_gatepass-lrdate )."ls_gatepass-lrdate.
          gatepass-irnno         = COND #( WHEN gatepass-irnno IS NOT INITIAL AND gatepass-irnno <> ls_gatepass-irnnumber
                                           THEN gatepass-irnno "ls_gatepass-irnnumber
                                           ELSE ls_gatepass-irnnumber ).
          gatepass-irdate        = COND #( WHEN gatepass-irdate IS NOT INITIAL AND gatepass-irdate <> ls_gatepass-irndate
                                           THEN gatepass-irdate
                                           ELSE ls_gatepass-irndate ).
          gatepass-odn           = COND #( WHEN gatepass-odn IS NOT INITIAL AND gatepass-odn <> ls_gatepass-odnno
                                           THEN gatepass-odn
                                           ELSE ls_gatepass-odnno ).
          gatepass-ewaybillno    = COND #( WHEN gatepass-ewaybillno IS NOT INITIAL AND gatepass-ewaybillno <> ls_gatepass-ewaybillno
                                           THEN gatepass-ewaybillno
                                           ELSE ls_gatepass-ewaybillno ).
          gatepass-ewaybilldate  = COND #( WHEN gatepass-ewaybilldate IS NOT INITIAL AND gatepass-ewaybilldate <> ls_gatepass-ewaybilldate
                                           THEN gatepass-ewaybilldate
                                           ELSE ls_gatepass-ewaybilldate ).
          gatepass-ponumber      = lv_pono.
        ENDIF.
      ENDIF.

      IF action = 'A12' AND ref = 'R02'.
        ASSIGN (lv_matdocno) TO FIELD-SYMBOL(<fs_matdoc>).
        IF <fs_matdoc> IS ASSIGNED.
          matdocno = <fs_matdoc>.
          UNASSIGN <fs_matdoc>.
        ENDIF.

        SELECT SINGLE vbelv FROM vbfa
                  WHERE vbeln = @matdocno
                  INTO @DATA(outbounddel).

        ASSIGN (lv_invno) TO <fs_invno>.
        IF outbounddel IS INITIAL.
          IF <fs_invno> IS ASSIGNED .
            <fs_del_note> = <fs_invno>.
*          changedinvno = <fs_invno>.
            UNASSIGN <fs_invno>.
          ENDIF.
        ENDIF.

        IF gatepass-gatepassno IS NOT INITIAL AND gatepass-vendorinvno IS NOT INITIAL.
          gatepass-gatepassno    = COND #( WHEN ls_gatepass-gatepassno IS INITIAL
                                           THEN gatepass-gatepassno
                                           ELSE ls_gatepass-gatepassno ). "gatepass-gatepassno.
          gatepass-gatepassdate  = getdatetime.
          gatepass-vendorinvno   = COND #( WHEN gatepass-vendorinvno IS NOT INITIAL AND gatepass-vendorinvno <> ls_gatepass-invoiceno
                                           THEN gatepass-vendorinvno
                                           ELSE ls_gatepass-invoiceno ).
          gatepass-vendorinvdate = COND #( WHEN ls_gatepass-invoicedate IS INITIAL
                                           THEN gatepass-vendorinvdate
                                           ELSE ls_gatepass-invoicedate ).
          gatepass-vechileno     = COND #( WHEN ls_gatepass-vechilecnumber IS INITIAL
                                           THEN gatepass-vechileno
                                           ELSE ls_gatepass-vechilecnumber ).

          gatepass-vechilecap    = COND #( WHEN ls_gatepass-vechilecapacity IS INITIAL
                                           THEN gatepass-vechilecap
                                           ELSE ls_gatepass-vechilecapacity ).

          gatepass-vechiletyp    = COND #( WHEN ls_gatepass-vechiletype IS INITIAL
                                           THEN gatepass-vechiletyp
                                           ELSE ls_gatepass-vechiletype ).

          gatepass-lrno          = COND #( WHEN gatepass-lrno IS NOT INITIAL AND gatepass-lrno <> ls_gatepass-lrnumber
                                           THEN gatepass-lrno
                                           ELSE ls_gatepass-lrnumber ).

          gatepass-lrdate        = COND #( WHEN ls_gatepass-lrdate IS INITIAL
                                           THEN gatepass-lrdate
                                           ELSE ls_gatepass-lrdate )."ls_gatepass-lrdate.
          gatepass-irnno         = COND #( WHEN ls_gatepass-irndate IS INITIAL
                                           THEN gatepass-irnno
                                           ELSE ls_gatepass-irndate ).
          gatepass-irdate        = COND #( WHEN ls_gatepass-irndate  IS INITIAL
                                           THEN gatepass-irdate
                                           ELSE ls_gatepass-irndate ).
          gatepass-odn           = COND #( WHEN ls_gatepass-odnno  IS INITIAL
                                           THEN gatepass-odn
                                           ELSE ls_gatepass-odnno ).
          gatepass-ponumber      = lv_pono.

          EXPORT gatepass FROM gatepass TO MEMORY ID 'ZZ_GATEPASS'.      "Added by NTT_ABAP9 on 05.09.2025
        ENDIF.
      ENDIF.

  ENDCASE.
  CASE sy-ucomm.
    WHEN 'OK_POST'.
      EXPORT gatepass FROM gatepass TO MEMORY ID 'ZGATEPASS'.
      IF action = 'A12' AND ref = 'R02' AND matdocno IS NOT INITIAL.
        SELECT SINGLE invoiceno,itemuuid FROM zgtpassitem
                                WHERE invoiceno = @gatepassdetails-invoiceno
                                INTO @DATA(lv_invoice).
        IF lv_invoice <> gatepass-vendorinvno.
          SELECT SINGLE invoiceno FROM zgtpassitem
                              WHERE invoiceno = @gatepass-vendorinvno
                              INTO @DATA(existing_invoice).

          IF sy-subrc = 0.
            " Duplicate found — show error and exit
            MESSAGE |{ gatepass-vendorinvno } already exists!' | TYPE 'E'.
          ENDIF.
        ENDIF.
        SELECT * FROM zgtpassitem INTO TABLE @DATA(gatepassitem)
                                    WHERE invoiceno  = @lv_invoice-invoiceno.


        SELECT * FROM zgatepassheader INTO TABLE @DATA(gatepassheader)
                                      WHERE gatepassno = @gatepass-gatepassno.
        IF sy-subrc = 0.
          MODIFY gatepassheader FROM VALUE #( vechilecnumber = gatepass-vechileno
                                            vechiletype      = gatepass-vechiletyp
                                            vechilecapacity  = gatepass-vechilecap
                                            lrnumber         = gatepass-lrno
                                            lrdate           = gatepass-lrdate )
*                 TRANSPORTING vechilecnumber vechilecnumber vechilecapacity lrnumber lrdate        "Commented by NTT_ABAP9 on 05.09.2025
                  TRANSPORTING vechilecnumber vechiletype vechilecapacity lrnumber lrdate           "Added by NTT_ABAP9 on 05.09.2025
                 WHERE gatepassno = gatepassdetails-gatepassno.
          IF sy-subrc = 0.
            MODIFY zgatepassheader FROM TABLE gatepassheader.
            IF sy-subrc EQ 0.                                                                       "Added by NTT_ABAP9 on 05.09.2025
              COMMIT WORK.
              WAIT UP TO 1 SECONDS.
            ENDIF.
          ENDIF.
          MODIFY gatepassitem FROM VALUE #( invoiceno   = gatepass-vendorinvno
                                            invoicedate = gatepass-vendorinvdate
                                            irnnumber   = gatepass-irnno
                                            irndate     = gatepass-irdate
                                            odnno       = gatepass-odn
                                            grnno       = matdocno )
                       TRANSPORTING invoiceno invoicedate irnnumber irndate grnno odnno
*                       WHERE invoiceno  = gatepassdetails-invoiceno.
                       WHERE itemuuid  = lv_invoice-itemuuid.
          IF sy-subrc  = 0.
            MODIFY zgtpassitem FROM TABLE gatepassitem.
            IF sy-subrc EQ 0.                                                                       "Added by NTT_ABAP9 on 05.09.2025
              COMMIT WORK.
              WAIT UP TO 1 SECONDS.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF action = 'A01' AND ref = 'R01'.
        IF lv_pono = ls_gatepass-ponumber.
          ASSIGN (lv_delnote) TO <fs_del_note>.
          modifiedinv = <fs_del_note>.
          IF <fs_del_note> IS ASSIGNED AND modifiedinv IS INITIAL.
            <fs_del_note> = ls_gatepass-invoiceno.
            UNASSIGN <fs_del_note>.
          ENDIF.
          IF modifiedinv <> ls_gatepass-invoiceno.
            SELECT SINGLE invoiceno FROM zgtpassitem
             WHERE invoiceno = @modifiedinv
           INTO @DATA(lv_existing_invoice).
            IF sy-subrc = 0.
              " Duplicate found — show error and exit
              MESSAGE |{ gatepass-vendorinvno } already exists!' | TYPE 'E'.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE |No Matching PO For Invoice { gatepassdetails-invoiceno }| TYPE 'E'.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  DATA: lv_vendinvno(50)     TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASS-VENDORINVNO'.

  ASSIGN (lv_outbounddel) TO FIELD-SYMBOL(<fs_outbounddel>).
  IF <fs_outbounddel> IS ASSIGNED.
    outbounddelno = <fs_outbounddel>.
  ENDIF.

  ASSIGN (lv_ponumber) TO FIELD-SYMBOL(<fs_po>).
  IF <fs_po> IS ASSIGNED.
    lv_pono  = <fs_po>.
    UNASSIGN <fs_po>.
  ENDIF.

  ASSIGN (lv_action) TO FIELD-SYMBOL(<fs_action>).
  IF <fs_action> IS ASSIGNED.
    action = <fs_action>.
    UNASSIGN <fs_action>.
  ENDIF.

  ASSIGN (lv_ref) TO FIELD-SYMBOL(<fs_ref>).
  IF <fs_ref> IS ASSIGNED.
    ref = <fs_ref>.
    UNASSIGN <fs_ref>.
  ENDIF.

  ASSIGN (lv_matdocno) TO <fs_matdoc>.
  IF <fs_matdoc> IS ASSIGNED.
    matdocno = <fs_matdoc>.
    UNASSIGN <fs_matdoc>.
  ENDIF.

  SELECT SINGLE FROM matdoc
                LEFT OUTER JOIN zgtpassitem    AS gtitem    ON matdoc~mblnr      = gtitem~grnno
                LEFT OUTER JOIN zgatepassheader AS gtheader ON gtitem~headeruuid = gtheader~headeruuid
              FIELDS
                  gtheader~gatepassno,
                  gtheader~gatepassdatetime,
                  gtitem~invoiceno,
                  gtitem~invoicedate,
                  gtheader~vechilecnumber,
                  gtheader~vechiletype,
                  gtheader~vechilecapacity,
                  gtheader~lrnumber,
                  gtheader~lrdate,
                  gtitem~irnnumber,
                  gtitem~irndate,
                  gtitem~odnno,
                  gtitem~ponumber,
                  gtitem~ewaybillno,
                  gtitem~ewaybilldate,
                  gtitem~incoterms,
                  gtitem~shipfrmloc,
                  gtitem~grnno
               WHERE gtitem~grnno  = @outbounddelno
                 OR  gtitem~grnno  = @matdocno
                 OR  gtitem~materialdocno = @matdocno
               INTO @DATA(outbounddetails).

  IF action = 'A04' AND ( ref = 'R02' OR ref = 'R05' ) AND matdocno IS NOT INITIAL.
    IF outbounddetails IS NOT INITIAL.
      dttime = outbounddetails-gatepassdatetime.
      TRY.
          getdatetime = |{ dttime+6(2) }.{ dttime+4(2) }.{ dttime+0(4) } { dttime+8(2) }:{ dttime+10(2) }|.
        CATCH cx_sy_range_out_of_bounds.
      ENDTRY.

      gatepass-gatepassno    = outbounddetails-gatepassno.
      gatepass-gatepassdate  = getdatetime.
      gatepass-vendorinvno   = COND #( WHEN changedinvno IS NOT INITIAL
                                       THEN changedinvno
                                       WHEN modifiedinv IS NOT INITIAL
                                       THEN modifiedinv
                                       ELSE outbounddetails-invoiceno ).
      gatepass-vendorinvdate = outbounddetails-invoicedate.
      gatepass-vechileno     = outbounddetails-vechilecnumber.
      gatepass-vechilecap    = outbounddetails-vechilecapacity.
      gatepass-vechiletyp    = outbounddetails-vechiletype.
      gatepass-lrno          = COND #( WHEN modifiedlrnno IS NOT INITIAL
                                       THEN modifiedlrnno
                                       ELSE outbounddetails-lrnumber ).
      gatepass-lrdate        = outbounddetails-lrdate.
      gatepass-irnno         = COND #( WHEN outbounddetails-irnnumber IS NOT INITIAL
                                       THEN outbounddetails-irnnumber
                                       ELSE modifiedirnno ).
      gatepass-irdate        = outbounddetails-irndate.
      gatepass-odn           = COND #( WHEN outbounddetails-odnno IS NOT INITIAL
                                       THEN outbounddetails-odnno
                                       ELSE gatepass-odn ).
      gatepass-ewaybillno    = outbounddetails-ewaybillno .
      gatepass-ewaybilldate  = outbounddetails-ewaybilldate .
    ENDIF.
  ELSEIF action = 'A12' AND ref = 'R02' AND matdocno IS NOT INITIAL.
    IF outbounddetails IS NOT INITIAL.
      dttime = outbounddetails-gatepassdatetime.
      TRY.
          getdatetime = |{ dttime+6(2) }.{ dttime+4(2) }.{ dttime+0(4) } { dttime+8(2) }:{ dttime+10(2) }|.
        CATCH cx_sy_range_out_of_bounds.
      ENDTRY.

      gatepass-gatepassno    = outbounddetails-gatepassno.
      gatepass-gatepassdate  = getdatetime.
      gatepass-vendorinvno   = COND #( WHEN gatepass-vendorinvno IS NOT INITIAL AND gatepass-vendorinvno <> outbounddetails-invoiceno
                                       THEN gatepass-vendorinvno
                                       ELSE outbounddetails-invoiceno ).
      gatepass-vendorinvdate = COND #( WHEN gatepass-vendorinvdate IS NOT INITIAL AND gatepass-vendorinvdate <> outbounddetails-invoicedate
                                       THEN gatepass-vendorinvdate
                                       ELSE outbounddetails-invoicedate ).
      gatepass-vechileno     = outbounddetails-vechilecnumber.
      gatepass-vechilecap    = outbounddetails-vechilecapacity.
      gatepass-vechiletyp    = outbounddetails-vechiletype.
      gatepass-lrno          = COND #( WHEN gatepass-lrno  IS NOT INITIAL AND gatepass-lrno <> outbounddetails-lrnumber
                                       THEN gatepass-lrno
                                       ELSE outbounddetails-lrnumber ).
      gatepass-lrdate        = COND #( WHEN gatepass-lrdate IS NOT INITIAL AND gatepass-lrdate <> outbounddetails-lrdate
                                       THEN gatepass-lrdate "outbounddetails-irndate
                                       ELSE outbounddetails-lrdate ).
      gatepass-irnno         = COND #( WHEN gatepass-irnno IS NOT INITIAL AND gatepass-irnno <> outbounddetails-irnnumber
                                       THEN gatepass-irnno "outbounddetails-irnnumber
                                       ELSE outbounddetails-irnnumber ).
      gatepass-irdate        = COND #( WHEN gatepass-irdate IS NOT INITIAL AND gatepass-irdate <> outbounddetails-irndate
                                       THEN gatepass-irdate "outbounddetails-irndate
                                       ELSE outbounddetails-irndate ).
      gatepass-odn           = COND #( WHEN gatepass-odn IS NOT INITIAL AND gatepass-odn <> outbounddetails-odnno
                                       THEN gatepass-odn "outbounddetails-odnno
                                       ELSE outbounddetails-odnno ).
      gatepass-ewaybillno    = COND #( WHEN gatepass-ewaybillno IS NOT INITIAL AND gatepass-ewaybillno <> outbounddetails-ewaybillno
                                       THEN gatepass-ewaybillno "outbounddetails-ewaybillno
                                       ELSE outbounddetails-ewaybillno ).
      gatepass-ewaybilldate  = COND #( WHEN gatepass-ewaybilldate IS NOT INITIAL AND gatepass-ewaybilldate <> outbounddetails-ewaybilldate
                                       THEN gatepass-ewaybilldate "outbounddetails-ewaybillno
                                       ELSE outbounddetails-ewaybilldate ).

*      EXPORT gatepass FROM gatepass TO MEMORY ID 'ZZ_GATEPASS'.      "Added by NTT_ABAP9 on 04.09.2025
    ENDIF.
  ENDIF.

  IF action = 'A03' AND ref = 'R02' .
    IF invoiceno IS NOT INITIAL AND gatepassno IS NOT INITIAL.
      LOOP AT SCREEN.
        screen-input = 0.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.
  ENDIF.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GATEPASSNOMASOP_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gatepassnomasop_f4 INPUT.
  SELECT FROM zgatepassheader
     FIELDS gatepassno
     WHERE withoutpo = 'X'
    INTO TABLE @DATA(gatepassmasop_vh).                 "#EC CI_NOWHERE
  IF sy-subrc = 0.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'GATEPASSNO'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'GATEPASSMIGOMASOP-GATEPASSNO'
        value_org       = 'S'
      TABLES
        value_tab       = gatepassmasop_vh
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
    ELSE.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHALLANNO_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE challanno_f4 INPUT.
  DATA: lt_101_dynpfields     TYPE TABLE OF dynpread,
        ls_101_dynpfields     TYPE dynpread,
        lv_gatepassmasop      TYPE zgatepassheader-gatepassno,
        lt_challanno          TYPE TABLE OF zgtpassitem-invoiceno,  " or use line type with just invoice no
        lt_101_return         TYPE TABLE OF ddshretval,
        lv_101_selected_value TYPE zgatepassheader-gatepassno.

  " Read the entered GATEPASSNO from screen
  CLEAR ls_dynpfields.
  ls_101_dynpfields-fieldname = 'GATEPASSMIGOMASOP-GATEPASSNO'.  " Use your screen field name
  APPEND ls_101_dynpfields TO lt_101_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_101_dynpfields.

  READ TABLE lt_101_dynpfields INTO ls_101_dynpfields INDEX 1.
  IF sy-subrc = 0.
    lv_gatepassmasop = ls_101_dynpfields-fieldvalue.
  ENDIF.

  IF lv_gatepassmasop IS NOT INITIAL.
    SELECT FROM zgatepassheader
      INNER JOIN zgtpassitem ON zgatepassheader~headeruuid = zgtpassitem~headeruuid
      FIELDS zgtpassitem~challanno
      WHERE zgatepassheader~gatepassno = @lv_gatepassmasop
      INTO TABLE @DATA(challanno_vh).

    IF sy-subrc = 0.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'CHALLANNO'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          dynprofield     = 'GATEPASSMIGOMASOP-CHALLANNO'
          value_org       = 'S'
        TABLES
          value_tab       = challanno_vh
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        IF challanno_vh IS INITIAL.
          MESSAGE 'Please select an Challan number from the list.' TYPE 'E'.
        ELSE.
          READ TABLE challanno_vh INTO DATA(ls_101_return_tab) INDEX 1.
          IF sy-subrc = 0.
            lv_101_selected_value = ls_101_return_tab-challanno.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE 'Error displaying invoice list.' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  DATA: challannoval(55)    TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASSMIGOMASOP-CHALLANNO',
        gatepassnomasop(55) TYPE c VALUE '(ZMM_R010_MIGO_GRECEIPT)GATEPASSMIGOMASOP-GATEPASSNO',
        materialslip(40)    TYPE c VALUE '(SAPLMIGO)GOHEAD-MTSNR'.
  DATA: challanno       TYPE zgtpassitem-challanno,
        initialchallano TYPE  zgtpassitem-challanno,
        materialslipval TYPE mtsnr.


  ASSIGN (gatepassnomasop) TO FIELD-SYMBOL(<fs_gatepassno>).
  IF <fs_gatepassno> IS ASSIGNED.
    gatepassno = <fs_gatepassno>.
    UNASSIGN <fs_gatepassno>.
  ENDIF.

  ASSIGN (lv_action) TO <fs_action>.
  IF <fs_action> IS ASSIGNED.
    action = <fs_action>.
    UNASSIGN <fs_action>.
  ENDIF.

  ASSIGN (lv_ref) TO <fs_ref>.
  IF <fs_ref> IS ASSIGNED.
    ref = <fs_ref>.
    UNASSIGN <fs_ref>.
  ENDIF.

  SELECT FROM zgatepassheader
   FIELDS gatepassno
   WHERE withoutpo = 'X' AND gatepassno = @gatepassno
   INTO TABLE @gatepassvaluehelp.
  IF sy-subrc <> 0.
    MESSAGE |No Matching gatepass { gatepassno }| TYPE 'E'.
  ENDIF.

  SELECT SINGLE FROM zgatepassheader AS gtheader
         LEFT OUTER JOIN zgtpassitem AS gtitem ON gtitem~headeruuid = gtheader~headeruuid
          FIELDS
                gtheader~gatepassno,
                gtheader~gatepassdatetime,
                gtitem~challanno,
                gtitem~challandate,
                gtheader~vechilecnumber,
                gtheader~vechiletype,
                gtheader~vechilecapacity,
                gtheader~lrnumber,
                gtheader~lrdate,
                gtitem~custponumber,
                gtitem~grnno,
                gtitem~ewaybillno,
                gtitem~ewaybilldate
             WHERE gtheader~gatepassno = @gatepassmigomasop-gatepassno
             AND ( gtitem~challanno  = @gatepassmigomasop-challanno OR gtitem~challanno  = @initialchallano )
             INTO @gatepassmasop.
  IF gatepassmasop IS NOT INITIAL.
*    gatepassdetails = ls_gatepass.
    EXPORT gatepassmasop FROM gatepassmasop TO MEMORY ID 'ZGATEPASSMASOP'.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'OK_GO'.

      IF action = 'A01' AND ref = 'R10' .
        ASSIGN (challannoval) TO FIELD-SYMBOL(<fs_challanno>).
        ASSIGN (materialslip) TO FIELD-SYMBOL(<fs_matslip>).
        IF <fs_challanno> IS ASSIGNED AND <fs_matslip> IS ASSIGNED.
          challanno = <fs_challanno>.
          <fs_matslip> = <fs_challanno>.

        ENDIF.
      ENDIF.


      dttime = gatepassmasop-gatepassdatetime.
      TRY.
          getdatetime = |{ dttime+6(2) }.{ dttime+4(2) }.{ dttime+0(4) } { dttime+8(2) }:{ dttime+10(2) }|.
        CATCH cx_sy_range_out_of_bounds.
      ENDTRY.
      IF ( action = 'A01' AND ref = 'R10' ) OR ( action = 'A12' AND ref = 'R02' ) .
*        IF gatepassmasop-gatepassno IS NOT INITIAL AND gatepassmasop-challanno IS NOT INITIAL.
        gatepassmigomasop-gatepassno        = gatepassmigomasop-gatepassno. "gatepass-gatepassno.
        gatepassmigomasop-gatepassdatetime  = getdatetime.
        gatepassmigomasop-challanno         = COND #( WHEN gatepassmigomasop-challanno IS NOT INITIAL AND gatepassmigomasop-challanno <> gatepassmasop-challanno
                                                      THEN gatepassmigomasop-challanno
                                                      ELSE gatepassmasop-challanno ).
        gatepassmigomasop-challandate       = COND #( WHEN gatepassmigomasop-challandate IS NOT INITIAL AND gatepassmigomasop-challandate <> gatepassmasop-challandate
                                                      THEN gatepassmigomasop-challandate
                                                      ELSE gatepassmasop-challandate ).
        gatepassmigomasop-vechilecnumber    = gatepassmasop-vechilecnumber.
        gatepassmigomasop-vechilecapacity   = gatepassmasop-vechilecapacity.
        gatepassmigomasop-vechiletype       = gatepassmasop-vechiletype.
        gatepassmigomasop-lrnumber          = COND #( WHEN gatepassmigomasop-lrnumber IS NOT INITIAL AND gatepassmigomasop-lrnumber <> gatepassmasop-lrnumber
                                                      THEN gatepassmigomasop-lrnumber
                                                      ELSE gatepassmasop-lrnumber ).
        gatepassmigomasop-lrdate            = COND #( WHEN gatepassmigomasop-lrdate IS NOT INITIAL AND gatepassmigomasop-lrdate <> gatepassmasop-lrdate
                                                      THEN gatepassmigomasop-lrdate
                                                      ELSE gatepassmasop-lrdate )."ls_gatepass-lrdate.
        gatepassmigomasop-custponumber      = gatepassmasop-customerpono.
        gatepassmigomasop-ewaybillno        = COND #( WHEN gatepassmigomasop-ewaybillno IS NOT INITIAL AND gatepassmigomasop-ewaybillno <> gatepassmasop-ewaybillno
                                                      THEN gatepassmigomasop-ewaybillno
                                                      ELSE gatepassmasop-ewaybillno ).
        gatepassmigomasop-ewaybilldate      = COND #( WHEN gatepassmigomasop-ewaybilldate IS NOT INITIAL AND gatepassmigomasop-ewaybilldate <> gatepassmasop-ewaybilldate
                                                        THEN gatepassmigomasop-ewaybilldate
                                                        ELSE gatepassmasop-ewaybilldate ).
*        ENDIF.
        EXPORT gatepassmigomasop FROM gatepassmigomasop TO MEMORY ID 'ZZ_GATEPASSMIGOMASOP'.           "Added by NTT_ABAP9 on 05.09.2025
      ENDIF.

  ENDCASE.

  CASE sy-ucomm.
    WHEN 'OK_POST'.
      EXPORT gatepassmigomasop FROM gatepassmigomasop TO MEMORY ID 'ZMIGOMASOP'.
      SELECT SINGLE challanno,itemuuid FROM zgtpassitem
                                WHERE challanno = @gatepassmasop-challanno
                                INTO @DATA(challandetails).
      IF action = 'A12' AND ref = 'R02' AND matdocno IS NOT INITIAL.
        IF challandetails-challanno <> gatepassmigomasop-challanno.
          SELECT SINGLE challanno FROM zgtpassitem
                                  WHERE challanno = @gatepassmigomasop-challanno
                                  INTO @DATA(existing_challan).
          IF sy-subrc = 0.
            " Duplicate found — show error and exit
            MESSAGE |{ gatepassmigomasop-challanno } already exists!' | TYPE 'E'.
          ENDIF.

          SELECT * FROM zgatepassheader INTO TABLE @DATA(gatepassheadermasop)
                                        WHERE gatepassno = @gatepassmigomasop-gatepassno.
          IF sy-subrc = 0.
            MODIFY gatepassheadermasop FROM VALUE #( vechilecnumber  = gatepassmigomasop-vechilecnumber
                                                     vechiletype     = gatepassmigomasop-vechiletype
                                                     vechilecapacity = gatepassmigomasop-vechilecapacity
                                                     lrnumber        = gatepassmigomasop-lrnumber
                                                     lrdate          = gatepassmigomasop-lrdate )
*                   TRANSPORTING vechilecnumber vechilecnumber vechilecapacity lrnumber lrdate           "Commented by NTT_ABAP9 on 04.09.2025
                   TRANSPORTING vechilecnumber vechiletype vechilecapacity lrnumber lrdate               "Added by NTT_ABAP9 on 04.09.2025
                   WHERE gatepassno = gatepassmigomasop-gatepassno.
            IF sy-subrc = 0.
              MODIFY zgatepassheader FROM TABLE gatepassheadermasop.
              IF sy-subrc EQ 0.                                                                       "Added by NTT_ABAP9 on 05.09.2025
                COMMIT WORK.
                WAIT UP TO 1 SECONDS.
              ENDIF.
            ENDIF.
          ENDIF.
          SELECT * FROM zgtpassitem INTO TABLE @DATA(gatepassitemmasop)
                                  WHERE challanno  = @challandetails-challanno.
          IF sy-subrc = 0.
            MODIFY gatepassitemmasop FROM VALUE #( challanno   = gatepassmigomasop-challanno
                                                   challandate = gatepassmigomasop-challandate )
                         TRANSPORTING challanno challandate
                         WHERE itemuuid  = challandetails-itemuuid.
            IF sy-subrc  = 0.
              MODIFY zgtpassitem FROM TABLE gatepassitemmasop.
              IF sy-subrc EQ 0.                                                                       "Added by NTT_ABAP9 on 05.09.2025
                COMMIT WORK.
                WAIT UP TO 1 SECONDS.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF action = 'A01' AND ref = 'R10'.
        IF challandetails-challanno <> gatepassmigomasop-challanno.
          SELECT SINGLE challanno FROM zgtpassitem
                                  WHERE challanno = @gatepassmigomasop-challanno
                                  INTO @existing_challan.
          IF sy-subrc = 0.
            " Duplicate found — show error and exit
            MESSAGE |{ gatepassmigomasop-challanno } already exists!' | TYPE 'E'.
          ENDIF.
        ENDIF.
        "Added by NTT_ABAP9 on 08.09.2025
        SELECT SINGLE grnno FROM zgtpassitem
                            WHERE challanno = @gatepassmigomasop-challanno
                            INTO @DATA(lv_grn_no).
        IF sy-subrc = 0 AND lv_grn_no IS NOT INITIAL.
          " Duplicate found — show error and exit
          MESSAGE |GRN No already exists for the { gatepassmigomasop-challanno }| TYPE 'E'.
        ENDIF.
      ENDIF.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0101 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  ASSIGN (lv_action) TO <fs_action>.
  IF <fs_action> IS ASSIGNED.
    action = <fs_action>.
    UNASSIGN <fs_action>.
  ENDIF.

  ASSIGN (lv_ref) TO <fs_ref>.
  IF <fs_ref> IS ASSIGNED.
    ref = <fs_ref>.
    UNASSIGN <fs_ref>.
  ENDIF.

  ASSIGN (lv_matdocno) TO <fs_matdoc>.
  IF <fs_matdoc> IS ASSIGNED.
    matdocno = <fs_matdoc>.
    UNASSIGN <fs_matdoc>.
  ENDIF.

  SELECT SINGLE FROM matdoc
                LEFT OUTER JOIN zgtpassitem    AS gtitem    ON matdoc~mblnr      = gtitem~grnno
                LEFT OUTER JOIN zgatepassheader AS gtheader ON gtitem~headeruuid = gtheader~headeruuid
              FIELDS
                  gtheader~gatepassno,
                  gtheader~gatepassdatetime,
                  gtheader~vechilecnumber,
                  gtheader~vechiletype,
                  gtheader~vechilecapacity,
                  gtheader~lrnumber,
                  gtheader~lrdate,
                  gtitem~challanno,
                  gtitem~challandate,
                  gtitem~grnno,
                  gtitem~ewaybillno,
                  gtitem~ewaybilldate,
                  gtitem~custponumber
               WHERE gtitem~grnno  = @matdocno
               INTO @DATA(matdocdetails).
  IF action = 'A04' AND ( ref = 'R02' OR ref = 'R05' ) AND matdocno IS NOT INITIAL.
    IF matdocdetails IS NOT INITIAL.
      dttime = matdocdetails-gatepassdatetime.
      TRY.
          getdatetime = |{ dttime+6(2) }.{ dttime+4(2) }.{ dttime+0(4) } { dttime+8(2) }:{ dttime+10(2) }|.
        CATCH cx_sy_range_out_of_bounds.
      ENDTRY.

      gatepassmigomasop-gatepassno        = matdocdetails-gatepassno.
      gatepassmigomasop-gatepassdatetime  = getdatetime.
      gatepassmigomasop-challanno         = matdocdetails-challanno.
      gatepassmigomasop-challandate       = matdocdetails-challandate.
      gatepassmigomasop-vechilecnumber    = matdocdetails-vechilecnumber.
      gatepassmigomasop-vechilecapacity   = matdocdetails-vechilecapacity.
      gatepassmigomasop-vechiletype       = matdocdetails-vechiletype.
      gatepassmigomasop-lrnumber          = matdocdetails-lrnumber.
      gatepassmigomasop-lrdate            = matdocdetails-lrdate.
      gatepassmigomasop-ewaybillno        = matdocdetails-ewaybillno.
      gatepassmigomasop-ewaybilldate      = matdocdetails-ewaybilldate.
      gatepassmigomasop-custponumber      = matdocdetails-custponumber.
    ENDIF.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
*  ELSEIF action = 'A12' AND ref = 'R02' AND matdocno IS NOT INITIAL.
  ELSEIF ( action = 'A12' OR action = 'A01' )  AND ref = 'R02' AND matdocno IS NOT INITIAL.
    IF matdocdetails IS NOT INITIAL.
      initialchallano = matdocdetails-challanno.
      dttime = matdocdetails-gatepassdatetime.
      TRY.
          getdatetime = |{ dttime+6(2) }.{ dttime+4(2) }.{ dttime+0(4) } { dttime+8(2) }:{ dttime+10(2) }|.
        CATCH cx_sy_range_out_of_bounds.
      ENDTRY.
      gatepassmigomasop-gatepassno        = matdocdetails-gatepassno. "gatepass-gatepassno.
      gatepassmigomasop-gatepassdatetime  = getdatetime.
      gatepassmigomasop-challanno         = COND #( WHEN gatepassmigomasop-challanno IS NOT INITIAL AND gatepassmigomasop-challanno <> matdocdetails-challanno
                                                    THEN gatepassmigomasop-challanno
                                                    ELSE matdocdetails-challanno ).
      gatepassmigomasop-challandate       = COND #( WHEN gatepassmigomasop-challandate IS NOT INITIAL AND gatepassmigomasop-challandate <> matdocdetails-challandate
                                                    THEN gatepassmigomasop-challandate
                                                    ELSE matdocdetails-challandate ).
      gatepassmigomasop-vechilecnumber    = matdocdetails-vechilecnumber.
      gatepassmigomasop-vechilecapacity   = matdocdetails-vechilecapacity.
      gatepassmigomasop-vechiletype       = matdocdetails-vechiletype.
      gatepassmigomasop-lrnumber          = COND #( WHEN gatepassmigomasop-lrnumber IS NOT INITIAL AND gatepassmigomasop-lrnumber <> matdocdetails-lrnumber
                                                    THEN gatepassmigomasop-lrnumber
                                                    ELSE matdocdetails-lrnumber ).
      gatepassmigomasop-lrdate            = COND #( WHEN gatepassmigomasop-lrdate IS NOT INITIAL AND gatepassmigomasop-lrdate <> matdocdetails-lrdate
                                                    THEN gatepassmigomasop-lrdate
                                                    ELSE matdocdetails-lrdate )."ls_gatepass-lrdate.

      gatepassmigomasop-ewaybillno        = COND #( WHEN gatepassmigomasop-ewaybillno IS NOT INITIAL AND gatepassmigomasop-ewaybillno <> matdocdetails-ewaybillno
                                                    THEN gatepassmigomasop-ewaybillno
                                                    ELSE matdocdetails-ewaybillno ).

      gatepassmigomasop-ewaybilldate      = COND #( WHEN gatepassmigomasop-ewaybilldate IS NOT INITIAL AND gatepassmigomasop-ewaybilldate <> matdocdetails-ewaybilldate
                                                    THEN gatepassmigomasop-ewaybilldate
                                                    ELSE matdocdetails-ewaybilldate ).

      gatepassmigomasop-custponumber      = COND #( WHEN gatepassmigomasop-custponumber IS NOT INITIAL AND gatepassmigomasop-custponumber <> matdocdetails-custponumber
                                                    THEN gatepassmigomasop-custponumber
                                                    ELSE matdocdetails-custponumber ).
    ENDIF.
  ENDIF.

ENDMODULE.
***************************************************************************************************************************************************************
