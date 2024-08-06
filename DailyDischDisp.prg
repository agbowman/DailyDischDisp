drop program DailyDischDisp go
create program DailyDischDisp

prompt 
	"Output to File/Printer/MINE" = "MINE"   ;* Enter or select the printer or file name to send this report to.
	, "Disposition Type" = 0
	, "Encounter Type:" = 0
	, "Outpatient ED Encounter Type" = 0
	, "Admit Date/Time:" = "SYSDATE"
	, "Discharge Date/Time" = "SYSDATE"
	, "Facility:" = 0
	, "Unit" = 0
	, "Emails" = "" 

with OUTDEV, disp_type, enc_type, outpatient_ed_enc_type, admit_dt_tm, 
	disch_dt_tm, f_fname, f_unit, email_list
DECLARE mf_admit_dt_tm = f8 WITH protect, noconstant(cnvtdatetime( $admit_dt_tm))
DECLARE mf_disch_dt_tm = f8 WITH protect, noconstant(cnvtdatetime( $disch_dt_tm))
 DECLARE ml_gcnt = i4 WITH noconstant(0), protect
 DECLARE ms_lcheck = vc WITH protect
   DECLARE mf_cs319_FIN_NBR_CD = f8 with protect, constant(uar_get_code_by("DISPLAYKEY", 319, "FINNBR"))
   DECLARE cs319_MRN_CD = f8 WITH constant(uar_get_code_by("MEANING",319,"MRN"))
    declare rpt_line = vc with protect, noconstant(" ")
    declare return_var = c2 with protect

   DECLARE mf_outpatient = f8 with protect, constant(uar_get_code_by("DISPLAYKEY", 69, "OUTPATIENT"))

DECLARE mf_emergency = f8 with protect, constant(uar_get_code_by("DISPLAYKEY", 71, "EMERGENCY"))
DECLARE mf_downtimeed = f8 with protect, constant(uar_get_code_by("DISPLAYKEY", 71, "DOWNTIMEED"))
DECLARE mf_triage = f8 with protect, constant(uar_get_code_by("DISPLAYKEY", 71, "TRIAGE"))
DECLARE mf_disches = f8 with protect, constant(uar_get_code_by("DISPLAYKEY", 71, "DISCHES"))
DECLARE mf_expiredes = f8 with protect, constant(uar_get_code_by("DISPLAYKEY", 71, "EXPIREDES"))


declare mf_AUTH_CD              = f8 with protect, constant(uar_get_code_by("MEANING", 8, "AUTH")) 

declare mf_MODIFIED_CD          = f8 with protect, constant(uar_get_code_by("MEANING", 8, "MODIFIED")) 

declare mf_ALTERED_CD           = f8 with protect, constant(uar_get_code_by("MEANING", 8, "ALTERED")) 


 DECLARE dischargevnahomecare = f8 WITH constant(uar_get_code_by("DISPLAYKEY",72,
   "DISCHARGEVNAHOMECARE")), protect
 DECLARE dischargenursingfacilities = f8 WITH constant(uar_get_code_by("DISPLAYKEY",72,
   "DISCHARGENURSINGFACILITIES")), protect
 DECLARE dischargelongtermcarefacility = f8 WITH constant(uar_get_code_by("DISPLAYKEY",72,
   "DISCHARGELONGTERMCAREFACILITY")), protect
    DECLARE nameofreceivingshorttermgenhosp = f8 WITH constant(uar_get_code_by("DISPLAYKEY",72,
   "NAMEOFRECEIVINGSHORTTERMGENHOSP")), protect
 DECLARE dischargelevelofcareatdischarge = f8 WITH constant(uar_get_code_by("DISPLAYKEY",72,
   "DISCHARGELEVELOFCAREATDISCHARGE")), protect
  DECLARE dischargemedicalequipmentcompanies = f8 WITH constant(uar_get_code_by("DISPLAYKEY",72,
   "DISCHARGEMEDICALEQUIPMENTCOMPANIES")), protect  
   

DECLARE ms_opr_var1 = c2 with constant(set_opr_var(2))
DECLARE ms_opr_var2 = c2 with constant(set_opr_var(3))
DECLARE ms_opr_var3 = c2 with constant(set_opr_var(8))
DECLARE ms_opr_var4 = c2 with constant(set_opr_var(7))


 declare exp_indx = i4 with protect, noconstant(0)
  declare loc_indx = i4 with protect, noconstant(0)
 
  declare denominator = i4 with protect, noconstant(0)
    declare denominator2 = i4 with protect, noconstant(0)

    declare ops_ind = i4 with protect, noconstant(1)
    DECLARE pos = i4 WITH protect, noconstant(0)
     DECLARE idx = i4 WITH protect, noconstant(0) 
         DECLARE enc_prompt_flag = i4 WITH protect, noconstant(0) 
    
    
    
    
declare mn_email_ind = i2 with protect, noconstant(0)
declare ms_address_list = vc with protect, noconstant(" ")
declare ms_output_dest = vc with protect, noconstant(" ")
declare ms_subject_line = vc with protect, noconstant(curprog)

 declare s_dischargevnahomecare = vc with protect, noconstant(" ")
  declare s_dischargenursingfacilities = vc with protect, noconstant(" ")
   declare s_dischargelongtermcarefacility = vc with protect, noconstant(" ")
    declare s_dischargelevelofcareatdischarge = vc with protect, noconstant(" ")
     declare s_nameofreceivingshorttermgenhosp = vc with protect, noconstant(" ")
      declare s_dischargemedicalequipmentcompanies = vc with protect, noconstant(" ")
 DECLARE where_clause = vc WITH noconstant("")


free record disp_cv
 record disp_cv(
   1 cv_cnt = i4
   1 list[*]
     2 f_cv = f8
     2 disp_cnt = i4
     2 disposition = vc
 )
 

free record ed_enc
  record ed_enc(
  1 ed_enc_cnt = i4
  1 list[*]
    2 encntr_id = f8
  )

if ( not (validate(reply->status_data.status,0)))
  record reply(
    1 ops_event = vc
    1 status_data
      2 status = c1
      2 subeventstatus[1]
        3 operationname = c25
        3 operationstatus = c1
        3 targetobjectname = c25
        3 targetobjectvalue = vc
  ) with protect
endif


if (findstring("@", $email_list) > 0)
  set ms_email_lower = cnvtlower($email_list)
  ;validate that it's @baystatehealth.org or @bhs.org
  if (findstring("@bhs.org", ms_email_lower) > 0
    or findstring("@baystatehealth.org", ms_email_lower) > 0)
    set mn_email_ind = 1
    set ms_address_list =  $email_list
    set ms_output_dest = trim(concat(trim(cnvtlower(curprog)),"_",format(cnvtdatetime(sysdate),
      "MMDDYYYYHHMMSS;;D"),".csv"))
  endif
else
  set mn_email_ind = 0
  set ms_output_dest =  $OUTDEV
endif






select distinct into "nl:"
  cv1.code_value
from
    code_value   cv1
where cv1.code_set =  19  and cv1.active_ind = 1
detail 
disp_cv->cv_cnt+=1
stat = alterlist(disp_cv->list,disp_cv->cv_cnt)
disp_cv->list[disp_cv->cv_cnt].f_cv = cv1.code_value
disp_cv->list[disp_cv->cv_cnt].disp_cnt = 0
disp_cv->list[disp_cv->cv_cnt].disposition = trim(replace(cv1.display,","," ",0 ),3)
with nocounter









free record disch_form
record disch_form
(
  1 disch_form_ref_nbr_cnt = i4
  1 disch_form_nbr_list[*]
    2 dfa_form_ref_nbr = vc
    ) with protect

free record m_info
record m_info
(	1 enc_cnt = i4
	1 enc_list[*]
		2 encntr_id = f8
)with protect




if($outpatient_ed_enc_type != 0.0)
select into "nl:" ;builds list of encounters given criterea from prompt with ED Outpatient enc type
from encounter e
where e.reg_dt_tm >= cnvtdatetime(mf_admit_dt_tm)
and e.disch_dt_tm <= cnvtdatetime(mf_disch_dt_tm)
and e.encntr_type_class_cd = mf_outpatient
and e.encntr_type_cd in (mf_emergency, mf_downtimeed, mf_triage, mf_disches, mf_expiredes)
and operator(e.loc_facility_cd,ms_opr_var4, $f_fname)
and operator(e.loc_nurse_unit_cd,ms_opr_var3, $f_unit)
and operator(e.disch_disposition_cd, ms_opr_var1, $disp_type)
order by e.encntr_id
head e.encntr_id
m_info->enc_cnt +=1
if(mod(m_info->enc_cnt,10)=1)
	stat = alterlist(m_info->enc_list, m_info->enc_cnt + 9)
endif
m_info->enc_list[m_info->enc_cnt].encntr_id = e.encntr_id
with nocounter


select into "nl:" ;gets total encounters from selected ED Outpatient enc type with any disposition
cnt = count(e.encntr_id)
from encounter e
where e.reg_dt_tm >= cnvtdatetime(mf_admit_dt_tm)
and e.disch_dt_tm <= cnvtdatetime(mf_disch_dt_tm)

and operator(e.loc_facility_cd,ms_opr_var4, $f_fname)
and e.encntr_type_class_cd = mf_outpatient
and e.encntr_type_cd in (mf_emergency, mf_downtimeed, mf_triage, mf_disches, mf_expiredes)
and operator(e.loc_nurse_unit_cd,ms_opr_var3, $f_unit)
and e.disch_disposition_cd not in (0.0,null)


order by e.encntr_id
head report
denominator = cnt

with nocounter
endif

call echo(build2("1 denom: " , denominator))





if(ms_opr_var2 != "NA" and $enc_type != -1) ;checking to see if encounter prompt var was selected at prompt or passed in via ops.
select into "nl:" ;gets total encounters from selected enc type with any disposition
e.encntr_id
from encounter e
where e.reg_dt_tm >= cnvtdatetime(mf_admit_dt_tm)
and e.disch_dt_tm <= cnvtdatetime(mf_disch_dt_tm)

and operator(e.loc_facility_cd,ms_opr_var4, $f_fname)
and operator(e.encntr_type_cd, ms_opr_var2, $enc_type)
and operator(e.loc_nurse_unit_cd,ms_opr_var3, $f_unit)
and e.disch_disposition_cd not in (0.0,null)


order by e.encntr_id
head e.encntr_id

pos = locateval(loc_indx,1,m_info->enc_cnt, e.encntr_id, m_info->enc_list[loc_indx].encntr_id)
if (pos = 0) ;if we havent already counted this encounter from $outpatient_ed_enc_type query above
denominator += 1
call echo(build2("2 denom: " , denominator))
endif
with nocounter


select into "nl:" ;builds list of encounters given criterea from prompt
from encounter e
where e.reg_dt_tm >= cnvtdatetime(mf_admit_dt_tm)
and e.disch_dt_tm <= cnvtdatetime(mf_disch_dt_tm)
and operator(e.loc_facility_cd,ms_opr_var4, $f_fname)
and operator(e.encntr_type_cd, ms_opr_var2, $enc_type)
and operator(e.loc_nurse_unit_cd ,ms_opr_var3, $f_unit)
and operator(e.disch_disposition_cd, ms_opr_var1, $disp_type)

order by e.encntr_id
head e.encntr_id
m_info->enc_cnt +=1
if(mod(m_info->enc_cnt,10)=1)
	stat = alterlist(m_info->enc_list, m_info->enc_cnt + 9)
endif
m_info->enc_list[m_info->enc_cnt].encntr_id = e.encntr_id
with nocounter
endif





;gets total encounters given the selected dispostion/s
select into value(ms_output_dest)
from encounter e,
encntr_alias ea,
encntr_alias ea2,
person p,
clinical_event ce,
dummyt d

plan e
where expand(exp_indx,1,m_info->enc_cnt,e.encntr_id,m_info->enc_list[exp_indx].encntr_id)
and e.disch_disposition_cd not in (0.0, null)
join p 
where p.person_id = e.person_id
join ea 
where ea.encntr_id = e.encntr_id
and ea.encntr_alias_type_cd = mf_cs319_FIN_NBR_CD  
join ea2
where ea2.encntr_id = e.encntr_id
and ea2.encntr_alias_type_cd = cs319_MRN_CD
join d
join ce
    where ce.encntr_id = e.encntr_id
    and ce.valid_until_dt_tm    > sysdate 

    and ce.result_status_cd in (mf_AUTH_CD, mf_MODIFIED_CD, mf_ALTERED_CD)  
	and ce.event_cd in (dischargevnahomecare,dischargenursingfacilities,dischargelongtermcarefacility, 
	dischargelevelofcareatdischarge, nameofreceivingshorttermgenhosp, dischargemedicalequipmentcompanies)
	
ORDER BY ce.encntr_id, ce.event_cd, ce.event_end_dt_tm
head report 
rpt_line = build2(
    "Admission Date", ",",
    "Discharge Date", ",",
    "Facility", ",",
    "Nurse Unit", ",",
    "Room Number", ",",
    "Name", ",",
    "Account Number", ",",
    "Discharge Disposition", ",",
    "MRN", ",",
    "DOB", ",",
    "Encounter Type" , "," ,
    "Discharge VNA Homecare" , "," ,
    "Discharge Nurse Facility" , "," ,
    "Discharge Long Term Care Facility" , "," ,
    "Discharge Level of Care Discharge" , "," ,
    "Name of Receiving Short Term Gen Hospital" , "," ,
    "Discharge Medical Equipment Companies" , "," 
    
)

  col 0, rpt_line, row+1

head e.encntr_id
denominator2 += 1

    pos = locateval(idx, 1, disp_cv->cv_cnt, e.disch_disposition_cd, disp_cv->list[idx].f_cv)
    if (pos > 0)
        disp_cv->list[pos].disp_cnt += 1
    endif

s_dischargevnahomecare = ""
s_dischargenursingfacilities = "" 
s_dischargelongtermcarefacility = "" 
s_dischargelevelofcareatdischarge = "" 
s_nameofreceivingshorttermgenhosp = "" 
s_dischargemedicalequipmentcompanies = "" 

head ce.event_cd

case(ce.event_cd)
	of dischargevnahomecare:
		s_dischargevnahomecare = trim(ce.result_val,3)
	of dischargenursingfacilities:
		s_dischargenursingfacilities = trim(ce.result_val,3)
	of dischargelongtermcarefacility:
		s_dischargelongtermcarefacility = trim(ce.result_val,3)
	of dischargelevelofcareatdischarge:
		s_dischargelevelofcareatdischarge = trim(ce.result_val,3)
	of nameofreceivingshorttermgenhosp:
		s_nameofreceivingshorttermgenhosp = trim(ce.result_val,3)
	of dischargemedicalequipmentcompanies:
		s_dischargemedicalequipmentcompanies = trim(ce.result_val,3)
	
endcase


foot e.encntr_id
rpt_line = build2(
    e.reg_dt_tm, ",",   
    e.disch_dt_tm, ",", 
    uar_get_code_display(e.loc_facility_cd), ",", 
    uar_get_code_display(e.loc_nurse_unit_cd), ",", 
    uar_get_code_display(e.loc_room_cd), ",",
    trim(replace(p.name_full_formatted,","," ",0 ),3), ",", 
    ea.alias, ",", 
    trim(replace(uar_get_code_display(e.disch_disposition_cd),","," ",0 ),3), ",",

    ea2.alias, ",",
    p.birth_dt_tm, "," ,
    build2("Encounter Type Class: ", trim(uar_get_code_display(e.encntr_type_class_cd),3),
    " - ", "Encounter Type: ", trim(uar_get_code_display(e.encntr_type_cd), 3)), "," ,
    s_dischargevnahomecare , "," ,
    s_dischargenursingfacilities , "," ,
    s_dischargelongtermcarefacility , "," ,
    s_dischargelevelofcareatdischarge , "," ,
    s_nameofreceivingshorttermgenhosp , "," ,
    s_dischargemedicalequipmentcompanies , ","
)




col 0, rpt_line, row + 1


foot report 
	col 0, "", row + 1
    rpt_line = build2("Total Discharged Encounters From Selected Facility(ies)/ Unit(s)/ Encounter-Type(s) **given any disposition**:  ", cnvtstring(denominator))
    call echo(rpt_line)
    col 0, rpt_line, row + 1
    rpt_line = build2("Total Discharged Encounters From Selected Facility(ies)/ Unit(s)/ Encounter-Type(s)/ Disposition(s):  ", cnvtstring(denominator2))
    call echo(rpt_line)
    col 0, rpt_line, row + 1
    col 0, "", row + 1
    rpt_line = "Disposition Counts:"
    col 0, rpt_line, row + 1
    for (exp_indx = 1 to  disp_cv->cv_cnt)
      if(disp_cv->list[exp_indx].disp_cnt > 0)
        rpt_line = build2(disp_cv->list[exp_indx].disposition, ": ", cnvtstring(disp_cv->list[exp_indx].disp_cnt))
        col 0, rpt_line, row + 1
        endif
    endfor
with nocounter, maxcol = 3000, format, maxrow = 1 , outerjoin = d, expand = 1


 IF (curqual = 0)
 
SELECT INTO $OUTDEV 
FROM dummyt 
HEAD REPORT 
  msg = "NO DATA FOUND.", 
  msg1 =  "PLEASE SELECT ENCOUTNER TYPE FROM DROPDOWN" , 
  msg2 =  "OR SELECT ED OUTPATIENT ENCOUNTER TYPE",
  col 0, 
  "{PS/792 0 translate 90 rotate/}", 
  y_pos = 18, 
  row + 1, 

  "{F/1}{CPI/7}", 
    CALL print(calcpos(20, (y_pos + 0))), msg,
    row +1
  CALL print(calcpos(20, (y_pos + 15))), msg1,
  row + 1
  CALL print(calcpos(20, (y_pos + 30))), msg2


WITH dio = 08 
go to exit_script
 endif





subroutine (set_opr_var(param_number = i4) = vc with Protect, Copy)
    declare lcheck = c1 with protect, noconstant("")
    declare lcnt = i4 with protect, noconstant(0)
    free record temp_list
    record temp_list(
        1 list[*]
            2 code_val = f8
    )

    set lcheck = substring(1,1,reflect(parameter(param_number, 0)))
    

    
    
    if (lcheck = "L") ; Multiple select
        set return_var = "IN"
        while (lcheck > " ")
            set lcnt = lcnt + 1
            set lcheck = substring(1,1,reflect(parameter(param_number, lcnt)))
            
            if (lcheck > " ")
                if (mod(lcnt, 5) = 1)
                    set stat = alterlist(temp_list->list, lcnt + 4)
                endif
                
                set temp_list->list[lcnt].code_val = cnvtint(parameter(param_number, lcnt))
            endif
        endwhile
        set lcnt = lcnt - 1
        set stat = alterlist(temp_list->list, lcnt)
  
    else
    	if (lcheck = "I")
    		return("NA") ;nothing selected at prompt!!
    	endif

    	
        set stat = alterlist(temp_list->list, 1) ;selecting  1 or all
        set lcnt = 1
        
        set temp_list->list[1].code_val = parameter(param_number, 0)

        if (temp_list->list[1].code_val = 0.0) ;if any * is set to 0.0 in prompt builder
            set return_var = "!="
        else
            set return_var = "="
         
        endif
    endif
    
    return(return_var)
end


if (mn_email_ind=1)
  set ms_filename_in = trim(ms_output_dest,3)
  execute bhs_ma_email_file
  call emailfile(ms_filename_in,ms_filename_in,ms_address_list,build2("TEST-AB 21 ",ms_subject_line),1)
  set reply->status_data.status = "S"
  set reply->ops_event = "Ops Job completed successfully"
  set reply->status_data.subeventstatus[1].operationstatus = "S"
  set reply->status_data.subeventstatus[1].targetobjectvalue = "Ops Job completed successfully"
  set reply->status_data.subeventstatus[1].targetobjectname = ""
endif





SELECT INTO $OUTDEV 
FROM dummyt 
HEAD REPORT 
  msg = ms_output_dest, 
  msg1 = "sent to: ", 
  msg2 = $email_list, 
  col 0, 
  "{PS/792 0 translate 90 rotate/}", 
  y_pos = 18, 
  row + 1, 

  "{F/1}{CPI/7}", 
  CALL print(calcpos(20, (y_pos + 0))), msg,
  row + 1, 
  CALL print(calcpos(20, (y_pos + 15))), msg1, 
  row + 1, 
  CALL print(calcpos(20, (y_pos + 30))), msg2

WITH dio = 08 
;end select 


#exit_script


end
go



;execute DailyDischDisp "MINE", value(679411.00,679413.00), 0.0, cnvtdatetime(curdate-200, curtime3), cnvtdatetime(curdate, curtime3), 673936.00, 0.0
;, "alex.bowman@bhs.org" go



;
;execute DailyDischDisp "MINE", value(679411.00,679413.00), value(679413.00,319455.00), cnvtdatetime(curdate-2000, curtime3), cnvtdatetime(curdate, curtime3), 673936.00, 0.0
;, "alex.bowman@bhs.org" go 

;execute DailyDischDisp "MINE", value(0.0), value(309309.00,679668.00,309308.00), 0,
; cnvtdatetime(curdate-2000, curtime3), cnvtdatetime(curdate, curtime3), 673936.00, 0.0, "alex.bowman@bhs.org" go 
;

;
;



;
;execute DailyDischDisp "MINE", value(0.0), value(0.0), 1, cnvtdatetime(curdate-4, curtime3), cnvtdatetime(curdate, curtime3), 
;673936.00, value(0.0), "alex.bowman@bhs.org" go 
;
;execute DailyDischDisp "MINE", value(0.0), -1, 1, cnvtdatetime(curdate-4, curtime3), cnvtdatetime(curdate, curtime3), 
;673936.00, value(0.0), "alex.bowman@bhs.org" go 
