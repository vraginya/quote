DELETE QPROG20_STORE;
REDEFINE PROCEDURE QPROG20_STORE
!  WRITTEN BY:     REBECCA SHAEVEL
!  DATE WRITTEN:   16-MAY-1992
!  REQUESTED BY:   JAVENS_B,BOELKE_J,SOLA_S
!  DEPARTMENT:     LPL
!  DOMAINS USED:   QPROG20
!  PURPOSE:        This procedure makes it easy to add new records to the QPROG file.
!  MODIFICATIONS: 
!                 17-MAR-2004 GALVAN  Change how we store Outside Activities so we don't need to attach all
!                                     the apps to the policies - also did Canada changes/broker.
!                 15-JAN-2010 GALVAN  Kick out if previous record was ERP.
!                 31-MAR-2010 TEMPLET Added UPL stuff - auto for GODR.
!                 02-JUN-2010 SHAEVEL AILA PERCENT_AP cannot be blank; AILA limits must be only what's allowed
!                 09-JUN-2010 GALVAN  Change GODR from ASSOC to UPL = G.
!                 11-JUN-2010 GALVAN  Remove GODR, no longer doing as an association at all.
!                 07-JUL-2010 SHAEVEL Per change form, removed IAQDE
!                 04-AUG-2010 TEMPLET Added a QN line to filing_fee because of non-LL carriers and their fees or mem dues.
!                 20-AUG-2010 TEMPLET Added 5 new fields, dddc, facil, inj, fact, coach.
!                 10-SEP-2010 SHAEVEL Change TTY to CELL_PHONE
!                 02-DEC-2010 TEMPLET Added type=IMM to all code where applicable and wherever aila was previously.
!                 14-FEB-2011 TEMPLET Added NIL, ZERO (ZD) for IMM policies.
!                 23-FEB-2011 GALVAN  Don't allow to store if cus num entered doesn't match cus in previous record.
!                 07-APR-2011 TEMPLET Per UW's/SP 10020449 is allowed to have lower ded than normal, same as last year.
!                                     So store 199,000 for income then modify after quote is done.
!                 23-MAY-2011 SHAEVEL Removed SCCCR
!                 08-JUL-2011 TEMPLET Added block for IOS job codes per change form.
!                 15-NOV-2011 GALVAN  Auto-store PL ded as 0 for POLY per change form.
!                 07-DEC-2011 SHAEVEL for IMM remove requirement of 1500 min ded per change form 
!                 06-MAR-2012 TEMPLET Added special POLY non-assoc code for Mel's PI's that are non-LL's per request-see network.
!                 23-APR-2012 GALVAN  Use same parameters as PROG store for "former ACR" (was not done when PROG was in 2011).
!                 09-AUG-2012 TEMPLET Per UW's 10000992 uses ATR wording, there was code to force MISC w/jc=C to use CML.  CML is semi-obsolete.
!                 04-SEP-2012 SHAEVEL Per change form 08/30/12 do not quote customer 10021923 who has an uncashed check situation
!                 12-OCT-2012 TEMPLET Clarified the blurb for exp_save.  It pulls from acct record so they might extend and hist is updated.x
!                 09-NOV-2012 GALVAN Add new type MJM (Mel misc).
!                 18-JAN-2013 TEMPLET Per gmail/change form, removed block on Maine non-pg quotes that are not brokered. No need for Broker anymore.
!                 31-JAN-2013 TEMPLET Per changes forms, POLY program revamped.
!                 06-FEB-2013 GALVAN  Cleaned up DSB retro date stuff, now stored in record.
!                 08-APR-2013 TEMPLET Per change form, finished up the MED storing revisions.
!                 17-JUL-2013 KORNICK Per change form, TS=MO & PG=N do not allow to quote unless Sales contacted C/S
!                 20-FEB-2015 TEMPLET Per change form, remove blocking of BN that are not current. Handling in tape not balance section-storing stage.
!                 04-AUG-2015 TEMPLET FYI ONLY-Per Mike Powell, old OAE doesn't need to be asked due to newer OIAE. Prompt was still there but OAE gone from req years ago.
!                 26-OCT-2015 KORNICK Per change form, add DDDC for LIM
!                 10-JAN-2017 KORNICK Per change form, add CBI for retro date stuff
!                 08-JUN-2018 TEMPLET per change form, remove auto-store arbx and hox for VMN and WMA.
!                 05-SEP-2018 TEMPLET Per Mel and emails, remove Activities 1 and 2, no longer need to store or display, saved copy of prc.
!                 21-JAN-2019 TEMPLET Per change form, ASSE now ASSP.
!                 25-JUN-2019 TEMPLET Per change forms, FL changes due to Mel license, see change forms. 
!                 01-NOV-2019 TEMPLET RC did many change forms, several states have been updated, see gmail/change forms.
!                 17-FEB-2020 TEMPLET Mike using revised IMM wording starting April 1, EB5 now included so making it NO.
!                 25-AUG-2020 TEMPLET Added EPL questions.
!**
SET ABORT
!PRINT "HOLDING ALL QPROG QUOTES UNTIL FURTHER NOTICE!!!"
!PRINT "GIVE TO RENEE OR TARI!"
!ABORT
DECLARE EFFECTIVE_DATE  USAGE DATE
                        QUERY_NAME EFF.
DECLARE TOTAL_PREMIUM   PIC 9(1).
EFF       = "TODAY"
:QPROG20_AV
PRINT "Getting QPROG20 ready to write..."
READY QPROG20 SHARED WRITE
! year end chg below
READY PROG17 SHARED
READY PROG18 SHARED
READY PROG19 SHARED
READY PROG20 SHARED
READY MEDHIST SHARED
!**********************************************
! DECLARES
!**********************************************
DECLARE EXCLO              PIC X.
DECLARE EXCLE              PIC X.
DECLARE EXTS               PIC X.
DECLARE EXTE               PIC X.
DECLARE EB_REQUEST         PIC X(3).
DECLARE OAE                PIC X(1).
DECLARE CON_NUM            PIC X.
DECLARE EXCL_CHOICE        PIC 9(2).
DECLARE MED_PF             PIC 9(1)
			   VALID IF MED_PF = 1,2.
DECLARE TOTAL_100          PIC X(1).
DECLARE FN_SAVE            PIC X(200).
DECLARE LN_SAVE            PIC X(200).
DECLARE T_SAVE             PIC X(200).
DECLARE GENDER_SAVE        PIC X(8).
DECLARE ORG_SAVE           PIC X(200).
DECLARE EXP_SAVE           USAGE DATE.
DECLARE JC_SAVE            PIC X(8).
DECLARE ASSOC_SAVE         PIC X(8).
DECLARE RETRO_SAVE         USAGE DATE.
DECLARE DP_RETRO_SAVE      USAGE DATE.
DECLARE EMP_PAD            USAGE DATE.
DECLARE EMP_RETRO_SAVE     USAGE DATE.
DECLARE CRM_RETRO_SAVE     USAGE DATE.
DECLARE CON_RETRO_SAVE     USAGE DATE.
DECLARE DSB_RETRO_SAVE     USAGE DATE.
DECLARE DDDC_RETRO_SAVE     USAGE DATE.
DECLARE CBI_RETRO_SAVE     USAGE DATE.
DECLARE DSB_SAVE           PIC X(1).
DECLARE ACTS_DATE          PIC X(32).
DECLARE CLC_SAVE           PIC X(1).
DECLARE LNA_SAVE           PIC X(1).
DECLARE KCE_SAVE           PIC X(1).
DECLARE OSE_SAVE           PIC X(1).
DECLARE OCE_SAVE           PIC X(1).
DECLARE OAE_SAVE           PIC X(1).
DECLARE ORIG_EFF        COMPUTED BY CHOICE
                           EXP NE " " THEN EXP
                           ELSE VALID_DATE
                        END_CHOICE.
DECLARE PREV_EXCL        PIC X(1).
DECLARE YORN            PIC X(1).
DECLARE SAME_AS_LAST_YEAR PIC X(1).
DECLARE ACTCHANGE       PIC X(1).
DECLARE ONLINEAPP       PIC X(1).
DECLARE PREV_NUM_REQ    PIC X(25).
DECLARE ACT_REQ        PIC X(200).
DECLARE ACT2_REQ       PIC X(200).
DECLARE EXCLS_REQ        PIC X(200).
DECLARE EXCLO_REQ        PIC X(200).
DECLARE EXCLE_REQ        PIC X(200).
DECLARE EXTS_REQ         PIC X(200).
DECLARE EXTE_REQ        PIC X(200).
DECLARE NAME1           PIC X(150).
DECLARE NAME2           PIC X(150).
!DECLARE EXCL_SPEC       PIC X(200).
DECLARE CER_NUM         PIC 9(25).
!DECLARE CS              PIC 9(8).
DECLARE QN_REQUEST      PIC 9(8).
DECLARE TYPE_REQUEST    PIC X(6).
DECLARE JC_REQUEST      PIC X(6).
DECLARE ACR_YN             PIC X(1).
DECLARE QAX             PIC X
                        VALID IF QAX = "Y", "N".
DECLARE DATE_CHANGED    USAGE DATE.
DECLARE HI_NUM         PIC 9(6).
!*********************************************************************
QN_REQUEST = *."the quote number you are storing, space through if none"
HI_NUM = 0
READY CEMI05:[DTR.COMMON]CERNUM SHARED WRITE
FIND CERNUM WITH TYPE = "Q20"
FOR CURRENT
   BEGIN
      IF QN_REQUEST = " " THEN
         BEGIN
            HI_NUM = CER_NUM + 1
            MODIFY USING CER_NUM = HI_NUM
            MODIFY USING DATE_CHANGED = "TODAY"
         END
   END
FINISH CERNUM
IF HI_NUM NE 0 THEN
   BEGIN
      FN$DCL("CLS")
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT "*****************************************************"
      PRINT " "
      PRINT "        PICK UP YOUR PEN AND WRITE THIS DOWN: "
      PRINT " "
      ! for year end make sure you change the year in the following
      PRINT "        Your assigned quote number is: 20"|(FORMAT HI_NUM USING 9999)
      PRINT " "
      PRINT "*****************************************************"
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
      PRINT " "
   END
! for year end make sure you change the year in the following
IF QN_REQUEST = " " THEN QN_REQUEST = "20"|(FORMAT HI_NUM USING 9999)
FIND QPROG20 WITH QN = QN_REQUEST
IF COUNT NE 0 THEN
   BEGIN
      PRINT " "
      PRINT " "
      PRINT "There is already a quote with that number stored!"
      PRINT "Ending this procedure now!"
      ABORT
   END
!********************** DO CUSNUM AND MADDR *******************************************
:CEMI05:[DTR.LPL]CUS_MODIFY_FOR_ALL
! Do NOT remove the line below - it is needed to avoid problems with goldmail notifications after the cus procedure!
PRINT " "
IF CUS_REQUEST = " " THEN
   PRINT "No customer number entered...please wait"
TYPE_REQUEST = FN$UPCASE(*."type")
IF CUS_REQUEST NE " " THEN
   PREV_NUM_REQ = *."the previous cer num, or space through if none"
! year end chg below
FIND PROG17 W CER_NUM = PREV_NUM_REQ AND ENDORSEMENT NE "E" AND (BDM NE 0 OR (BDM = 0 AND CON_NUM NOT CONT "CP","BA"))
IF COUNT = 0 AND PREV_NUM_REQ NE " ","0" THEN
   FIND PROG18 W CER_NUM = PREV_NUM_REQ AND ENDORSEMENT NE "E" AND (BDM NE 0 OR (BDM = 0 AND CON_NUM NOT CONT "CP","BA"))
IF COUNT = 0 AND PREV_NUM_REQ NE " ","0" THEN
   FIND PROG19 W CER_NUM = PREV_NUM_REQ AND ENDORSEMENT NE "E" AND (BDM NE 0 OR (BDM = 0 AND CON_NUM NOT CONT "CP","BA"))
IF COUNT = 0 AND PREV_NUM_REQ NE " ","0" THEN
   FIND PROG20 W CER_NUM = PREV_NUM_REQ AND ENDORSEMENT NE "E" AND (BDM NE 0 OR (BDM = 0 AND CON_NUM NOT CONT "CP","BA"))
IF COUNT NE 0 AND PREV_NUM_REQ NE " ","0" THEN
   FOR CURRENT
      BEGIN
         ! 10018122 was erp now not per MP. - RT5.2.12
         IF END = "R" AND CUS_NUM NE "10018598","10017694","10018122","10017491","10021676" THEN
            BEGIN
               PRINT "You cannot quote ERP - use INVOICE!"
               PRINT "Ending this procedure now!"
               ABORT
            END
          IF CUS_REQUEST NE CUS_NUM AND QN_REQUEST NE "88888","888888","110999","133166" THEN
            BEGIN
               PRINT "The CUSTOMER NUMBER in the previous record does NOT match"
               PRINT "      the one you entered!!!"
               PRINT "Confirm you have the correct previous number and customer number!!"
               PRINT "Ending this procedure now!!!"
               ABORT
            END
         FN_SAVE = FIRST_NAME
         LN_SAVE = LAST_NAME
         T_SAVE = TITLE
         GENDER_SAVE = GENDER
         ORG_SAVE = ORGANIZATION
         EXP_SAVE = EXP
         JC_SAVE = JOB_CODE
         ASSOC_SAVE = ASSOC
         RETRO_SAVE = PL_PAD
         DP_RETRO_SAVE = DP_PAD
         EMP_RETRO_SAVE = EMP_PAD
         CON_RETRO_SAVE = CON_PAD
         DSB_RETRO_SAVE = DSB_PAD
         DDDC_RETRO_SAVE = DDDC_PAD
         CBI_RETRO_SAVE = CBI_PAD
         DSB_SAVE = DSB_YN
         CRM_RETRO_SAVE = CRM_PAD
         IF TYPE = "IMM" THEN
            BEGIN
               CLC_SAVE = CLC
               LNA_SAVE = LNA
               KCE_SAVE = KCE
               OSE_SAVE = OSE
               OCE_SAVE = OCE
            END
         OAE_SAVE = OAE
         PRINT " "
         PRINT "EXCLUDING SERVICES:"|||EXCLUSIONS USING T(50)
         PRINT " "
         IF TYPE = "SC","LIMSC" AND QN VIA GRAM_TABLE = "N" AND EXCLUSIONS CONT "PASE" THEN
            BEGIN
               PRINT "***************************************************************"
               PRINT " "
               PRINT "WARNING: PASE is in the EXCLUSIONS - do NOT enter Y below!!!!!!"
               PRINT " "
               PRINT "***************************************************************"
            END
         PREV_EXCL = CHOICE
                        CUS_REQUEST = " " THEN "N"
                        ELSE FN$UPCASE(*."Y if excluding services is the same as last year, N if not")
                     END_CHOICE
         EXCLS_REQ  = CHOICE
                         PREV_EXCL = "Y" THEN EXCLUSIONS
                         ELSE *."EXCLUDING SERVICES"
                      END_CHOICE
         PRINT "EXTENDING SERVICES:"|||EXTS USING T(50)
         PREV_EXCL = FN$UPCASE(*."Y if extending services is the same as last year, N if not")
         EXTS_REQ   = CHOICE
                         PREV_EXCL = "Y" THEN EXTS
                         ELSE *."EXTENDING SERVICES"
                      END_CHOICE
         PRINT "EXCLUDING OTHERS WORK FOR INSURED:"|||EXCLO USING T(50)
         PREV_EXCL = FN$UPCASE(*."Y if excluding others work for insured is the same as last year, N if not")
         EXCLO_REQ  = CHOICE
                         PREV_EXCL = "Y" THEN EXCLO
                         ELSE *."EXCLUDING OTHERS WORK FOR INSURED (PEOPLE)"
                      END_CHOICE
         PRINT "EXCLUDING INSUREDS WORK FOR OTHERS:"|||EXCLE USING T(50)
         PREV_EXCL = FN$UPCASE(*."Y if excluding insureds work for others is the same as last year, N if not")
         EXCLE_REQ  = CHOICE
                         PREV_EXCL = "Y" THEN EXCLE
                         ELSE *."EXCLUDING INSUREDS WORK FOR OTHERS (PEOPLE)"
                      END_CHOICE
         PRINT "EXTENDING ENTITIES:"|||EXTE USING T(50)
         PREV_EXCL = FN$UPCASE(*."Y if extending entities is the same as last year, N if not")
         IF PREV_EXCL = "Y" THEN
            EXTE_REQ = EXTE
         IF PREV_EXCL = "N" THEN
            BEGIN
               PREV_EXCL = FN$UPCASE(*."Y to store EXTENDING ENTITIES, N if not")
               IF PREV_EXCL = "Y" THEN
                  BEGIN
                     PRINT "NOTE: Extending entities will be stored as follows:"
                     PRINT " "
                     PRINT "*** NAME 1 for work done for and on behalf of NAME 2 ***"
                     PRINT " "
                     PRINT "You will be prompted only for NAME 1 and NAME 2, enter in exact case!"
                     PRINT "The rest of the wording will be stored automatically!"
                     PRINT " "
                     NAME1 = *."NAME 1"
                     NAME2 = *."NAME 2"
                     EXTE_REQ = NAME1|||"for work done for and on behalf of"|||NAME2
                  END
            END
      END
DECLARE NEXT_EFF USAGE DATE.
DECLARE ACR_DONE PIC X(1).
ACR_DONE = "N"
!IF TYPE_REQUEST = "MED" AND ASSOC_SAVE = " " THEN
!   FIND MEDHIST WITH CUS_NUM = CUS_REQUEST SORTED BY EFF
!IF TYPE_REQUEST = "MED" AND ASSOC_SAVE = " " AND COUNT NE 0 THEN
!   BEGIN
!      FOR CURRENT SORTED BY EFF
!         BEGIN
!            IF ACR_DONE = "Y" AND ASSOC = "ACR" THEN
!               ACR_DONE = "N"
!            IF NEXT_EFF NE " " AND NEXT_EFF NE EFF THEN
!               ACR_DONE = "Y"
!            ACR_YN = CHOICE
!                        ACR_DONE = "Y" THEN "N"
!                        ASSOC = "ACR" THEN "Y"
!                        ASSOC = " " THEN ACR_YN
!                        ELSE "N"
!                     END_CHOICE
!            NEXT_EFF = EXP
!         END
!   END
STORE QPROG20 USING
   BEGIN
      QN                           = QN_REQUEST
   END VERIFY USING
      BEGIN
         CUS_NUM                   = CUS_REQUEST
         TYPE                      = TYPE_REQUEST
         INITIAL_RENEWAL        = CHOICE
                                     TYPE= "IMM", "CD" AND PREV_NUM_REQ = " ","0" THEN FN$UPCASE(*."I for initial quote, P for premium indication")
                                     PREV_NUM_REQ = " ","0" THEN "I"
                                     ELSE "R"
                                  END_CHOICE
         IF IR = "R" THEN
            BEGIN
               PRINT " "
               PRINT "Previous Gender:"|||GENDER_SAVE
               PRINT " "
               YORN = FN$UPCASE(*."Y if this is correct, N to change")
            END
         GENDER                    = CHOICE
                                        YORN = "Y" THEN GENDER_SAVE
                                        ELSE *."GENDER"
                                     END_CHOICE
         IF IR = "R" THEN
            BEGIN
               PRINT " "
               PRINT "Previous First Name:"|||(FORMAT FN_SAVE USING T(150))
               PRINT " "
               YORN = FN$UPCASE(*."Y if this is correct, N to change")
            END
         FIRST_NAME                = CHOICE
                                        YORN = "Y" THEN FN_SAVE
                                        ELSE *."First Name"
                                     END_CHOICE
         IF IR = "R" THEN
            BEGIN
               PRINT " "
               PRINT "Previous Last Name:"|||(FORMAT LN_SAVE USING T(150))
               PRINT " "
               YORN = FN$UPCASE(*."Y if this is correct, N to change")
            END
         LAST_NAME                 = CHOICE
                                        YORN = "Y" THEN LN_SAVE
                                        ELSE *."Last Name"
                                     END_CHOICE
         IF IR = "R" THEN
            BEGIN
               PRINT " "
               PRINT "Previous TITLE:"|||(FORMAT T_SAVE USING T(150))
               PRINT " "
               YORN = FN$UPCASE(*."Y if this is correct, N to change")
            END
         TITLE                     = CHOICE
                                        YORN = "Y" THEN T_SAVE
                                        TYPE = "FOR"      THEN " "
                                        TYPE = "ENV","SC" THEN " "
                                        TYPE = "CD"       THEN "Attorney at Law"
                                        ELSE *."TITLE, if any indicated by salesperson"
                                     END_CHOICE
         IF IR = "R" THEN
            BEGIN
               PRINT " "
               PRINT "Previous Organization:"|||(FORMAT ORG_SAVE USING T(200))
               PRINT " "
               YORN = FN$UPCASE(*."Y if this is correct, N to change")
            END
         ORGANIZATION              = CHOICE
                                        YORN = "Y" THEN ORG_SAVE
                                        ELSE *."ORGANIZATION"
                                     END_CHOICE
         IF ORGANIZATION NE " " AND TYPE = "CD" THEN
            TITLE = " "
         INSURED                   = CHOICE
                                        CUS_REQUEST = " " THEN FN$UPCASE(*."INSURED")
                                        GLINS_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLINS_REQUEST
                                        ELSE (FORMAT INS_REQUEST USING T(200))
                                     END_CHOICE
         ADDRESS                   = CHOICE
                                        CUS_REQUEST = " " THEN *."ADDRESS"
                                        GLA1_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLA1_REQUEST
                                        ELSE A1_REQUEST
                                     END_CHOICE
         CITY                      = CHOICE
                                        CUS_REQUEST = " " THEN *."CITY"
                                        GLC_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLC_REQUEST
                                        ELSE C_REQUEST
                                     END_CHOICE
         STATE                     = CHOICE
                                        CUS_NUM = 10007869 THEN "IL"
                                        CUS_REQUEST = " " THEN FN$UPCASE(*."STATE - use PHYSICAL state if different from MAILING state!")
                                        GLS_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLS_REQUEST
                                        ELSE S_REQUEST
                                     END_CHOICE
! take out qn below for year end
         IF STATE = "KY" AND CUS_REQUEST = " " AND QN NE 99999,204019 THEN
            BEGIN
               PRINT " "
               PRINT " "
               PRINT "FIRST - write down the QN that was assigned on the quote"
               PRINT " "
               PRINT " "
               PRINT "DO NOT QUOTE KENTUCKY!!!"
               PRINT " "
               PRINT "Look for the email from KY AIF in the quote request packet"
               PRINT "if none, give this quote back to the SP or to RC!"
               PRINT " "
               PRINT " "
               PRINT "ONLY When ready to program give to RENEE AND/OR TARI!!!"
               PRINT " "
               PRINT " "
               YORN = FN$UPCASE(*."Y to exit")
               IF YORN NE "R","T" THEN
                  ABORT
            END
         IF CANADA_YN = "Y" AND TYPE NE "SFPE","GEN","PAR","BSM","IMM","FOOD","MISC","FOR","MED" THEN
            BEGIN
               PRINT "DO NOT PROCESS CANADIAN"
               PRINT "Give this quote to RENEE AND/OR TARI to fix the quote form!!!"
               YORN = FN$UPCASE(*."Y to exit")
               IF YORN NE "R","T" THEN
                  ABORT
            END
         IF STATE = "KY" AND MUNI_RATE = "?" THEN
            BEGIN
               PRINT "You will need to see R/C about this city in KY!!!"
               PRINT "You cannot store this record until you talk to them!!!"
               PRINT "Ending this procedure now!!!!!!!"
               ABORT
            END
         IF JC_SAVE = "IOS" THEN
            BEGIN
               PRINT "The previous record has a job code of IOS"
               PRINT "Can't pull this prev record to store this quote"
               PRINT "See Nikki to look at the previous record"
               PRINT "Ending this procedure now!!!!!!!"
               ABORT
            END
         IF STATE = "NB","QU" THEN
            BEGIN
               PRINT "You CANNOT store insureds in this province due to license issues"
               PRINT "See Regulatory Compliance for instructions on what to do!"
               PRINT "Ending this procedure now!!"
               ABORT
            END
         WHILE STATE = " "
            BEGIN
               PRINT "STATE MUST BE ENTERED!!  TAKE BACK TO THE SALESPERSON!"
               ABORT
            END
!         IF STATE = "KY" AND TYPE = "IMM" THEN
!            BEGIN
!               PRINT "IMM KY - GL needs taxes so can't combo"
!               PRINT "see MIKE or rc"
!               PRINT "Ending this procedure now!!!!!!!"
!               ABORT
!            END
! take out qn for year end
         IF STATE = "VI" AND TYPE_REQUEST NE "SC","JUD" AND QN NE 99999 THEN
            BEGIN
               PRINT "We cannot sell in the Virgin Islands!"
               PRINT "Return to requestor!"
               ABORT
            END
         ! if Nikki is not here, you can allow per mail message 8/28/2008, but let Nikki know ASAP - not filed!
! take out qn for year end
         IF STATE = "IL" AND TYPE = "FOOD" AND QN NE 99999 THEN
            BEGIN
               PRINT " "
               PRINT " "
               PRINT "******************************************************************"
               PRINT "You cannot do a quote for FOOD insureds in IL!!!!!!!"
               PRINT "See Nikki/Renee immediately!!!!!!!!!!!!!!!"
               PRINT "Ending this procedure now!!!!!!!!!!!!!!!"
               PRINT "******************************************************************"
               ABORT
            END
! added below on 6/15/11. at this time we have only a DE in IL non-assoc,non-pg.-rt
! s/b (TYPE = "HA" OR (TYPE = "DE" AND ASSOC NE " ")) or (TYPE = "HA" OR (TYPE = "DE" AND (ASSOC NE " " OR PG = "Y"))) 
! to also exclude PGs? I don't know how this could be working, but not changing because I have not heard of any problems 12/8/11 tg
         IF STATE = "IL" AND (TYPE = "HA" OR ASSOC NE " ") AND QN NE 99999 THEN
            BEGIN
               PRINT " "
               PRINT " "
               PRINT "******************************************************************"
               PRINT "You cannot do a quote for type HAND DOC or ASSOC-BASED insureds in IL!!!!!!!"
               PRINT "We only have filed docs for type DE, non-assoc, non-pg!!"
               PRINT "See Nikki/PR immediately!!!!!!!!!!!!!!!"
               PRINT "Ending this procedure now!!!!!!!!!!!!!!!"
               PRINT "******************************************************************"
               ABORT
            END
         ZIP                          = CHOICE
                                           CUS_REQUEST = " " THEN *."ZIP"
                                           GLZ_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLZ_REQUEST
                                           ELSE Z_REQUEST
                                        END_CHOICE
         PHONE                        = CHOICE
                                           CUS_REQUEST = " " THEN *."phone number"
                                           GLPHONE_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLPHONE_REQUEST
                                           ELSE PHONE_REQUEST
                                        END_CHOICE
         FAX                          = CHOICE
                                           CUS_REQUEST = " " THEN *."fax number"
                                           GLFAX_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLFAX_REQUEST
                                           ELSE FAX_REQUEST
                                        END_CHOICE
         CELL_PHONE                         = CHOICE
                                          CUS_REQUEST = " " THEN *."CELL PHONE"
                                          GLCELLPHONE_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLCELLPHONE_REQUEST
                                          ELSE CELLPHONE_REQUEST
                                       END_CHOICE
         EMAIL                          = CHOICE
                                           CUS_REQUEST = " " THEN *."email address"
                                           GLEMAIL_REQUEST NE " " AND TYPE = "LIM","NON","MISC" THEN GLEMAIL_REQUEST
                                           ELSE EMAIL_REQUEST
                                        END_CHOICE
         IF INITIAL_RENEWAL = "R" THEN
            BEGIN
               PREV_NUM               = PREV_NUM_REQ
               PRINT " "
               PRINT "EXPIRATION DATE:"|||(FORMAT EXP_SAVE USING NN/DD/YYYY)
               PRINT " "
               YORN = FN$UPCASE(*."Y if this is correct, N to change")
               ! year end remove qns below
               IF YORN = "N" AND EXP_SAVE NE " " AND QN NE 99999 THEN
                  BEGIN
                     PRINT " "
                     PRINT " "
                     PRINT "SEE RC ABOUT THIS SITUATION!!"
                     PRINT "There could be a problem with the exp date or cer-num"
                     PRINT "Read Below and print this out and contact RC."
                     PRINT " "
                     PRINT "You have indicated this is not the correct EXP of the previous policy."
                     PRINT "Make sure you NOTE THIS ON THE QUOTE REQUEST."
                     PRINT " "
                     PRINT "It may be an incorrect prev num - SP needs to know!!!"
                     PRINT "OR it may just be the history has new date. we pull from acct record"
                     PRINT " "
                     PRINT "Just confirm CER NUM is correct and exp written is correct"
                     PRINT " GET RC or SP approval and quote with new exp"
                     PRINT " "
                     PRINT " "
                     PRINT " "
                  END
               EXP                    = CHOICE
                                           IR = "R" AND YORN = "Y" THEN EXP_SAVE
                                           ELSE *."EXPIRATION DATE"
                                        END_CHOICE
              WHILE EXP = " "
                 BEGIN
                    EXP = *."expiration - MUST be entered for all RENEWAL quotes!"
                 END
            END
         VALID_DATE             = *."VALID DATE"
         ! remove qns for year end
         WHILE VALID_DATE LT TODAY_DATE AND QN NE 99999
            BEGIN
               PRINT "You CANNOT have a valid date that has already passed!"
               VALID_DATE             = *."VALID DATE"
            END
! take out qn for year end
         WHILE EXP NE " " AND (FORMAT VALID_DATE USING YYYY GT FORMAT EXP USING YYYY) AND QN NE 99999
            BEGIN
               PRINT "You CANNOT have a valid date NEXT YEAR for an expiration THIS YEAR!"
               VALID_DATE             = *."VALID DATE"
            END
         IF IR = "R" THEN
            BEGIN
               PRINT " "
               PRINT "PREVIOUS PL RETRO DATE:"|||(FORMAT RETRO_SAVE USING NN/DD/YYYY)
               PRINT " "
               YORN = FN$UPCASE(*."Y if this is correct, N to change")
            END
         PL_PAD                 = CHOICE
                                     IR = "R" AND YORN = "Y" THEN RETRO_SAVE
                                     ELSE *." PL RETRO DATE if any, leave blank now if you need I, N, or S later"
                                  END_CHOICE
         IF TYPE = "CD","NON","LIM","JUD","IMM","FOOD" THEN
            BEGIN
               IF IR = "R" THEN
                  BEGIN
                     PRINT " "
                     IF TYPE NE "GEN","FOOD" THEN
                        PRINT "PREVIOUS DP RETRO DATE:"|||(FORMAT DP_RETRO_SAVE USING NN/DD/YYYY) ELSE
                        PRINT "PREVIOUS BI/PD RETRO DATE:"|||(FORMAT DP_RETRO_SAVE USING NN/DD/YYYY)
                     PRINT " "
                     YORN = FN$UPCASE(*."Y if this is correct, N to change")
                  END
! made dp_pad double as the BI/PD date so as not to restructure -rt**
               DP_PAD                     = CHOICE
                                               IR = "R" AND YORN = "Y" THEN DP_RETRO_SAVE
                                               TYPE = "FOOD" THEN *."BI/PD RETRO DATE if any, leave blank now if you need I, N, or S later"
                                               ELSE *." DP RETRO DATE if any, leave blank now if you need I, N, or S later"
                                            END_CHOICE
            END
         IF TYPE = "POLY" THEN
            DP_PAD = PL_PAD
         IF TYPE = "JUD","EPL" THEN
            BEGIN
               IF IR = "R" THEN
                  BEGIN
                     PRINT " "
                     PRINT "PREVIOUS EMP RETRO DATE:"|||(FORMAT EMP_RETRO_SAVE USING NN/DD/YYYY)
                     PRINT " "
                     YORN = FN$UPCASE(*."Y if this is correct, N to change")
                  END
               EMP_PAD                    = CHOICE
                                               IR = "R" AND YORN = "Y" THEN EMP_RETRO_SAVE
                                               ELSE *."EMP RETRO DATE if any, leave blank now if you need I or S later"
                                            END_CHOICE
            END
         IF TYPE = "CD" THEN
            BEGIN
               IF IR = "R" THEN
                  BEGIN
                     PRINT " "
                     PRINT "PREVIOUS CRM RETRO DATE:"|||(FORMAT CRM_RETRO_SAVE USING NN/DD/YYYY)
                     PRINT " "
                     YORN = FN$UPCASE(*."Y if this is correct, N to change")
                  END
               CRM_PAD                    = CHOICE
                                               IR = "R" AND YORN = "Y" THEN CRM_RETRO_SAVE
                                               ELSE *."CRM RETRO DATE if any, leave blank now if you need I or S later"
                                            END_CHOICE
               IF IR = "R" THEN
                  BEGIN
                     PRINT " "
                     PRINT "PREVIOUS CON RETRO DATE:"|||(FORMAT CON_RETRO_SAVE USING NN/DD/YYYY)
                     PRINT " "
                     YORN = FN$UPCASE(*."Y if this is correct, N to change")
                  END
               CON_PAD                    = CHOICE
                                               IR = "R" AND YORN = "Y" THEN CON_RETRO_SAVE
                                               ELSE *."CON RETRO DATE if any, leave blank now if you need I or S later"
                                            END_CHOICE
            END
         IF TYPE = "CD","LIM","POLY" THEN
            BEGIN
               IF IR = "R" AND (TYPE NE "POLY" OR (TYPE = "POLY" AND EXP GE "01/01/2014")) THEN
                  BEGIN
                     PRINT " "
                     PRINT "PREVIOUS DSB RETRO DATE:"|||(FORMAT DSB_RETRO_SAVE USING NN/DD/YYYY)
                     PRINT " "
                     YORN = FN$UPCASE(*."Y if this is correct, N to change")
                  END
               IF IR = "I" THEN
                  DSB_SAVE = FN$UPCASE(*."Y if quoting DSB, N if not")
               DSB_PAD = CHOICE
                            TYPE = "POLY" AND IR = "R" AND EXP LT "01/01/2014" THEN " "
                            IR = "R" AND YORN = "Y" THEN DSB_RETRO_SAVE
                            IR = "I" AND DSB_SAVE = "N" THEN " "
                            ELSE *."DSB RETRO DATE if any, leave blank now if NQ or you need I or S later"
                         END_CHOICE
            END
         IF TYPE = "CD","LIM" THEN
            BEGIN
               IF IR = "R" AND (TYPE NE "LIM" OR (TYPE = "LIM" AND EXP GE "01/01/2016")) THEN
                  BEGIN
                     PRINT " "
                     PRINT "PREVIOUS DDDC RETRO DATE:"|||(FORMAT DDDC_RETRO_SAVE USING NN/DD/YYYY)
                     PRINT " "
                     YORN = FN$UPCASE(*."Y if this is correct, N to change")
                  END
               DDDC_PAD = CHOICE
                            TYPE = "LIM" AND IR = "R" AND EXP LT "01/01/2016" THEN " "
                            IR = "R" AND YORN = "Y" THEN DDDC_RETRO_SAVE
                            ELSE *."DDDC RETRO DATE if any, leave blank now if you need I or S later"
                         END_CHOICE
            END
         IF TYPE = "POLY" THEN
            BEGIN
               IF IR = "R" THEN
                  BEGIN
                     PRINT " "
                     PRINT "PREVIOUS CBI RETRO DATE:"|||(FORMAT CBI_RETRO_SAVE USING NN/DD/YYYY)
                     PRINT " "
                     YORN = FN$UPCASE(*."Y if this is correct, N to change")
                  END
               CBI_PAD = CHOICE
                            TYPE = "POLY" AND IR = "R" AND EXP LT "01/01/2016" THEN " "
                            IR = "R" AND YORN = "Y" THEN CBI_RETRO_SAVE
                            ELSE *."CBI RETRO DATE if any, leave blank now if you need I or S later"
                         END_CHOICE
            END
         APP_DATE                     = *."APPLICATION DATE"
         IF TYPE = "CD" THEN
            APP_DATE_WORDS            = *."APP WORDS (ADDL WORDING FOR APP DATE)"
         IF IR = "R" THEN
            BEGIN
               PRINT " "
               PRINT "PREVIOUS ASSOCIATION:"|||ASSOC_SAVE
               PRINT " "
               YORN = FN$UPCASE(*."Y if this is correct, N to change (AILA will store as blank)")
            END
         ASSOC                     = CHOICE
!                                        ASSOC_SAVE = "ASSE" AND ORIG_EFF GE "02/01/2019" THEN "ASSP"
                                        IR = "R" AND YORN = "Y"                          THEN ASSOC_SAVE
! 5/6/2009 - add QN to GRAM_TABLE in PROG, should be the only place to add and will work for everything TG
                                        QN VIA GRAM_TABLE NE "N"                         THEN " "
                                        CUS_NUM = 10010609 AND TYPE = "IMM"              THEN " "
                                        TYPE = "SFPE"                                    THEN "SFPE"
                                        TYPE = "IMM","JUD","LNC","ENV","NON","PSY","GEN","FOOD","BROK","MISC","PAR",
                                               "BSM","MJM","EPL" THEN " "
                                        ELSE FN$UPCASE(*."association - GET FROM QUOTE REQUEST (blank, not NASPPG)") ! everyone else: FOR, CD, DE, HA, LIM, MED
                                     END_CHOICE
         WHILE CANADA_YN = "Y" AND TYPE = "MED" AND ASSOC NE "FMC","OAFM"," "
            BEGIN
               ASSOC = FN$UPCASE(*."association again - FMC, OAFM or blank ONLY for Canada MEDS!")
            END
         IF ASSOC = "SCCCR", "CCMMO", "CCMO" THEN
            BEGIN
               PRINT "Nothing with SCCCR or CCMMO / CCMO"
               PRINT "You cannot store this record, see BECKY / PR!!!"
               YORN = FN$UPCASE(*."Y, N to exit")
               IF YORN NE "R","T" THEN
               ABORT
            END
         IF ORIG_EXP GE "01/01/2020" AND TYPE = "JUD" THEN
            BEGIN
               PRINT " "
               PRINT " "
               PRINT "test judges in new system"
               PRINT "give to renee for now"
               PRINT " "
               PRINT " "
               YORN = FN$UPCASE(*."Y, N to exit")
               IF YORN NE "R","T" THEN
               ABORT
            END
         WHILE ASSOC = "CRN"
            BEGIN
               ASSOC = FN$UPCASE(*."ASSOC - cannot store CRN anymore!")
            END
         WHILE ASSOC = "CRN","FMC","OAFM" AND CANADA_YN = "N"
            BEGIN
               ASSOC = FN$UPCASE(*."ASSOC - you cannot store a Canadian assoc if insured is not in Canada!")
            END
!         IF TYPE = "ISP" AND ASSOC = " " THEN
!            BEGIN
!               PRINT "Non-association ISP wording needs to be proofed - see Mel/RT!!!!"
!               PRINT "Ending this procedure now!!!"
!               ABORT
!            END
         PRINT " "
         PRINT "Association equals:"|||ASSOCIATION
         PRINT " "
         IF IR = "R" THEN
            BEGIN
               IF TYPE = "FOR","LNC","LIM","MISC","ENV","PAR","BSM","GEN","BROK","FOOD","NON",
                         "MJM","SC","LIMSC","EPL" OR ASSOC = "APA","NPA" THEN
                  BEGIN
                     PRINT " "
                     PRINT "PREVIOUS JOB CODE:"|||JC_SAVE
                     PRINT " "
                     YORN = FN$UPCASE(*."Y if this is correct, N to change")
                  END ELSE
                     YORN = "Y"
            END
         JOB_CODE                     = CHOICE
!                                           ACR_YN = "Y" AND IR = "R" AND ASSOC = " " THEN "ACR"
                                           TYPE    = "MED" AND ASSOC = "PEP"                THEN "PEP"
                                           TYPE    = "MED" AND ASSOC = "VMN"        THEN "FAMMED"
                                           TYPE    = "MED" AND ASSOC = "AFCC","CCMO","CCMMO","MAC" THEN "MED"
                                           TYPE    = "MED"                                  THEN "ARBMED"
! This is not always true and request always has to type it in so why auto-store? 1/2/15-rt     ASSOC   = "NDAA"                                 THEN "DA"
                                           ASSOC   = "AAPP"                                 THEN "LE"
                                           CUS_NUM = 10020464 AND TYPE = "BROK"             THEN "MISCEO"
                                           CUS_NUM = 10003096 AND TYPE = "NON"              THEN "CD"
                                           IR = "R" AND YORN = "Y" AND JC_SAVE NE "AILA" THEN JC_SAVE
                                           TYPE    = "BROK","PAR","BSM","NON","ENV","FOOD","FOR","LNC","LIM","MISC","GEN","MJM" THEN FN$UPCASE(*."JOB_CODE")
                                           ASSOC   = "APA","NPA" OR (TYPE = "POLY" AND ASSOC = " ") THEN FN$UPCASE(*."JOB_CODE")
                                           ELSE TYPE
                                        END_CHOICE
         JC_REQUEST                = JOB_CODE
         IF JC_REQUEST = "COMBO" AND TYPE NE "MJM","GEN" THEN
            BEGIN
               PRINT "You cannot store a combo quote THAT IS NOT GEN/MJM!"
               PRINT "Return to requestor so they can notify R/C!"
               PRINT "Ending this procedure now!"
               ABORT
            END
!         IF TYPE_REQUEST = "BSM" AND JC_REQUEST = "PAR","PARA" AND VALID_DATE GE "01/01/2014" THEN
!            BEGIN
!               PRINT " "
!               PRINT " "
!               PRINT "STARTING JAN 2014 ONLY!"
!               PRINT "Paralegals have new acct code!"
!               PRINT "Give to PR for testing"
!               PRINT " "
!               PRINT " "
!               YORN = FN$UPCASE(*."Y to exit now")
!               IF YORN NE "R","T" THEN
!                  ABORT
!            END
         IF TYPE = "LIM" AND JOB_CODE = "MED" THEN
            MED_END = FN$UPCASE(*."Y to add MEDIATOR ENDORSEMENT, N if not")
         WORDING                   = CHOICE
                                        ! TG 10/30/08 No wording stored for below (cuz we don't run policy?) Leave in for now per BJ
                                        CUS_NUM = 10012420 THEN "LPO430"
                                        TYPE = "BSM" AND CUS_NUM = 10018636 THEN "CMEL95"
                                        TYPE = "PAR","BSM" THEN FN$UPCASE(*."wording")
                                        TYPE = "CD" AND CUS_NUM = 10003096 THEN "LAW93"
                                        TYPE = "ENV" THEN FN$UPCASE(*."WORDING - LAP or CML")
                                        TYPE = "FOOD" THEN "ATR"
                                        TYPE = "EPL" THEN "NOPW"
                                        TYPE = "MJM" AND JOB_CODE = "RCE" THEN "ATR"
                                        TYPE = "GEN" AND JOB_CODE = "ACPA" THEN "AAACPA"
                                        TYPE = "GEN" AND JOB_CODE = "CALI" THEN "CALI"
                                        TYPE = "GEN" AND JOB_CODE = "CPA" THEN "LSW249"
                                        TYPE = "GEN" AND JOB_CODE = "MEDX" THEN "ME"
                                        TYPE = "GEN" AND JOB_CODE = "PAR" THEN "JRR"
                                        TYPE = "BROK" AND CUS_NUM = 10020464 THEN "ATR" !(BROK_NON) 
                                        TYPE = "MISC" AND CUS_NUM = 10017464 THEN "PARA" !(new para for shulman) 
                                        TYPE = "MISC" AND CUS_NUM = 10018098, 10005570, 10017235 THEN "LAW93"
! need pep and pcp isn't triggering right-RT see mods                    TYPE = "MISC" THEN "ATR"
!                                        TYPE = "NON" AND CUS_NUM = 10007895 THEN "LPO 425C"
                                        TYPE = "NON" AND CUS_NUM = 10018453 THEN "ATR"
                                        TYPE = "GEN","NON","MISC","MJM" THEN FN$UPCASE(*."WORDING - enter NOPW if quoting non-Lloyd's")
                                        TYPE = "BSM","BROK" THEN FN$UPCASE(*."WORDING - if you are getting this prompt you probably need to see a programmer")
                                        ELSE " "
                                     END_CHOICE
         TS                        = CHOICE
                                        CANADA_YN = "Y" AND TYPE = "IMM","DE","FOOD","FOR","POLY","SC","SFPE" THEN STATE
                                        ELSE TAX_STATE
                                     END_CHOICE
! moved PG below TS as it's not calculating right if above TS.-RT 10.21.14
         PG                        = PURCH_GROUP
! make sure you check COMMON_AV for the type/assoc to assign the correct Canadian account code
         WHILE TAXABLE_STATE = "**" AND CANADA_YN = "Y"
            BEGIN
               PRINT "You cannot store Canada insureds for this type/assoc!"
               PRINT "Check with PROGRAMMERS or R/C for what to do."
               PRINT "Ending this procedure now."
               ABORT
            END
         !All numeric queries
         BROKER_NUMBER = CHOICE
!                            TS = "VI" THEN 815
!                            TS = "FL" THEN 1198
                            CUS_NUM = "10014108" THEN 793
                            ELSE *."BROKER NUMBER - 9999 IF NEW, ONLY SPACE THRU IF NONE!"
                         END_CHOICE
         IF TS = "MT" AND BN = 9999 AND IR = "I" THEN ! ADD "AND QN NE ######" IF WE GET ONE THAT IS OK
            BEGIN
               PRINT "This is an initial MT insured with a new broker (have not used before)!"
               PRINT "We need to confirm if broker is paying taxes and what stamp rate to use!"
               PRINT "See R/C for more information if necessary!"
               PRINT "Ending this procedure now!!!!"
               ABORT
            END
         WHILE BN NE "1108","1055" AND CANADA_YN = "Y"
            BEGIN
               PRINT "Broker number does not = 1055 or 1108!!!"
               BROKER_NUMBER = *."BROKER NUMBER - MUST BE 1055 or 1108 FOR ALL CANADA!"
            END
         WHILE BN = 607
            BEGIN
               PRINT "You cannot store this broker for this type!"
               PRINT "See R/C if you disagree!"
               BROKER_NUMBER                   = *."BROKER NUMBER"
            END
         WHILE BN = 129, 147 AND TYPE = "SC"
            BEGIN
               PRINT "This broker is no longer used for Safety; enter zero"
               QPROG20.BROKER_NUMBER          = *."BROKER NUMBER"
            END
         QAX = CHOICE
                  TYPE = "FOOD","EPL"                                                THEN "Y"
                  TYPE = "PAR","BSM","PSY","AE","ENV","GEN","NON","MISC","MED","JUD","MJM" THEN "Y"
                  TYPE = "SC" AND CUS_NUM = 10014865                                 THEN "Y"
                  QN VIA GRAM_TABLE NE "N"                                           THEN "Y"
                  ELSE FN$UPCASE(*."quote as expiring (QAX) Y or N")
               END_CHOICE
         WHILE TYPE = "IMM" AND QAX = "N" AND PERCENT_AP = " "
            BEGIN
               PERCENT_AP    = *."FINAL MULTIPLIER a.k.a. PERCENT_AP a.k.a. AP percent NOT BLANK!!!"
               IF PERCENT_AP = " " THEN PRINT "FINAL MULTIPLIER CANNOT BE BLANK!!!"
            END
         IF TYPE NE "IMM","SFPE","HA","DE","PAR","BSM","BROK","GEN","FOOD","PSY","MISC","MED","CD","NON","EPL","ENV","JUD" THEN
            PERCENT_AP          = CHOICE
                                     TYPE = "POLY","FOR","LIM","ISP","LIMSC","SC" AND QN VIA GRAM_TABLE = "N" THEN *."additional premium% (AP)"
                                     TYPE = "FOOD" THEN *."additional premium% (AP)"
                                     ELSE *."FINAL MULTIPLIER a.k.a. PERCENT_AP a.k.a. AP percent"
                                  END_CHOICE
	IF TYPE = "MED" AND TS NE "FL" THEN
	   MED_PF = *."Mediator Policy Fee - 1 for $25.00 OR 2 for $0."
         POLICY_FEE             = CHOICE
                                     TS = "DC","HI","ID","LA","MD","MS","MT" AND ORIG_EFF LT "01/01/2020" THEN 0
                                     TS VIA CEMI05:[DTR.COMMON]POL_FEE_TABLE = "N"      THEN 0
                                     CANADA_YN = "Y"                                    THEN 0
                                     CUS_NUM = 10025555 AND TYPE = "LIM"                THEN 0
                                     CUS_NUM = 10005509                                 THEN 0
                                     TYPE = "IMM","PSY","JUD"                           THEN 0
                                     ASSOC = "NACDL","SCMA","CADC","NDAA"               THEN 0
                                     TYPE = "HA","DE"                                   THEN 50
                                     TS = "ID" AND PG NE "N"                            THEN 0
                                     TYPE = "MED" AND MED_PF = 1                        THEN 25
                                     TYPE = "MED" AND MED_PF = 2                        THEN 0
                                     TYPE = "LIM" AND STATE = "IL"                      THEN 75
                                     TYPE = "LIM" AND STATE = "CA","NY"                 THEN 100
                                     TYPE = "LIM"                                       THEN 50
                                     TYPE = "POLY"                                      THEN 75
                                     ELSE *."SERVICE CHARGE (FORMERLY POLICY FEE)"
                                  END_CHOICE
         IF TS VIA CEMI05:[DTR.COMMON]POL_FEE_TABLE = "N" THEN
            PRINT "No service charge stored - not allowed in this state!"
         FILING_FEE             = CHOICE
                                     CANADA_YN = "Y" THEN 0
                                     TYPE = "EPL","NON","BROK","GEN" THEN *."any NON-TAXABLE (taxed=N) non-LL carrier, broker, etc fees"
                                     TS = "WA","FL" THEN 0
                                     ELSE FILING_FEE_PRINT
                                  END_CHOICE
         IF TYPE = "FOR","LNC","POLY" THEN
            PART_TIME_CASES           = *."CASES"
         IF TYPE = "POLY","AE","ENV","FOR","ISP","LIMSC","LNC","SC","FOOD" AND QN VIA GRAM_TABLE = "N" THEN
            INCOME                    = *."INCOME"
         IF TYPE = "SFPE" THEN
             INCOME                   = *."CONSULTING fees"
         IF TYPE = "EPL" THEN
            BEGIN
               ATTORNEYS  = *."the number of full time emps"
               PARALEGALS = *."the number of part time emps"
!               SALESPERSON = FN$UPCASE(*."the salesperson")
             END
         !All alpha queries
         IF TYPE = "SC","LIM" AND QN VIA GRAM_TABLE = "N" THEN
            CATEGORY                  = FN$UPCASE(*."CATEGORY")
         ! DP - in case we need to find and forget it is called DISPRO - also fixed IMM query for new included DP 7/29/2009 TG
         ! NEW - eff 9/1/15, most WILL get optional higher limits for IMM, so add new prompt (MOST are YES) for those 6/25/15 TG
         DISPRO                 = CHOICE
                                     TYPE = "JUD" THEN "Y"
                                     TYPE = "IMM" THEN FN$UPCASE(*."Y to quote higher DP limits, N if not (MOST are YES now!)")
                                     TYPE = "LIM","CD" THEN FN$UPCASE(*."DISPRO(Y/N) (Remember: MOST are Y)")
                                     TYPE = "POLY" THEN "Y"
                                     ELSE "N"
                                  END_CHOICE
! ****************************************************************************************************
!                     IMM SECTION
! ****************************************************************************************************
         IF TYPE = "IMM" THEN
            BEGIN
!               NID        = FN$UPCASE(*."Y if quoting NIL Immigration Claims Deductible, N if not")
               ZD         = FN$UPCASE(*."Y if quoting Zero (amending) ded, N if not")
               ATTORNEYS  = *."the number of attorneys"
               PARALEGALS = *."the number of paralegals"
               !*************************************************
               RL1        = FN$UPCASE(*."the first LIMITS letter")
               WHILE RL1 NE "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                  BEGIN
                     PRINT "the only valid limits are I, J, K, L, AS, AM"
                     RL1 = FN$UPCASE(*."the first LIMITS letter")
                  END
               DED1       = FN$UPCASE(*."the first DEDUCTIBLE letter")
               IF QAX = "Y" THEN
                  PREMIUM_Q1 = *."the first QAX PREMIUM amount"
               !*************************************************
               RL2 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
               WHILE RL2 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                  BEGIN
                     PRINT "the only valid limits are I, J, K, L, AS, AM"
                     RL2 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                  END
               IF RL2 NE " " THEN
                  BEGIN
                     DED2 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q2 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL3 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL3 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL3 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL3 NE " " THEN
                  BEGIN
                     DED3 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q3 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL4 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL4 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL4 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL4 NE " " THEN
                  BEGIN
                     DED4 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q4 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL5 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL5 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL5 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL5 NE " " THEN
                  BEGIN
                     DED5 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q5 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL6 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL6 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL6 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL6 NE " " THEN
                  BEGIN
                     DED6 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q6 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL7 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL7 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL7 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL7 NE " " THEN
                  BEGIN
                     DED7 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q7 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL8 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL8 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL8 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL8 NE " " THEN
                  BEGIN
                     DED8 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q8 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL9 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL9 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL9 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL9 NE " " THEN
                  BEGIN
                     DED9 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q9 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL10 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL10 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL10 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL10 NE " " THEN
                  BEGIN
                     DED10 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q10 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL11 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL11 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL11 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL11 NE " " THEN
                  BEGIN
                     DED11 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q11 = *."the next QAX PREMIUM amount"
                     !*************************************************************************
                     RL12 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     WHILE RL12 NE " ", "I", "J", "K", "L", "AS", "AM" AND QAX = "N"
                        BEGIN
                           PRINT "the only valid limits are I, J, K, L, AS, AM"
                           RL12 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        END
                  END
               IF RL12 NE " " THEN
                  BEGIN
                     DED12 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q12 = *."the next QAX PREMIUM amount"
                  END
            END ! IMM SECTION
! ****************************************************************************************************
!                     FOOD SECTION
! ****************************************************************************************************
! sorry if this is confusing to other programmers - to make storing clearer for the data entry person and
! still be able to use specified fields for PL only and PL with BI/PD we have to ask for limits/deds out of order
! Now to add to the confusion, a new 500 sublimit for BI/PD in addition to the 250 BI/PD sublimit
         IF TYPE = "FOOD" THEN
            BEGIN
               PRINT " "
               PRINT "   CHOICES FOR QUOTE OPTIONS:"
               PRINT "1 - PL only"
               PRINT "2 - PL with 250 BI/PD LIMITS only"
               PRINT "3 - PL with 500 BI/PD LIMITS only"
               PRINT "4 - all"
               PRINT " "
               YORN = *."choice"
               PRINT " "
               PRINT "First we will store limits and deductibles..."
               PRINT " "
! There is a maximum of 6 quote options so ask for 6 limits and deds first and store in lim and ded 1 thru 6
               RL1 = FN$UPCASE(*."the first LIMITS letter")
               DED1 = FN$UPCASE(*."the first DEDUCTIBLE letter")
               ! year end remove qns
               WHILE INCOME GT 200000 AND DED1 VIA CEMI05:[DTR.COMMON]COMMON_DED LT 2500 AND DED1 NE " " AND QN NE 99999
                  BEGIN
                     PRINT "You must store at least $2500 deductible for income over $200,000!"
                     DED1 = FN$UPCASE(*."the first DEDUCTIBLE letter")
                  END
               RL2 = FN$UPCASE(*."the next LIMITS letter, space thru if done")
               IF RL2 NE " " THEN
                  BEGIN
                     DED2 = FN$UPCASE(*."the next DEDUCTIBLE letter")
! take out qn for year end
                     WHILE INCOME GT 200000 AND DED2 VIA CEMI05:[DTR.COMMON]COMMON_DED LT 2500 AND DED2 NE " " AND QN NE 99999
                        BEGIN
                           PRINT "You must store at least $2500 deductible for income over $200,000!"
                           DED2 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                        END
                     RL3 = FN$UPCASE(*."the next LIMITS letter, space thru if done")
                  END
               IF RL3 NE " " THEN
                  BEGIN
                     DED3 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     WHILE INCOME GT 200000 AND DED3 VIA CEMI05:[DTR.COMMON]COMMON_DED LT 2500 AND DED3 NE " "
                        BEGIN
                           PRINT "You must store at least $2500 deductible for income over $200,000!"
                           DED3 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                        END
                     RL4 = FN$UPCASE(*."the next LIMITS letter, space thru if done")
                  END
               IF RL4 NE " " THEN
                  BEGIN
                     DED4 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     WHILE INCOME GT 200000 AND DED4 VIA CEMI05:[DTR.COMMON]COMMON_DED LT 2500 AND DED4 NE " "
                        BEGIN
                           PRINT "You must store at least $2500 deductible for income over $200,000!"
                           DED4 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                        END
                     RL5 = FN$UPCASE(*."the next LIMITS letter, space thru if done")
                  END
               IF RL5 NE " " THEN
                  BEGIN
                     DED5 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     WHILE INCOME GT 200000 AND DED5 VIA CEMI05:[DTR.COMMON]COMMON_DED LT 2500 AND DED5 NE " "
                        BEGIN
                           PRINT "You must store at least $2500 deductible for income over $200,000!"
                           DED5 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                        END
                     RL6 = FN$UPCASE(*."the next LIMITS letter, space thru if done")
                  END
               IF RL6 NE " " THEN
                  BEGIN
                     DED6 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     WHILE INCOME GT 200000 AND DED6 VIA CEMI05:[DTR.COMMON]COMMON_DED LT 2500 AND DED6 NE " "
                        BEGIN
                           PRINT "You must store at least $2500 deductible for income over $200,000!"
                           DED6 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                        END
                  END
! premiums are stored as follows:
! All PL only in premiums 1 thru 6 and correspond with limits 1 thru 6
! All PL with 250 BI/PD sublimit in premiums 7 thru 12 and correspond to limits 1 thru 6
! All PL with 500 BI/PD sublimit in premiums 13 thru 18 and correspond with limits 1 thru 6
               PRINT " "
               PRINT "Now we will store all PREMIUMS..."
               PRINT " "
               PREMIUM_Q1 = *."the first PL only PREMIUM amount, space thru if not quoted"
! 1st 250
               IF YORN = "2","4" THEN
                  BEGIN
                     PREMIUM_Q7 = *."the first PL with BI/PD 250 PREMIUM amount"
! old code                     PREMIUM_Q7 = CHOICE
!                                     DED1 = "D" THEN 0
!                                     DED1 = "E" AND RL1 VIA CEMI05:[DTR.COMMON]COMMON_LIMITS_AGG GT 1000000 THEN 0
!                                     DED1 = "E" AND INCOME GT 200000 THEN 0
!                                     ELSE *."the first PL with BI/PD 250 PREMIUM amount"
!                                  END_CHOICE
!                     IF RL1 NE " " AND PREMIUM_Q7 = 0 THEN
!                        BEGIN
!                           PRINT " "
!                           PRINT "*******************************************************"
!                           PRINT "No BI/PD 250 will store for this limit/deductible"
!                           PRINT "  due to minimums for income or category"
!                           PRINT "*******************************************************"
!                           PRINT " "
!                        END
                  END
! 1st 500
               IF YORN = "3","4" THEN
                  BEGIN
                     PREMIUM_Q13 = *."the first PL with BI/PD 500 PREMIUM amount"
!                     PREMIUM_Q13 = CHOICE
!                                      DED1 = "D" THEN 0
!                                      DED1 = "E" AND RL1 VIA CEMI05:[DTR.COMMON]COMMON_LIMITS_AGG GT 1000000 THEN 0
!                                      DED1 = "E" AND INCOME GT 200000 THEN 0
!                                      ELSE *."the first PL with BI/PD 500 PREMIUM amount"
!                                   END_CHOICE
!                     IF RL1 NE " " AND PREMIUM_Q13 = 0 THEN
!                        BEGIN
!                           PRINT " "
!                           PRINT "*******************************************************"
!                           PRINT "No BI/PD 500 will store for this limit/deductible"
!                           PRINT "  due to minimums for income or category"
!                           PRINT "*******************************************************"
!                           PRINT " "
!                        END
                  END
               IF RL2 NE " " THEN
                  BEGIN
                     PREMIUM_Q2 = *."the second PL only PREMIUM amount, space thru if not quoted"
! 2nd 250
                     IF YORN = "2","4" THEN
                        BEGIN
                           PREMIUM_Q8 = *."the second PL with BI/PD 250 PREMIUM amount"
                        END
! 2nd 500
                     IF YORN = "3","4" THEN
                        BEGIN
                           PREMIUM_Q14 = *."the second PL with BI/PD 500 PREMIUM amount"
                        END
                  END
               IF RL3 NE " " THEN
                  BEGIN
                     PREMIUM_Q3 = *."the third PL only PREMIUM amount, space thru if not quoted"
! 3rd 250
                     IF YORN = "2","4" THEN
                        BEGIN
                           PREMIUM_Q9 = *."the third PL with BI/PD 250 PREMIUM amount"
                        END
! 3rd 500
                     IF YORN = "3","4" THEN
                        BEGIN
                           PREMIUM_Q15 = *."the third PL with BI/PD 500 PREMIUM amount"
                        END
                  END
               IF RL4 NE " " THEN
                  BEGIN
                     PREMIUM_Q4 = *."the fourth PL only PREMIUM amount, space thru if not quoted"
! 4th 250
                     IF YORN = "2","4" THEN
                        BEGIN
                           PREMIUM_Q10 = *."the fourth PL with BI/PD 250 PREMIUM amount"
                        END
! 4th 500
                     IF YORN = "3","4" THEN
                        BEGIN
                           PREMIUM_Q16 = *."the fourth PL with BI/PD 500 PREMIUM amount"
                        END
                  END
               IF RL5 NE " " THEN
                  BEGIN
                     PREMIUM_Q5 = *."the fifth PL only PREMIUM amount, space thru if not quoted"
! 5th 250
                     IF YORN = "2","4" THEN
                        BEGIN
                           PREMIUM_Q11 = *."the fifth PL with BI/PD 250 PREMIUM amount"
                        END
! 5th 500
                     IF YORN = "3","4" THEN
                        BEGIN
                           PREMIUM_Q17 = *."the fifth PL with BI/PD 500 PREMIUM amount"
                        END
                  END
               IF RL6 NE " " THEN
                  BEGIN
                     PREMIUM_Q6 = *."the sixth PL only PREMIUM amount, space thru if not quoted"
! 6th 250
                     IF YORN = "2","4" THEN
                        BEGIN
                           PREMIUM_Q12 = *."the sixth PL with BI/PD 250 PREMIUM amount"
                        END
! 6th 500
                     IF YORN = "3","4" THEN
                        BEGIN
                           PREMIUM_Q18 = *."the sixth PL with BI/PD 500 PREMIUM amount"
                        END ! YORN = 3,4
                  END !if RL6 ne " "
            END ! FOOD SECTION
! ****************************************************************************************************
!                       POLY SECTION         
! ****************************************************************************************************
         IF QAX = "N" AND TYPE = "POLY" THEN
            BEGIN
               RL1 = CHOICE
!                        IR = "I" AND INCOME LE 50000 AND (S = "TX","AK" OR (S = "CA" AND C = "Los Angeles","San Francisco") OR (S = "FL" AND C = "Miami")) THEN "E"
                        IR = "I" THEN "H"
                        ELSE FN$UPCASE(*."the first LIMITS letter")
                     END_CHOICE
               RL2 = CHOICE
!                        IR = "I" AND INCOME LE 50000 AND (S = "TX","AK" OR (S = "CA" AND C = "Los Angeles","San Francisco") OR (S = "FL" AND C = "Miami")) THEN "H"
                        IR = "I" THEN "T"
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL3 = CHOICE
                        RL2 = " " THEN " "
!                        IR = "I" AND INCOME LE 50000 AND (S = "TX","AK" OR (S = "CA" AND C = "Los Angeles","San Francisco") OR (S = "FL" AND C = "Miami")) THEN "T"
                        IR = "I" THEN "K"
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL4 = CHOICE
                        RL3 = " " THEN " "
!                        IR = "I" AND INCOME LE 50000 AND (S = "TX","AK" OR (S = "CA" AND C = "Los Angeles","San Francisco") OR (S = "FL" AND C = "Miami")) THEN "K"
                        IR = "I" THEN "L"
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL5 = CHOICE
                        RL4 = " " THEN " "
!                        IR = "I" AND INCOME LE 50000 AND (S = "TX","AK" OR (S = "CA" AND C = "Los Angeles","San Francisco") OR (S = "FL" AND C = "Miami")) THEN "K"
                        IR = "I" THEN "AS"
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               DED1 = "Z"
            END ! POLY SECTION
! ****************************************************************************************************
!                            SC SECTION         
! ****************************************************************************************************
         IF QAX = "N" AND TYPE = "SC" THEN
            BEGIN
               YORN = FN$UPCASE(*."Y if this is a special (SMILEY) quote, N if not")
               RL1 = CHOICE
                        IR = "I" AND TYPE = "SC" THEN "H"
                        ELSE FN$UPCASE(*."the first LIMITS letter")
                     END_CHOICE
               RL2 = CHOICE
                        TYPE = "SC" AND IR = "R" AND YORN = "Y" AND CATEGORY = "B" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        TYPE = "SC" AND IR = "I" AND YORN = "Y" AND CATEGORY = "B" THEN "T"
                        TYPE = "SC" AND IR = "R" THEN RL1
                        ELSE "H"
                     END_CHOICE
               RL3 = CHOICE
                        RL2 = " " THEN " "
                        TYPE = "SC" AND IR = "R" AND YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "B" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        IR = "R" AND TYPE = "SC" AND CATEGORY = "A" THEN RL1
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "B" AND YORN = "Y" THEN "K"
                        TYPE = "SC" AND IR = "I" AND (CATEGORY = "B" OR (CATEGORY = "A" AND YORN = "Y")) THEN "T"
                        ELSE "H"
                     END_CHOICE
               RL4 = CHOICE
                        RL3 = " " THEN " "
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "B" AND YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "A" AND YORN = "Y" THEN RL3
                        IR = "R" AND TYPE = "SC" AND CATEGORY = "A" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "B" THEN RL3
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "B" AND YORN = "Y" THEN "L"
                        ELSE "T"
                     END_CHOICE
               RL5 = CHOICE
                        RL4 = " " THEN " "
                        TYPE = "SC" AND IR = "R" AND YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        IR = "R" AND TYPE = "SC" AND CATEGORY = "A" THEN RL4
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "B" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                        TYPE = "SC" AND IR = "I" AND (CATEGORY = "B" OR (CATEGORY = "A" AND YORN = "Y")) THEN "K"
                        ELSE "T"
                     END_CHOICE
               RL6 = CHOICE
                        RL5 = " " THEN " "
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "A" AND YORN = "Y" THEN RL5
                        IR = "R" AND TYPE = "SC" AND CATEGORY = "A" THEN RL4
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "B" THEN RL5
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "A" AND YORN = "Y" THEN "K"
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "B" THEN "K"
                        ELSE "T"
                     END_CHOICE
               RL7 = CHOICE
                        RL6 = " " THEN " "
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "A" AND YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        TYPE = "SC" AND IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "A" AND YORN = "Y" THEN "L"
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "B" THEN "L"
                        ELSE "K"
                     END_CHOICE
               RL8 = CHOICE
                        RL7 = " " THEN " "
                        TYPE = "SC" AND IR = "R" AND CATEGORY = "A" AND YORN = "Y" THEN RL7
                        TYPE = "SC" AND IR = "R" THEN RL7
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "A" AND YORN = "Y" THEN "L"
                        TYPE = "SC" AND IR = "I" AND CATEGORY = "B" THEN "L"
                        ELSE "K"
                     END_CHOICE
               RL9 = CHOICE
                        RL8 = " " THEN " "
                        TYPE = "SC" AND CATEGORY = "B" THEN " "
                        TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN " "
                        TYPE = "SC" AND IR = "R" THEN RL7
                        ELSE "K"
                     END_CHOICE
               RL10 = CHOICE
                         RL9 = " " THEN " "
                         TYPE = "SC" AND IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                         ELSE "L"
                      END_CHOICE
               RL11 = CHOICE
                         RL10 = " " THEN " "
                         TYPE = "SC" AND IR = "R" THEN RL10
                         ELSE "L"
                      END_CHOICE
               RL12 = CHOICE
                         RL11 = " " THEN " "
                         TYPE = "SC" AND IR = "R" THEN RL10
                         ELSE "L"
                      END_CHOICE
               RL13 = CHOICE
                         RL12 = " " THEN " "
                         IR = "I" THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               RL14 = CHOICE
                         RL13 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               RL15 = CHOICE
                         RL14 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               RL16 = CHOICE
                         RL15 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               RL17 = CHOICE
                         RL16 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               RL18 = CHOICE
                         RL17 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               DED1 = CHOICE
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "D"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN "F"
                         TYPE = "SC" THEN "D"
                         ELSE "B"
                      END_CHOICE
               DED2 = CHOICE
                         RL2 = " " THEN " "
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN "F"
                         ELSE "E"
                      END_CHOICE
               DED3 = CHOICE
                         RL3 = " " THEN " "
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN "F"
                         ELSE "F"
                      END_CHOICE
               DED4 = CHOICE
                         RL4 = " " THEN " "
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "D"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN "F"
                         TYPE = "SC" THEN "D"
                         ELSE "B"
                      END_CHOICE
               DED5 = CHOICE
                         RL5 = " " THEN " "
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                         ELSE "E"
                      END_CHOICE
               DED6 = CHOICE
                         RL6 = " " THEN " "
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                         ELSE "F"
                      END_CHOICE
               DED7 = CHOICE
                         RL7 = " " THEN " "
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "D"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                         TYPE = "SC" THEN "D"
                         ELSE "B"
                      END_CHOICE
               DED8 = CHOICE
                         RL8 = " " THEN " "
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                         ELSE "E"
                      END_CHOICE
               DED9 = CHOICE
                         RL9 = " " THEN " "
                         TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "F"
                         TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "E"
                         TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                         ELSE "F"
                       END_CHOICE
               DED10 = CHOICE
                          RL10 = " " THEN " "
                          TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "D"
                          TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "F"
                          TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "F"
                          TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                          TYPE = "SC" THEN "D"
                          ELSE "B"
                       END_CHOICE
               DED11 = CHOICE
                          RL11 = " " THEN " "
                          TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "E"
                          TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "E"
                          TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "E"
                          TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                          ELSE "E"
                       END_CHOICE
               DED12 = CHOICE
                          RL12 = " " THEN " "
                          TYPE = "SC" AND CATEGORY = "A" AND YORN NE "Y" THEN "F"
                          TYPE = "SC" AND CATEGORY = "A" AND YORN = "Y" THEN "F"
                          TYPE = "SC" AND CATEGORY = "B" AND YORN NE "Y" THEN "F"
                          TYPE = "SC" AND CATEGORY = "B" AND YORN = "Y" THEN " "
                          ELSE "F"
                       END_CHOICE
               DED13 = CHOICE
                          TYPE NE "SC" THEN " "
                          IR = "I" THEN " "
                          IR = "R" AND RL13 = " " THEN " "
                          IR = "R" AND CATEGORY = "A" AND YORN NE "Y" THEN "D"
                          IR = "R" AND CATEGORY = "B" THEN " "
                          TYPE = "SC" THEN "D"
                          ELSE "B"
                       END_CHOICE
               DED14 = CHOICE
                          TYPE NE "SC" THEN " "
                          IR = "I" THEN " "
                          IR = "R" AND RL14 = " " THEN " "
                          IR = "R" AND CATEGORY = "A" AND YORN NE "Y" THEN "E"
                          IR = "R" AND CATEGORY = "B" THEN " "
                          ELSE "E"
                       END_CHOICE
               DED15 = CHOICE
                          TYPE NE "SC" THEN " "
                          IR = "I" THEN " "
                          IR = "R" AND RL15 = " " THEN " "
                          IR = "R" AND CATEGORY = "A" AND YORN NE "Y" THEN "F"
                          IR = "R" AND CATEGORY = "B" THEN " "
                          ELSE "F"
                       END_CHOICE
               DED16 = CHOICE
                          TYPE NE "SC" THEN " "
                          IR = "I" THEN " "
                          IR = "R" AND RL16 = " " THEN " "
                          IR = "R" AND CATEGORY = "A" AND YORN NE "Y" THEN "D"
                          IR = "R" AND CATEGORY = "B" THEN " "
                          TYPE = "SC" THEN "D"
                          ELSE "B"
                       END_CHOICE
               DED17 = CHOICE
                          TYPE NE "SC" THEN " "
                          IR = "I" THEN " "
                          IR = "R" AND RL17 = " " THEN " "
                          IR = "R" AND CATEGORY = "A" AND YORN NE "Y" THEN "E"
                          IR = "R" AND CATEGORY = "B" THEN " "
                          ELSE "E"
                       END_CHOICE
               DED18 = CHOICE
                          TYPE NE "SC" THEN " "
                          IR = "I" THEN " "
                          IR = "R" AND RL18 = " " THEN " "
                          IR = "R" AND CATEGORY = "A" AND YORN NE "Y" THEN "F"
                          IR = "R" AND CATEGORY = "B" THEN " "
                          ELSE "F"
                       END_CHOICE
            END ! SC SECTION
! ****************************************************************************************************
!                       ISP SECTION         
! ****************************************************************************************************
         IF QAX = "N" AND TYPE = "ISP" THEN
            BEGIN
!               YORN = FN$UPCASE(*."Y if this is a special (SMILEY) quote, N if not")
               RL1 = CHOICE
                        IR = "I" THEN "H"
                        ELSE FN$UPCASE(*."the first LIMITS letter")
                     END_CHOICE
               RL2 = RL1
               RL3 = CHOICE ! can just be RL1 and probably don't need the blank line until rl6
                        RL2 = " " THEN " "
!                        TYPE = "ISP" AND YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE RL1
                     END_CHOICE
               RL4 = CHOICE
                        RL3 = " " THEN " "
!                        TYPE = "ISP" AND YORN = "Y" THEN RL3
                        IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE "T"
                     END_CHOICE
               RL5 = CHOICE
                        RL4 = " " THEN " "
!                        TYPE = "ISP" AND YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE RL4
                     END_CHOICE
               RL6 = CHOICE
                        RL5 = " " THEN " "
!                        TYPE = "ISP" AND YORN = "Y" THEN RL5
                        ELSE RL4
                     END_CHOICE
               RL7 = CHOICE
                        RL6 = " " THEN " " ! keep this one
!                        TYPE = "ISP" AND YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE "K"
                     END_CHOICE
               RL8 = RL7
               RL9 = CHOICE
                        RL8 = " " THEN " "
!                        TYPE = "ISP" AND YORN = "Y" THEN " "
                        ELSE RL7
                     END_CHOICE
               RL10 = CHOICE
                         RL9 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN " "
                         IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                         ELSE "L"
                      END_CHOICE
               RL11 = CHOICE
                         RL10 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN " "
                         ELSE RL10
                      END_CHOICE
               RL12 = CHOICE
                         RL11 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN " "
                         ELSE RL10
                      END_CHOICE
               DED1 = "B"
               DED2 = CHOICE
                         RL2 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN "F"
                         ELSE "E"
                      END_CHOICE
               DED3 = CHOICE
                         RL3 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN "E"
                         ELSE "F"
                      END_CHOICE
               DED4 = CHOICE
                         RL4 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN "F"
                         ELSE "B"
                      END_CHOICE
               DED5 = CHOICE
                         RL5 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN "E"
                         ELSE "E"
                      END_CHOICE
               DED6 = CHOICE
                         RL6 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN "F"
                         ELSE "F"
                      END_CHOICE
               DED7 = CHOICE
                         RL7 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN "E"
                         ELSE "B"
                      END_CHOICE
               DED8 = CHOICE
                         RL8 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN "F"
                         ELSE "E"
                      END_CHOICE
               DED9 = CHOICE
                         RL9 = " " THEN " "
!                         TYPE = "ISP" AND YORN = "Y" THEN " "
                         ELSE "F"
                       END_CHOICE
               DED10 = CHOICE
                          RL10 = " " THEN " "
!                          TYPE = "ISP" AND YORN = "Y" THEN " "
                          ELSE "B"
                       END_CHOICE
               DED11 = CHOICE
                          RL11 = " " THEN " "
!                          TYPE = "ISP" AND YORN = "Y" THEN " "
                          ELSE "E"
                       END_CHOICE
               DED12 = CHOICE
                          RL12 = " " THEN " "
!                          TYPE = "ISP" AND YORN = "Y" THEN " "
                          ELSE "F"
                       END_CHOICE
            END ! ISP SECTION
! ****************************************************************************************************
!                       LIMSC SECTION         
! ****************************************************************************************************
! tried to clean up and simplify this - put all "smiley" options in as choices because initial
! smileys were not working right 1/7/2010 TG
         IF QAX = "N" AND TYPE = "LIMSC" THEN
            BEGIN
               YORN = FN$UPCASE(*."Y if this is a special (SMILEY) quote, N if not")
               RL1 = CHOICE
                        IR = "I" THEN "H"
                        ELSE FN$UPCASE(*."the first LIMITS letter")
                     END_CHOICE
               RL2 = RL1
               RL3 = CHOICE ! can just be RL1 and probably don't need the blank line until rl6
                        RL2 = " " THEN " "
                        IR = "I" AND YORN = "Y" THEN "T"
                        IR = "I" THEN "H"
                        YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE RL1
                     END_CHOICE
               RL4 = CHOICE
                        RL3 = " " THEN " "
                        IR = "I" AND YORN = "Y" THEN "T"
                        IR = "I" THEN "T"
                        YORN = "Y" THEN RL3
                        IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE "T"
                     END_CHOICE
               RL5 = CHOICE
                        RL4 = " " THEN " "
                        IR = "I" AND YORN = "Y" THEN "K"
                        IR = "I" THEN "T"
                        YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE RL4
                     END_CHOICE
               RL6 = CHOICE
                        RL5 = " " THEN " "
                        IR = "I" AND YORN = "Y" THEN "K"
                        IR = "I" THEN "T"
                        YORN = "Y" THEN RL5
                        ELSE RL4
                     END_CHOICE
               RL7 = CHOICE
                        RL6 = " " THEN " " ! keep this one
                        IR = "I" AND YORN = "Y" THEN "L"
                        IR = "I" THEN "K"
                        YORN = "Y" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE "K"
                     END_CHOICE
               RL8 = CHOICE
                        RL7 = " " THEN " " ! keep this one
                        IR = "I" AND YORN = "Y" THEN "L"
                        IR = "I" THEN "K"
                        ELSE RL7
                     END_CHOICE
               RL9 = CHOICE
                        RL8 = " " THEN " "
                        IR = "I" AND YORN = "Y" THEN " "
                        IR = "I" THEN "K"
                        YORN = "Y" THEN " "
                        ELSE RL7
                     END_CHOICE
               RL10 = CHOICE
                         RL9 = " " THEN " "
                         IR = "I" THEN "L"
                         YORN = "Y" THEN " "
                         IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                         ELSE "L"
                      END_CHOICE
               RL11 = CHOICE
                         RL10 = " " THEN " "
                         IR = "I" THEN "L"
                         YORN = "Y" THEN " "
                         ELSE RL10
                      END_CHOICE
               RL12 = CHOICE
                         RL11 = " " THEN " "
                         IR = "I" THEN "L"
                         YORN = "Y" THEN " "
                         ELSE RL10
                      END_CHOICE
               DED1 = CHOICE
                         YORN = "Y" THEN "E"
                         ELSE "B"
                      END_CHOICE
               DED2 = CHOICE
                         RL2 = " " THEN " "
                         YORN = "Y" THEN "F"
                         ELSE "E"
                      END_CHOICE
               DED3 = CHOICE
                         RL3 = " " THEN " "
                         YORN = "Y" THEN "E"
                         ELSE "F"
                      END_CHOICE
               DED4 = CHOICE
                         RL4 = " " THEN " "
                         YORN = "Y" THEN "F"
                         ELSE "B"
                      END_CHOICE
               DED5 = CHOICE
                         RL5 = " " THEN " "
                         YORN = "Y" THEN "E"
                         ELSE "E"
                      END_CHOICE
               DED6 = CHOICE
                         RL6 = " " THEN " "
                         YORN = "Y" THEN "F"
                         ELSE "F"
                      END_CHOICE
               DED7 = CHOICE
                         RL7 = " " THEN " "
                         YORN = "Y" THEN "E"
                         ELSE "B"
                      END_CHOICE
               DED8 = CHOICE
                         RL8 = " " THEN " "
                         YORN = "Y" THEN "F"
                         ELSE "E"
                      END_CHOICE
               DED9 = CHOICE
                         RL9 = " " THEN " "
                         YORN = "Y" THEN " "
                         ELSE "F"
                       END_CHOICE
               DED10 = CHOICE
                          RL10 = " " THEN " "
                          YORN = "Y" THEN " "
                          ELSE "B"
                       END_CHOICE
               DED11 = CHOICE
                          RL11 = " " THEN " "
                          YORN = "Y" THEN " "
                          ELSE "E"
                       END_CHOICE
               DED12 = CHOICE
                          RL12 = " " THEN " "
                          YORN = "Y" THEN " "
                          ELSE "F"
                       END_CHOICE
            END ! LIMSC SECTION
! ****************************************************************************************************
!                       LIM SECTION         
! ****************************************************************************************************
         IF TYPE = "LIM" AND QAX = "N" THEN
            BEGIN
               IF IR = "R" THEN
                  BEGIN
                     PRINT " "
                     PRINT " "
                     PRINT "=========================================="
                     PRINT "WARNING!!!"
                     PRINT "LIMITS questions are asked several times, but"
                     PRINT "DEDUCTIBLE question is asked only ONCE and then"
                     PRINT "automatically stored for all other limits"
                     PRINT "=========================================="
                     PRINT " "
                     PRINT " "
                     PRINT " "
                  END
               RL1 = CHOICE
                        IR = "I" THEN "H"
                        ELSE FN$UPCASE(*."the first LIMITS letter")
                     END_CHOICE
               DED1 = CHOICE
                         IR = "I" THEN "B"
                         ELSE FN$UPCASE(*."the first DEDUCTIBLE letter")
                     END_CHOICE
               RL2 = CHOICE
                        IR = "I" THEN RL1
                        DED1 = "B","E" THEN RL1
                        ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                     END_CHOICE
               DED2 = CHOICE
                         RL2 = " " THEN " "
                         IR = "I" THEN "E"
                         IR = "R" AND DED1 = "B" THEN "E"
                         ELSE "F"
                     END_CHOICE
               RL3 = CHOICE
                        IR = "I" OR DED1 = "B" THEN RL1
                        ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                     END_CHOICE
               DED3 = CHOICE
                         RL3 = " " THEN " "
                         IR = "R" AND DED1 = "E" THEN "E"
                         ELSE "F"
                     END_CHOICE
               RL4 = CHOICE
                        RL3 = " " THEN " "
                        IR = "I" THEN "T"
                        DED1 = "E" THEN RL3
                        ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                     END_CHOICE
               DED4 = CHOICE
                         RL4 = " " THEN " "
                         IR = "I" OR DED1 = "B" THEN "B"
                         ELSE "F"
                     END_CHOICE
               RL5 = CHOICE
                        RL4 = " " THEN " "
                        IR = "I" THEN RL4
                        DED1 = "B" THEN RL4
                        ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                     END_CHOICE
               DED5 = CHOICE
                         RL5 = " " THEN " "
                         ELSE "E"
                     END_CHOICE
               RL6 = CHOICE
                        RL5 = " " THEN " "
                        IR = "I" OR DED1 = "B" THEN RL4
                        DED1 = "E" THEN RL5
                        ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                     END_CHOICE
               DED6 = CHOICE
                         RL6 = " " THEN " "
                         ELSE "F"
                     END_CHOICE
               RL7 = CHOICE
                        RL6 = " " THEN " "
                        IR = "I" THEN "K"
                        ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                     END_CHOICE
               DED7 = CHOICE
                         RL7 = " " THEN " "
                         IR = "I" OR DED1 = "B" THEN "B"
                         ELSE "E"
                     END_CHOICE
               RL8 = CHOICE
                        RL7 = " " THEN " "
                        IR = "I" OR DED1 = "B" THEN RL7
                        DED1 = "E" THEN RL7
                        ELSE " "
                     END_CHOICE
               DED8 = CHOICE
                         RL8 = " " THEN " "
                         IR = "I" OR DED1 = "B" THEN "E"
                         ELSE "F"
                     END_CHOICE
               RL9 = CHOICE
                        RL8 = " " THEN " "
                        IR = "I" OR DED1 = "B" THEN RL7
                        ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                     END_CHOICE
               DED9 = CHOICE
                         RL9 = " " THEN " "
                         IR = "I" OR DED1 = "B" THEN "F"
                         ELSE "E"
                     END_CHOICE
               RL10 = CHOICE
                         RL9 = " " THEN " "
                         IR = "I" THEN "L"
                         DED1 = "B" THEN FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                         ELSE RL9
                      END_CHOICE
               DED10 = CHOICE
                         RL10 = " " THEN " "
                         IR = "I" OR DED1 = "B" THEN "B"
                         ELSE "F"
                     END_CHOICE
               RL11 = CHOICE
                         RL10 = " " THEN " "
                         IR = "I" OR DED1 = "B" THEN RL10
                         ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                      END_CHOICE
               DED11 = CHOICE
                         RL11 = " " THEN " "
                         ELSE "E"
                     END_CHOICE
               RL12 = CHOICE
                         RL11 = " " THEN " "
                         IR = "I" OR DED1 = "B" THEN RL10
                         ELSE RL11
                      END_CHOICE
               DED12 = CHOICE
                         RL12 = " " THEN " "
                         ELSE "F"
                     END_CHOICE
               RL13 = CHOICE
                         RL12 = " " THEN " "
                         IR = "I" THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                      END_CHOICE
               DED13 = CHOICE
                         RL13 = " " THEN " "
                         IR = "R" AND DED1 = "B" THEN "B"
                         ELSE "E"
                     END_CHOICE
               RL14 = CHOICE
                         RL13 = " " THEN " "
                         ELSE RL13
                      END_CHOICE
               DED14 = CHOICE
                         RL14 = " " THEN " "
                         IR = "R" AND DED1 = "B" THEN "E"
                         ELSE "F"
                     END_CHOICE
               RL15 = CHOICE
                         RL14 = " " THEN " "
                         DED1 = "B" THEN RL13
                         ELSE " "
                      END_CHOICE
               DED15 = CHOICE
                         RL15 = " " THEN " "
                         IR = "R" AND DED1 = "B" THEN "F"
                         ELSE " "
                     END_CHOICE
               RL16 = CHOICE
                         RL15 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS (NOT DEDUCTIBLE!) letter, blank if finished with limits")
                      END_CHOICE
               DED16 = CHOICE
                         RL16 = " " THEN " "
                         ELSE "B"
                     END_CHOICE
               RL17 = CHOICE
                         RL16 = " " THEN " "
                         ELSE RL16
                      END_CHOICE
               DED17 = CHOICE
                         RL17 = " " THEN " "
                         ELSE "E"
                     END_CHOICE
               RL18 = CHOICE
                         RL17 = " " THEN " "
                         ELSE RL16
                      END_CHOICE
               DED18 = CHOICE
                         RL18 = " " THEN " "
                         ELSE "F"
                     END_CHOICE
            END ! LIM SECTION
! ****************************************************************************************************
!                       FOR/LNC SECTION         
! ****************************************************************************************************
         IF TYPE = "FOR","LNC" AND QAX = "N" THEN
            BEGIN
               RL1 = CHOICE
                        IR = "I" THEN "H"
                        ELSE FN$UPCASE(*."the first LIMITS letter")
                     END_CHOICE
               DED1 = "D"
               RL2 = CHOICE
                        IR = "R" THEN RL1
                        ELSE "H"
                     END_CHOICE
               DED2 = CHOICE
                         RL2 = " " THEN " "
                         ELSE "E"
                      END_CHOICE
               RL3 = CHOICE
                        RL2 = " " THEN " "
                        IR = "R" THEN RL1
                        ELSE "H"
                     END_CHOICE
               DED3 = CHOICE
                         RL3 = " " THEN " "
                         ELSE "F"
                      END_CHOICE
               RL4 = CHOICE
                        RL3 = " " THEN " "
                        IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE "T"
                     END_CHOICE
               DED4 = CHOICE
                         RL4 = " " THEN " "
                         ELSE "D"
                      END_CHOICE
               RL5 = CHOICE
                        RL4 = " " THEN " "
                        IR = "R" THEN RL4
                        ELSE "T"
                     END_CHOICE
               DED5 = CHOICE
                         RL5 = " " THEN " "
                         ELSE "E"
                      END_CHOICE
               RL6 = CHOICE
                        RL5 = " " THEN " "
                        IR = "R" THEN RL4
                        ELSE "T"
                     END_CHOICE
               DED6 = CHOICE
                         RL6 = " " THEN " "
                         ELSE "F"
                      END_CHOICE
               RL7 = CHOICE
                        RL6 = " " THEN " "
                        IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                        ELSE "K"
                     END_CHOICE
               DED7 = CHOICE
                         RL7 = " " THEN " "
                         ELSE "D"
                      END_CHOICE
               RL8 = CHOICE
                        RL7 = " " THEN " "
                        IR = "R" THEN RL7
                        ELSE "K"
                     END_CHOICE
               DED8 = CHOICE
                         RL8 = " " THEN " "
                         ELSE "E"
                      END_CHOICE
               RL9 = CHOICE
                        RL8 = " " THEN " "
                        IR = "R" THEN RL7
                        ELSE "K"
                     END_CHOICE
               DED9 = CHOICE
                         RL9 = " " THEN " "
                         ELSE "F"
                      END_CHOICE
               RL10 = CHOICE
                         RL9 = " " THEN " "
                         IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                         ELSE "L"
                      END_CHOICE
               DED10 = CHOICE
                         RL10 = " " THEN " "
                         ELSE "D"
                      END_CHOICE
               RL11 = CHOICE
                         RL10 = " " THEN " "
                         IR = "R" THEN RL10
                         ELSE "L"
                      END_CHOICE
               DED11 = CHOICE
                         RL11 = " " THEN " "
                         ELSE "E"
                      END_CHOICE
               RL12 = CHOICE
                         RL11 = " " THEN " "
                         IR = "R" THEN RL10
                         ELSE "L"
                      END_CHOICE
               DED12 = CHOICE
                         RL12 = " " THEN " "
                         ELSE "F"
                      END_CHOICE
               RL13 = CHOICE
                         RL12 = " " THEN " "
                         IR = "R" THEN FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                         ELSE "AS"
                      END_CHOICE
               DED13 = CHOICE
                         RL13 = " " THEN " "
                         ELSE "D"
                      END_CHOICE
               RL14 = CHOICE
                         RL13 = " " THEN " "
                         IR = "R" THEN RL13
                         ELSE "AS"
                      END_CHOICE
               DED14 = CHOICE
                         RL14 = " " THEN " "
                         ELSE "E"
                      END_CHOICE
               RL15 = CHOICE
                         RL14 = " " THEN " "
                         IR = "R" THEN RL13
                         ELSE "AS"
                      END_CHOICE
               DED15 = CHOICE
                         RL15 = " " THEN " "
                         ELSE "F"
                      END_CHOICE
            END ! FOR SECTION
! ****************************************************************************************************
!                       HA/DE SECTION         
! ****************************************************************************************************
         IF TYPE = "HA","DE" AND QAX = "N" THEN
            BEGIN
               RL1 = FN$UPCASE(*."the first LIMITS letter")
               RL2 = FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
               RL3 = CHOICE
                        RL2 = " " THEN " "
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL4 = CHOICE
                        RL3 = " " THEN " "
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL5 = CHOICE
                        RL4 = " " THEN " "
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL6 = CHOICE
                        RL5 = " " THEN " "
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL7 = CHOICE
                        RL6 = " " THEN " "
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL8 = CHOICE
                        RL7 = " " THEN " "
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL9 = CHOICE
                        RL8 = " " THEN " "
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               RL10 = CHOICE
                         RL9 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               RL11 = CHOICE
                         RL10 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               RL12 = CHOICE
                         RL11 = " " THEN " "
                         ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                      END_CHOICE
               DED1 = CHOICE
                         CUS_NUM = 10004496 THEN "A"
                         TYPE = "HA" AND ASSOC = " " THEN "A"
                         TYPE = "DE" AND ASSOC = " " THEN "B"
                         TYPE = "HA" AND ASSOC NE " " THEN "B"
                         ELSE "D"
                      END_CHOICE
            END ! HA/DE SECTION
! ****************************************************************************************************
!                       CRIMINAL DEFENSE SECTION         
! ****************************************************************************************************
         IF TYPE = "CD" AND QAX = "N" THEN
            BEGIN
               RL1                    = FN$UPCASE(*."first LIMITS letter")
               DED1                   = FN$UPCASE(*."DEDUCTIBLE letter for first limit")
               WHILE DED1 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                  BEGIN
                     PRINT "All CD deductibles must be a minimum of $1500!"
                     DED1 = FN$UPCASE(*."DED1 again")
                  END
               RL2                    = FN$UPCASE(*."second LIMITS letter, blank if finished with limits")
               IF RL2 NE " " THEN
                  BEGIN
                     DED2                = FN$UPCASE(*."DEDUCTIBLE letter for second limit")
                     WHILE DED2 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED2 = FN$UPCASE(*."DED2 again")
                        END
                     RL3                 = FN$UPCASE(*."third LIMITS letter, blank if finished with limits")
                  END
               IF RL3 NE " " THEN
                  BEGIN
                     DED3                = FN$UPCASE(*."DEDUCTIBLE letter for third limit")
                     WHILE DED3 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED3 = FN$UPCASE(*."DED3 again")
                        END
                     RL4                 = FN$UPCASE(*."fourth LIMITS letter, blank if finished with limits")
                  END
               IF RL4 NE " " THEN
                  BEGIN
                     DED4                = FN$UPCASE(*."DEDUCTIBLE letter for fourth limit")
                     WHILE DED4 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED4 = FN$UPCASE(*."DED4 again")
                        END
                     RL5                 = FN$UPCASE(*."fifth LIMITS letter, blank if finished with limits")
                  END
               IF RL5 NE " " THEN
                  BEGIN
                     DED5                = FN$UPCASE(*."DEDUCTIBLE letter for fifth limit")
                     WHILE DED5 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED5 = FN$UPCASE(*."DED5 again")
                        END
                     RL6                 = FN$UPCASE(*."6th LIMITS letter, blank if finished with limits")
                  END
               IF RL6 NE " " THEN
                  BEGIN
                     DED6                = FN$UPCASE(*."DEDUCTIBLE letter for 6th limit")
                     WHILE DED6 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED6 = FN$UPCASE(*."DED6 again")
                        END
                     RL7                 = FN$UPCASE(*."7th LIMITS letter, blank if finished with limits")
                  END
               IF RL7 NE " " THEN
                  BEGIN
                     DED7                = FN$UPCASE(*."DEDUCTIBLE letter for 7th limit")
                     WHILE DED7 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED7 = FN$UPCASE(*."DED7 again")
                        END
                     RL8                 = FN$UPCASE(*."8th LIMITS letter, blank if finished with limits")
                  END
               IF RL8 NE " " THEN
                  BEGIN
                     DED8                = FN$UPCASE(*."DEDUCTIBLE letter for 8th limit")
                     WHILE DED8 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED8 = FN$UPCASE(*."DED8 again")
                        END
                     RL9                 = FN$UPCASE(*."9th LIMITS letter, blank if finished with limits")
                  END
               IF RL9 NE " " THEN
                  BEGIN
                     DED9                = FN$UPCASE(*."DEDUCTIBLE letter for 9th limit")
                     WHILE DED9 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED9 = FN$UPCASE(*."DED9 again")
                        END
                     RL10                = FN$UPCASE(*."10th LIMITS letter, blank if finished with limits")
                  END
               IF RL10 NE " " THEN
                  BEGIN
                     DED10               = FN$UPCASE(*."DEDUCTIBLE letter for 10th limit")
                     WHILE DED10 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED10 = FN$UPCASE(*."DED10 again")
                        END
                     RL11                = FN$UPCASE(*."11th LIMITS letter, blank if finished with limits")
                  END
               IF RL11 NE " " THEN
                  BEGIN
                     DED11               = FN$UPCASE(*."DEDUCTIBLE letter for 11th limit")
                     WHILE DED11 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED11 = FN$UPCASE(*."DED11 again")
                        END
                     RL12                = FN$UPCASE(*."12th LIMITS letter, blank if finished with limits")
                  END
               IF RL12 NE " " THEN
                  BEGIN
                     DED12               = FN$UPCASE(*."DEDUCTIBLE letter for 12th limit")
                     WHILE DED12 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED12 = FN$UPCASE(*."DED12 again")
                        END
                     RL13                = FN$UPCASE(*."13th LIMITS letter, blank if finished with limits")
                  END
               IF RL13 NE " " THEN
                  BEGIN
                     DED13               = FN$UPCASE(*."DEDUCTIBLE letter for 13th limit")
                     WHILE DED13 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED13 = FN$UPCASE(*."DED13 again")
                        END
                     RL14                = FN$UPCASE(*."14th LIMITS letter, blank if finished with limits")
                  END
               IF RL14 NE " " THEN
                  BEGIN
                     DED14               = FN$UPCASE(*."DEDUCTIBLE letter for 14th limit")
                     WHILE DED14 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED14 = FN$UPCASE(*."DED14 again")
                        END
                     RL15                = FN$UPCASE(*."15th LIMITS letter, blank if finished with limits")
                  END
               IF RL15 NE " " THEN
                  BEGIN
                     DED15               = FN$UPCASE(*."DEDUCTIBLE letter for 15th limit")
                     WHILE DED15 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED15 = FN$UPCASE(*."DED15 again")
                        END
                     RL16                = FN$UPCASE(*."16th LIMITS letter, blank if finished with limits")
                  END
               IF RL16 NE " " THEN
                  BEGIN
                     DED16               = FN$UPCASE(*."DEDUCTIBLE letter for 16th limit")
                     WHILE DED16 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED16 = FN$UPCASE(*."DED16 again")
                        END
                     RL17                = FN$UPCASE(*."17th LIMITS letter, blank if finished with limits")
                  END
               IF RL17 NE " " THEN
                  BEGIN
                     DED17               = FN$UPCASE(*."DEDUCTIBLE letter for 17th limit")
                     WHILE DED17 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED17 = FN$UPCASE(*."DED17 again")
                        END
                     RL18                = FN$UPCASE(*."18th LIMITS letter, blank if finished with limits")
                  END
               IF RL18 NE " " THEN
                  BEGIN
                     DED18               = FN$UPCASE(*."DEDUCTIBLE letter for 18th limit")
                     WHILE DED18 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED18 = FN$UPCASE(*."DED18 again")
                        END
                     RL19                = FN$UPCASE(*."19th LIMITS letter, blank if finished with limits")
                  END
               IF RL19 NE " " THEN
                  BEGIN
                     DED19               = FN$UPCASE(*."DEDUCTIBLE letter for 19th limit")
                     WHILE DED19 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED19 = FN$UPCASE(*."DED19 again")
                        END
                     RL20                = FN$UPCASE(*."20th LIMITS letter, blank if finished with limits")
                  END
               IF RL20 NE " " THEN
                  BEGIN
                     DED20               = FN$UPCASE(*."DEDUCTIBLE letter for 20th limit")
                     WHILE DED20 VIA CEMI05:[DTR.COMMON]COMMON_DED_SHORT LT 1500
                        BEGIN
                           PRINT "All CD deductibles must be a minimum of $1500!"
                           DED20 = FN$UPCASE(*."DED20 again")
                        END
                  END
            END ! CRIMINAL DEFENSE SECTION
! ****************************************************************************************************
!                     SFPE SECTION
! ****************************************************************************************************
         IF TYPE = "SFPE" THEN
            BEGIN
               RL1 = CHOICE
                        IR = "I" AND QAX = "N" THEN "H"
                        ELSE FN$UPCASE(*."the first LIMITS letter")
                      END_CHOICE
               DED1 = FN$UPCASE(*."the first DEDUCTIBLE letter")
               IF QAX = "Y" THEN
                  PREMIUM_Q1 = *."the first QAX PREMIUM amount"
               RL2 = CHOICE
                        IR = "I" AND QAX = "N" THEN "T"
                        ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                     END_CHOICE
               IF RL2 NE " " THEN
                  BEGIN
                     DED2 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q2 = *."the next QAX PREMIUM amount"
                     RL3 = CHOICE
                              IR = "I" AND QAX = "N" THEN "K"
                              ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                          END_CHOICE
                  END
               IF RL3 NE " " THEN
                  BEGIN
                     DED3 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q3 = *."the next QAX PREMIUM amount"
                     RL4 = CHOICE
                              IR = "I" AND QAX = "N" THEN "L"
                              ELSE FN$UPCASE(*."the next LIMITS letter, blank if finished with limits")
                           END_CHOICE
                  END
               IF RL4 NE " " THEN
                  BEGIN
                     DED4 = FN$UPCASE(*."the next DEDUCTIBLE letter")
                     IF QAX = "Y" THEN
                        PREMIUM_Q4 = *."the next QAX PREMIUM amount"
                  END
            END ! SFPE SECTION
! ****************************************************************************************************
!                       JUDGES SECTION         
! ****************************************************************************************************
         IF QAX = "Y" AND TYPE = "JUD" THEN
            BEGIN
               RL1 = FN$UPCASE(*."PL LIMITS letter")
               RL2 = FN$UPCASE(*."CIV LIMITS letter")
               RL3 = FN$UPCASE(*."DP LIMITS letter")
               RL4 = FN$UPCASE(*."EMP LIMITS letter")
               DED1 = FN$UPCASE(*."the DEDUCTIBLE letter")
               PREMIUM_Q1 = *."the PL/CIV alone premium"
               PREMIUM_Q2 = *."the DP alone premium"
               PREMIUM_Q3 = *."the PL/CIV/DP combined premium (ABC premium)"
               PREMIUM_Q4 = *."the PL/CIV/EMP combined premium (ABD premium)"
               PREMIUM_Q5 = *."the PL/CIV/DP/EMP combined premium (ABCD premium)"
            END
! ****************************************************************************************************
!                                 QAX SECTION         
! ****************************************************************************************************
         IF QAX = "Y" AND TYPE NE "IMM","JUD","SFPE","FOOD" THEN
            BEGIN
               RL1                    = FN$UPCASE(*."first LIMITS letter")
               PREMIUM_Q1             = *."PREMIUM 1, 0 if none"
               IF TYPE = "FOR","LNC" THEN
                  DED1             = FN$UPCASE(*."DEDUCTIBLE letter for first limit")
               RL2                    = FN$UPCASE(*."second LIMITS letter, blank if finished with limits")
               IF RL2 NE " " THEN
                  BEGIN
                     PREMIUM_Q2          = *."PREMIUM 2, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q2 NE 0 THEN
                        DED2             = FN$UPCASE(*."DEDUCTIBLE letter for second limit")
                     RL3                 = FN$UPCASE(*."third LIMITS letter, blank if finished with limits")
                  END
               IF RL3 NE " " THEN
                  BEGIN
                     PREMIUM_Q3          = *."PREMIUM 3, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q3 NE 0 THEN
                        DED3             = FN$UPCASE(*."DEDUCTIBLE letter for third limit")
                     RL4                 = FN$UPCASE(*."fourth LIMITS letter, blank if finished with limits")
                  END
               IF RL4 NE " " THEN
                  BEGIN
                     PREMIUM_Q4          = *."PREMIUM 4, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q4 NE 0 THEN
                        DED4             = FN$UPCASE(*."DEDUCTIBLE letter for fourth limit")
                     RL5                 = FN$UPCASE(*."fifth LIMITS letter, blank if finished with limits")
                  END
               IF RL5 NE " " THEN
                  BEGIN
                     PREMIUM_Q5          = *."PREMIUM 5, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q5 NE 0 THEN
                        DED5             = FN$UPCASE(*."DEDUCTIBLE letter for fifth limit")
                     RL6                 = FN$UPCASE(*."6th LIMITS letter, blank if finished with limits")
                  END
               IF RL6 NE " " THEN
                  BEGIN
                     PREMIUM_Q6          = *."PREMIUM 6, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q6 NE 0 THEN
                        DED6             = FN$UPCASE(*."DEDUCTIBLE letter for sixth limit")
                     RL7                 = FN$UPCASE(*."7th LIMITS letter, blank if finished with limits")
                  END
               IF RL7 NE " " THEN
                  BEGIN
                     PREMIUM_Q7          = *."PREMIUM 7, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q7 NE 0 THEN
                        DED7             = FN$UPCASE(*."DEDUCTIBLE letter for 7th limit")
                     RL8                 = FN$UPCASE(*."8th LIMITS letter, blank if finished with limits")
                  END
               IF RL8 NE " " THEN
                  BEGIN
                     PREMIUM_Q8          = *."PREMIUM 8, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q8 NE 0 THEN
                        DED8             = FN$UPCASE(*."DEDUCTIBLE letter for 8th limit")
                     RL9                 = FN$UPCASE(*."9th LIMITS letter, blank if finished with limits")
                  END
               IF RL9 NE " " THEN
                  BEGIN
                     PREMIUM_Q9          = *."PREMIUM 9, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q9 NE 0 THEN
                        DED9             = FN$UPCASE(*."DEDUCTIBLE letter for 9th limit")
                     RL10                = FN$UPCASE(*."10th LIMITS letter, blank if finished with limits")
                  END
               IF RL10 NE " " THEN
                  BEGIN
                     PREMIUM_Q10         = *."PREMIUM 10, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q10 NE 0 THEN
                        DED10            = FN$UPCASE(*."DEDUCTIBLE letter for 10th limit")
                     RL11                = FN$UPCASE(*."11th LIMITS letter, blank if finished with limits")
                  END
               IF RL11 NE " " THEN
                  BEGIN
                     PREMIUM_Q11         = *."PREMIUM 11, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q11 NE 0 THEN
                        DED11            = FN$UPCASE(*."DEDUCTIBLE letter for 11th limit")
                     RL12                = FN$UPCASE(*."12th LIMITS letter, blank if finished with limits")
                  END
               IF RL12 NE " " THEN
                  BEGIN
                     PREMIUM_Q12         = *."PREMIUM 12, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q12 NE 0 THEN
                        DED12            = FN$UPCASE(*."DEDUCTIBLE letter for 12th limit")
                     RL13                = FN$UPCASE(*."13th LIMITS letter, blank if finished with limits")
                  END
               IF RL13 NE " " THEN
                  BEGIN
                     PREMIUM_Q13         = *."PREMIUM 13, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q13 NE 0 THEN
                        DED13            = FN$UPCASE(*."DEDUCTIBLE letter for 13th limit")
                     RL14                = FN$UPCASE(*."14th LIMITS letter, blank if finished with limits")
                  END
               IF RL14 NE " " THEN
                  BEGIN
                     PREMIUM_Q14         = *."PREMIUM 14, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q14 NE 0 THEN
                        DED14            = FN$UPCASE(*."DEDUCTIBLE letter for 14th limit")
                     RL15                = FN$UPCASE(*."15th LIMITS letter, blank if finished with limits")
                  END
               IF RL15 NE " " THEN
                  BEGIN
                     PREMIUM_Q15         = *."PREMIUM 15, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q15 NE 0 THEN
                        DED15            = FN$UPCASE(*."DEDUCTIBLE letter for 15th limit")
                     RL16                = FN$UPCASE(*."16th LIMITS letter, blank if finished with limits")
                  END
               IF RL16 NE " " THEN
                  BEGIN
                     PREMIUM_Q16         = *."PREMIUM 16, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q16 NE 0 THEN
                        DED16            = FN$UPCASE(*."DEDUCTIBLE letter for 16th limit")
                     RL17                = FN$UPCASE(*."17th LIMITS letter, blank if finished with limits")
                  END
               IF RL17 NE " " THEN
                  BEGIN
                     PREMIUM_Q17         = *."PREMIUM 17, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q17 NE 0 THEN
                        DED17            = FN$UPCASE(*."DEDUCTIBLE letter for 17th limit")
                     RL18                = FN$UPCASE(*."18th LIMITS letter, blank if finished with limits")
                  END
               IF RL18 NE " " THEN
                  BEGIN
                     PREMIUM_Q18         = *."PREMIUM 18, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q18 NE 0 THEN
                        DED18            = FN$UPCASE(*."DEDUCTIBLE letter for 18th limit")
                     RL19                = FN$UPCASE(*."19th LIMITS letter, blank if finished with limits")
                  END
               IF RL19 NE " " THEN
                  BEGIN
                     PREMIUM_Q19         = *."PREMIUM 19, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q19 NE 0 THEN
                        DED19            = FN$UPCASE(*."DEDUCTIBLE letter for 19th limit")
                     RL20                = FN$UPCASE(*."20th LIMITS letter, blank if finished with limits")
                  END
               IF RL20 NE " " THEN
                  BEGIN
                     PREMIUM_Q20            = *."PREMIUM 20, 0 if none"
                     IF TYPE = "FOR","LNC" AND PREMIUM_Q20 NE 0 THEN
                        DED20            = FN$UPCASE(*."DEDUCTIBLE letter for 20th limit")
                  END
               IF TYPE = "HA","DE" THEN DED1 = FN$UPCASE(*."DEDUCTIBLE letter for all limits")
            END ! END QAX = "Y"
! ****************************************************************************************************
!                                 MORE QUESTIONS SECTION
! ****************************************************************************************************
         IF TYPE = "POLY" THEN
            BEGIN
               INT_YN              = FN$UPCASE(*."INTERVIEWING    Y, N or I")
               WT_YN               = FN$UPCASE(*."WRITTEN TESTING Y, N or I")
            END
         IF TYPE = "MED","MISC","CD","JUD" THEN
            CL = FN$UPCASE(*."Y if there is a previous claim or incident, N if none") ELSE
            CL = "N"
         CLC = CHOICE
                  CLC_SAVE = "Y" THEN "Y"
                  QN VIA GRAM_TABLE NE "N" THEN "N"
                  TYPE = "IMM" THEN FN$UPCASE(*."CROSS LIABILITY - Y, G or N (use G for special Canadians)")
                  TYPE = "GEN","MISC","MED","POLY","LIM","SC","LIMSC","ENV","CD","NON","FOR","LNC" THEN FN$UPCASE(*."CROSS LIABILITY - Y or N")
                  TYPE = "FOOD" THEN FN$UPCASE(*."CROSS LIABILITY - Y or N")
                  ELSE "N"
               END_CHOICE
! NOTE FOR MED QUESTIONS BELOW: To make it easier I just added the line for Canada Med's above since they still need the prompts-RT
         EB5 = CHOICE
                  TYPE = "IMM" AND CANADA_YN = "Y" THEN "Y"
                  TYPE = "IMM" AND ORIG_EFF LT "04/01/2020" THEN "Y"
                  TYPE = "GEN","CD", "LIM", "NON" THEN FN$UPCASE(*."(EB5) INVESTMENT Vehicle Services Exclusion - Y or N")
                  TYPE = "MISC" THEN "N"
                  ! removed type MISC from the line below, add line to auto store "N" from now on, just in case 3/20/2008 TG
                  CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."A = ARB cov (DRS excl), D = DRS cov (ARB excl), or N = both cov (no excl)")
                  ELSE "N"
               END_CHOICE
         KCE = CHOICE
                  KCE_SAVE = "Y" THEN "Y"
                  CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."CUSTODY EVALUATION - Y or N")
                  ((EXP LT "05/01/2013" AND PREV_NUM NE "0"," ") OR VALID_DATE LT "05/01/2013") AND TYPE = "MED" THEN FN$UPCASE(*."CUSTODY EVALUATION - Y or N")
                  TYPE = "MISC" THEN FN$UPCASE(*."CUSTODY EVALUATION - Y or N")
! The line below is due to the new LII 14-1 change form, cus eval is included now.  Claims for MED's are in the other claims field.  clean new system!.-RT
                  TYPE = "MED" THEN "N"
                  ELSE FN$UPCASE(*."(KCE) KNOWN CLAIMS EXCLUSION - Y or N")
! old lines below - per Mike all should have KCE from now on 11/23/2009 TG
!                  TYPE = "FOOD","GEN","IMM","CD","LIM","NON","JUD","POLY","FOR","SC" THEN FN$UPCASE(*."(KCE) KNOWN CLAIMS EXCLUSION - Y or N")
!                  JOB_CODE = "MEDX" THEN FN$UPCASE(*."(KCE) KNOWN CLAIMS EXCLUSION - Y or N")
!                  ELSE "N"
               END_CHOICE
         LNA = CHOICE
                  LNA_SAVE = "Y" THEN "Y"
                  TYPE = "CD","GEN","IMM","POLY","LIM","MED","MISC","NON","PSY" THEN FN$UPCASE(*."LIMITING NAMED ASSURED - Y or N")
                  TYPE = "FOOD" THEN FN$UPCASE(*."LIMITING NAMED ASSURED - Y or N")
                  ELSE "N"
               END_CHOICE
         OCE = CHOICE
                  ((EXP LT "05/01/2013" AND PREV_NUM NE "0"," ") OR VALID_DATE LT "05/01/2013") AND OCE_SAVE = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."Y for MRLS only, Q for QDRO")
                  OCE_SAVE = "Y" THEN "Y"
                  CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."Y for MRLS only, Q for QDRO, N for neither")
                  ((EXP LT "05/01/2013" AND PREV_NUM NE "0"," ") OR VALID_DATE LT "05/01/2013") AND TYPE = "MED" THEN FN$UPCASE(*."Y for MRLS only, Q for QDRO, N for neither")
                  TYPE = "MISC" THEN FN$UPCASE(*."Y for MRLS only, Q for QDRO, N for neither")
                  TYPE = "GEN","IMM","NON" THEN FN$UPCASE(*."OF COUNSEL EXCLUSION - Y or N")
                  ELSE "N"
               END_CHOICE
         OSE = CHOICE
                  OSE_SAVE = "Y" THEN "Y"
                  CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."GUARDIAN AD LITEM   - Y or N")
                  ((EXP LT "05/01/2013" AND PREV_NUM NE "0"," ") OR VALID_DATE LT "05/01/2013") AND TYPE = "MED" THEN FN$UPCASE(*."GUARDIAN AD LITEM   - Y or N")
                  TYPE = "MISC" THEN FN$UPCASE(*."GUARDIAN AD LITEM   - Y or N")
                  TYPE = "BSM","GEN","IMM","CD","LIM","NON","PSY" THEN FN$UPCASE(*."OFFICE SHARER'S EXCLUSION - Y or N")
                  ELSE "N"
               END_CHOICE
         HOX = CHOICE
                     TYPE = "MED" AND CANADA_YN = "N" THEN FN$UPCASE(*."HEARING OFFICER EXCLUSION - Y or N")
                     ELSE "N"
                END_CHOICE
         CLE  = CHOICE
                   TYPE = "NON","MED" OR (TYPE = "LIM" AND MED_END = "Y") THEN FN$UPCASE(*."COLLABORATIVE LAW - Y or N")
                   ELSE "N"
                END_CHOICE
         EXPWIT_YN  = CHOICE
                         TYPE = "SC","LIMSC","ISP" THEN FN$UPCASE(*."Expert Witness - Y or N")
                         ELSE "N"
                      END_CHOICE
         ! LIM/MED FOR MEDIATOR ENDS
         IF TYPE = "NON" OR (TYPE = "LIM" AND MED_END = "Y") THEN
            BEGIN
               YORN = FN$UPCASE(*."Y for CUSTODY EVAL, N for none")
               KCE = CHOICE
                        KCE = "Y" AND YORN = "Y" THEN "Y"
                        KCE = "Y" AND YORN = "N" THEN "K"
                        KCE = "N" AND YORN = "Y" THEN "C"
                        ELSE "N"
                     END_CHOICE
               OCE = FN$UPCASE(*."Q for QDRO, N if none")
               YORN = FN$UPCASE(*."Y for GUARDIAN AD LITUM, N for none")
               OSE = CHOICE
                        OSE = "Y" AND YORN = "Y" THEN "Y"
                        OSE = "Y" AND YORN = "N" THEN "O"
                        OSE = "N" AND YORN = "Y" THEN "G"
                        ELSE "N"
                     END_CHOICE
! this will not work here, adding below-RT 5.9.12               OAE = FN$UPCASE(*."CONSULTING SERVICES - Y or N")
            END ! end LIM/MED and NON/MED section
         TSVS = CHOICE
                   TYPE = "LIM" AND MED_END = "Y" THEN FN$UPCASE(*."TRAINING SERVICES   - Y or N")
                   CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."TRAINING SERVICES   - Y or N")
                   TYPE = "MISC","GEN" THEN FN$UPCASE(*."TRAINING SERVICES   - Y or N")
                   ELSE "N"
                END_CHOICE
         UPL = CHOICE
                  CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."UNAUTHORIZED PRACTICE OF LAW - G if GODR (see app), Y or N")
                  TYPE = "MISC" THEN FN$UPCASE(*."UNAUTHORIZED PRACTICE OF LAW - G if GODR (see app), Y or N")
                  ELSE "N"
               END_CHOICE
         DDDC = CHOICE
                  ((CANADA_YN = "Y" AND TYPE = "MED") OR CUS_NUM = 10005972) THEN FN$UPCASE(*."DISCOVERY DEMAND DEFENSE COSTS - Y or N")
                  TYPE = "MISC" THEN FN$UPCASE(*."DISCOVERY DEMAND DEFENSE COSTS - Y or N")
                  TYPE = "LIM" THEN "Y"
                  ELSE "N"
               END_CHOICE
         FACIL = CHOICE
                    CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."FACILITATION SERVICES - Y or N")
                    TYPE = "MISC","NON" OR (TYPE = "LIM" AND MED_END = "Y") THEN FN$UPCASE(*."FACILITATION SERVICES - Y or N")
                    ELSE "N"
               END_CHOICE
         INJ = CHOICE
                  CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."INJUNCTIVE RELIEF - Y or N")
                  TYPE = "MISC" OR (TYPE = "LIM" AND MED_END = "Y") THEN FN$UPCASE(*."INJUNCTIVE RELIEF - Y or N")
                  ELSE "N"
               END_CHOICE
         FACT = CHOICE
                   CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."FACT FINDING SERVICES - Y or N")
                   TYPE = "MISC" OR (TYPE = "LIM" AND MED_END = "Y") THEN FN$UPCASE(*."FACT FINDING SERVICES - Y or N")
                   ELSE "N"
               END_CHOICE
         COACH = CHOICE
                    TYPE = "MED" THEN FN$UPCASE(*."CONFLICT COACHING - Y or N")
                    TYPE = "MISC" OR (TYPE = "LIM" AND MED_END = "Y") THEN FN$UPCASE(*."CONFLICT COACHING - Y or N")
                    ELSE "N"
               END_CHOICE
         PCP  = CHOICE
                   CUS_NUM = 10019634 THEN "Y"
                   QN VIA GRAM_TABLE NE "N" THEN "N"
                   TYPE = "SC" AND EXCLUSIONS CONT "PASE" THEN "Y"
                   ((CANADA_YN = "Y" AND TYPE = "MED") OR CUS_NUM = 10005972) THEN FN$UPCASE(*."PARENT COORDINATION - Y or N")
                   TYPE = "MISC" THEN FN$UPCASE(*."PARENT COORDINATION - Y or N")
                   TYPE = "NON" OR (TYPE = "LIM" AND MED_END = "Y") THEN FN$UPCASE(*."PARENT COORDINATION - Y or N")
                   TYPE = "SC","LIMSC" THEN FN$UPCASE(*."PASE - Y or N")
                   ELSE "N"
                END_CHOICE
          OAE = CHOICE
                   OAE_SAVE = "Y" AND TYPE NE "MED","CD" THEN "Y"
                   TYPE = "FOR","LNC","SC" THEN FN$UPCASE(*."EXCLUDE MEDICAL SERVICES - Y or N")
                   CANADA_YN = "Y" AND TYPE = "MED" THEN FN$UPCASE(*."CONSULTING SERVICES - Y or N")
                   TYPE = "MISC" OR (TYPE = "LIM" AND MED_END = "Y") THEN FN$UPCASE(*."CONSULTING SERVICES - Y or N")
                   TYPE = "CD" THEN FN$UPCASE(*."Y to add Outside Activities, N for none")
                   ELSE "N"
                END_CHOICE
         NID = CHOICE
                  TYPE = "CD","IMM" THEN FN$UPCASE(*."Y if quoting NIL Claims Deductible, N if not")
                  ELSE "N"
               END_CHOICE
!          SETTLE = CHOICE
!                      TYPE = "IMM" THEN FN$UPCASE(*."Settlement special deductible - Y or N")
!                      ELSE "N"
!                   END_CHOICE
!          IMMC = CHOICE
!                    TYPE = "IMM" THEN FN$UPCASE(*."Imm claims special deductible - Y or N")
!                    ELSE "N"
!                 END_CHOICE
         EXCLUSIONS                = CHOICE
                                        IR = "R" THEN EXCLS_REQ
                                        ELSE *."EXCLUDING SERVICES"
                                     END_CHOICE
         EXTS                      = CHOICE
                                        IR = "R" THEN EXTS_REQ
                                        ELSE *."EXTENDING SERVICES"
                                     END_CHOICE
         EXCLO                     = CHOICE
                                        IR = "R" THEN EXCLO_REQ
                                        ELSE *."EXCLUDING OTHERS WORK FOR INSURED (PEOPLE)"
                                     END_CHOICE
         EXCLE                     = CHOICE
                                        IR = "R" THEN EXCLE_REQ
                                        ELSE *."EXCLUDING INSUREDS WORK FOR OTHERS (PEOPLE)"
                                     END_CHOICE
         IF IR = "I" THEN
            BEGIN
               PREV_EXCL = FN$UPCASE(*."Y to store EXTENDING ENTITIES, N if not")
               IF PREV_EXCL = "Y" THEN
                  BEGIN
                     PRINT "NOTE: Extending entities will be stored as follows:"
                     PRINT " "
                     PRINT "*** NAME 1 for work done for and on behalf of NAME 2 ***"
                     PRINT " "
                     PRINT "You will be prompted only for NAME 1 and NAME 2, enter in exact case!"
                     PRINT "The rest of the wording will be stored automatically!"
                     PRINT " "
                     NAME1 = *."NAME 1"
                     NAME2 = *."NAME 2"
                     EXTE          = NAME1|||"for work done for and on behalf of"|||NAME2
                  END
            END ELSE
            EXTE = EXTE_REQ
         EB_REQUEST                   = FN$UPCASE(*."your initials")
         ENTERED_BY                   = EB_REQUEST
         DATE_ENTERED                 = "TODAY"
   END ! VERIFY USING
! ****************************************************************************************************
!                                 PERCENTS SECTION         
! ****************************************************************************************************
:PERCENTS_STORE
!:QPROG20_TAPE_PRINT
END_PROCEDURE

