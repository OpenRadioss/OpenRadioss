Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  HM_READ_INTER_TYPE18          source/interfaces/int18/hm_read_inter_type18.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_FSI             source/interfaces/reader/hm_read_inter_fsi.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE18(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     3          IGRNOD     ,IGRSURF    ,IGRBRIC    ,XFILTR    ,FRIC_P   ,
     3          IDDLEVEL   ,TITR, UNITAB, LSUBMODEL,MULTI_FVM)
C============================================================================
C-----------------------------------------------
C   D e s c r i  p t i o n
C-----------------------------------------------
C This subroutine is reading user input file.
C Parameters are checked and default values are introduced.
C Parameter and flags are stored in working buffer (IPARI:integer; FRIGAP:real)
C Buffer arrays are later written in restart file to be read by Engine program.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)::IDDLEVEL
      INTEGER ISU1,ISU2,IS1,IS2,NI,NOINT
      INTEGER IPARI(NPARI)
      my_real FRIGAP(*),FRIC_P(*),STFAC,XFILTR
      CHARACTER,INTENT(IN) :: TITR*nchartitle
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM      
C-----------------------------------------------
      TYPE(GROUP_),TARGET, DIMENSION(NGRNOD) :: IGRNOD
      TYPE(SURF_),TARGET ,DIMENSION(NSURF) :: IGRSURF
      TYPE(GROUP_),TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB 
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "inter18.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER GRBRIC_ID,GRQUAD_ID,GRTRIA_ID,IBAG,IDEL7N,IGAP,IGAP0,IDUM,NTYP,INACTI,
     .        IDELKEEP,ISU1_user,ISU2_user,ISU3_user,ISTIFF
      my_real GAP,STARTT,STOPT,BUMULT,VISC,FRIC,VREF,SCALE
      CHARACTER MESS*40, MSGTITL*nchartitle
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      INTEGER,EXTERNAL :: NGR2USR
      LOGICAL :: IS_AVAILABLE,
     .           IS_AVAILABLE_VISC,
     .           IS_AVAILABLE_BUMULT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C  /INTER/TYPE18 READING
C-----------------------------------------------
C Initializations
      MSGTITL(1:nchartitle)=' '
      IS1=0
      IS2=0            
      IGAP=0
      FRIC=ZERO          
      IDELKEEP=0          
      XFILTR=ZERO         
      BUMULT=ZERO         
      VISC=ZERO
      ISTIFF=0 
      VREF=ZERO         
      STARTT=ZERO
      STOPT=EP30
!Interface 18 <=> NTYP=7 &INACTI=7
      NTYP = 7
      INACTI = 7
      IPARI(15)=NOINT
      IPARI(7)=NTYP
C------------------------------------------------------------
C  Line1
C------------------------------------------------------------
      CALL HM_GET_INTV('tempsecondaryentityids', ISU1, IS_AVAILABLE, LSUBMODEL)
      IF (.NOT. IS_AVAILABLE) THEN
        ISU1 = 0
      ENDIF
      CALL HM_GET_INTV('mainentityids', ISU2, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('secondaryentityids', GRBRIC_ID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Igap', IGAP, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ipres', IBAG, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Idel', IDEL7N, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Iauto', ISTIFF, IS_AVAILABLE, LSUBMODEL)
C------------------------------------------------------------
C  Line2
C------------------------------------------------------------
      CALL HM_GET_FLOATV('STFAC', STFAC, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('VREF', VREF, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('GAP', GAP, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('TSTART', STARTT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('TSTOP', STOPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
C------------------------------------------------------------
C  Line3
C------------------------------------------------------------
      CALL HM_GET_FLOATV('STIFF_DC', VISC, IS_AVAILABLE_VISC, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SORT_FACT', BUMULT, IS_AVAILABLE_BUMULT, LSUBMODEL, UNITAB)
      
!===BACKUP TO BE ABLE TO OUTPUT USER ids
       ISU1_user=ISU1
       ISU2_user=ISU2
       ISU3_user=GRBRIC_ID

!===CHECK USER FLAG FOR GAP VALUE
      IF(IGAP == 0)IGAP=1000 !default (constant gap)
      IF(IGAP /= 1000 .AND. IGAP /= 1)IGAP = 1000 !unexpected value => default value 
      IF(IGAP == 1)INTER18_IS_VARIABLE_GAP_DEFINED = .TRUE.
      IGAP0=IGAP

!===CHECK USER FLAG FOR STIFFNESS VALUE
      IF(ISTIFF==0)ISTIFF=1 !default
      IF(ISTIFF <= -1 .OR. ISTIFF >2)ISTIFF = 1  !default
      IF(ISTIFF == 2) INTER18_AUTOPARAM = 1
      
!===CHECK STRUCTURE IDENTIFIER :ISU2=SURF_ID                                                                                                      
       IF(ISU2 == 0) THEN 
             MSGTITL='LAGRANGIAN SURFACE IS EMPTY (SURF_ID)'
             CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO, I1=NOINT,C1=TITR,C2=MSGTITL)                
            IS2=0      
          ELSE 
            IS2=1 
            INGR2USR => IGRSURF(1:NSURF)%ID  
            ISU2=NGR2USR(ISU2,INGR2USR,NSURF) 
            MSGTITL='SURFACE CANNOT BE FOUND (SURF_ID)' 
            IF(ISU2 == 0)CALL ANCMSG(MSGID=1115, MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)                        
       ENDIF 

!===CHECK ALE GROUP IDENTIFIER:ISU1=GRNOD_ID (old format) otherwise use Group of solids (GRBRIC_ID,GRQUAD_ID,GRTRIA_ID                                    
       IF(ISU1 /= 0 .AND. GRBRIC_ID /= 0)GRBRIC_ID=0 ! Possible Istf flag defined in input (was removed from manuabecause Istf is always 1) 
c          IF(ISU1 /= 0 .AND. GRBRIC_ID /= 0)THEN                                                                          
c             MSGTITL='YOU CANNOT DEFINE BOTH GRNOD_ID and GRBRIC_ID'                                                                       
c             CALL ANCMSG(MSGID=1115,                                                                                                    
c     .                   MSGTYPE=MSGERROR,  
c     .                   ANMODE=ANINFO,                                   
c     .                   I1=NOINT,                                        
c     .                   C1=TITR,                                         
c     .                   C2=MSGTITL)                                      
c          ENDIF                                                           
        IF(ISU1 /= 0)THEN                                                  
            INGR2USR => IGRNOD(1:NGRNOD)%ID                                
            ISU1=NGR2USR(ISU1,INGR2USR,NGRNOD)                             
            IS1 =2  
             IF(ISU1 == 0)THEN
               MSGTITL='GROUP OF NODES CANNOT BE FOUND (GRNOD_ID)'              
               CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)  
             ELSEIF(MULTI_FVM%IS_USED)THEN            
               MSGTITL='GRBRIC_id (COLUMN 3) MUST BE PROVIDED INSTEAD OF GRNOD_id (COLUMN 1)' 
               CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)
             ENDIF 
        ELSE                                                               
            !GRBRIC_ID,GRQUAD_ID,GRTRIA_ID                                 
            IF(GRBRIC_ID /= 0)THEN                                         
              INGR2USR => IGRBRIC(1:NGRBRIC)%ID                            
              GRBRIC_ID = NGR2USR(GRBRIC_ID,INGR2USR,NGRBRIC)              
              IS1 = 5                                                      
            ENDIF                                                          
            IF(GRBRIC_ID == 0) THEN                                        
             MSGTITL='GROUP OF ALE CELLS IS EMPTY (GRBRIC_ID)'             
             CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)             
            ELSE                                                           
              ISU1=GRBRIC_ID !ISU1 outgoing argument used to get nodes in grbrick  
            ENDIF                                              
        ENDIF 

!===CHECK GRBRIC_ID IS PROVIDED TO CALL LATER LECINT > INGRBRIC_DX (automatic gap)        

        !Variable gap (Igap=1) requires a Group of Bricks
        IF(IGAP == 1 .AND. GRBRIC_ID == 0)THEN
          MSGTITL='GRBRIC_ID MUST BE DEFINED TO ENABLE VARIABLE GAP' 
          CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)            
        ENDIF 

        !Constant gap with Gap=0.0 requires a Group of Bricks
        IF(IGAP == 1000 .AND. GRBRIC_ID == 0 .AND. GAP == ZERO)THEN
          MSGTITL='GRBRIC_ID MUST BE DEFINED TO ESTIMATE CONSTANT GAP VALUE' 
          CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)            
        ENDIF 
                                                 
!===CHECK STFAC VALUE                                               
        IF(STFAC <= ZERO .AND. ISTIFF==1)THEN                                  
             MSGTITL='STIFFNESS VALUE MUST BE DEFINED (STFVAL)'
             CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR, ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)                          
        ENDIF
        SCALE = ONE
        IF(ISTIFF==2)THEN
          IF(STFAC == ZERO)STFAC=ONE
          SCALE = STFAC
        ENDIF                                                                                                  
      
      IF(ISTIFF == 2 .AND. GRBRIC_ID == 0)THEN
        MSGTITL='GROUP OF ALE CELLS (GRBRIC_ID) MUST BE DEFINED WHEN ISTIFF=2'                                                   
        CALL ANCMSG(MSGID=1115, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=NOINT, C1=TITR, C2=MSGTITL)        
      ENDIF
      
!===DEFAULT
        IF(IBAG <= -1 .OR. IBAG >= 4)IBAG=1
        IF(IDEL7N <= -1 .OR. IDEL7N >= 3)IDEL7N=0
        IF(STFAC == ZERO)STFAC=ONE
        STFAC=-STFAC 
        
        !IF DEFINTER IS CALLED IN A FURTHER VERSION : remove this line
        IF(IGAP==1000)IGAP=0
          
        IF (STOPT == ZERO) STOPT = EP30
        IF(BUMULT == ZERO)  BUMULT = BMUL0  
        IF(ISTIFF==2)THEN
          STFAC=STFAC*VREF*VREF ! will be updated in lecint.F
        ENDIF 
                  
!===BUFFER STORAGE  
        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP
        FRIGAP(3)=STARTT     
        FRIGAP(4)=BUMULT          
        FRIGAP(10)=FLOAT(0) !FRIGAP(10) is initialized but used only in engine for storing number of couples candidates   
        FRIGAP(11)=STOPT               
        FRIGAP(13)=ONE      !GAPSCALE
        FRIGAP(14)=VISC   
        FRIGAP(15)=ZERO
        FRIGAP(16)=EP30     !GAPMAX
        FRIGAP(17)=ZERO     !STMIN
        FRIGAP(18)=ZERO     !STMAX
        
        FRIC_P(1:6) = ZERO  !C1..C6          

        IPARI(7)  = NTYP
        IPARI(12) = 0                
        IPARI(13) = IS1*10+IS2             
        IPARI(14) = 0        ! tag for collocated scheme
        IPARI(17) = IDEL7N         
        IPARI(18) = INACTI 
        IPARI(20) = 0
        IPARI(21) = IGAP
        IPARI(22) = INACTI
        IPARI(23) = 4       !MULTIMP        
        IPARI(29) = ISTIFF
        IPARI(30) = 0       !MFROT
        IPARI(31) = 0       !IFQ
        IPARI(32) = IBAG
        IPARI(34) = 0       !ILAGM   
        IPARI(39) = 0       !ICURV
        IPARI(40) = 0       !NA1
        IPARI(41) = 0       !NA2
        IPARI(45) = ISU1
        IPARI(46) = ISU2       
        IPARI(61) = 0       !IDELKEEP           
        IPARI(65) = 0
        IPARI(83) = GRBRIC_ID        

C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
        !==OUTPUTS USER IDS FOR MAIN/SECONDARY DEFINITION
        WRITE(IOUT,3017)  
        IF(GRBRIC_ID > 0)THEN
            WRITE(IOUT,6002)ISU3_user  !SECONDARY side from grbrick_id
        ELSE
            WRITE(IOUT,6001)ISU1_user  !SECONDARY side from grnod_id
        ENDIF
        WRITE(IOUT,6003) ISU2_user   !MAIN side from surf_id     

        WRITE(IOUT,3018)IGAP0,ISTIFF,IBAG 
                
        !stiffness output
        ! USER          INTERNAL
        ! ISTIFF=0,1 ->  1       (constant)
        ! ISTIFF=2   ->  2       (calculated from Vref & scale factor)
        WRITE(IOUT,3015)
        IF(ISTIFF==1)THEN
          !constant user value
          WRITE(IOUT,3024)-STFAC                                               
        ELSE
          !automatic constat value (needs VREF to be calculated)
          WRITE(IOUT,3025)
          WRITE(IOUT,3020)SCALE !scale factor                   
          WRITE(IOUT,3021)VREF                                                 
        ENDIF

        !gap output 
        WRITE(IOUT,3014)  
        ! USER          INTERNAL
        ! IGAP=0,1000 ->  0      (constant gap : defaut = auto)
        ! IGAP=1      ->  1      (variable gap)      
        IF(IGAP == 0)THEN                                  
          !CONSTANT GAP  
          IF(GAP > ZERO)THEN  
            !user value                              
            WRITE(IOUT,3024)GAP              
          ELSE
            !computed value
            WRITE(IOUT,3025)
          ENDIF
        ELSE                                             
          !VARIABLE GAP                                 
          WRITE(IOUT,3026)                             
        ENDIF                                            
                                      
        IF(IS_AVAILABLE_VISC .OR. IS_AVAILABLE_BUMULT)THEN 
          !VISC & BUMULT may be available with old input version                  
          WRITE(IOUT,3028)STARTT,STOPT,VISC,BUMULT
        ELSE
          WRITE(IOUT,3029)STARTT,STOPT
        ENDIF
        
        IF(IDEL7N /= 0) THEN
           WRITE(IOUT,'(A,A,I5/)')'    DELETION FLAG ON FAILURE OF MAIN ELEMENT','  (1:YES-ALL/2:YES-ANY) : ',IDEL7N
           IF(IDELKEEP == 1)THEN
             WRITE(IOUT,'(A)')    '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
           ENDIF         
        ENDIF
        
C--------------------------------------------------------------
C 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN


 3014 FORMAT('    --- GAP ---' )  
 3015 FORMAT('    --- STIFFNESS ---' )  
          
 3017 FORMAT('    TYPE == 18 ALE-LAGRANGE COUPLING' /)
 3018 FORMAT(
     .    '    IGAP FLAG FORMULATION . . . . . . . . . . . ',I10/,
     .    '    ISTF FLAG FORMULATION . . . . . . . . . . . ',I10/,
     .    '    PRESSURE CORRECTION FLAG . . . . . . . . . .',I10/)
          
 3020 FORMAT(
     .    '    SCALE FACTOR. . . . . . . . . . . . . . . . ',1PG20.13) 
 3021 FORMAT(
     .    '    REFERENCE VELOCITY. . . . . . . . . . . . . ',1PG20.13)          
 3024 FORMAT(
     .    '    CONSTANT USER VALUE . . . . . . . . . . . . ',1PG20.13)
 3025 FORMAT(
     .    '    AUTOMATIC CONSTANT VALUE')
 3026 FORMAT(
     .    '    AUTOMATIC VARIABLE VALUE')
 3028 FORMAT(
     .   /'    START TIME. . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .    '    SORTING FACTOR. . . . . . . . . . . . . . . ',1PG20.13) 
 3029 FORMAT(
     .   /'    START TIME. . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . . ',1PG20.13)     
     
 6001 FORMAT(
     .       '    NODE GROUP IDENTIFIER.  . . . . . . . . ',I10)
 6002 FORMAT(
     .       '    BRICK GROUP IDENTIFIER  . . . . . . . . ',I10)
 6003 FORMAT(
     .       '    SURFACE GROUP IDENTIFIER. . . . . . . . ',I10/)   

      END
