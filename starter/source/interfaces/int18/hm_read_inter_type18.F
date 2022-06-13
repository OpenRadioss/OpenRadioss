Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE18(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     3          IGRNOD     ,IGRSURF    ,IGRBRIC    ,XFILTR    ,FRIC_P   ,
     3          IDDLEVEL   ,TITR, UNITAB, LSUBMODEL,MULTI_FVM)
C============================================================================
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
!     NSUBMOD
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)::IDDLEVEL
      INTEGER ISU1,ISU2,IS1,IS2,NI,NOINT
      INTEGER IPARI(*)
      my_real
     .   FRIGAP(*),FRIC_P(*),STFAC,XFILTR
      CHARACTER,INTENT(IN) :: TITR*nchartitle
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM      
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (UNIT_TYPE_), INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr12_c.inc"
#include      "sysunit.inc"
#include      "inter18.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER GRBRIC_ID,GRQUAD_ID,GRTRIA_ID,IBID,IBAG,IDEL7N,IGAP,IBUC,
     .    IDUM,NTYP,INACTI,IGSTI,BCOPT,ITIED,VISCF,ILEV,MULTIMP,ICURV,
     .    IVIS2,IDELKEEP,MFROT,IFQ,MODFR,ISU1_user,ISU2_user,ISU3_user,
     .    INTKG,IDIR,ISTIFF
      my_real
     .   BID,GAP,STARTT,STOPT,BUMULT,RDUM,VISC,
     .   GAPSCALE,GAPMAX,STMIN,STMAX,FRIC,ALPHA,DTMIN,VREF
      CHARACTER MESS*40, MSGTITL*nchartitle
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
C    /INTER/TYPE18 reading 
C=======================================================================

C Initializations
      IS1=0
      IS2=0
      IGSTI = 1 !in any cases otherwise negative stiffness node error at cycle 0            
!-INIT-default
      BCOPT=0             
      ITIED=0             
      VISCF=0             
      ILEV=0              
      IBUC = 0            
      IGAP=0              
      MULTIMP=0           
      ICURV=0             
      GAPSCALE=ONE         
      GAPMAX=EP30         
      STMIN=ZERO          
      STMAX=ZERO          
      FRIC=ZERO           
      IVIS2=0             
      IDELKEEP=0          
      MFROT=0             
      IFQ=00              
      ALPHA=ZERO          
      MODFR=1             
      XFILTR=ZERO         
      BUMULT=ZERO         
      VISC=ZERO           
      INTKG = 0 
      IDIR=0
      ISTIFF=0 
      VREF=ZERO         

      STARTT = ZERO
      STOPT=EP30

!Interface 18 <=> NTYP=7 &INACTI=7
      NTYP = 7
      INACTI = 7
      IPARI(15)=NOINT
      IPARI(7)=NTYP
      
      MSGTITL(1:nchartitle)=' '
C------------------------------------------------------------
C  Card1
C------------------------------------------------------------
      CALL HM_GET_INTV('tempsecondaryentityids', ISU1, IS_AVAILABLE, LSUBMODEL)
      IF (.NOT. IS_AVAILABLE) ISU1 = 0
      CALL HM_GET_INTV('mainentityids', ISU2, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('secondaryentityids', GRBRIC_ID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ipres', IBAG, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Idel', IDEL7N, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Iauto', ISTIFF, IS_AVAILABLE, LSUBMODEL)
C------------------------------------------------------------
C  Card2
C------------------------------------------------------------
      CALL HM_GET_FLOATV('Stfval', STFAC, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Vref', VREF, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Gap', GAP, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Tstart', STARTT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Tstop', STOPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
C------------------------------------------------------------
C  Card3
C------------------------------------------------------------
      CALL HM_GET_FLOATV('VISs', VISC, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bumult', BUMULT, IS_AVAILABLE, LSUBMODEL, UNITAB)

!===TO BE ABLE TO OUTPUT USER ids
       ISU1_user=ISU1
       ISU2_user=ISU2
       ISU3_user=GRBRIC_ID


!===CHECK FLAG FORMULATION
      IF(ISTIFF==0)ISTIFF=1 !default
      IF(ISTIFF <= -1 .OR. ISTIFF >2)ISTIFF=1  !default
      IF(ISTIFF == 2) INTER18_AUTOPARAM = 1
      
!===CHECK STRUCTURE:ISU2=SURF_ID                                                                                                      
       IF(ISU2 == 0) THEN 
             MSGTITL='LAGRANGIAN SURFACE IS EMPTY (SURF_ID)'
             !IF(IDDLEVEL==1)CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO, I1=NOINT,C1=TITR,C2=MSGTITL)
             CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO, I1=NOINT,C1=TITR,C2=MSGTITL)                
            IS2=0      
          ELSE 
            IS2=1 
            INGR2USR => IGRSURF(1:NSURF)%ID  
            ISU2=NGR2USR(ISU2,INGR2USR,NSURF) 
            MSGTITL='SURFACE CANNOT BE FOUND (SURF_ID)' 
            !IF(ISU2 == 0 .AND. IDDLEVEL==1)CALL ANCMSG(MSGID=1115, MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL) 
            IF(ISU2 == 0)CALL ANCMSG(MSGID=1115, MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)                        
       ENDIF 

!===CHECK ALE CELLS:ISU1=GRNOD_ID (old format) otherwise use Group of solids (GRBRIC_ID,GRQUAD_ID,GRTRIA_ID                                    
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
             !IF(ISU1 == 0 .AND. IDDLEVEL==1)CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)
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
             !IF(IDDLEVEL==1)CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)
             CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)             
            ELSE                                                           
              ISU1=GRBRIC_ID !ISU1 outgoing argument used to get nodes in grbrick  
            ENDIF                                              
        ENDIF                                                  
!===CHECK STFAC                                                
        IF(STFAC <= ZERO .AND. ISTIFF==1)THEN                                  
             MSGTITL='STIFFNESS VALUE MUST BE DEFINED (STFVAL)'
             !IF(IDDLEVEL==1)CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR, ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)  
             CALL ANCMSG(MSGID=1115,MSGTYPE=MSGERROR, ANMODE=ANINFO,I1=NOINT,C1=TITR,C2=MSGTITL)                          
        ENDIF
        IF(ISTIFF==2)THEN
          IF(STFAC == ZERO)STFAC=ONE
        ENDIF                                                                                                  
      
      IF(ISTIFF == 2 .AND. GRBRIC_ID == 0)THEN
        MSGTITL='GROUP OF ALE CELLS (GRBRIC_ID) MUST BE DEFINED WHEN ISTIFF=2'                                                   
        !IF(IDDLEVEL==1)CALL ANCMSG(MSGID=1115, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=NOINT, C1=TITR, C2=MSGTITL)
        CALL ANCMSG(MSGID=1115, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=NOINT, C1=TITR, C2=MSGTITL)        
      ENDIF
      
!===DEFAULT
        IF(IBAG <= -1 .OR. IBAG >= 4)IBAG=1
        IF(IDEL7N <= -1 .OR. IDEL7N >= 3)IDEL7N=0
        IF(STFAC == ZERO)STFAC=ONE
        STFAC=-STFAC 
          
!===STORAGE  

        FRIGAP(13) = ONE     !GAPSCALE
        FRIGAP(16) = EP30   !GAPMAX
        FRIGAP(17) = ZERO   !STMIN
        FRIGAP(18) = ZERO   !STMAX
        IF(ISTIFF==2)THEN
          STFAC=STFAC*VREF*VREF ! will be updated in lecint.F
        ENDIF

        IPARI(7)=NTYP
        IPARI(45)=ISU1
        IPARI(46)=ISU2     
        IPARI(13)=IS1*10+IS2             
        IPARI(14) = 0       !IVIS2
        IPARI(17) = IDEL7N         
        IPARI(18) = INACTI 
        IPARI(30) = 0       !MFROT
        IPARI(31) = 0       !IFQ
        IPARI(32) = IBAG
        IPARI(34) = IDIR    !ILAGM   
        IPARI(39) = 0       !ICURV
        IPARI(40) = 0       !NA1
        IPARI(41) = 0       !NA2
        IPARI(61) = 0       !IDELKEEP
        IPARI(83) = GRBRIC_ID
        FRIC_P(1:6) = ZERO  !C1..C6

C------------------------------------------------------------

        IPARI(12)=IBUC
        IPARI(29)=ISTIFF
        IPARI(65) = INTKG
        IPARI(20)=ILEV

        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP


        IF (STOPT == ZERO) STOPT = EP30

        FRIGAP(3)=STARTT
        FRIGAP(11)=STOPT

        IF(BUMULT == ZERO)  BUMULT = BMUL0
        FRIGAP(4)=BUMULT

C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
        FRIGAP(10)=FLOAT(0)

C       POUR STOCKER LE FLAG D'INACTIVATION DES PENE INITIALES
C       (0 = RIEN , 1 = NOEUDS , 2 = FACETTES )
        IPARI(21)=IGAP
        IPARI(22)=INACTI
        MULTIMP = 4
        IPARI(23)=MULTIMP
C       FRIGAP(13)=INACTI
        FRIGAP(15)=VISCF**2

        FRIGAP(14)=VISC
C
        IPARI(13)=IS1*10+IS2
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
      DTMIN = ZERO
      
      !IF(IDDLEVEL==0)THEN

        !==OUTPUTS USER IDS FOR MAIN/SECONDARY DEFINITION
        WRITE(IOUT,3017)  
        IF(GRBRIC_ID > 0)THEN
            WRITE(IOUT,6002)ISU3_user  !SECONDARY side from grbrick_id
        ELSE
            WRITE(IOUT,6001)ISU1_user  !SECONDARY side from grnod_id
        ENDIF
        WRITE(IOUT,6003) ISU2_user   !MAIN side from surf_id     

        IF(ISTIFF==1)THEN
          WRITE(IOUT,3015)
          WRITE(IOUT,3018)ISTIFF,-STFAC,GAP,STARTT,STOPT,VISC,BUMULT,IBAG!,IDIR                                                  
        ELSE
          WRITE(IOUT,3016)
          WRITE(IOUT,3019)ISTIFF,VREF,GAP,STARTT,STOPT,VISC,BUMULT,IBAG!,IDIR 
          IF(GAP > ZERO)WRITE(IOUT,3020)GAP     
          IF(STFAC > ZERO)WRITE(IOUT,3020)GAP                                                    
        ENDIF
        
        IF(IDEL7N /= 0) THEN
           WRITE(IOUT,'(A,A,I5/)')'    DELETION FLAG ON FAILURE OF MAIN ELEMENT','  (1:YES-ALL/2:YES-ANY) : ',IDEL7N
           IF(IDELKEEP == 1)THEN
             WRITE(IOUT,'(A)')    '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
           ENDIF         
        ENDIF
        
      !ENDIF

C--------------------------------------------------------------
C 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN
 999  CALL FREERR(3)
      RETURN

 3015 FORMAT('    --- USER STIFFNESS ---' )  
 3016 FORMAT('    --- AUTOMATIC STIFFNESS ---' )  
          
 3017 FORMAT('    TYPE == 18 ALE-LAGRANGE COUPLING' /)
 3018 FORMAT(
     .    '    ISTF FLAG FORMULATION . . . . . . . . . . . ',I10/,
     .    '    STFVAL. . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    USER GAP. . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .    '    SORTING FACTOR. . . . . . . . . . . . . . . ',1PG20.13/,          
     .    '    PRESSURE CORRECTION FLAG . . . . . . . . . .',I10)!  /,
C     .    '    FLAG FOR REACTION FORCE ORIENTATION. . . . .',I10/)
 3019 FORMAT(
     .    '    ISTF FLAG FORMULATION . . . . . . . . . . . ',I10/,       
     .    '    REFERENCE VELOCITY. . . . . . . . . . . . . ',1PG20.13/,
     .    '    USER GAP. . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . . ',1PG20.13/, 
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .    '    SORTING FACTOR. . . . . . . . . . . . . . . ',1PG20.13/,         
     .    '    PRESSURE CORRECTION FLAG . . . . . . . . . .',I10)   !/,
C     .    '    FLAG FOR REACTION FORCE ORIENTATION. . . . .',I10) 
 3020 FORMAT(
     .    '    GAP VALUE . . . . . . . . . . . . . . . . . ',1PG20.13) 
 6001 FORMAT(
     .       '    NODE GROUP IDENTIFIER.  . . . . . . . . ',I10)
 6002 FORMAT(
     .       '    BRICK GROUP IDENTIFIER  . . . . . . . . ',I10)
 6003 FORMAT(
     .       '    SURFACE GROUP IDENTIFIER. . . . . . . . ',I10/)   
      END
