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
Chd|  READ_INTER_KINE_TYPE18        source/interfaces/int18/read_inter_kine_type18.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREDEC_3KEY_2ID_T             source/starter/freform.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL                       source/starter/freform.F      
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE READ_INTER_KINE_TYPE18(
     1        IPARI        ,STFAC       ,FRIGAP        ,XFILTR    ,FRIC_P      ,
     2        IGRNOD       ,IGRSURF    ,IGRBRIC        ,ILAGM     ,NI          ,
     3        NOM_OPT      ,TITR       ,UNITAB         ,IDDLEVEL  ) 
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IDDLEVEL
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER ISU1,ISU2,ILAGM,NI,IS1,IS2
      INTEGER IPARI(*)
      my_real
     .   FRIGAP(*),FRIC_P(*),STFAC,XFILTR
      CHARACTER TITR*nchartitle
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   IFLAGUNIT,NOINT,UID,NTYP,USR_TYP,J,IBUC,
     .   GRBRIC_ID,GRQUAD_ID,GRTRIA_ID,IBID,IBAG,IDEL7N,IGAP,
     .   IDUM,INACTI,IGSTI,BCOPT,ITIED,VISCF,ILEV,MULTIMP,ICURV,INTKG,
     .   IVIS2,IDELKEEP,MFROT,IFQ,MODFR,ISU1_user,ISU2_user,ISU3_user
      my_real
     .   FAC_L,FAC_T,FAC_M,FAC_C,
     .   BID,GAP,STARTT,STOPT,BUMULT,RDUM,VISC,
     .   GAPSCALE,GAPMAX,STMIN,STMAX,FRIC,ALPHA,DTMIN
      CHARACTER MESS*40, MSGTITL*nchartitle
      CHARACTER OPT*ncharkey,KEY*ncharkey
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
C    /INTER/KINE/TYPE18 reading 
C=======================================================================

C Initializations
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
      FAC_M = 1
      FAC_L = 1
      FAC_T = 1
      FAC_C = 1
      INTKG = 0
C
!Interface 18 <=> NTYP=7 &INACTI=7
       NTYP = 7
       INACTI = 7

C
      ILAGM = -2
C
      CALL FREDEC_3KEY_2ID_T(OPT,KEY,NOINT,UID,TITR)
c
      IFLAGUNIT = 0
      DO J=1,NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN                       
            FAC_M = UNITAB%FAC_M(J)     
            FAC_L = UNITAB%FAC_L(J)     
            FAC_T = UNITAB%FAC_T(J) 
            FAC_C = FAC_M / ( FAC_L * FAC_T * FAC_T )
            IFLAGUNIT = 1
            EXIT
          ENDIF
      ENDDO
      IF (UID.NE.0.AND.IFLAGUNIT.EQ.0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=NOINT,C1='INTERFACE',
     .                 C2='INTERFACE',
     .                 C3=TITR)
      ENDIF
      IREC=IREC+1

C---------------------------------
      NOM_OPT(1,NI)=NOINT

      WRITE(IOUT,1000) NOINT,TRIM(TITR)
C---------------------------------
      CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
      IPARI(33) = MAX(0,ILAGM)
C---------------------------------------------------------------

C------------------------------------------------------------
C  Card1
C------------------------------------------------------------
       READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       READ(LINE,ERR=999,FMT=FMT_9I)ISU1,ISU2,GRBRIC_ID,GRQUAD_ID,GRTRIA_ID,IBID,IBAG,IDEL7N,IBID 
C------------------------------------------------------------
C  Card2
C------------------------------------------------------------
       IREC=IREC+1
       READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       READ(LINE,ERR=999,FMT=FMT_5F)STFAC,BID,GAP,STARTT,STOPT
C------------------------------------------------------------
C  Card3
C------------------------------------------------------------
       IREC=IREC+1
       READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       IF(LINE(1:1).NE."/" .AND. LINE(1:1).NE."#" .AND. LINE(1:1).NE."$")THEN
          READ(LINE,ERR=999,FMT=FMT_5F)BID,BID,VISC,BID,BUMULT
       ENDIF

!===TO BE ABLE TO OUTPUT USER ids
       ISU1_user=ISU1
       ISU2_user=ISU2
       ISU3_user=GRBRIC_ID

!===CHECK STRUCTURE:ISU2=SURF_ID                                                                                                      
       IF(ISU2.EQ.0) THEN                                                                                                                    
             MSGTITL='LAGRANGIAN SURFACE IS EMPTY (SURF_ID)'                                                                                    
             IF(IDDLEVEL==1)CALL ANCMSG(MSGID=1115,                                                                                                            
     .                   MSGTYPE=MSGERROR,                                                                                                      
     .                   ANMODE=ANINFO,                                                                                                         
     .                   I1=NOINT,                                                                                                              
     .                   C1=TITR,                                                                                                               
     .                   C2=MSGTITL)                                                                                                            
            IS2=0                                                                                                                               
          ELSE                                                                                                                                  
            IS2=1                                                                                                                               
            INGR2USR => IGRSURF(1:NSURF)%ID                                                                                                     
            ISU2=NGR2USR(ISU2,INGR2USR,NSURF) 
            MSGTITL='SURFACE CANNOT BE FOUND (SURF_ID)' 
            IF(ISU2.EQ.0 .AND. IDDLEVEL==1)CALL ANCMSG(MSGID=1115,                                                                                                            
     .                   MSGTYPE=MSGERROR,                                                                                                      
     .                   ANMODE=ANINFO,                                                                                                         
     .                   I1=NOINT,                                                                                                              
     .                   C1=TITR,                                                                                                               
     .                   C2=MSGTITL)                                                                                                                 
       ENDIF                                                                                                                                 
          
!===CHECK ALE CELLS:ISU1=GRNOD_ID (old format) otherwise use Group of solids (GRBRIC_ID,GRQUAD_ID,GRTRIA_ID                           
          
       IF(ISU1.NE.0 .AND. GRBRIC_ID.NE.0)GRBRIC_ID=0 ! Possible Istf flag defined in input (was removed from manuabecause Istf is always 1)  
c          IF(ISU1.NE.0 .AND. GRBRIC_ID.NE.0)THEN                                                                                               
c             MSGTITL='YOU CANNOT DEFINE BOTH GRNOD_ID and GRBRIC_ID'                                                                           
c             CALL ANCMSG(MSGID=1115,                                                                                                           
c     .                   MSGTYPE=MSGERROR,                                                                                                     
c     .                   ANMODE=ANINFO,                                                                                                        
c     .                   I1=NOINT,                                                                                                             
c     .                   C1=TITR,                                                                                                              
c     .                   C2=MSGTITL)                                                                                                           
c          ENDIF                                                                                                                                
        IF(ISU1.NE.0)THEN                                                                                                                     
            INGR2USR => IGRNOD(1:NGRNOD)%ID                                                                                                     
            ISU1=NGR2USR(ISU1,INGR2USR,NGRNOD)                                                                                                  
            IS1 =2  
             MSGTITL='GROUP OF NODES CANNOT BE FOUND (GRNOD_ID)' 
             IF(ISU1.EQ.0 .AND. IDDLEVEL==1)CALL ANCMSG(MSGID=1115,                                                                                                            
     .                   MSGTYPE=MSGERROR,                                                                                                      
     .                   ANMODE=ANINFO,                                                                                                         
     .                   I1=NOINT,                                                                                                              
     .                   C1=TITR,                                                                                                               
     .                   C2=MSGTITL)                                                                                                                                          
        ELSE                                                                                                                                  
            !GRBRIC_ID,GRQUAD_ID,GRTRIA_ID                                                                                                      
            IF(GRBRIC_ID.NE.0)THEN                                                                                                              
              INGR2USR => IGRBRIC(1:NGRBRIC)%ID                                                                                                 
              GRBRIC_ID = NGR2USR(GRBRIC_ID,INGR2USR,NGRBRIC)                                                                                   
              IS1 = 5                                                                                                                           
            ENDIF                                                                                                                               
            IF(GRBRIC_ID.EQ.0) THEN                                                                                                             
             MSGTITL='GROUP OF ALE CELLS IS EMPTY (GRBRIC_ID)'                                                                                  
             IF(IDDLEVEL==1)CALL ANCMSG(MSGID=1115,                                                                                                            
     .                   MSGTYPE=MSGERROR,                                                                                                      
     .                   ANMODE=ANINFO,                                                                                                         
     .                   I1=NOINT,                                                                                                              
     .                   C1=TITR,                                                                                                               
     .                   C2=MSGTITL)                                                                                                            
            ELSE                                                                                                                                
              ISU1=GRBRIC_ID !ISU1 outgoing argument used to get nodes in grbrick                                                               
            ENDIF                                                                                                                               
        ENDIF                                                                                                                                 
!===CHECK STFAC                                                                                                                       
        IF(STFAC.LE.ZERO)THEN                                                                                                                 
             MSGTITL='STIFFNESS VALUE MUST BE DEFINED (STFVAL)'                                                                                 
             IF(IDDLEVEL==1)CALL ANCMSG(MSGID=1115,                                                                                                            
     .                   MSGTYPE=MSGERROR,                                                                                                      
     .                   ANMODE=ANINFO,                                                                                                         
     .                   I1=NOINT,                                                                                                              
     .                   C1=TITR,                                                                                                               
     .                   C2=MSGTITL)                                                                                                            
        ENDIF                                                                                                                                 
!===CHECK GAP                                                                                                                         
        IF(GAP.LE.ZERO)THEN                                                                                                                   
             MSGTITL='GAP VALUE MUST BE DEFINED (GAP)'                                                                                          
             IF(IDDLEVEL==1)CALL ANCMSG(MSGID=1115,                                                                                                            
     .                   MSGTYPE=MSGERROR,                                                                                                      
     .                   ANMODE=ANINFO,                                                                                                         
     .                   I1=NOINT,                                                                                                              
     .                   C1=TITR,                                                                                                               
     .                   C2=MSGTITL)                                                                                                            
        ENDIF                                                                                                                                 

!===DEFAULT
        IF(IBAG.LE.-1 .OR. IBAG.GE.4)IBAG=1
        IF(IDEL7N.LE.-1 .OR. IDEL7N.GE.3)IDEL7N=0
        IF(STFAC.EQ.ZERO)STFAC=ONE
        STFAC=-STFAC
          
!===UNITS
        STFAC = STFAC * FAC_M / ( FAC_T ** 2)
        GAP = GAP * FAC_L
          
!===STORAGE  

        FRIGAP(13) = ONE     !GAPSCALE
        FRIGAP(16) = EP30   !GAPMAX
        FRIGAP(17) = ZERO   !STMIN
        FRIGAP(18) = ZERO   !STMAX

        IPARI(7)=NTYP
        IPARI(45)=ISU1
        IPARI(46)=ISU2                  
        IPARI(14) = 0       !IVIS2
        IPARI(17) = IDEL7N         
        IPARI(18) = INACTI 
        IPARI(30) = 0       !MFROT
        IPARI(31) = 0       !IFQ
        IPARI(32) = IBAG
        IPARI(34) = 0       !ILAGM 
          ! specific case of /INTER/KINE/TYPE18
        IF(ILAGM == -2) THEN
          IPARI(34) = ILAGM
          ILAGM = 0
        ENDIF    
        IPARI(39) = 0       !ICURV
        IPARI(40) = 0       !NA1
        IPARI(41) = 0       !NA2
        IPARI(61) = 0       !IDELKEEP
        IPARI(83) = GRBRIC_ID
        FRIC_P(1:6) = ZERO  !C1..C6

C------------------------------------------------------------

        IPARI(12)=IBUC
        IPARI(65) = INTKG
        IPARI(20)=ILEV

        FRIGAP(1)=FRIC
        FRIGAP(2)=GAP

        IF(BUMULT.EQ.ZERO)  BUMULT = BMUL0
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
        WRITE(IOUT,3018)-STFAC,GAP,STARTT,STOPT, 
     .                 VISC,BUMULT,IBAG
        IF(IDEL7N.NE.0) THEN
           WRITE(IOUT,'(A,A,I5/)')'    DELETION FLAG ON FAILURE OF MAIN ELEMENT','  (1:YES-ALL/2:YES-ANY) : ',IDEL7N
           IF(IDELKEEP == 1)THEN
             WRITE(IOUT,'(A)')    '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
           ENDIF         
       ENDIF
!==OUTPUTS USER IDS FOR MAIN/SECONDARY DEFINITION
       IF(GRBRIC_ID.GT.0)THEN
           WRITE(IOUT,6002)ISU3_user  !SECONDARY side from grbrick_id
       ELSE
           WRITE(IOUT,6001)ISU1_user  !SECONDARY side from grnod_id
       ENDIF
       WRITE(IOUT,6003) ISU2_user   !MAIN side from surf_id     

C--------------------------------------------------------------
      IF(IS1.EQ.0)THEN
        WRITE(IOUT,'(6X,A)')'NO SECONDARY SURFACE INPUT'
      ELSEIF(IS1.EQ.1)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1.EQ.2)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES'
      ELSEIF(IS1.EQ.3)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1.EQ.4 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY BRICKS'
      ELSEIF(IS1.EQ.5 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY SOLID ELEMENTS'        
      ENDIF
      IF(IS2.EQ.0)THEN
        WRITE(IOUT,'(6X,A)')'NO MAIN SURFACE INPUT'
      ELSEIF(IS2.EQ.1)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2.EQ.2)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY NODES'
      ELSEIF(IS2.EQ.3)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2.EQ.4)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE REFERS ',
     .                      'TO HYPER-ELLIPSOIDAL SURFACE'
      ENDIF
C  
C--------------------------------------------------------------
C=======================================================================
C  
C--------------------------------------------------------------
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN
 999  CALL FREERR(3)
      RETURN

 3018 FORMAT(//
     .    '    TYPE.EQ.18 ALE-LAGRANGE COUPLING' //,
     .    '    STIFFNESS . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    GAP . . . . . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . . ',1PG20.13/,     
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . . ',1PG20.13/,
     .    '    SORTING FACTOR. . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    PRESSURE CORRECTION FLAG . . . . . . . . . .',I10/)
C
 6001 FORMAT(
     .       '    NODE GROUP IDENTIFIER.  . . . . . . . . ',I10)
 6002 FORMAT(
     .       '    BRICK GROUP IDENTIFIER  . . . . . . . . ',I10)
 6003 FORMAT(
     .       '    SURFACE GROUP IDENTIFIER. . . . . . . . ',I10/)  

      END
