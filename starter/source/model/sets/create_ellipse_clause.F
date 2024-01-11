Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CREATE_ELLIPSE_CLAUSE         source/model/sets/create_ellipse_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|====================================================================
      SUBROUTINE CREATE_ELLIPSE_CLAUSE(ID        ,TITLE    ,SUB_ID   ,SKEW   ,RTRANS,
     .                                 CLAUSE    ,NRTRANS  ,LSUBMODEL,UNITAB ,ISKN  ,
     .                                 IAD       ,NTRANSF  ,NUMSKW   ,LISKN  ,LSKEW ,
     .                                 SSKEW     ,SISKWN   ,NSPCOND  ,NUMSPH )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create PART Clause from LIST
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================,
C     JCLAUSE       parameter with HM_READER (current clause read)
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE MY_ALLOC_MOD
      USE NAMES_AND_TITLES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: IAD
      INTEGER, INTENT(IN) :: NTRANSF,NUMSKW,LISKN,LSKEW,NRTRANS,
     .                       SSKEW,SISKWN,NSPCOND,NUMSPH
      INTEGER, INTENT(IN) :: ID,SUB_ID,ISKN(LISKN,SISKWN/LISKN)
      MY_REAL, INTENT(IN) :: SKEW(LSKEW,SSKEW/LSKEW),RTRANS(NTRANSF,NRTRANS)
      CHARACTER(LEN=NCHARTITLE), INTENT(IN) ::  TITLE
!
      TYPE (SET_), INTENT(INOUT) :: CLAUSE
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,DGR1,DGR,ISKEW
!
      MY_REAL  XG,YG,ZG,S_A,S_B,S_C
      LOGICAL :: lFOUND
      LOGICAL :: IS_AVAILABLE
C=======================================================================
      DGR1=0

      CALL HM_GET_INTV('Skew_ID' ,ISKEW,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('n' ,DGR1,IS_AVAILABLE,LSUBMODEL)

      CLAUSE%ELLIPSE_ID_MADYMO=ISKEW ! No de l'entite qui impose le mvt de la surface.
      !                                --> No systeme Radioss ou MaDyMO.
      !skew:temporary storage of user id
      !get internal id from user id

      lFOUND=.FALSE.                
      DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD                      
        IF (ISKEW==ISKN(4,J+1)) THEN                                     
          ISKEW=J+1  
          lFOUND=.TRUE.                                                    
          EXIT                                                     
        ENDIF                                                           
      END DO

      IF(.NOT.lFOUND)THEN                                                         
        CALL ANCMSG(MSGID=184,                                         
     .                    MSGTYPE=MSGERROR,                                  
     .                    ANMODE=ANINFO,                                     
     .                    C1='SURFACE',                                      
     .                    I1=ID,                                             
     .                    C2='SURFACE',                                      
     .                    C3=TITLE,                                           
     .                    I2=ISKEW)
      ELSE                                                       
        ! Init surface rotation                     
        !DO J=1,9                                                         
          !!BUFSF(IAD+7+J-1)=SKEW(J,ISKEW)                                  
        !END DO                                                           
      ENDIF

      CALL HM_GET_FLOATV('Xc' ,XG,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Yc' ,YG,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Zc' ,ZG,IS_AVAILABLE,LSUBMODEL,UNITAB)                                                         
      IF(SUB_ID /= 0)CALL SUBROTPOINT(XG,YG,ZG,RTRANS,SUB_ID,LSUBMODEL)

      !BUFSF(IAD+4)=XG                                                  
      !BUFSF(IAD+5)=YG                                                  
      !BUFSF(IAD+6)=ZG                                                  
      !Init application point for force and momentum     
      !/* ellipsoides : defining center ! */      
      !BUFSF(IAD+16)=XG                                                 
      !BUFSF(IAD+17)=YG                                                 
      !BUFSF(IAD+18)=ZG                                                 

      CALL HM_GET_FLOATV('a' ,S_A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('b' ,S_B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('c' ,S_C,IS_AVAILABLE,LSUBMODEL,UNITAB)                        
      DGR = 0                             
      IF ( S_A==0. .OR. S_B==0. .OR. S_C==0.) THEN                        
         CALL ANCMSG(MSGID=185,                                        
     .                     MSGTYPE=MSGERROR,                                 
     .                     ANMODE=ANINFO,                                    
     .                     I1=ID,                                            
     .                     C1=TITLE)                                          
      ENDIF                                           
                 
      IF (DGR==0.AND.DGR1==0) THEN                                     
       DGR1=2                                                          
      ELSEIF (DGR1==0) THEN                                            
       DGR1=DGR                                                        
      ENDIF                                                            
                                                  
      !BUFSF(IAD+1)=S_A                                                  
      !BUFSF(IAD+2)=S_B                                                  
      !BUFSF(IAD+3)=S_C                                                  
      !BUFSF(IAD+36)=DGR1                                               


      ! Copy in final SET
      ! ------------------

      !------------------------------------!
      !  create SURF clause                !
      !------------------------------------!      print*,"clause ELLIPSE_SECOND", ISKEW
      CLAUSE%NB_ELLIPSE=1 ! only one ellipsoid per /SET
      CALL MY_ALLOC(CLAUSE%ELLIPSE_SKEW,9)

      CLAUSE%ELLIPSE_IAD_BUFR= IAD ! Analytical Surfaces address (reals BUFSF - temp)

      CLAUSE%ELLIPSE_N=DGR1
      CLAUSE%ELLIPSE_XC=XG
      CLAUSE%ELLIPSE_YC=YG
      CLAUSE%ELLIPSE_ZC=ZG
      CLAUSE%ELLIPSE_A=S_A
      CLAUSE%ELLIPSE_B=S_B
      CLAUSE%ELLIPSE_C=S_C
      CLAUSE%ELLIPSE_SKEW(1:9)=SKEW(1:9,ISKEW)

      IAD=IAD+36
!
      RETURN
      END
