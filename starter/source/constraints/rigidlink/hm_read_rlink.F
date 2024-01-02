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
Chd|  HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        SZ_R2R                        source/coupling/rad2rad/routines_r2r.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_LINK(NNLINK  ,LLLINK  ,ITAB   ,ITABM1  ,IKINE   ,
     .                        IGRNOD  ,ISKN    ,IFRAME ,NOM_OPT ,LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD    
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNLINK(10,*), LLLINK(*), ITAB(*), ITABM1(*),
     .        IKINE(*),ISKN(LISKN,*),IFRAME(LISKN,*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, NSL, NUSER, J, I, IGU,IC,ICR,J10(10),
     .   IGRS, NOSYS,J6(6),IS,IPOL,IDIR,ISL,IKINE1(3*NUMNOD),NLK
      CHARACTER MESS*40,KEY*ncharkey,TITR*nchartitle,
     .          STRING*ncharfield,CODE*7
      my_real
     .   BID
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS, NGR2USR
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C
      DATA MESS/'STANDARD RIGID LINK DEFINITION          '/
C-----------------------------------------------
      LOGICAL IS_AVAILABLE
C=======================================================================
      IS_AVAILABLE = .FALSE.
C-----------------------------------------------
      NSL = 0
      K=0
      WRITE(IOUT,1000)
      NLK = 0
C
C-----------------------------------------------
      CALL HM_OPTION_START('/RLINK')       
C-----------------------------------------------
C
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C
      DO N=1,NLINK
C
C      LECTURE DES CARTES D'INPUT
C
       NLK=NLK+1
C----------Multidomaines --> on ignore les rlink non tages--------
       IF(NSUBDOM > 0)THEN
         IF(TAGLNK(NLK) == 0)CALL SZ_R2R(TAGLNK,NLK)
       END IF
C-----------------------------------------------------------------
       CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = NUSER, 
     .                          OPTION_TITR    = TITR)           
       NOM_OPT(1,N) = NUSER
       CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
!Tx, Ty, Tz, OmegaX, OmegaY, OmegaZ, SKEW_CSID, dependentnodeset, RLINK_IPOL);
C--------------------read card-------------------------------------
       CALL HM_GET_INTV('Tx'      ,J6(1) ,IS_AVAILABLE,LSUBMODEL)  
       CALL HM_GET_INTV('Ty'      ,J6(2) ,IS_AVAILABLE,LSUBMODEL)  
       CALL HM_GET_INTV('Tz'      ,J6(3) ,IS_AVAILABLE,LSUBMODEL) 
       CALL HM_GET_INTV('OmegaX'  ,J6(4) ,IS_AVAILABLE,LSUBMODEL)  
       CALL HM_GET_INTV('OmegaY'  ,J6(5) ,IS_AVAILABLE,LSUBMODEL)  
       CALL HM_GET_INTV('OmegaZ'  ,J6(6) ,IS_AVAILABLE,LSUBMODEL) 

       CALL HM_GET_INTV('SKEW_CSID'         ,IS  ,IS_AVAILABLE,LSUBMODEL) 
       CALL HM_GET_INTV('dependentnodeset'  ,IGU ,IS_AVAILABLE,LSUBMODEL) 
       CALL HM_GET_INTV('RLINK_IPOL'        ,IPOL,IS_AVAILABLE,LSUBMODEL) 
C-----------------------------------------------------------------
       IC =J6(1)*4 +J6(2)*2 +J6(3)
       ICR=J6(4)*4 +J6(5)*2 +J6(6)
       IF (ICR > 0.AND.IRODDL==0) THEN
         CALL ANCMSG(MSGID=844,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=NUSER,
     .               C1=TITR)
       END IF
C------------------------------------------------------------------
C            RECHERCHE DES NOEUDS SECONDARY
C------------------------------------------------------------------
C     GROUPE DE NOEUDS
C------------------------------------------------------------------
       NSL = 0
       INGR2USR => IGRNOD(1:NGRNOD)%ID
       IGRS=NGR2USR(IGU,INGR2USR,NGRNOD)
       IF(IGRS /= 0)THEN
        DO J=1,IGRNOD(IGRS)%NENTITY
          NSL = NSL + 1
          LLLINK(K+NSL)=IGRNOD(IGRS)%ENTITY(J)
        ENDDO
       ENDIF

C------------------------------------------------------------
C      Condition cinematique incompatible
C------------------------------------------------------------
        DO J=1,NSL
         DO IDIR=1,6
            CALL KINSET(1024,ITAB(LLLINK(J+K)),IKINE(LLLINK(J+K)),
     .                   IDIR,0,IKINE1(LLLINK(J+K)))
          ENDDO
        ENDDO
C------------------------------------------------------------------
C      IMPRESSIONS
C------------------------------------------------------------------
       IF(IPOL == 0)THEN
         WRITE(IOUT,1100) NUSER,TRIM(TITR),J6,IS,NSL
       ELSE
         WRITE(IOUT,1200) NUSER,TRIM(TITR),J6,IS,NSL
       ENDIF
C
       IF(IPRI >= 1) THEN
         WRITE(IOUT,'(/10X,A)')'SECONDARY NODES :   '
         WRITE(IOUT,FMT=FMW_10I) (ITAB(LLLINK(I+K)),I=1,NSL)
         WRITE(IOUT,'(//)')
       ENDIF
       IF(IPOL == 0)THEN
         DO 640 J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
           IF(IS == ISKN(4,J+1)) THEN
             IS=J+1
             GO TO 660
           ENDIF
 640      CONTINUE
C
         CALL ANCMSG(MSGID=184,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               C1='RIGID LINK',
     .               I1=NUSER,
     .               C2='RIGID LINK',
     .               C3=TITR,
     .               I2=IS)
 660     CONTINUE
       ELSE
         DO J=0,NUMFRAM
           IF(IS==IFRAME(4,J+1)) THEN
             IS=J+1
             GO TO 661
           ENDIF
         ENDDO
         WRITE(ISTDO,*)' ** ERROR WRONG FRAME NUMBER'
         WRITE(IOUT,*)' ** ERROR WRONG FRAME NUMBER'
         IERR=IERR+1
 661     CONTINUE
       ENDIF
       NNLINK(1,N)=NSL
       NNLINK(2,N)=NUSER
       NNLINK(3,N)=IC
       NNLINK(4,N)=ICR
       NNLINK(5,N)=IS
       NNLINK(6,N)=IPOL
C
       K  = K+NSL
      ENDDO
C-------------------------------------
C     Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(NOM_OPT,LNOPT1,NLINK,MESS,0,BID)
      RETURN
C
 1000 FORMAT(
     . '      RIGID LINK DEFINITIONS '/
     . '      ---------------------- '/)
 1100 FORMAT(/10X,'RIGID LINK NUMBER . . . .',I10,/,A,
     .       /10X,'DOF ( X,Y,Z, XX,YY,ZZ). .  ',3I1,2X,3I1
     .       /10X,'SKEW FRAME. . . . . . . .',I10
     .       /10X,'NUMBER OF NODES . . . . .',I10,//)
 1200 FORMAT(/10X,'POLAR RIGID LINK NUMBER .',I10,/,A,
     .       /10X,'DOF ( X,Y,Z, XX,YY,ZZ). . ',3I1,2X,3I1
     .       /10X,'POLAR FRAME . . . . . . .',I10
     .       /10X,'NUMBER OF NODES . . . . .',I10,//)
      END
