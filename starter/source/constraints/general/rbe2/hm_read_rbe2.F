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
Chd|  HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        RBE2MODIF_ND                  source/elements/solid/solide10/dim_s10edg.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        NODGRNR6                      source/starter/freform.F      
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_RBE2(IRBE2   ,LRBE2  ,ITAB    ,ITABM1  ,IGRNOD,
     .                        ISKN    ,IKINE  ,IDDLEVEL,NOM_OPT ,ITAGND,
     .                        ICDNS10 ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
#include      "r2r_c.inc"
#include      "sphcom.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*), LRBE2(*), ITAB(*),ITABM1(*),
     .        ISKN(LISKN,*),
     .        IKINE(*),IDDLEVEL,ITAGND(*),ICDNS10(*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, NSL, NSLT, ITYP, NUSER,  NM,M, NI,
     .        ISK, ISENS, INGU, IGM, J, P,IAD,NS,NN,J6(6),JJ,II,
     .        IC,IC1,IC2,IROT,ISKS,IADS,IERR1,NC,ID,IDIR,
     .        IKINE1(3*NUMNOD),IRAD,NRB,ICP,IER,NMOVE,SUB_INDEX
      INTEGER, DIMENSION(NUMNOD) :: ITAGM,ITAGIC
      CHARACTER TITR*nchartitle,KEY*ncharkey,MESS*40
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NODGRNR6
      INTEGER FMAIN(PARASIZ)
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C
      DATA MESS/'RBE2 RIGID BODY  '/
C-----------------------------------------------
C     IRBE2(1,I) : IAD0 for LRBE2
C     IRBE2(2,I) : TYPE   usr' id temporaire (print)
C     IRBE2(3,I) : INDEPENDENT NODE
C     IRBE2(4,I) : REF_DOF
C     IRBE2(5,I) : NUMBER OF DEPENDENT NODES
C     IRBE2(6,I) : m_iad if same node as several Rbe2 main (init.in engine)
C     IRBE2(7,I) : iskew
C     IRBE2(8,I) : SBE2
C     IRBE2(9,I) : hierarchy level 0-NHRBE2
C     IRBE2(10,I) : id for modif/spmd
C     IRBE2(11,I) : flag to associate REF_DOF to main node
C========================================================================|
      WRITE(IOUT,1000)
      IF (IPRI<5) WRITE(IOUT,1201)
C
      NRB = 0
C
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
      K = 0
C
      CALL HM_OPTION_START('/RBE2')
      IAD = 0
      DO I=1,NRBE2
        NRB=NRB+1
C----------Multidomaines --> on ignore les rbe3 non tages---------
        IF(NSUBDOM>0)THEN
               IF(TAGRB2(NRB)==0)CALL HM_SZ_R2R(TAGRB2,NRB,LSUBMODEL)
              END IF
C-----------------------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NUSER,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = TITR)

        NOM_OPT(1,I)=NUSER
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
        IRBE2(2,I) = NUSER
        IRBE2(10,I) = I
        CALL HM_GET_INTV('independentnode',NM,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VX',J6(1),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VY',J6(2),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VZ',J6(3),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WX',J6(4),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WY',J6(5),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WZ',J6(6),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('dependentnodeset',INGU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Iflag',IRAD,IS_AVAILABLE,LSUBMODEL)
C
        IF (ISK == 0 .and. SUB_INDEX > 0) ISK = LSUBMODEL(SUB_INDEX)%SKEW
C
        M = USR2SYS(NM,ITABM1,MESS,NUSER)
        IC1=J6(1)*4 +J6(2)*2 +J6(3)
        IC2=J6(4)*4 +J6(5)*2 +J6(6)
        IC =IC1*512+IC2*64
        IF (IC==0) IC =7*512+7*64
        IRBE2(3,I) = M
        IRBE2(4,I) = IC
        IRBE2(1,I) = IAD
        IRBE2(11,I) = IRAD
        NS = NODGRNR6(M     ,INGU  ,IGM   ,LRBE2(IAD+1),IGRNOD,
     .                ITABM1,MESS,NUSER)
        IF (NS10E > 0) THEN
C----partial dof of RBE2 will be treated correctly
C         
           IF (ITAGND(M)/=0) THEN
          CALL ANCMSG(MSGID=1211,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ITAB(M),
     .               C1='RBE2',
     .               I2=NUSER,
     .               C2='RBE2')
           END IF
        END IF
        ISKS = 0
        IF ((J6(1)+J6(2)+J6(3)+J6(4)+J6(5)+J6(6))==0) THEN
         J6(1)=1
         J6(2)=1
         J6(3)=1
         J6(4)=1
         J6(5)=1
         J6(6)=1
        ENDIF
        IF (ISK/=0) THEN
         DO JJ=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF(ISK==ISKN(4,JJ+1)) THEN
            ISKS=JJ+1
            GO TO 10
          ENDIF
         ENDDO
         CALL ANCMSG(MSGID=184,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               C1='RBE2',
     .               I1=NUSER,
     .               C2='RBE2',
     .               C3=TITR,
     .               I2=ISK)
 10      CONTINUE
        ENDIF
        IRBE2(7,I) = ISKS
C
        IF (IDDLEVEL == 0) THEN
         DO J=1,NS
          DO IDIR=1,6
          IF ( J6(IDIR) == 1)
     .     CALL KINSET(2048,ITAB(LRBE2(J+K)),IKINE(LRBE2(J+K)),IDIR,ISK,
     .                 IKINE1(LRBE2(J+K)))
          ENDDO
         ENDDO
        ENDIF
        IAD = IAD+NS
        IRBE2(5,I) = NS
       IF (IPRI>=5) THEN
        WRITE(IOUT,1100) NUSER,NM,J6,ISK,NS,IRAD
       ELSE
        WRITE(IOUT,1200) NUSER,NM,J6,ISK,NS,IRAD
       END IF
        K = K + NS
      END DO
C------treatment compatibility w/ Itetra10=2      
       IF (NS10E > 0) THEN
C------can have the same MAIN node in several RBE2      
        ITAGM(1:NUMNOD)=0
        ITAGIC(1:NUMNOD)=0
        DO I=1,NRBE2
         IAD = IRBE2(1,I)
         M = IRBE2(3,I)
         NSL = IRBE2(5,I)
         IC  = IRBE2(4,I) 
         DO J=1,NSL 
          NS =LRBE2(IAD+J)
          IF (ITAGM(NS)==0) THEN
           ITAGM(NS) = M
          ELSEIF (ITAGM(NS)/=M) THEN
C------- error-out          
          END IF 
          ITAGIC(NS) = ITAGIC(NS) + IC
         END DO
        END DO
        NMOVE = 0
        DO I=1,NRBE2
         IAD = IRBE2(1,I)
         M = IRBE2(3,I)
         NSL = IRBE2(5,I)
         NUSER = IRBE2(2,I) 
         CALL RBE2MODIF_ND(NSL,LRBE2(IAD+1),ITAGND,ICDNS10,NUSER,ITAB,
     .                     ITAGM,M,ITAGIC)
         IF (IRBE2(5,I)>NSL) THEN
          NMOVE = NMOVE+IRBE2(5,I)-NSL
          IRBE2(5,I) = NSL
         END IF
        END DO
        IF (NMOVE>0) THEN
          CALL ANCMSG(MSGID=1729,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=NMOVE)
        END IF
       END IF
C--------for decompo 
      IF (IDDLEVEL > 0) THEN
       DO I=1,NRBE2
        IAD = IRBE2(1,I)
        M = IRBE2(3,I)
        NS = IRBE2(5,I)
        IF (NSPMD > 1.AND.NS>0) THEN
          FMAIN(1:NSPMD) = 0
          DO P = 1, NSPMD
            DO J = 1, NS
              IF (NLOCAL(LRBE2(IAD+J),P)/=0)THEN
                FMAIN(P) = 1
                GO TO 85
              ENDIF
            ENDDO
 85         CONTINUE
          END DO
C  noeud main sur les procs ayant au moins 1 SECONDARY
          DO P = 1, NSPMD
            IF (FMAIN(P)==1) THEN
                CALL IFRONTPLUS(M,P)
            ENDIF
          ENDDO
        ENDIF
       END DO
      END IF !(IDDLEVEL > 0) THEN
C
      RETURN
C
 1000 FORMAT(//
     .'       RIGID ELEMENT (RBE2)   '/
     . '      ---------------------- ')
 1100 FORMAT(/10X,'NUMBER . . . . . . . . . . .',I10,/,
     .       /10X,'INDEPENDENT NODE NUMBER . . ',I10,
     .       /10X,'DOF ( X,Y,Z, XX,YY,ZZ). . . .  ',3I1,2X,3I1
     .       /10X,'SKEW NUMBER . . . . . . . . .',I10,
     .       /10X,'NUMBER OF DEPENDENT NODES. . .',I10,
     .       /10X,'FORMULATION FLAG . . . . . . ',I10,//)
 1201 FORMAT('      RBE2_ID IND._NODE REF_DOF   SKEW_ID    #SECONDARY     IFLAG'/)
 1200 FORMAT(3X,2I10,3X,3I1,1X,3I1,3I10)
      END SUBROUTINE HM_READ_RBE2
Chd|====================================================================
Chd|  HM_PREREAD_RBE2               source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        NGR2USRN                      source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_RBE2(LNUM    ,LREAL,  IGRNOD, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LNUM    ,LREAL
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IGU,IGS,NI,NN,NJ,JJ, NUSER, NUM,NRB
      CHARACTER TITR*nchartitle
      INTEGER NGR2USRN,ISK
      LOGICAL IS_AVAILABLE
C========================================================================|
      LNUM  = 0
      LREAL  = 0
      IF (NRBE2==0) RETURN

      NRB = 0

      CALL HM_OPTION_START('/RBE2')
      DO I=1,NRBE2
        NRB=NRB+1
C----------Multidomaines --> on ignore les rbe2 non tages---------
        IF(NSUBDOM>0)THEN
            IF(TAGRB2(NRB)==0)CALL HM_SZ_R2R(TAGRB2,NRB,LSUBMODEL)
        END IF
C-----------------------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NUSER,
     .                       OPTION_TITR = TITR)

        CALL HM_GET_INTV('dependentnodeset',IGU,IS_AVAILABLE,LSUBMODEL)
        IGS = NGR2USRN(IGU,IGRNOD,NGRNOD,NN)
C
        LREAL  = LREAL + NN
        LNUM  = LNUM +NRBE2L
      ENDDO
C-----------
      RETURN
      END SUBROUTINE HM_PREREAD_RBE2
Chd|====================================================================
Chd|  REORBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        INIRBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|        SETIADM                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|====================================================================
      SUBROUTINE REORBE2(IRBE2  ,LRBE2  ,NC )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*), LRBE2(*),NC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, NSL,NM, NI, NMT,M,IROT,ID,IMO,IC0,NZ,
     .        J, P,IAD,NS,NN,II,IT,IADS,IERR1,IAD1,IC,NIT,I0,I1
C
      INTEGER ITAG(NUMNOD),LCOPY(SLRBE2),ICOPY(NRBE2L,NRBE2),
     .        NHIE,IORDER(NRBE2),INDICE(NRBE2),ITAG1(NRBE2),
     .        IAD_N(NUMNOD+1)
C========================================================================|
C--------re-ordering if with hierarchy---------------------------------------
      NC=0
      DO I=1,NUMNOD
       ITAG(I)=0
      ENDDO
C--------if same node as several IND. of /RBE2---------------------------------------
      NZ=0
      DO I=1,NRBE2
       M = IRBE2(3,I)
       IF (ITAG(M)==0) THEN
        ITAG(M)=I
       ELSE
        NZ=NZ+1
        ITAG1(NZ)=I
       ENDIF
      ENDDO
      CALL SETIADM(ITAG1,NZ,IAD_N,IRBE2)
C---------  if hierarchy
      DO I=1,NRBE2
        IAD = IRBE2(1,I)
        M = IRBE2(3,I)
        NSL = IRBE2(5,I)
        DO J =1,NSL
         NS = LRBE2(IAD+J)
         IF (ITAG(NS)>I) NC=NC+1
         DO K=IAD_N(NS),IAD_N(NS+1)-1
          IF (ITAG1(K)>I) NC=NC+1
         ENDDO
        ENDDO
      ENDDO
      IF (NC==0) RETURN
C---------
      DO I=1,NRBE2
       IORDER(I) = I
       INDICE(I) = I
      ENDDO
      NC = 0
C---------   ite=
      IERR1 = 0
      NIT =5
      DO IT=1,NIT
       II = NC
      DO I=1,NRBE2
       IAD = IRBE2(1,I)
       M = IRBE2(3,I)
       NSL = IRBE2(5,I)
       DO J =1,NSL
        NS = LRBE2(IAD+J)
        IF (ITAG(NS)==0) CYCLE
        IC=INDICE(ITAG(NS))
        I1=INDICE(I)
C-----
         IF (IC>I1) THEN
          NC = NC+1
C-------exchange IORDER(IC) & IORDER(I) --
          I0 = IORDER(I1)
              IORDER(I1) = IORDER(IC)
              IORDER(IC) = I0
          IC0 = INDICE(I)
          INDICE(I) = IC
          INDICE(ITAG(NS)) = I1
              IF (IT==NIT) IERR1 = IRBE2(2,I)
         ENDIF
         DO K=IAD_N(NS),IAD_N(NS+1)-1
          IC=INDICE(ITAG1(K))
          I1=INDICE(I)
           IF (IC>I1) THEN
               NC = NC+1
C--exchange IORDER(IC) & IORDER(I) --
           I0 = IORDER(I1)
               IORDER(I1) = IORDER(IC)
               IORDER(IC) = I0
           INDICE(I) = IC
           INDICE(ITAG1(K)) = I1
           ENDIF
          ENDDO
        ENDDO
      ENDDO
       II = NC -II
       IF (II<=0) GOTO 100
      ENDDO
 100  CONTINUE
C
      IF (IERR1>0) NC=-IERR1
C----------copy---
      DO I=1,NRBE2
        IAD = IRBE2(1,I)
        M = IRBE2(3,I)
        NSL = IRBE2(5,I)
        DO J =1,NRBE2L
         ICOPY(J,I) = IRBE2(J,I)
        ENDDO
        DO J =1,NSL
         LCOPY(IAD+J) = LRBE2(IAD+J)
        ENDDO
      ENDDO
C----------reodering---
      IAD1 = 0
      DO N=1,NRBE2
        I = IORDER(N)
        IAD = ICOPY(1,I)
        M = ICOPY(3,I)
        NSL = ICOPY(5,I)
        IRBE2(1,N) = IAD1
        DO J =2,NRBE2L
         IRBE2(J,N) = ICOPY(J,I)
        ENDDO
        DO J =1,NSL
         LRBE2(IAD1+J)=LCOPY(IAD+J)
        ENDDO
        IAD1 =IAD1+NSL
      ENDDO
C
      RETURN
      END SUBROUTINE REORBE2
Chd|====================================================================
Chd|  SETIADM                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        HIERARBE2                     source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        REORBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SETIADM(IADM,NZ,IAD_N,IRBE2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*), IADM(*),NZ,IAD_N(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NM,NS,J,NSL,IAD,M
C
      INTEGER ITAG(NUMNOD),IADM_CP(NZ)
C========================================================================|
      DO I=1,NUMNOD
       ITAG(I)=0
      ENDDO
      DO J=1,NZ
        I = IADM(J)
        M = IRBE2(3,I)
        ITAG(M)=ITAG(M)+1
      ENDDO
      NM =0
      IAD_N(1)=1
      DO N=1,NUMNOD
       IF (ITAG(N)>0) THEN
        DO J=1,NZ
         I = IADM(J)
         M = IRBE2(3,I)
         IF (M==N) THEN
          NM=NM+1
          IADM_CP(NM)=I
         ENDIF
        END DO
       ENDIF
       IAD_N(N+1)=NM+1
      ENDDO
      DO J=1,NZ
       IADM(J)=IADM_CP(J)
      ENDDO
C
      RETURN
      END SUBROUTINE SETIADM
Chd|====================================================================
Chd|  HIERARBE2                     source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        INIRBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|        SETIADM                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|====================================================================
      SUBROUTINE HIERARBE2(IRBE2  ,LRBE2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*), LRBE2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, IM1,NS,J,NSL,IAD,M,IH1,K,NZ,II,IAD1
C
      INTEGER ITAG(NUMNOD),ITAG1(NRBE2),IAD_N(NUMNOD+1),
     .        LCOPY(SLRBE2),ICOPY(NRBE2L,NRBE2)
C========================================================================|
C--------defining hierarchy---------------------------------------
      DO I=1,NUMNOD
       ITAG(I)=0
      ENDDO
C--------if same node as several MAINs---------------------------------------
      NZ=0
      DO I=1,NRBE2
       M = IRBE2(3,I)
       IF (ITAG(M)==0) THEN
        ITAG(M)=I
       ELSE
            NZ=NZ+1
        ITAG1(NZ)=I
       ENDIF
      ENDDO
      CALL SETIADM(ITAG1,NZ,IAD_N,IRBE2)
C--------------------------------------------
      DO I=1,NRBE2
       IAD = IRBE2(1,I)
       M = IRBE2(3,I)
       NSL = IRBE2(5,I)
       DO J =1,NSL
        NS = LRBE2(IAD+J)
        IF (ITAG(NS)>0) THEN
         IM1=ITAG(NS)
         IH1 = IRBE2(9,IM1)+1
         IRBE2(9,I) = MAX(IRBE2(9,I),IH1)
         DO K=IAD_N(NS),IAD_N(NS+1)-1
          IM1=ITAG1(K)
          IH1 = IRBE2(9,IM1)+1
          IRBE2(9,I) = MAX(IRBE2(9,I),IH1)
         ENDDO
        ENDIF
       ENDDO
      ENDDO
      NHRBE2=0
      DO I=1,NRBE2
       NHRBE2 = MAX(NHRBE2,IRBE2(9,I))
       M = IRBE2(3,I)
      ENDDO
      IF (NHRBE2==0) RETURN
C-------reordering according hiera---
       DO I=1,NRBE2
       IAD = IRBE2(1,I)
       M = IRBE2(3,I)
       NSL = IRBE2(5,I)
        DO J =1,NRBE2L
         ICOPY(J,I) = IRBE2(J,I)
        ENDDO
        DO J =1,NSL
         LCOPY(IAD+J) = LRBE2(IAD+J)
        ENDDO
       ENDDO
C----------reodering---
      IAD1 = 0
      II = 0
      DO N=0,NHRBE2
       DO I=1,NRBE2
        IF (ICOPY(9,I)/=N) CYCLE
        II = II + 1
        IAD = ICOPY(1,I)
        M  = ICOPY(3,I)
        NSL = ICOPY(5,I)
        IRBE2(1,II) = IAD1
        DO J =2,NRBE2L
         IRBE2(J,II) = ICOPY(J,I)
            ENDDO
        DO J =1,NSL
         LRBE2(IAD1+J)=LCOPY(IAD+J)
        ENDDO
        IAD1 =IAD1+NSL
       ENDDO
      ENDDO
C
      RETURN
      END SUBROUTINE HIERARBE2
Chd|====================================================================
Chd|  INIRBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HIERARBE2                     source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        RBE2_MERGE                    source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        REORBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIRBE2(IRBE2  ,LRBE2  ,ITAB  ,X    ,MS   ,
     .                   IN     ,STIFN  ,STIFR ,TOTMAS,XGT  ,
     .                   YGT    ,ZGT   ,B1    ,B2   ,B3    ,
     .                   B5    ,B6    ,B9     ,NOM_OPT,ITAGND )
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*), LRBE2(*),ITAB(*),ITAGND(*)
      my_real
     .     X(3,*),MS(*),IN(*),STIFN(*)  ,STIFR(*),TOTMAS,
     .     B1, B2, B3, B5, B6, B9,XGT ,YGT ,ZGT 
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,N, K, NSL,M, NC,NS,ICT,ICR,IC,IAD,ID,J6(6),IRAD
C
      my_real
     .     XX, XY, XZ, YY, YZ, ZZ,IXX,IYY,IZZ,DD,MASRB,INRB,INS,INS0
      CHARACTER*nchartitle,
     .   TITR
C========================================================================|
      CALL RBE2_MERGE(IRBE2  ,LRBE2  )
      CALL REORBE2(IRBE2  ,LRBE2  ,NC )
      CALL HIERARBE2(IRBE2  ,LRBE2  )
      IF (NC<0) THEN
       ID= -NC
        CALL ANCMSG(MSGID=803,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=ID)
      ELSEIF(NHRBE2>0) THEN
        WRITE(IOUT,1200) NHRBE2
       IF (IPRI>=5) THEN
             WRITE(IOUT,1000)
        DO I=1,NRBE2
         M = ITAB(IRBE2(3,I))
         NSL = IRBE2(5,I)
         IC=IRBE2(4,I)
         ICT=IC/512
         ICR=(IC-512*(ICT))/64
         DO J =1,6
          J6(J)=0
         ENDDO
         SELECT CASE (ICT)
          CASE(1)
           J6(3)=1
          CASE(2)
           J6(2)=1
          CASE(3)
           J6(2)=1
           J6(3)=1
          CASE(4)
           J6(1)=1
          CASE(5)
           J6(1)=1
           J6(3)=1
          CASE(6)
           J6(1)=1
           J6(2)=1
          CASE(7)
           J6(1)=1
           J6(2)=1
           J6(3)=1
         END SELECT
         SELECT CASE (ICR)
          CASE(1)
           J6(6)=1
          CASE(2)
           J6(5)=1
          CASE(3)
           J6(5)=1
           J6(6)=1
          CASE(4)
           J6(4)=1
          CASE(5)
           J6(4)=1
           J6(6)=1
          CASE(6)
           J6(4)=1
           J6(5)=1
          CASE(7)
           J6(4)=1
           J6(5)=1
           J6(6)=1
         END SELECT
         WRITE(IOUT,1100) IRBE2(2,I),IRBE2(9,I),M,J6,IRBE2(7,I),NSL
         ENDDO
       END IF
      ENDIF
C
      INRB=ZERO
      DO I=1,NRBE2
       IAD = IRBE2(1,I)
       NOM_OPT(1,I) = IRBE2(2,I)
       M = IRBE2(3,I)
       NSL = IRBE2(5,I)
       IC=IRBE2(4,I)
       IRAD = IRBE2(11,I)
       ICT=IC/512
       IF (ICT>0) THEN
        IF (NS10E>0) THEN
         DO J =1,NSL
          NS = LRBE2(IAD+J)
          IF (ITAGND(NS)/=0) CYCLE
          MS(M) = MS(M)+MS(NS)
          STIFN(M)= STIFN(M)+STIFN(NS)
         ENDDO
        ELSE
        DO J =1,NSL
         NS = LRBE2(IAD+J)
         MS(M) = MS(M)+MS(NS)
         STIFN(M)= STIFN(M)+STIFN(NS)
        ENDDO
        END IF !(NS10E>0) THEN
       ENDIF
       ICR=(IC-512*(ICT))/64
       IF (IRODDL==0) ICR =0
        IF (ICR>0.OR.IRAD==0) THEN
         IF (ICR>0) THEN
          DO J =1,NSL
           NS = LRBE2(IAD+J)
           IN(M) = IN(M)+IN(NS)
           STIFR(M)= STIFR(M)+STIFR(NS)
          ENDDO
         END IF
         IF (ICT>0) THEN
          IF (NS10E>0) THEN
           DO J =1,NSL
            NS = LRBE2(IAD+J)
            IF (ITAGND(NS)/=0) CYCLE
            XX=(X(1,NS)-X(1,M))*(X(1,NS)-X(1,M))
            YY=(X(2,NS)-X(2,M))*(X(2,NS)-X(2,M))
            ZZ=(X(3,NS)-X(3,M))*(X(3,NS)-X(3,M))
            IXX=YY+ZZ
            IYY=ZZ+XX
            IZZ=XX+YY
            INS = (IXX+IYY+IZZ)*MS(NS)
            IN(M) = IN(M)+ INS
            IF (ICT==7) INRB = INRB+INS
            DD = XX+YY+ZZ
            STIFR(M)= STIFR(M)+DD*STIFN(NS)
           ENDDO
          ELSE
          DO J =1,NSL
           NS = LRBE2(IAD+J)
           XX=(X(1,NS)-X(1,M))*(X(1,NS)-X(1,M))
           YY=(X(2,NS)-X(2,M))*(X(2,NS)-X(2,M))
           ZZ=(X(3,NS)-X(3,M))*(X(3,NS)-X(3,M))
           IXX=YY+ZZ
           IYY=ZZ+XX
           IZZ=XX+YY
           INS = (IXX+IYY+IZZ)*MS(NS)
           IN(M) = IN(M)+ INS
           IF (ICT==7) INRB = INRB+INS
           DD = XX+YY+ZZ
           STIFR(M)= STIFR(M)+DD*STIFN(NS)
          ENDDO
          END IF !(NS10E>0) THEN
         ENDIF
        ENDIF
      ENDDO
C-----Correction -only for the case 111---
      MASRB=ZERO
      DO I=1,NRBE2
       IAD = IRBE2(1,I)
       NSL = IRBE2(5,I)
       IC=IRBE2(4,I)
       ICT=IC/512
       IF (ICT==7) THEN
        DO J =1,NSL
         NS = LRBE2(IAD+J)
         STIFN(NS)= EM20
         MASRB = MASRB+MS(NS)
         XX=(X(1,NS))*(X(1,NS))
         XY=(X(1,NS))*(X(2,NS))
         XZ=(X(1,NS))*(X(3,NS))
         YY=(X(2,NS))*(X(2,NS))
         YZ=(X(2,NS))*(X(3,NS))
         ZZ=(X(3,NS))*(X(3,NS))
         B1 = B1 -(YY+ZZ)*MS(NS)
         B2 = B2 + XY*MS(NS)
         B3 = B3 + XZ*MS(NS)
         B5 = B5 -(ZZ+XX)*MS(NS)
         B6 = B6 + YZ*MS(NS)
         B9 = B9 - (XX+YY)*MS(NS)
         XGT = XGT - MS(NS)*X(1,NS)
         YGT = YGT - MS(NS)*X(2,NS)
         ZGT = ZGT - MS(NS)*X(3,NS)
        ENDDO
       ENDIF
        ICR=(IC-512*(ICT))/64
        IF (ICR==7.AND.IRODDL>0) THEN
         DO J =1,NSL
          NS = LRBE2(IAD+J)
          STIFR(NS)= EM20
          INRB=INRB+IN(NS)
          B1 = B1 -IN(NS)
          B5 = B5 -IN(NS)
          B9 = B9 -IN(NS)
         ENDDO
        ENDIF
      ENDDO
      TOTMAS = TOTMAS - MASRB
C------INRB will not be taken into account due to solide elements as dependent nodes but defined as 111 111      
C
      DO I=1,NRBE2
         ID=NOM_OPT(1,I)
         CALL FRETITL2(TITR,
     .                 NOM_OPT(LNOPT1-LTITR+1,I),LTITR)

        M = IRBE2(3,I)
        IF(MS(M)<=1.0E-25) THEN
           CALL ANCMSG(MSGID=804,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
           RETURN
        ENDIF
       IF (IPRI>=3) THEN
        WRITE(IOUT,1300)
        IF (IRODDL==0) THEN
         WRITE(IOUT,1600) IRBE2(2,I),ITAB(IRBE2(3,I)),MS(M)
        ELSE
         WRITE(IOUT,1400) IRBE2(2,I),ITAB(IRBE2(3,I)),MS(M),IN(M)
        END IF
       ENDIF
      ENDDO
C
      RETURN
 1000 FORMAT(//
     .'     RIGID ELEMENT(RBE2) WITH HIERARCHY LEVEL AFTER REORDERING:'/
     . '    --------------------------------------------------------- ')
 1100 FORMAT(/10X,'NUMBER . . . . . . . . . . .',I10,/,
     .       /10X,'HIERARCHY LEVEL.  . . . . . ',I10,
     .       /10X,'INDEPENDENT NODE NUMBER. . .',I10,
     .       /10X,'DOF ( X,Y,Z, XX,YY,ZZ). . . .  ',3I1,2X,3I1
     .       /10X,'SKEW NUMBER (LOCAL) . . . . .',I10,
     .       /10X,'NUMBER OF DEPENDENT NODES . .',I10,//)
 1200 FORMAT(/10X,'RBE2 HIERARCHY LEVEL . . . . =',I5,2X,//)
 1300 FORMAT(//
     .'     RIGID ELEMENT(RBE2) INDEPENDENT NODE MASSES AND INERTIA (NEW):'/
     . '    --------------------------------------------------------- ')
 1400 FORMAT(/10X,'NUMBER . . . . . . . . . . .',I10,/,
     .       /10X,'INDEPENDENT NODE NUMBER. . .',I10,
     .       /10X,'NEW MASS. . . . . . . . . . .',1PG20.13,
     .       /10X,'NEW SPHERIC INERTIA. . . . . ',1PG20.13,//)
 1600 FORMAT(/10X,'NUMBER . . . . . . . . . . .',I10,/,
     .       /10X,'INDEPENDENT NODE NUMBER. . .',I10,
     .       /10X,'NEW MASS. . . . . . . . . . .',1PG20.13,//)
      END SUBROUTINE INIRBE2
Chd|====================================================================
Chd|  CONTRBE2                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CONTRBE2(ICR,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, NSL, NSLT, ITYP, NUSER,  NM,M, NI,
     .        ISK, ISENS, INGU, IGM, J, P,IAD,NS,NN,J6(6),JJ,II,
     .        IC,IC1,IC2,IROT,ISKS,IADS,IERR1,NC,ID,IRAD
      CHARACTER TITR*nchartitle,KEY*ncharkey
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
C=====================================================================|
C
C-----initialise NHRBE2--au cas no rbe2---add new option-----------
      NHRBE2 = 0
      ICR =0
      CALL HM_OPTION_START('/RBE2')
      DO I=1,NRBE2
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NUSER,
     .                       OPTION_TITR = TITR)
C
        CALL HM_GET_INTV('independentnode',NM,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VX',J6(1),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VY',J6(2),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VZ',J6(3),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WX',J6(4),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WY',J6(5),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WZ',J6(6),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('dependentnodeset',INGU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Iflag',IRAD,IS_AVAILABLE,LSUBMODEL)
C
        IF ((J6(1)+J6(2)+J6(3)+J6(4)+J6(5)+J6(6))==0) THEN
         J6(4)=1
         J6(5)=1
         J6(6)=1
        ENDIF
       ICR = J6(4) + J6(5) + J6(6)
       IF (IRAD == 0) ICR = 1
       IF (ICR >0) RETURN
      ENDDO
C
      RETURN
C
      END SUBROUTINE CONTRBE2
Chd|====================================================================
Chd|  SETELOFF2                     source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SETELOFF2(IXS    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                     IXTG   ,IPARG  ,ISOLOFF,ISHEOFF,
     3                     ITRUOFF,IPOUOFF,IRESOFF,ITRIOFF,IGRNRB2,
     4                     IGRNOD ,IRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-------------------------------------
C    PRE LECTURE STRUCTURE RIGIDES POUR OPTIMIZATION
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISOLOFF(*), ISHEOFF(*), ITRIOFF(*),ITRUOFF(*),
     .        IPOUOFF(*), IRESOFF(*),
     .        IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*),
     .        IPARG(NPARG,*),IGRNRB2(*),
     .        IRBE2(NRBE2L,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, MLW, ITY, NEL, NFT, IAD, I, II, IGOF, NR, IG,
     .        NSN, NALL, ISHFT, IOK, IRBYON,M,IC,IC0,
     .        ITAG(NUMNOD)
C-----------------------
C     MISE DE OFF A -OFF
C======================================================================|
      IF (NRBE2==0) RETURN
      IF(IPRI>=5) THEN
        WRITE(IOUT,*)' '

        WRITE(IOUT,*)' LIST OF DEACTIVATED ELEMENTS FROM RBE2'
        WRITE(IOUT,*)' ----------------------------------------------'
      END IF
C
      IRBYON = 20
      IC0 = 7*512+7*64
C
      DO NR = 1, NRBE2
        IG = IGRNRB2(NR)
        M  = IRBE2(3,NR)
        IC = IRBE2(4,NR)
        IF(IG>0.AND.IC==IC0)THEN
          NSN = IGRNOD(IG)%NENTITY
          DO I=1,NUMNOD
            ITAG(I)=0
          ENDDO
          ITAG(M)=1
          DO I=1,NSN
            ITAG(IGRNOD(IG)%ENTITY(I))=1
          END DO
C
          DO II = 1, NUMELT
            NALL = ITAG(IXT(2,II)) * ITAG(IXT(3,II))
            IF(NALL/=0)THEN
              ITRUOFF(II) = IRBYON
            END IF
          END DO
C
          DO II = 1, NUMELP
            NALL = ITAG(IXP(2,II)) * ITAG(IXP(3,II))
            IF(NALL/=0)THEN
              IPOUOFF(II) = IRBYON
            END IF
          END DO
C
          DO II = 1, NUMELR
            NALL = ITAG(IXR(2,II)) * ITAG(IXR(3,II))
            IF(NALL/=0)THEN
              IRESOFF(II) = IRBYON
            END IF
          END DO
        END IF
C
      END DO
C  -----DEACTIVATED ELEMENTS will done in SETELOFF------
      RETURN
      END SUBROUTINE SETELOFF2
Chd|====================================================================
Chd|  SETRB2ON                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NGR2USRN                      source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SETRB2ON(IXS    ,IXC    ,IXTG   ,IGRNOD ,
     2                    IGRNRB2,ISOLOFF,ISHEOFF,ITRIOFF,ITABM1,
     3                    LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-------------------------------------
C    PRE LECTURE STRUCTURE RIGIDES POUR OPTIMIZATION
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGRNRB2(*),ISOLOFF(*),ISHEOFF(*),ITRIOFF(*),
     .        IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*),ITABM1(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NR, IDUM, I, L, ISENS, IGS,NSN,IAD,II,NALL,
     .        IGU,ID,ILAGM,ISU,UID,IRBYON,
     .        ITAG(NUMNOD),NN,NM, NUSER, NUM,M,J6(6),IC
      CHARACTER TITR*nchartitle,MESS*40
      INTEGER NGR2USRN
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NODGRNR,NGR2USR
C-----------------------------------
      IF (NRBE2==0) RETURN
C
      CALL HM_OPTION_START('/RBE2')
      IRBYON = 20
      DO NR=1,NRBE2
        IGRNRB2(NR)=0
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NUSER,
     .                       OPTION_TITR = TITR)
C
        CALL HM_GET_INTV('independentnode',NM,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VX',J6(1),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VY',J6(2),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VZ',J6(3),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WX',J6(4),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WY',J6(5),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WZ',J6(6),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('dependentnodeset',IGU,IS_AVAILABLE,LSUBMODEL)
C
        M = USR2SYS(NM,ITABM1,MESS,NUSER)
        IGS = NGR2USRN(IGU,IGRNOD,NGRNOD,NN)
        IC= J6(1)+J6(2)+J6(3)+J6(4)+J6(5)+J6(6)
C
        IF(IGS/=0.AND.(IC==0.OR.IC==6)) THEN
C
          DO I=1,NUMNOD
           ITAG(I)=0
          ENDDO
          IGRNRB2(NR)=IGS
          NSN = IGRNOD(IGS)%NENTITY
          ITAG(M)=1
          DO I=1,NSN
            ITAG(IGRNOD(IGS)%ENTITY(I))=1
          END DO
C
          DO II = 1, NUMELC
            NALL = ITAG(IXC(2,II)) * ITAG(IXC(3,II)) *
     +             ITAG(IXC(4,II)) * ITAG(IXC(5,II))
            IF(NALL/=0)THEN
              ISHEOFF(II) = IRBYON
            END IF
          END DO
C
          DO II = 1, NUMELTG
            NALL = ITAG(IXTG(2,II)) * ITAG(IXTG(3,II)) *
     +             ITAG(IXTG(4,II))
            IF(NALL/=0)THEN
              ITRIOFF(II) = IRBYON
            END IF
          END DO
        END IF
C
      END DO
C------------solid elements
C
      CALL HM_OPTION_START('/RBE2')
      DO NR=1,NRBE2
        IGRNRB2(NR)=0
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NUSER,
     .                       OPTION_TITR = TITR)
C
        CALL HM_GET_INTV('independentnode',NM,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VX',J6(1),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VY',J6(2),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('VZ',J6(3),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WX',J6(4),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WY',J6(5),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('WZ',J6(6),IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('dependentnodeset',IGU,IS_AVAILABLE,LSUBMODEL)
C
        M = USR2SYS(NM,ITABM1,MESS,NUSER)
        IGS = NGR2USRN(IGU,IGRNOD,NGRNOD,NN)
        IC= J6(1)+J6(2)+J6(3)
C
        IF(IGS/=0.AND.(IC==0.OR.IC==3)) THEN
C
          DO I=1,NUMNOD
           ITAG(I)=0
          ENDDO
          IGRNRB2(NR)=IGS
          NSN = IGRNOD(IGS)%NENTITY
          ITAG(M)=1
          DO I=1,NSN
            ITAG(IGRNOD(IGS)%ENTITY(I))=1
          END DO
C
          DO II = 1, NUMELS
            NALL = ITAG(IXS(2,II)) * ITAG(IXS(3,II)) *
     +             ITAG(IXS(4,II)) * ITAG(IXS(5,II)) *
     +             ITAG(IXS(6,II)) * ITAG(IXS(7,II)) *
     +             ITAG(IXS(8,II)) * ITAG(IXS(9,II))
            IF(NALL/=0)THEN
              ISOLOFF(II) = IRBYON
            END IF
          END DO
        END IF
C
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  RBE2_MERGE                    source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        INIRBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|        IC_MRG                        source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        SAME_NSN                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|====================================================================
      SUBROUTINE RBE2_MERGE(IRBE2  ,LRBE2  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*), LRBE2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, NSL,NM, NI, NMT,M,IROT,ID,IMO,IC0,NZ,NSLN,NSLJ,
     .        J, P,IAD,NS,NN,II,IT,IADS,IERR1,IAD1,IC,MJ,ICJ,NSJ,IADJ
C
      INTEGER ITAG(NUMNOD),ISAME
C========================================================================|
C--------merging RBE2 w/ the same NS/M (separated IC)--------------------
      ITAG(1:NUMNOD)=0
C--------if same node as several IND. of /RBE2------------
      NZ=0
      DO I=1,NRBE2
        IAD = IRBE2(1,I)
        NSL = IRBE2(5,I)
        DO J =1,NSL
         NS = LRBE2(IAD+J)
         IF (ITAG(NS)>0) NZ = NZ + 1
         ITAG(NS)=ITAG(NS)+1
        ENDDO
      ENDDO
      IF (NZ==0) RETURN
C --------merge if same all excepting IC     
      DO I=1,NRBE2
        IAD = IRBE2(1,I)
        NSL = IRBE2(5,I)
        M   = IRBE2(3,I)
        IF (NSL==0) CYCLE
        DO II=I+1,NRBE2
          IADJ = IRBE2(1,II)
          NSLJ = IRBE2(5,II)
          MJ = IRBE2(3,II)
          IF (MJ/=M.OR.NSLJ/=NSL) CYCLE
          CALL SAME_NSN(NSL,LRBE2(IAD+1),LRBE2(IADJ+1),ITAG,ISAME)
          IF (ISAME==1) THEN
           CALL IC_MRG(IC,IRBE2(4,I),IRBE2(4,II))
           IRBE2(4,I) = IC
           IRBE2(5,II) = 0
          END IF
        ENDDO
      ENDDO
C
      RETURN
      END SUBROUTINE RBE2_MERGE
Chd|====================================================================
Chd|  SAME_NSN                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        RBE2_MERGE                    source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SAME_NSN(NSL,LRBE2_1  ,LRBE2_2,ITAG,ISAME)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,LRBE2_1(*)  ,LRBE2_2(*),ITAG(*),ISAME
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NS1,NS2
C========================================================================|
      ISAME=1
      DO I=1,NSL
       NS1=LRBE2_1(I)
       NS2=LRBE2_2(I)
       IF (NS1/=NS2.OR.ITAG(NS1)/=ITAG(NS2).OR.ITAG(NS1)<=1) THEN
        ISAME=0 
        CYCLE
       END IF
      ENDDO
C
      RETURN
      END SUBROUTINE SAME_NSN
Chd|====================================================================
Chd|  IC_MRG                        source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        RBE2_MERGE                    source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|        ICT2JT                        source/constraints/general/rbe2/hm_read_rbe2.F
Chd|====================================================================
      SUBROUTINE IC_MRG(IC_N,IC1 ,IC2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IC_N,IC1 ,IC2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ICT,ICR,JT1(3),JR1(3),JT2(3),JR2(3),IUN
C========================================================================|
        ICT=IC1/512
        ICR=(IC1-512*(ICT))/64
        CALL ICT2JT(ICT,JT1)
        CALL ICT2JT(ICR,JR1)
        ICT=IC2/512
        ICR=(IC2-512*(ICT))/64
        CALL ICT2JT(ICT,JT2)
        CALL ICT2JT(ICR,JR2)
        IUN=1
        DO I =1,3
         JT1(I) = JT1(I)+JT2(I)
         JR1(I) = JR1(I)+JR2(I)
         JT1(I) = MIN(IUN,JT1(I))
         JR1(I) = MIN(IUN,JR1(I))
        END DO
        ICT=JT1(1)*4 +JT1(2)*2 +JT1(3)
        ICR=JR1(1)*4 +JR1(2)*2 +JR1(3)
        IC_N =ICT*512+ICR*64
C
      RETURN
      END SUBROUTINE IC_MRG
Chd|====================================================================
Chd|  ICT2JT                        source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- called by -----------
Chd|        IC_MRG                        source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ICT2JT(ICT,JT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICT,JT(3) 
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C======================================================================|
C        ICT=IC/512
C        ICR=(IC-512*(ICT))/64
        JT(1:3)= 0
        SELECT CASE (ICT)
          CASE(1)
           JT(3)=1
          CASE(2)
           JT(2)=1
          CASE(3)
           JT(2)=1
           JT(3)=1
          CASE(4)
           JT(1)=1
          CASE(5)
           JT(1)=1
           JT(3)=1
          CASE(6)
           JT(1)=1
           JT(2)=1
          CASE(7)
           JT(1)=1
           JT(2)=1
           JT(3)=1
       END SELECT
C---
      RETURN
      END SUBROUTINE ICT2JT

