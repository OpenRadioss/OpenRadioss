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
Chd|  DIM_S10EDG                    source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIM_S10EDG(NEDG, IXS10 ,IPARG ,ITAGND)
C=======================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEDG,ITAGND(*)
      INTEGER IXS10(6,*),IPARG(NPARG,NGROUP)
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,II, NF2, N, ITY,ISOLNOD,ISROT
C     REAL
C-----------------------------------------------
      NEDG = 0
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       ITY=IPARG(5,NG)
       ISOLNOD = IPARG(28,NG)
       ISROT  = IPARG(41,NG)
       IF(ISOLNOD == 10) ISROT  = IPARG(74,NG)
       IF(ITY == 1.AND.ISOLNOD == 10.AND.ISROT == 2)THEN
        NF2  = NFT-NUMELS8
C
        DO I=1,NEL
	 II = I+NF2
         DO J = 1 , 6
 	  N = IXS10(J,II)
 	 IF (N >0) THEN
	  IF (ITAGND(N)==0) THEN
	   NEDG = NEDG + 1
	   ITAGND(N)=NEDG
	  END IF
	 END IF
         ENDDO
        ENDDO
       END IF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IND_S10EDG                    source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IND_S10EDG(ICNDS10, IXS, IXS10 ,IPARG,ITAGND)
C=======================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAGND(*)
      INTEGER ICNDS10(3,*),IXS(NIXS,*),IXS10(6,*),IPARG(NPARG,*)
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NG, NEL, NFT,NF1, NF2, N, ITY,ISOLNOD,ISROT,NC(4)
      INTEGER IPERM1(6),IPERM2(6),N1,N2,NEDG,ND,II,JJ
C     REAL
      DATA IPERM1/1,2,3,1,2,3/
      DATA IPERM2/2,3,1,4,4,4/
C-----------------------------------------------
      NEDG = 0
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       ITY=IPARG(5,NG)
       ISOLNOD = IPARG(28,NG)
       ISROT  = IPARG(41,NG)
       IF(ISOLNOD == 10) ISROT  = IPARG(74,NG)
       IF(ITY == 1.AND.ISOLNOD == 10.AND.ISROT == 2)THEN
        NF1  = NFT
        NF2  = NFT-NUMELS8
C
        DO I=1,NEL
          II = I + NF1
          JJ = I + NF2
          NC(1) =IXS(2,II)
          NC(2) =IXS(4,II)
          NC(3) =IXS(7,II)
          NC(4) =IXS(6,II)
         DO J = 1 , 6
 	  N = IXS10(J,JJ)
 	  IF (N >0) THEN
	   IF (ITAGND(N)==0) THEN
	    NEDG = NEDG + 1
            ITAGND(N) = NEDG
            N1=IPERM1(J)
            N2=IPERM2(J)
            ICNDS10(1,NEDG) = N
            ICNDS10(2,NEDG) = NC(N1)
            ICNDS10(3,NEDG) = NC(N2)
	   END IF
          END IF
         END DO
        END DO
       END IF
      ENDDO
      IF (NEDG/=NS10E) THEN
       print *,'error!!! NEDG,NS10EDG=',NEDG,NS10E
      END IF

C ----- ITAGND : > 0 < NS10E : Id of ICNDS10 (Nd)
C                < 0           to be degenerated      
C                > NS10E : to be tagged first and be degenerated after      
C
      RETURN
      END
Chd|====================================================================
Chd|  REORD_ICND                    source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORD_ICND(ICNDS10, ITAGND)
C=======================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICNDS10(3,*), ITAGND(*)
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ICND_CP(3,NS10E),IE,N
C     REAL
C------reordering for P/ON----------------------------
      ICND_CP(1:3,1:NS10E)=ICNDS10(1:3,1:NS10E)
C
      IE = 0
      DO N= 1,NUMNOD
       IF (ITAGND(N)>0) THEN
        J = ITAGND(N)
        IE = IE + 1
	ICNDS10(1:3,IE)=ICND_CP(1:3,J)
	ITAGND(N) = IE
       END IF
      END DO
      IF (IE /= NS10E) print *,'Error of re-ordering in REORD_ICND',IE,NS10E
C
      RETURN
      END
Chd|====================================================================
Chd|  REMOVE_ND                     source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REMOVE_ND(NN, INN, ITAGND)
C=======================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, INN(*), ITAGND(*)
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,NF1, NF2, N,ND
C     REAL
C------remove tagged nodes in INN(*)----------------------------
      ND = 0
      DO I=1,NN
       N = INN(I)
       IF (ITAGND(N) ==0 ) THEN
        ND = ND + 1
	INN(ND) =INN(I)
       END IF
      ENDDO
C      
      NN = ND
C
      RETURN
      END
Chd|====================================================================
Chd|  REMDEG_ND                     source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REMDEG_ND(NN, INN, ITAGND)
C=======================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, INN(*), ITAGND(*)
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,NF1, NF2, N,ND
C     REAL
C------remove tagged nodes in INN(*) and will be degenerated-----------
      ND = 0
      DO I=1,NN
       N = INN(I)
       IF (ITAGND(N) ==0 ) THEN
        ND = ND + 1
	INN(ND) =INN(I)
       ELSEIF (ITAGND(N) >0 ) THEN
        ITAGND(N) = -ITAGND(N)
       END IF
      ENDDO
C
      NN = ND
C
      RETURN
      END
Chd|====================================================================
Chd|  RIGMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RIGMODIF_ND(NN, INN, ITAGND,ICNDS10,IU,TITR,ITAB)
C=======================================================================
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, INN(*), ITAGND(*),ICNDS10(3,*),IU,ITAB(*)
      CHARACTER TITR*(*)
C     REAL
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,NF1, NF2, N,ND,N1,N2,NNEW,ID,IER1,IER2
      LOGICAL IS1,IS2
C     REAL
C------treatment for tagged nodes in INN(*) first passe-
C----- IER >0 warning; <0 error out 
C--- 1: (1,nd,2) are in Rbody, nd will be removed from Rbody
C--- 2: (1,nd) or (2,nd) are in Rbody, nd will be removed from Rbody
C--- -1: nd alone is in Rbody
      NNEW = 0
      IER1 = 0
      IER2 = 0
      DO I=1,NN
       N = INN(I)
       IF (ITAGND(N) /=0 ) THEN
        ID = IABS(ITAGND(N))
        ND = ICNDS10(1,ID)
        N1 = ICNDS10(2,ID)
        N2 = ICNDS10(3,ID)
        IS1 = INTAB(NN,INN,N1)
        IS2 = INTAB(NN,INN,N2)
        IF (IS1.AND.IS2) THEN
C----removed from INN and degenerating in 2nd passe----------
         ITAGND(N) = ITAGND(N) + NS10E	
         NNEW = NNEW + 1
         INN(NNEW) =INN(I)
         IER1 =1
         IF (IPRI>=5)
     .    CALL ANCMSG(MSGID=1213,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RIGID BODY ',
     .                I1=ITAB(ND),
     .                PRMOD=MSG_CUMU)
        ELSEIF (.NOT.(IS1).AND..NOT.(IS2)) THEN
C----error out	ND is along in RB
          CALL ANCMSG(MSGID=1216,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ND),
     .                C1='RIGID BODY ',
     .                I2=IU,
     .                C2='RIGID BODY ')
        ELSE
C----removed from INN directely----------
         IER2 =1
         IF (IPRI>=5)
     .    CALL ANCMSG(MSGID=1210,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RIGID BODY ',
     .                I1=ITAB(ND),
     .                PRMOD=MSG_CUMU)
        END IF
       ELSE
        NNEW = NNEW + 1
        INN(NNEW) =INN(I)
       END IF
      ENDDO
C      
      NN = NNEW
       IF (IER1 >0.AND.IPRI>=5) THEN
          CALL ANCMSG(MSGID=1213,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RIGID BODY ',
     .                C2='RIGID BODY ',
     .                I1=IU,
     .                PRMOD=MSG_PRINT)
       END IF
       IF (IER2 >0.AND.IPRI>=5) THEN
          CALL ANCMSG(MSGID=1210,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RIGID BODY ',
     .                C2='RIGID BODY ',
     .                I1=IU,
     .                PRMOD=MSG_PRINT)
       END IF
      RETURN
      END
Chd|====================================================================
Chd|  RIGMODIF1_ND                  source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RIGMODIF1_ND(NPBY,LPBY,ITAGND)
C=======================================================================
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*), LPBY(*) ,ITAGND(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,KK,NSL,NSL_N,K_N,NN
C     REAL
C------removing Nd from LPBY(*)--degenerating--
      K = 0
      K_N = 0
      DO N = 1, NRBYKIN
        NSL=NPBY(2,N)
        NSL_N = 0
        DO KK = 1, NSL
          NN = LPBY(K+KK)
          IF(ITAGND(NN)>NS10E)THEN
	   ITAGND(NN) = -(ITAGND(NN)-NS10E)
          ELSE
           NSL_N = NSL_N + 1
	   LPBY(K_N+NSL_N) =NN
          END IF
        ENDDO
        K = K + NSL
        K_N = K_N + NSL_N
	NPBY(2,N) = NSL_N
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2MODIF_ND                  source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RBE2MODIF_ND(NN, INN, ITAGND,ICNDS10,IU,ITAB,ITAGM,M,ITAGIC)
C=======================================================================
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, INN(*), ITAGND(*),ICNDS10(3,*),ITAGM(*),IU,ITAB(*),
     .        M,ITAGIC(*)
C     REAL
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,NF1, NF2, N,ND,N1,N2,NNEW,IER1,ID,IER2,M_1,M_2
      LOGICAL IS1,IS2
C     REAL
C----- IER >0 warning; <0 error out 
C--- 1: (1,nd,2) are in RBE2, nd will be removed from RBE2
C--- 2: (1,nd) or (2,nd) are in RBE2, nd will be removed from RBE2
C--- -1: nd alone is in RBE2
C--- -2: RBE2 has partial dof
C------treatment for tagged nodes in INN(*)---ICPAT=1: partial dof------------
      NNEW = 0
      IER1 = 0
      IER2 = 0
C----Allow only +2 Hierarchy levers
      M_1= ITAGM(M)
      IF (M_1==0) THEN
       M_2=0
      ELSE
       M_2= ITAGM(M_1)
      END IF
      DO I=1,NN
       N = INN(I)
C---- ITAGND(N) > NS10E  due the same N defined in sereval RBE2 (per dof)   
       IF (ITAGND(N) > NS10E) THEN
C---- removed in the 2nd passe
        NNEW = NNEW + 1
        INN(NNEW) =INN(I)
       ELSEIF (ITAGND(N) /=0 ) THEN
        ID = IABS(ITAGND(N))
        ND = ICNDS10(1,ID)
        N1 = ICNDS10(2,ID)
        N2 = ICNDS10(3,ID)
        IF (N1==M.OR.N1==M_1.OR.N1==M_2) THEN
         IS1 = .TRUE.
        ELSEIF (ITAGM(N1)==0) THEN
         IS1 = .FALSE.
        ELSEIF (ITAGM(N1)==M.OR.ITAGM(N1)==M_1.OR.ITAGM(N1)==M_2) THEN
         IF (ITAGIC(N1)==ITAGIC(ND)) THEN
          IS1 = .TRUE.
         ELSE
          IS1 = .FALSE.
         END IF
        ELSE
         IS1 = .FALSE.
        END IF
C        
        IF (N2==M.OR.N2==M_1.OR.N2==M_2) THEN
         IS2 = .TRUE.
        ELSEIF (ITAGM(N2)==0) THEN
         IS2 = .FALSE.
        ELSEIF (ITAGM(N2)==M.OR.ITAGM(N2)==M_1.OR.ITAGM(N2)==M_2) THEN
         IF (ITAGIC(N2)==ITAGIC(ND)) THEN
          IS2 = .TRUE.
         ELSE
C------!!!!add detail for message         
          IS2 = .FALSE.
         END IF
        ELSE
         IS2 = .FALSE.
        END IF
        IF (IS1.AND.IS2) THEN
C----degenerating in 2nd passe------ and removed from RBE2 in 2nd passe
         NNEW = NNEW + 1
         INN(NNEW) =INN(I)
         ITAGND(N) = ITAGND(N) + NS10E	
         IER1 = 1
         IF (IPRI>=5)
     .    CALL ANCMSG(MSGID=1213,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ND),
     .                C1='RBE2 ',
     .                PRMOD=MSG_CUMU)
        ELSEIF (.NOT.(IS1).AND..NOT.(IS2)) THEN
C----error out	ND is along in RBE2
          CALL ANCMSG(MSGID=1216,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ND),
     .                C1='RBE2 ',
     .                I2=IU,
     .                C2='RBE2 ')
        ELSE
C----remove Nd from RBE2	directely
         IER2 = 1
         IF (IPRI>=5)
     .     CALL ANCMSG(MSGID=1210,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RBE2 ',
     .                I1=ITAB(ND),
     .                PRMOD=MSG_CUMU)
	END IF
       ELSE
        NNEW = NNEW + 1
        INN(NNEW) =INN(I)
       END IF !IF (ITAGND(N) /=0 )
      ENDDO
C      
      NN = NNEW
       IF (IER1 >0.AND.IPRI>=5) THEN
          CALL ANCMSG(MSGID=1213,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RBE2 ',
     .                C2='RBE2 ',
     .                I1=IU,
     .                PRMOD=MSG_PRINT)
       END IF
       IF (IER2 >0.AND.IPRI>=5) THEN
          CALL ANCMSG(MSGID=1210,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='RBE2 ',
     .                C2='RBE2 ',
     .                I1=IU,
     .                PRMOD=MSG_PRINT)
       END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2MODIF1_ND                 source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RBE2MODIF1_ND(IRBE2,LRBE2,ITAGND)
C=======================================================================
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*), LRBE2(*), ITAGND(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,KK,NSL,NSL_N,K_N,NS
      INTEGER ITAG(NUMNOD)
C     REAL
C-----
      IF (NRBE2==0) RETURN
      ITAG(1:NUMNOD) = ITAGND(1:NUMNOD)
      DO I = 1, NRBE2
        NSL = IRBE2(5,I)
        K = IRBE2(1,I)
        NSL_N = 0
        DO J = 1, NSL
         NS = LRBE2(K+J)
         IF(ITAG(NS)>NS10E)THEN
          IF(ITAGND(NS)>NS10E) ITAGND(NS) = -(ITAGND(NS)-NS10E)
         ELSE
          NSL_N = NSL_N + 1
          LRBE2(K+NSL_N) =NS
         END IF
        ENDDO
        IRBE2(5,I) = NSL_N
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  BCSMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE BCSMODIF_ND(ICODE, ITAGND,ICNDS10,ITAB)
C=======================================================================
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODE(*), ITAGND(*),ICNDS10(3,*),ITAB(*)
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,NF1, NF2, N,ND,N1,N2,ID,IPR
      INTEGER IS1,IS2
C     REAL
C------treatment for /BCS ----------------------------
      IPR = 0
      DO N=1,NUMNOD
       IF (ICODE(N)>0 .AND. ITAGND(N) /=0 ) THEN
        ID = IABS(ITAGND(N))
	ND = ICNDS10(1,ID)
	N1 = ICNDS10(2,ID)
	N2 = ICNDS10(3,ID)
	IS1 = ICODE(N1)
	IS2 = ICODE(N2)
c	IF (IS1==IS2.AND.ICODE(N)==IS1) THEN
C----remove Nd from ICNDS10
c         ITAGND(N) = ITAGND(N) + NS10E	
c-----no need warning like gravity-----	
c          CALL ANCMSG(MSGID=1205,
c     .                MSGTYPE=MSGINFO,
c     .                ANMODE=ANINFO_BLIND_1,
c     .                C1='Boundary conditions ',
c     .                I1=IU,
c     .                I2=ITAB(ND),
c     .                I3=ITAB(ND))
	IF (IS1/=ICODE(N).AND.IS2/=ICODE(N)) THEN
C----error out	ND is along in /BCS
          CALL ANCMSG(MSGID=1208,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ND),
     .                C1='Boundary conditions ',
     .                C2='Boundary conditions')
	ELSE
C----remove Nd from /BCS +degenerating	
         ICODE(N) = 0
         IPR = 1
         IF (ITAGND(N)>0)ITAGND(N) = -ITAGND(N)
         IF (IPRI>=5)
     .    CALL ANCMSG(MSGID=1207,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ND),
     .                PRMOD=MSG_CUMU)
	END IF
       END IF
      ENDDO
       IF (IPR >0.AND.IPRI>=5) THEN
          CALL ANCMSG(MSGID=1207,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='Boundary conditions ',
     .                C2='Boundary conditions',
     .                PRMOD=MSG_PRINT)
       END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  FIXMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SAMEFVID                      source/elements/solid/solide10/dim_s10edg.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FIXMODIF_ND(IBFV, ITAGND,ICNDS10,ITAB)
C=======================================================================
      USE MESSAGE_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*), ITAGND(*),ICNDS10(3,*),ITAB(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, DIR, N,ND,N1,N2,K,ID,IPR
      LOGICAL IS1,IS2
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL SAMEFVID
      EXTERNAL SAMEFVID
C     REAL
C------treatment for tagged nodes in INN(*)----------------------------
      IPR = 0
      DO I=1,NFXVEL
       N = IABS(IBFV(1,I))
       K = IBFV(12,I)
       IF (ITAGND(N) /=0 ) THEN
        ID = IABS(ITAGND(N))
	ND = ICNDS10(1,ID)
	N1 = ICNDS10(2,ID)
	N2 = ICNDS10(3,ID)
C--------quadratic, but---	
	IS1 = SAMEFVID(K,IBFV,N1)
	IS2 = SAMEFVID(K,IBFV,N2)
c	IF (IS1.AND.IS2) THEN
C----remove Nd from ICNDS10
c         ITAGND(N) = ITAGND(N) + NS10E	
	IF (.NOT.(IS1).AND..NOT.(IS2)) THEN
C----error out	ND is along in FV
          CALL ANCMSG(MSGID=1208,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ND),
     .                C1='Imposed VEL/DISP/ACC ',
     .                C2='Imposed VEL/DISP/ACC')
	ELSE
C----remove Nd from FV and warning out-- will be done in ddsplit
         IPR = 1
         IF (IBFV(3,I)>0) IBFV(3,I) = -IBFV(3,I)
         IF (ITAGND(N)>0)ITAGND(N) = -ITAGND(N)
         IF (IPRI>=5)
     .    CALL ANCMSG(MSGID=1207,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ND),
     .                PRMOD=MSG_CUMU)
	END IF
       END IF
      ENDDO
C      
       IF (IPR >0.AND.IPRI>=5) THEN
          CALL ANCMSG(MSGID=1207,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='Imposed VEL/DISP/ACC',
     .                C2='Imposed VEL/DISP/ACC',
     .                PRMOD=MSG_PRINT)
       END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SAMEFVID                      source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        FIXMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|-- calls ---------------
Chd|====================================================================
      LOGICAL FUNCTION SAMEFVID(ID,IBFV,N)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ID,IBFV(NIFV,*),N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NI,K
C----6---------------------------------------------------------------7---------8
       SAMEFVID=.FALSE.
       DO I =1,NFXVEL
        NI = IABS(IBFV(1,I))
        IF (NI==N) THEN
	 K = IBFV(12,I)
         IF (K==ID) SAMEFVID=.TRUE.
         RETURN
        ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  INT2MODIF_ND                  source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INT2MODIF_ND(IPARI,INTBUF_TAB,ITAGND,ICNDS10,ITAB)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER),ITAGND(*),ITAB(*),ICNDS10(3,*)
      TYPE(INTBUF_STRUCT_), DIMENSION(NINTER) :: INTBUF_TAB
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NTY,NSN,NMN,ISL,NKIN,NOINT,IMODI,II,N1,N2,ND
      INTEGER K,ILEV,NUVAR,IDEL7N,INTTH,IPR,IML,ICOM
      INTEGER, DIMENSION(NUMNOD) :: ITAGS,ITAGMD
      CHARACTER*nchartitle, TITR
      LOGICAL IS1,IS2,ISD
C=======================================================================
C--------done before : switch to penalty if only ND is secnd (N1,N2 not)
C-----------NOT necessary after tests---------------
c      DO N=1,NINTER
c        NTY  = IPARI(7,N)
c        IF (NTY == 2 ) THEN
c         NSN   = IPARI(5,N)
c         NOINT = IPARI(15,N)
c         IPR = 0
c         DO I=1,NS10E
c          ND = ICNDS10(1,I)
c          N1 = ICNDS10(2,I)
c          N2 = ICNDS10(3,I)
c	  IF (ITAGND(ND)>NS10E) CYCLE
c	  ISD = INTAB(NSN,INTBUF_TAB(N)%NSV,ND)
c          IF (.NOT.(ISD)) THEN
c	   IS1 = INTAB(NSN,INTBUF_TAB(N)%NSV,N1)
c	   IF (IS1) THEN
c	    IS2 = INTAB(NSN,INTBUF_TAB(N)%NSV,N2)
c	    IF (IS2.AND.ITAGND(ND)>0) THEN
c	     ITAGND(ND)=-ITAGND(ND)
c             IPR = IPR +1
c             CALL ANCMSG(MSGID=1212,
c     .                MSGTYPE=MSGWARNING,
c     .                ANMODE=ANINFO_BLIND_1,
c     .                I1=ITAB(ND),
c     .                I2=ITAB(N1),
c     .                I3=ITAB(N2),
c     .                PRMOD=MSG_CUMU)
c	    END IF
c           END IF	
c          END IF
c         END DO
c         IF (IPR >0) THEN
c          CALL ANCMSG(MSGID=1212,
c     .                MSGTYPE=MSGWARNING,
c     .                ANMODE=ANINFO_BLIND_1,
c     .                I1=NOINT,
c     .                PRMOD=MSG_PRINT)
c         END IF
c        END IF !(NTY == 2 ) THEN
c      END DO
C--error out in case N1,N2 are S of int2 (kinematic)and Nd is M or S (/w penality)
      ITAGMD(1:NUMNOD)   = 0
      ITAGS(1:NUMNOD)   = 0
       DO N=1,NINTER
        NTY  = IPARI(7,N)
        IF (NTY == 2 ) THEN
         NMN =IPARI(6,N)
         NSN   = IPARI(5,N)
         ILEV = IPARI(20,N)
         NOINT   = IPARI(15,N)
         IF (ILEV <= 5 .or. ILEV == 30) THEN
          DO I=1,NSN
           ISL = INTBUF_TAB(N)%NSV(I)
           IF (ITAGS(ISL)==0) ITAGS(ISL)=NOINT
          END DO
         END IF
        END IF
       END DO
C       
       DO I = 1, NS10E
        N = IABS(ICNDS10(1,I))
        ITAGMD(N) = I
       END DO
      DO N=1,NINTER
        NTY  = IPARI(7,N)
        ILEV = IPARI(20,N)
        IF (NTY == 2 ) THEN
         NMN =IPARI(6,N)
          DO I=1,NMN
           IML = INTBUF_TAB(N)%MSR(I)
           IF (ITAGMD(IML)>0) ITAGMD(IML) = ITAGMD(IML) + NS10E
          ENDDO
         NSN   = IPARI(5,N)
         IF (ILEV == 27 .or. ILEV == 28) THEN
          DO I=1,NSN
           ISL = INTBUF_TAB(N)%NSV(I)
           IF (ITAGMD(ISL)>0 .AND.INTBUF_TAB(N)%IRUPT(I) == 1) ITAGMD(IML)=-ITAGMD(ISL)
          ENDDO
         ELSEIF (ILEV == 25 .or. ILEV == 26) THEN
          DO I=1,NSN
            ISL = INTBUF_TAB(N)%NSV(I)
            IF (ITAGMD(ISL)>0 ) ITAGMD(IML)=-ITAGMD(ISL)
          ENDDO
	 END IF
        END IF
      END DO
C
      ICOM = 0
      ISL=0
      NOINT =0
       DO I = 1, NS10E
        N = IABS(ICNDS10(1,I))
        N1 = ICNDS10(2,I)
        N2 = ICNDS10(3,I)
        IF (ITAGMD(N)>NS10E.OR.ITAGMD(N)<0) THEN
         IF (ITAGS(N1)>0.OR.ITAGS(N2)>0) THEN
          ICOM=ICOM+1
          IF (ITAGS(N1)>0) ITAGS(N1)=-ITAGS(N1)
          IF (ITAGS(N2)>0) ITAGS(N2)=-ITAGS(N2)
          IF (ISL==0) ISL = N
          IF (NOINT==0.AND.ITAGS(N1)<0)NOINT = -ITAGS(N1)
          IF (NOINT==0.AND.ITAGS(N2)<0)NOINT = -ITAGS(N2)
         END IF
        END IF
       END DO
      IF (ICOM>0) THEN
          CALL ANCMSG(MSGID=1638,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ICOM,
     .                I2=ITAB(ISL),
     .                I3=NOINT,
     .                I4=NOINT)
      END IF
C
      DO N=1,NINTER
        NTY  = IPARI(7,N)
        ILEV = IPARI(20,N)
        IF (NTY == 2 ) THEN
          NSN   = IPARI(5,N)
          NOINT = IPARI(15,N)
	  IMODI = 0
          IPR = 0
         IF (ILEV == 25 .or. ILEV == 26) THEN
         ELSEIF (ILEV == 27 .or. ILEV == 28) THEN
          DO I=1,NSN
            ISL = INTBUF_TAB(N)%NSV(I)
            IF (ITAGND(ISL)/=0 .AND.INTBUF_TAB(N)%IRUPT(I) /= 1)THEN
	     INTBUF_TAB(N)%NSV(I) = -ISL
	     IF (ITAGND(ISL)>0 ) ITAGND(ISL) = -ITAGND(ISL)
	     IMODI = IMODI + 1
	    END IF
          ENDDO
         ELSE
          DO I=1,NSN
            ISL = INTBUF_TAB(N)%NSV(I)
            IF (ITAGND(ISL)/=0 )THEN
	     INTBUF_TAB(N)%NSV(I) = -ISL
	     IF (ITAGND(ISL)>0 ) ITAGND(ISL) = -ITAGND(ISL)
	     IMODI = IMODI + 1
	    END IF
          ENDDO
	 END IF
C-----------------------------------------------
C     Compact INT,REAL BUFFER
C-----------------------------------------------
         IF (IMODI > 0 ) THEN
          IDEL7N = IPARI(17,N)
          NUVAR  = IPARI(35,N)
          INTTH  = IPARI(47,N)
           II = 0
           DO I = 1,NSN
             IF (INTBUF_TAB(N)%NSV(I) > 0) THEN
               II = II+1
               INTBUF_TAB(N)%NSV(II) = INTBUF_TAB(N)%NSV(I)
               INTBUF_TAB(N)%IRTLM(II) = INTBUF_TAB(N)%IRTLM(I)
               IF ((ILEV >= 10.AND.ILEV <= 22).OR.ILEV == 27.OR.ILEV == 28) THEN
	        INTBUF_TAB(N)%IRUPT(II) = INTBUF_TAB(N)%IRUPT(I)
	       END IF
              INTBUF_TAB(N)%CSTS(1+2*(II-1))   = INTBUF_TAB(N)%CSTS(1+2*(I-1))
              INTBUF_TAB(N)%CSTS(1+2*(II-1)+1) = INTBUF_TAB(N)%CSTS(1+2*(I-1)+1)
              INTBUF_TAB(N)%DPARA(1+7*(II-1))   = INTBUF_TAB(N)%DPARA(1+7*(I-1))
              INTBUF_TAB(N)%DPARA(1+7*(II-1)+1) = INTBUF_TAB(N)%DPARA(1+7*(I-1)+1)
              INTBUF_TAB(N)%DPARA(1+7*(II-1)+2) = INTBUF_TAB(N)%DPARA(1+7*(I-1)+2)
              INTBUF_TAB(N)%DPARA(1+7*(II-1)+3) = INTBUF_TAB(N)%DPARA(1+7*(I-1)+3)
              INTBUF_TAB(N)%DPARA(1+7*(II-1)+4) = INTBUF_TAB(N)%DPARA(1+7*(I-1)+4)
              INTBUF_TAB(N)%DPARA(1+7*(II-1)+5) = INTBUF_TAB(N)%DPARA(1+7*(I-1)+5)
              INTBUF_TAB(N)%DPARA(1+7*(II-1)+6) = INTBUF_TAB(N)%DPARA(1+7*(I-1)+6)
              IF (IDEL7N /= 0)THEN
               INTBUF_TAB(N)%SMAS(II)  = INTBUF_TAB(N)%SMAS(I)
               INTBUF_TAB(N)%SINER(II) = INTBUF_TAB(N)%SINER(I)
	      END IF
              IF ((ILEV>=10 .AND. ILEV<=22 ).OR. INTTH > 0) THEN
               INTBUF_TAB(N)%AREAS2(II) = INTBUF_TAB(N)%AREAS2(I)
                DO K = 0,NUVAR-1
                 INTBUF_TAB(N)%UVAR(1+NUVAR*(II-1)+K) =
     .                     INTBUF_TAB(N)%UVAR(1+NUVAR*(I-1)+K)
                ENDDO
	      END IF
              IF (( ILEV>=10 .AND. ILEV<=12).OR.( ILEV>=20 .AND. ILEV<=22)) THEN
               INTBUF_TAB(N)%SMAS(II)  = INTBUF_TAB(N)%SMAS(I)
               INTBUF_TAB(N)%SINER(II) = INTBUF_TAB(N)%SINER(I)
               DO K = 0,NUVAR-1
                INTBUF_TAB(N)%UVAR(1+NUVAR*(II-1)+K) =
     .                     INTBUF_TAB(N)%UVAR(1+NUVAR*(I-1)+K)
               ENDDO
               DO K = 0,2
                INTBUF_TAB(N)%XM0(1+3*(II-1)+K) = INTBUF_TAB(N)%XM0(1+3*(I-1)+K)
                INTBUF_TAB(N)%DSM(1+3*(II-1)+K) = INTBUF_TAB(N)%DSM(1+3*(I-1)+K)
                INTBUF_TAB(N)%FSM(1+3*(II-1)+K) = INTBUF_TAB(N)%FSM(1+3*(I-1)+K)
               ENDDO
              ELSEIF (ILEV==27 .OR. ILEV==28) THEN
               INTBUF_TAB(N)%SMAS(II)  = INTBUF_TAB(N)%SMAS(I)
               INTBUF_TAB(N)%SINER(II) = INTBUF_TAB(N)%SINER(I)
               INTBUF_TAB(N)%SPENALTY(II)  = INTBUF_TAB(N)%SPENALTY(I)
               INTBUF_TAB(N)%STFR_PENALTY(II) = INTBUF_TAB(N)%STFR_PENALTY(I)
               DO K = 0,8
                INTBUF_TAB(N)%SKEW(1+9*(II-1)+K) = INTBUF_TAB(N)%SKEW(1+9*(I-1)+K)
               ENDDO
               DO K = 0,2
                INTBUF_TAB(N)%DSM(1+3*(II-1)+K) = INTBUF_TAB(N)%DSM(1+3*(I-1)+K)
                INTBUF_TAB(N)%FSM(1+3*(II-1)+K) = INTBUF_TAB(N)%FSM(1+3*(I-1)+K)
                INTBUF_TAB(N)%FINI(1+3*(II-1)+K) = INTBUF_TAB(N)%FINI(1+3*(I-1)+K)
               ENDDO
 	      END IF
              IF (INTBUF_TAB(N)%S_CSTS_BIS>0) THEN
               DO K = 0,1
                INTBUF_TAB(N)%CSTS_BIS(1+2*(II-1)+K) = INTBUF_TAB(N)%CSTS_BIS(1+2*(I-1)+K)
               ENDDO
              END IF
	     ELSE
C-----warning out	 
             ISL = -INTBUF_TAB(N)%NSV(I)
             IPR = 1
           IF (IPRI>=5)
     .       CALL ANCMSG(MSGID=1209,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(ISL),
     .                PRMOD=MSG_CUMU)
             ENDIF
           ENDDO
           IPARI(5,N) = II
	 END IF !(IMODI > 0 )
         IF (IPR >0.AND.IPRI>=5) THEN
          CALL ANCMSG(MSGID=1209,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                C1='Interface Type2 ',
     .                I1=NOINT,
     .                PRMOD=MSG_PRINT)
         END IF
        END IF !(NTY == 2 )
      ENDDO
C
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  PRE_CNDPON                    source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE PRE_CNDPON(ICNDS10,ADSKYCND,CEPCND,CELCND  ,ITAGND)
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        ICNDS10(3,*), ADSKYCND(0:*),CEPCND(*),CELCND(*),ITAGND(*)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, I, IS,IAD, J, KK,  N, NL,NIR,NL_L,P,NI,ICOMP(NSPMD)
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
C
C Itet=2 of Tetra10 same than int2
C
C-----------------------------------------------------
C Preparation de ADDCNCND : Adresse matrice CNCND 
C-----------------------------------------------------
        DO N=0,NUMNOD+1
          ADSKYCND(N) = 0
        ENDDO
C
        NIR = 2
        DO I=1,NS10E
          K = IABS(ICNDS10(1,I))
	  IF (ITAGND(K)>NS10E) CYCLE
          DO J=1,NIR
            KK = ICNDS10(1+J,I)
            ADSKYCND(KK) = ADSKYCND(KK) + 1
          END DO
        END DO
C-----------------------------------------------
C   CALCUL DES ADRESSES DU VECTEUR SKYLINE
C-----------------------------------------------
C------remove zero value nodes at the beginning---
       IF (ADSKYCND(1)>0) THEN
        NI= 1
       ELSE
        NI = 0
        DO I=2,NUMNOD
          IF (ADSKYCND(I)>0) THEN
	   NI = I
	   GOTO 100
	  END IF
        ENDDO
 100     CONTINUE
       END IF
C-----------first activate node should begin from 1       
        IF (NI==1) THEN
	 ADSKYCND(1) = ADSKYCND(1)+1
	ELSE
	 ADSKYCND(1) = 1
	END IF
        DO I=2,NUMNOD+1
          ADSKYCND(I)=ADSKYCND(I)+ADSKYCND(I-1)
        ENDDO
        DO I=NUMNOD+1,NI+1,-1
          ADSKYCND(I)=ADSKYCND(I-1)
        ENDDO
        ADSKYCND(1:NI) = 1
C-----------------------------------------------
C Remplissage de CEPCND : connection Element/Local
C-----------------------------------------------
           ICOMP(1:NSPMD)=0
            DO I=1,NS10E
              K = IABS(ICNDS10(1,I))
	      IF (ITAGND(K)>NS10E) CYCLE
              DO P = 1, NSPMD
                IF(NLOCAL(K,P)==1)THEN
                  CEPCND(I) = P-1
		  ICOMP(P) = ICOMP(P) + 1
                  CELCND(I)= ICOMP(P)
	          GOTO 200
                ENDIF
              ENDDO
 200        CONTINUE
            ENDDO
C-----------------------------------------------
C Remplissage de CEL : connection Element/Local
C-----------------------------------------------
c        DO P = 1, NSPMD
c          NL_L = 0
c          DO I=1,NS10E
c            K = IABS(ICNDS10(1,I))
c	    IF (ITAGND(K)>NS10E) CYCLE
c            IF(CELCND(I)==0) THEN
c              IF(NLOCAL(K,P)==1)THEN
c                NL_L = NL_L + 1
c                CELCND(I) = NL_L
c              END IF
c            END IF
c          END DO
c        END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  FILLCNCND                     source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FILLCNCND(CNCND  ,ADDCNCND, ICNDS10,ITAGND)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNCND(0:*), CNCND(*),ICNDS10(3,*),ITAGND(*)

C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C---------------------------------------
      INTEGER I, J, L, K, N, OFF, NTY, NRTS, NRTM, NSN, NMN,
     .        KK, NIR,ADSKY(NUMNOD+1)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------------
C   CALCUL DE CNE ADDCNE
C-----------------------------------------------
      DO I = 1, NUMNOD+1
        ADSKY(I) = ADDCNCND(I)
      ENDDO
C
C ADDCNCND(I+1)-ADDCNCND(I): nb of node I (main)
        NIR = 2
        DO I=1,NS10E
          K = ICNDS10(1,I)
	  IF (ITAGND(K)>NS10E) CYCLE
          DO J=1,NIR
            KK = ICNDS10(1+J,I)
            CNCND(ADSKY(KK)) = I
            ADSKY(KK) = ADSKY(KK) + 1
          END DO
        END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  STIFN0_ND                     source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STIFN0_ND(ICNDS10,STIFN)
C=======================================================================
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICNDS10(3,*)
C     REAL
      my_real
     .   STIFN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND
C     REAL
C------put STIFN(ND)=0 for mscalling dt estimation
      DO I=1,NS10E
       ND = IABS(ICNDS10(1,I))
       STIFN(ND)=ZERO
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  STIFN1_ND                     source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STIFN1_ND(ICNDS10,STIFN)
C=======================================================================
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICNDS10(3,*)
C     REAL
      my_real
     .   STIFN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ND,N1,N2
C     REAL
      my_real
     .   STIF
C------condense STIFN(ND)
      DO I=1,NS10E
       ND = IABS(ICNDS10(1,I))
       IF (STIFN(ND)<=ZERO) CYCLE
       STIF =HALF*STIFN(ND)
       N1 = ICNDS10(2,I)
       N2 = ICNDS10(3,I)
       STIFN(N1)=STIFN(N1)+STIF
       STIFN(N2)=STIFN(N2)+STIF
       STIFN(ND)=ZERO
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  BCSCYCMODIF_ND                source/elements/solid/solide10/dim_s10edg.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE BCSCYCMODIF_ND(IBCSCYC,LBCSCYC,ITAGND,ITAB)
C=======================================================================
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCSCYC(4,*), LBCSCYC(2,*), ITAGND(*),ITAB(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,KK,NSL,NSL_N,K_N,NS,ID,N1,N2
C     REAL
C------removing Nd from LBCSCYC(*)--
      DO I = 1, NBCSCYC
        K = IBCSCYC(1,I)
        NSL =IBCSCYC(3,I)
        NSL_N = 0
        ID =IBCSCYC(4,I)
        DO J = 1, NSL
         N1 = LBCSCYC(1,K+J)
         N2 = LBCSCYC(2,K+J)
         IF(ITAGND(N1)==0.AND.ITAGND(N2)==0)THEN
          NSL_N = NSL_N + 1
          LBCSCYC(1,K+NSL_N) =N1
          LBCSCYC(2,K+NSL_N) =N2
         ELSEIF(ITAGND(N1)/=0.AND.ITAGND(N2)/=0) THEN
C--- remove         
         ELSE
C--- error out        
           CALL ANCMSG(MSGID=1758,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ID)
         END IF
        ENDDO
        IF (NSL>NSL_N) THEN
         KK = NSL-NSL_N
         IBCSCYC(3,I) = NSL_N
         IBCSCYC(1,I) = K+NSL_N
         CALL ANCMSG(MSGID=1759,ANMODE=ANINFO,MSGTYPE=MSGWARNING,I1=KK,I2=ID)
        END IF
      ENDDO
C
      RETURN
      END
