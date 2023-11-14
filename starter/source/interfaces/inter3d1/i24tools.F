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
Chd|  ADD_ID                        source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        COMM_SEG_E                    source/interfaces/inter3d1/i24inisu_nei.F
Chd|        I25NEIGH_SEG_E                source/interfaces/inter3d1/i25neigh.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ADD_ID(N,IC,ID)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C----add ID--at end--if not inside already----------------
      IF (ID/=0) THEN
       DO I =1,N
        IF (IC(I)==ID) RETURN
       ENDDO
      END IF 
      N =N+1
      IC(N)=ID
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ADD_N_ID                      source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        COMM_SEG_N                    source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ADD_N_ID(N,IC,ID)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C----add ID--at end----------------
      N =N+1
      IC(N)=ID
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        ADD_NS                        source/interfaces/inter3d1/i7remnode.F
Chd|        ADD_NSFIC                     source/interfaces/inter3d1/i7remnode.F
Chd|        COMM_SEG_E                    source/interfaces/inter3d1/i24inisu_nei.F
Chd|        I24PENMAX                     source/interfaces/inter3d1/i24pen3.F
Chd|        I25NEIGH_SEG_E                source/interfaces/inter3d1/i25neigh.F
Chd|        ITAGSL2                       source/interfaces/inter3d1/itagsl2.F
Chd|        RIGMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|        S10EDG_RLINK                  source/elements/solid/solide10/s10edg_rlink.F
Chd|        SEG_OPP                       source/interfaces/inter3d1/i24tools.F
Chd|        SAME_SEG                      source/interfaces/inter3d1/i24tools.F
Chd|-- calls ---------------
Chd|====================================================================
      LOGICAL FUNCTION INTAB(NIC,IC,N)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,NIC,IC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C----6---------------------------------------------------------------7---------8
       INTAB=.FALSE.
       DO I =1,NIC
        IF (N==IC(I)) THEN
         INTAB=.TRUE.
         RETURN
        ENDIF
       ENDDO 
C
      RETURN
      END
Chd|====================================================================
Chd|  NORMV3                        source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        I24NORMNS                     source/interfaces/inter3d1/i24sti3.F
Chd|        I25NEIGH_REMOVEALLBUT1        source/interfaces/inter3d1/i25neigh.F
Chd|        NORMVEC                       source/interfaces/inter3d1/i24sti3.F
Chd|        REMOVEIC                      source/interfaces/inter3d1/i24tools.F
Chd|        REMOVEIC1                     source/interfaces/inter3d1/i24tools.F
Chd|        VOISIN2                       source/interfaces/inter3d1/i24tools.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NORMV3(V,NORM)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   V(3),NORM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   S
C-----
       NORM = SQRT(V(1)*V(1)+V(2)*V(2)+V(3)*V(3))
       S=ONE/MAX(EM20,NORM)
       V(1)=V(1)*S
       V(2)=V(2)*S
       V(3)=V(3)*S
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  VOISIN2                       source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        REMOVEIC                      source/interfaces/inter3d1/i24tools.F
Chd|-- calls ---------------
Chd|        NORMA4N                       source/interfaces/inter3d1/norma1.F
Chd|        NORMA4NX                      source/interfaces/inter3d1/norma1.F
Chd|        NORMV3                        source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE VOISIN2(NC1,NC2,X ,I1 ,I2 ,YI,YJ,INV)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NC1(*),NC2(*),INV,I1 ,I2
      my_real
     .   X(3,*),YI(3),YJ(3)
C-----------------------------------------------
c FUNCTION: find the same orientation of two segments (4n)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NC1(1:4),NC2(1:4) - connectivity
c  I   I1,I2             - common nodes of two seg
c  I   X(3,*)            - co-ordinates
c  O   INV               - flag : 0 same orientation; 1 inverse one
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2
      my_real
     .   S,AREA,NXI,NYI,NZI,NXJ,NYJ,NZJ,NORM,YIYJ,Y0(3)
C----build Yi vectors for seg 1,2
        YIYJ=YI(1)*YJ(1)+YI(2)*YJ(2)+YI(3)*YJ(3)
C--------particular case-----        
        IF (ABS(YIYJ)<EM20) THEN
         CALL NORMV3(YJ,NORM)
         Y0(1)= -EM01*YJ(1)
         Y0(2)= -EM01*YJ(2)
         Y0(3)= -EM01*YJ(3)
         CALL NORMA4NX(NXI,NYI,NZI,AREA,NC1 ,X ,I1,I2,Y0)
         CALL NORMA4N(NXJ,NYJ,NZJ,AREA,NC2 ,X )
         S=NXI*NXJ+NYI*NYJ+NZI*NZJ
        ELSE
C----build normal vectors for seg 1,2
         CALL NORMA4N(NXI,NYI,NZI,AREA,NC1 ,X )
         CALL NORMA4N(NXJ,NYJ,NZJ,AREA,NC2 ,X )
         S=NXI*NXJ+NYI*NYJ+NZI*NZJ
        END IF
C        
        IF (YIYJ*S <= ZERO ) THEN
          INV =0
        ELSE
          INV = 1
        END IF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  RE_ORI                        source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        COMM_SEG_EN                   source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RE_ORI(I1,I2,IN,X )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I1,I2,IN(*)
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,J1,J2
      my_real
     .   S,U1(3),U2(3)
C----re-orienter IN(1),IN(2) if necessary----------------
      J1=IN(1)
      J2=IN(2)
      IF (J1 == 0 .OR. J2 == 0) RETURN
      U1(1)=X(1,I2)-X(1,I1)
      U1(2)=X(2,I2)-X(2,I1)
      U1(3)=X(3,I2)-X(3,I1)
      U2(1)=X(1,J2)-X(1,J1)
      U2(2)=X(2,J2)-X(2,J1)
      U2(3)=X(3,J2)-X(3,J1)
      S= U1(1)*U2(1)+U1(2)*U2(2)+U1(3)*U2(3)
      IF (S < 0) THEN
c       print *,'---------change order-----------'
       J=IN(2)
       IN(2)=IN(1)
       IN(1)=J
      END IF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  MSG_ERR                       source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        I24INISUR_NEI                 source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MSG_ERR(I1,I2,ITAB,IRR,ID,TITR)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I1,I2,ITAB(*),IRR
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C----Warning,ERROR out----------------
       IF(IPRI==0) RETURN
#ifndef HYPERMESH_LIB
       IF (IRR ==11) THEN
C-----multi-neibour but no valid one
            CALL ANCMSG(MSGID=993,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=Itab(I1),I3=Itab(I2))
c        write(iout,*) '***Warning: No validate commun Seg with line:',
c     +                 Itab(I1),Itab(I2)
       ELSEIF (IRR ==12) THEN
C-----multi-neibour but no valid one
            CALL ANCMSG(MSGID=994,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=Itab(I1),I3=Itab(I2))
c        write(iout,*) '***ERROR: Too much commun Seg with line:',
c     +                 Itab(I1),Itab(I2)
c        CALL ARRET(2)
       END IF
#endif
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SAME_SEG                      source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        COMM_SEG_E                    source/interfaces/inter3d1/i24inisu_nei.F
Chd|        I25NEIGH_SEG_E                source/interfaces/inter3d1/i25neigh.F
Chd|-- calls ---------------
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      LOGICAL FUNCTION SAME_SEG(IRECT1,IRECT2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT1(*),IRECT2(*)
C-----------------------------------------------
c FUNCTION: if two segs have the same node numbers
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   IRECT1,2           - connectivity of segment
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C----6---------------------------------------------------------------7---------8
       SAME_SEG=.TRUE.
       DO I = 1,4
        J= IRECT2(I) 
        IF (.NOT.INTAB(4,IRECT1,J)) THEN
         SAME_SEG=.FALSE.
         CYCLE
        END IF
       END DO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  SEG_OPP                       source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        COMM_SEG_EN                   source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- calls ---------------
Chd|        NORMA4N                       source/interfaces/inter3d1/norma1.F
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE SEG_OPP(EI,EJ,IRECT,X ,IOP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EI,EJ,IRECT(4,*),IOP
C     REAL
      my_real
     .   X(3,*)
C----if the normal of two segments are opposite or near opposite
C----if two common segs, will also be eliminated 
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   AREA,NXI,NYI,NZI,NXJ,NYJ,NZJ,S
      INTEGER I,J,NN
C---
         IOP=0
         CALL NORMA4N(NXI,NYI,NZI,AREA,IRECT(1,EI),X )
         CALL NORMA4N(NXJ,NYJ,NZJ,AREA,IRECT(1,EJ),X )
         S=NXI*NXJ+NYI*NYJ+NZI*NZJ
               IF (S<ZERO.AND.ABS(S)>0.99) IOP=1
         IF (IOP == 0 ) THEN
          NN = 0
          DO I = 1,3
           J= IRECT(I,EI) 
           IF (INTAB(3,IRECT(1,EJ),J)) NN = NN +1
           IF (IRECT(3,EJ)/=IRECT(4,EJ).AND.J==IRECT(4,EJ)) NN = NN +1
          END DO
          IF (IRECT(3,EI)/=IRECT(4,EI)) THEN
           J=IRECT(4,EI)
           IF (INTAB(3,IRECT(1,EJ),J)) NN = NN +1
           IF (IRECT(3,EJ)/=IRECT(4,EJ).AND.J==IRECT(4,EJ)) NN = NN +1
          END IF
                IF (NN > 2) IOP = 1
         END IF !IF (IOP == 0 )
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  REMOVEIC                      source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        NORMA4N                       source/interfaces/inter3d1/norma1.F
Chd|        NORMV3                        source/interfaces/inter3d1/i24tools.F
Chd|        VOISIN2                       source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE REMOVEIC(N,IC,ISELF,IRECT,X ,I1,I2,IASYM,IRR)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,IC(*),ISELF,IRECT(4,*),I1,I2,IE,IASYM,IRR
      my_real
     .   X(3,*)
C----ne reste qu'un seg
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,INV
      my_real
     .   S,YJ(3,N),YI(3),Y0(3),YJNI(N),ANGLE(N),SMIN,NORM,
     .   NXI,NYI,NZI
C----build ksi vectors for EI(-ksi) and EJ()
        Y0(1) = HALF*(X(1,I1)+X(1,I2))
        Y0(2) = HALF*(X(2,I1)+X(2,I2))
        Y0(3) = HALF*(X(3,I1)+X(3,I2))
        DO J=1,3
         YI(J)=-Y0(J)
         DO I=1,N
          YJ(J,I)=-Y0(J)
         END DO
        END DO
C        
        DO J=1,4
         J1=IRECT(J,ISELF)
         IF (J1 /= I1 .AND. J1 /= I2) THEN
          YI(1)=YI(1)+X(1,J1)
          YI(2)=YI(2)+X(2,J1)
          YI(3)=YI(3)+X(3,J1)
         END IF
        END DO
       CALL NORMV3(YI,NORM)
C        
        DO I=1,N
         IE=IC(I)
         DO J=1,4
          J1=IRECT(J,IE)
          IF (J1 /= I1 .AND. J1 /= I2) THEN
           YJ(1,I)=YJ(1,I)+X(1,J1)
           YJ(2,I)=YJ(2,I)+X(2,J1)
           YJ(3,I)=YJ(3,I)+X(3,J1)
          END IF
         END DO
        END DO
C        
        CALL NORMA4N(NXI,NYI,NZI,NORM,IRECT(1,ISELF) ,X )
C
        DO I=1,N
         CALL NORMV3(YJ(1,I),NORM)
         YJNI(I)=NXI*YJ(1,I)+NYI*YJ(2,I)+NZI*YJ(3,I)
         ANGLE(I)=ABS(YI(1)*YJ(1,I)+YI(2)*YJ(2,I)+YI(3)*YJ(3,I))
C---------remove asymmetric shell seg-----   
         IE=IC(I)
         IF (IASYM>0) THEN
          CALL VOISIN2(IRECT(1,ISELF),IRECT(1,IE),X ,I1 ,I2,
     +                       YI,YJ(1,I),INV)
          IF (INV > 0) ANGLE(I) = EP10
         END IF !(IASYM>0) THEN
        END DO
C        
        SMIN=EP10
        J1=0
C--------groupe YJ*Ni>=0 first
        J=0
        DO I=1,N
         IF (YJNI(I)>=ZERO) THEN
          IF(SMIN > ANGLE(I)) THEN
           SMIN=ANGLE(I)
           J1=I
          END IF
          J=J+1
         END IF
        END DO
C--------same side
        IF (J==N) THEN
C--------only groupe YJ*Ni<0  or no valid one      
        ELSEIF(J==0.OR.J1==0) THEN
         SMIN=EP10
         DO I=1,N
           IF(YJNI(I) < ZERO .AND. SMIN > ANGLE(I)) THEN
            SMIN=ANGLE(I)
            J1=I
           END IF
         END DO
        END IF
C ------still no valid one-----------       
        IF (J1==0) then
c        print *,'***warning** No valid Neighbour Segs of',N,' candidats'
         IRR = 11
         IC(1)=0         
        ELSE
         IC(1)=IC(J1)
        END IF
C        
        DO I=2,N
         IC(I)=0
        END DO 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  REMOVEIC1                     source/interfaces/inter3d1/i24tools.F
Chd|-- called by -----------
Chd|        COMM_SEG_EN                   source/interfaces/inter3d1/i24inisu_nei.F
Chd|-- calls ---------------
Chd|        NORMA4N                       source/interfaces/inter3d1/norma1.F
Chd|        NORMV3                        source/interfaces/inter3d1/i24tools.F
Chd|        VOISIN1                       source/interfaces/inter3d1/voisin1.F
Chd|====================================================================
      SUBROUTINE REMOVEIC1(N,IC,ISELF,IRECT,X ,I1,I2,IASYM,IRR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,IC(*),ISELF,IRECT(4,*),I1,I2,IE,IASYM,IRR
C     REAL
      my_real
     .   X(3,*)
C----ne reste qu'un seg
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,INV,ipr
      my_real
     .   S,YJ(3,N),YI(3),Y0(3),YJNI(N),ANGLE(N),SMIN,NORM,
     .   NXI,NYI,NZI,X12,Y12,Z12,NXJ,NYJ,NZJ,SMAX
C----elimine first one of asymmetric shell seg
        INV = 0
        DO I=1,N
         ANGLE(I)=ZERO
        END DO
        IF (IASYM>0) THEN
         DO I=1,N
C---------remove asymmetric shell seg-----   
          IE=IC(I)
          CALL VOISIN1(IRECT(1,ISELF),IRECT(1,IE),I1 ,I2 ,INV)
          IF (INV > 0) ANGLE(I) = EP10
         END DO
        END IF !(IASYM>0) THEN
C --------YI = N_iself ^ 12       
        CALL NORMA4N(NXI,NYI,NZI,NORM,IRECT(1,ISELF) ,X )
        X12= X(1,I2)-X(1,I1)
        Y12= X(2,I2)-X(2,I1)
        Z12= X(3,I2)-X(3,I1)
        YI(1)=NYI*Z12-NZI*Y12
        YI(2)=NZI*X12-NXI*Z12
        YI(3)=NXI*Y12-NYI*X12
        CALL NORMV3(YI,NORM)
        J=0
        DO I=1,N
         IF (ANGLE(I)==EP10) CYCLE
         IE=IC(I)
         CALL NORMA4N(NXJ,NYJ,NZJ,NORM,IRECT(1,IE) ,X )
C----YJ =  N_ie ^ 21        
         YJ(1,I)=-NYJ*Z12+NZJ*Y12
         YJ(2,I)=-NZJ*X12+NXJ*Z12
         YJ(3,I)=-NXJ*Y12+NYJ*X12
         CALL NORMV3(YJ(1,I),NORM)
         YJNI(I)=NXI*YJ(1,I)+NYI*YJ(2,I)+NZI*YJ(3,I)
         IF (YJNI(I)>=ZERO) J=J+1
         ANGLE(I)=YI(1)*YJ(1,I)+YI(2)*YJ(2,I)+YI(3)*YJ(3,I)
        END DO
C        
        SMAX=-ONEP01
        J1=0
C--------groupe YJ*Ni>=0 :concave keep angle (max_cos) only
        DO I=1,N
         IF (ANGLE(I)==EP10.OR.YJNI(I)<ZERO) CYCLE
          IF (ANGLE(I)>=-ONE) THEN
           IF(SMAX < ANGLE(I)) THEN
            SMAX=ANGLE(I)
            J1=I
           END IF
          END IF !(ANGLE(I)>=-ONE) THEN
        END DO
C------angle >180------        
        IF(J1==0.AND.J >0) THEN
         SMIN=EP10
         DO I=1,N
          IF (ANGLE(I)==EP10.OR.YJNI(I)<ZERO) CYCLE
          IF (SMIN > ANGLE(I)) THEN
            SMIN=ANGLE(I)
            J1=I
          END IF
         END DO
        END IF
C--------same side
        IF (J==N) THEN
C--------only groupe YJ*Ni<0(convex)  and no valid one before    
        ELSEIF(J==0.OR.J1==0) THEN
C------angle >180- first-----        
          SMAX=-ONEP01
          DO I=1,N
           IF (ANGLE(I)==EP10.OR.YJNI(I)>=ZERO) CYCLE
           IF(ANGLE(I)< -ONE .AND.SMAX < ANGLE(I)) THEN
            SMAX=ANGLE(I)
            J1=I
           END IF
          END DO
C ------------------         
         IF (J1==0) then
          SMIN=EP10
          DO I=1,N
           IF (ANGLE(I)==EP10.OR.YJNI(I)>=ZERO) CYCLE
C--------groupe YJ*Ni<0 :convex keep angle (min_cos) only
           IF(ANGLE(I)>= -ONE .AND. SMIN > ANGLE(I)) THEN
            SMIN=ANGLE(I)
            J1=I
           END IF
          END DO
         END IF !(J1==0) then
        END IF !(J==N) then
C ------still no valid one-----------       
        IF (J1==0) then
c        print *,'***warning** No valid Neighbour Segs of',N,' candidats'
         IRR = 11
         IC(1)=0         
        ELSE
         IC(1)=IC(J1)
        END IF
C        
        DO I=2,N
         IC(I)=0
        END DO 
C-----------------------------------------------
      RETURN
      END
