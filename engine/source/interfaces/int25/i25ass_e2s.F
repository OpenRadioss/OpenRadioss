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
Chd|  I25ASSE0_E2S                  source/interfaces/int25/i25ass_e2s.F
Chd|-- called by -----------
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25ASSE0_E2S(JLT  ,CS_LOC,N1   ,N2   ,M1   ,
     2                   M2   ,HS1   ,HS2  ,HM1  ,HM2  ,
     3                   FX1  ,FY1   ,FZ1  ,FX2  ,FY2  ,
     4                   FZ2  ,FX3   ,FY3  ,FZ3  ,FX4  ,
     5                   FY4  ,FZ4   ,A    ,STIFN,STIF ,
     6                   NEDGE ,NIN   ,JTASK,PENE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NEDGE, NIN,
     +        CS_LOC(*),
     +        N1(*),N2(*),M1(*),M2(*),JTASK
      my_real
     .        HS1(*),HS2(*),HM1(*),HM2(*),
     .        FX1(*),FY1(*),FZ1(*),
     .        FX2(*),FY2(*),FZ2(*),
     .        FX3(*),FY3(*),FZ3(*),
     .        FX4(*),FY4(*),FZ4(*),
     .        A(3,*), STIFN(*), STIF(*), PENE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1,NODFI,ISHIFT
C-----------------------------------------------
C
      NODFI = NLSKYFIE(NIN)
      ISHIFT = NODFI*(JTASK-1)
C
      DO I=1,JLT
        IF(PENE(I)==ZERO) CYCLE
C
        IF(CS_LOC(I)<=NEDGE) THEN
          J1=N1(I)
          A(1,J1)=A(1,J1)+FX1(I)
          A(2,J1)=A(2,J1)+FY1(I)
          A(3,J1)=A(3,J1)+FZ1(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS1(I))
C
          J1=N2(I)
          A(1,J1)=A(1,J1)+FX2(I)
          A(2,J1)=A(2,J1)+FY2(I)
          A(3,J1)=A(3,J1)+FZ2(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS2(I))
        ELSE
          J1=N1(I)
          AFIE(NIN)%P(1,J1+ISHIFT)=AFIE(NIN)%P(1,J1+ISHIFT)+FX1(I)
          AFIE(NIN)%P(2,J1+ISHIFT)=AFIE(NIN)%P(2,J1+ISHIFT)+FY1(I)
          AFIE(NIN)%P(3,J1+ISHIFT)=AFIE(NIN)%P(3,J1+ISHIFT)+FZ1(I)
          STNFIE(NIN)%P(J1+ISHIFT) = STNFIE(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS1(I))
C
          J1=N2(I)
          AFIE(NIN)%P(1,J1+ISHIFT)=AFIE(NIN)%P(1,J1+ISHIFT)+FX2(I)
          AFIE(NIN)%P(2,J1+ISHIFT)=AFIE(NIN)%P(2,J1+ISHIFT)+FY2(I)
          AFIE(NIN)%P(3,J1+ISHIFT)=AFIE(NIN)%P(3,J1+ISHIFT)+FZ2(I)
          STNFIE(NIN)%P(J1+ISHIFT) = STNFIE(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS2(I))
        END IF
      END DO
C
      DO I=1,JLT
       IF(PENE(I)==ZERO) CYCLE
C
       J1=M1(I)
       A(1,J1)=A(1,J1)+FX3(I)
       A(2,J1)=A(2,J1)+FY3(I)
       A(3,J1)=A(3,J1)+FZ3(I)
       STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM1(I))
C
       J1=M2(I)
       A(1,J1)=A(1,J1)+FX4(I)
       A(2,J1)=A(2,J1)+FY4(I)
       A(3,J1)=A(3,J1)+FZ4(I)
       STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM2(I))
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I25ASSE05_E2S                 source/interfaces/int25/i25ass_e2s.F
Chd|-- called by -----------
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25ASSE05_E2S(JLT  ,CS_LOC ,N1    ,N2   ,M1   ,
     2                    M2   ,HS1   ,HS2   ,HM1  ,HM2  ,
     3                    FX1  ,FY1   ,FZ1   ,FX2  ,FY2  ,
     4                    FZ2  ,FX3   ,FY3   ,FZ3  ,FX4  ,
     5                    FY4  ,FZ4   ,A     ,STIFN,NEDGE ,
     6                    K1   ,K2    ,K3    ,K4   ,C1   ,
     7                    C2   ,C3    ,C4    ,VISCN,NIN  ,
     8                    JTASK,PENE  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NEDGE, NIN,
     +        CS_LOC(*),
     +        N1(*),N2(*),M1(*),M2(*),JTASK
      my_real
     .        HS1(*),HS2(*),HM1(*),HM2(*),
     .        FX1(*),FY1(*),FZ1(*),
     .        FX2(*),FY2(*),FZ2(*),
     .        FX3(*),FY3(*),FZ3(*),
     .        FX4(*),FY4(*),FZ4(*),
     .        K1(*),K2(*),K3(*),K4(*),
     .        C1(*),C2(*),C3(*),C4(*),
     .        A(3,*), STIFN(*), VISCN(*), PENE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1,NODFI,ISHIFT
C-----------------------------------------------
C
      NODFI = NLSKYFIE(NIN)
      ISHIFT = NODFI*(JTASK-1)
C
      DO I=1,JLT
        IF(PENE(I)==ZERO) CYCLE
C
        IF(CS_LOC(I)<=NEDGE) THEN
          J1=N1(I)
          A(1,J1)=A(1,J1)+FX1(I)
          A(2,J1)=A(2,J1)+FY1(I)
          A(3,J1)=A(3,J1)+FZ1(I)
          STIFN(J1)=STIFN(J1)+K1(I)
          VISCN(J1)=VISCN(J1)+C1(I)
C
          J1=N2(I)
          A(1,J1)=A(1,J1)+FX2(I)
          A(2,J1)=A(2,J1)+FY2(I)
          A(3,J1)=A(3,J1)+FZ2(I)
          STIFN(J1)=STIFN(J1)+K2(I)
          VISCN(J1)=VISCN(J1)+C2(I)
        ELSE
          J1=N1(I)
          AFIE(NIN)%P(1,J1+ISHIFT)=AFIE(NIN)%P(1,J1+ISHIFT)+FX1(I)
          AFIE(NIN)%P(2,J1+ISHIFT)=AFIE(NIN)%P(2,J1+ISHIFT)+FY1(I)
          AFIE(NIN)%P(3,J1+ISHIFT)=AFIE(NIN)%P(3,J1+ISHIFT)+FZ1(I)
          STNFIE(NIN)%P(J1+ISHIFT)=STNFIE(NIN)%P(J1+ISHIFT)+K1(I)
          VSCFIE(NIN)%P(J1+ISHIFT)=VSCFIE(NIN)%P(J1+ISHIFT)+C1(I)
C
          J1=N2(I)
          AFIE(NIN)%P(1,J1+ISHIFT)=AFIE(NIN)%P(1,J1+ISHIFT)+FX2(I)
          AFIE(NIN)%P(2,J1+ISHIFT)=AFIE(NIN)%P(2,J1+ISHIFT)+FY2(I)
          AFIE(NIN)%P(3,J1+ISHIFT)=AFIE(NIN)%P(3,J1+ISHIFT)+FZ2(I)
          STNFIE(NIN)%P(J1+ISHIFT)=STNFIE(NIN)%P(J1+ISHIFT)+K2(I)
          VSCFIE(NIN)%P(J1+ISHIFT)=VSCFIE(NIN)%P(J1+ISHIFT)+C2(I)
        END IF
      END DO
C
      DO I=1,JLT
       IF(PENE(I)==ZERO) CYCLE
C
       J1=M1(I)
       A(1,J1)=A(1,J1)+FX3(I)
       A(2,J1)=A(2,J1)+FY3(I)
       A(3,J1)=A(3,J1)+FZ3(I)
       STIFN(J1)=STIFN(J1)+K3(I)
       VISCN(J1)=VISCN(J1)+C3(I)
C
       J1=M2(I)
       A(1,J1)=A(1,J1)+FX4(I)
       A(2,J1)=A(2,J1)+FY4(I)
       A(3,J1)=A(3,J1)+FZ4(I)
       STIFN(J1)=STIFN(J1)+K4(I)
       VISCN(J1)=VISCN(J1)+C4(I)
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I25ASSE2_E2S                  source/interfaces/int25/i25ass_e2s.F
Chd|-- called by -----------
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25ASSE2_E2S(JLT   ,CS_LOC ,N1    ,N2    ,M1      ,
     2                   M2    ,HS1    ,HS2   ,HM1   ,HM2     ,
     3                   FX1   ,FY1    ,FZ1   ,FX2   ,FY2     ,
     4                   FZ2   ,FX3    ,FY3   ,FZ3   ,FX4     ,
     5                   FY4   ,FZ4    ,FSKYI ,ISKY  ,NISKYFIE,
     6                   STIF  ,NEDGE   ,NIN   ,NOINT ,PENE    ,
     7                   EDGE_ID,TAGIP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: EDGE_ID(2,4*MVSIZ)
      INTEGER JLT, NEDGE,NISKYFIE,NIN,NOINT,
     +        CS_LOC(*),ISKY(*),
     +        N1(*),N2(*),M1(*),M2(*)
      INTEGER , INTENT(IN) :: TAGIP(MVSIZ)
      my_real
     .        HS1(*),HS2(*),HM1(*),HM2(*),
     .        FX1(*),FY1(*),FZ1(*),
     .        FX2(*),FY2(*),FZ2(*),
     .        FX3(*),FY3(*),FZ3(*),
     .        FX4(*),FY4(*),FZ4(*),
     .        FSKYI(LSKYI,NFSKYI), STIF(*), PENE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NISKYL1, NISKYL,IGP,IGM, NISKYFIEL
C
      NISKYL1 = 0
      DO I = 1, JLT
        IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
        IF (HM1(I)/=ZERO.OR.TAGIP(I)==1) NISKYL1 = NISKYL1 + 1
        IF (HM2(I)/=ZERO.OR.TAGIP(I)==1) NISKYL1 = NISKYL1 + 1
      ENDDO

      IGP = 0
      IGM = 0
      DO I=1,JLT
        IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
        IF(CS_LOC(I)<=NEDGE) THEN
           IGP = IGP+2
        ELSE
           IGM = IGM+1
        ENDIF
      ENDDO 
       
#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIEL = NISKYFIE
      NISKYFIE = NISKYFIE + IGM
#include "lockoff.inc"

      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIEL+IGM > NLSKYFIE(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
      DO I=1,JLT
        IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
C
        IF(CS_LOC(I)<=NEDGE) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX1(I)
          FSKYI(NISKYL,2)=FY1(I)
          FSKYI(NISKYL,3)=FZ1(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HS1(I))
          ISKY(NISKYL) = N1(I)
C
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX2(I)
          FSKYI(NISKYL,2)=FY2(I)
          FSKYI(NISKYL,3)=FZ2(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HS2(I))
          ISKY(NISKYL) = N2(I)

#ifdef WITH_ASSERT         
          WRITE(6,"(2I20,X,A,6Z20)") EDGE_ID(1,I),EDGE_ID(2,I),"A",FX1(I),FY1(I),FZ1(I),FX2(I),FY2(I),FZ2(I)
#endif
 
        ELSE
          NISKYFIEL = NISKYFIEL + 1
          FSKYFIE(NIN)%P(1,NISKYFIEL)=FX1(I)
          FSKYFIE(NIN)%P(2,NISKYFIEL)=FY1(I)
          FSKYFIE(NIN)%P(3,NISKYFIEL)=FZ1(I)
          FSKYFIE(NIN)%P(4,NISKYFIEL)=STIF(I)*ABS(HS1(I))
          FSKYFIE(NIN)%P(5,NISKYFIEL)=FX2(I)
          FSKYFIE(NIN)%P(6,NISKYFIEL)=FY2(I)
          FSKYFIE(NIN)%P(7,NISKYFIEL)=FZ2(I)
          FSKYFIE(NIN)%P(8,NISKYFIEL)=STIF(I)*ABS(HS2(I))
          ISKYFIE(NIN)%P(NISKYFIEL) = CS_LOC(I)-NEDGE
C         ASSERT(CS_LOC(I) - NEDGE > 0)

#ifdef WITH_ASSERT
          WRITE(6,"(2I20,X,A,6Z20)") EDGE_ID(1,I),EDGE_ID(2,I),"B",FX1(I),FY1(I),FZ1(I),FX2(I),FY2(I),FZ2(I)
#endif
        END IF
      END DO
C
      DO I=1,JLT
       IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
C
       IF (HM1(I)/=ZERO.OR.TAGIP(I)==1) THEN
        NISKYL = NISKYL + 1
        FSKYI(NISKYL,1)=FX3(I)
        FSKYI(NISKYL,2)=FY3(I)
        FSKYI(NISKYL,3)=FZ3(I)
        FSKYI(NISKYL,4)=STIF(I)*ABS(HM1(I))
        ISKY(NISKYL) = M1(I)
#ifdef WITH_ASSERT
        WRITE(6,"(2I20,X,A,3Z20)") EDGE_ID(1,I),EDGE_ID(2,I),"C",FX3(I),FY3(I),FZ3(I)
#endif
       ENDIF
      ENDDO
      DO I=1,JLT
       IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
C
       IF (HM2(I)/=ZERO.OR.TAGIP(I)==1) THEN
        NISKYL = NISKYL + 1
        FSKYI(NISKYL,1)=FX4(I)
        FSKYI(NISKYL,2)=FY4(I)
        FSKYI(NISKYL,3)=FZ4(I)
        FSKYI(NISKYL,4)=STIF(I)*ABS(HM2(I))
        ISKY(NISKYL) = M2(I)
#ifdef WITH_ASSERT
        WRITE(6,"(2I20,X,A,3Z20)") EDGE_ID(1,I),EDGE_ID(2,I),"D",FX4(I),FY4(I),FZ4(I)
#endif
       ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I25ASSE25_E2S                 source/interfaces/int25/i25ass_e2s.F
Chd|-- called by -----------
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25ASSE25_E2S(JLT   ,CS_LOC ,N1    ,N2      ,M1    ,
     2                    M2    ,HS1    ,HS2   ,HM1     ,HM2   ,
     3                    FX1   ,FY1    ,FZ1   ,FX2     ,FY2   ,
     4                    FZ2   ,FX3    ,FY3   ,FZ3     ,FX4   ,
     5                    FY4   ,FZ4    ,ISKY  ,NISKYFIE,NEDGE  ,
     6                    K1    ,K2     ,K3    ,K4      ,C1    ,
     7                    C2    ,C3     ,C4    ,NIN     ,NOINT ,
     8                    PENE  ,TAGIP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NEDGE,NISKYFIE,NIN,NOINT,
     +        CS_LOC(*),ISKY(*),
     +        N1(*),N2(*),M1(*),M2(*)
      my_real
     .        HS1(*),HS2(*),HM1(*),HM2(*),
     .        FX1(*),FY1(*),FZ1(*),
     .        FX2(*),FY2(*),FZ2(*),
     .        FX3(*),FY3(*),FZ3(*),
     .        FX4(*),FY4(*),FZ4(*),
     .        K1(*),K2(*),K3(*),K4(*),
     .        C1(*),C2(*),C3(*),C4(*),
     .        FSKYI(LSKYI,NFSKYI), PENE(*)
      INTEGER , INTENT(IN) :: TAGIP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NISKYL1, NISKYL,IGP,IGM, NISKYFIEL
C
      NISKYL1 = 0
      DO I = 1, JLT
        IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
        IF (HM1(I)/=ZERO.OR.TAGIP(I)==1) NISKYL1 = NISKYL1 + 1
        IF (HM2(I)/=ZERO.OR.TAGIP(I)==1) NISKYL1 = NISKYL1 + 1
      ENDDO

      IGP = 0
      IGM = 0
      DO I=1,JLT
        IF(PENE(I) == ZERO.AND.TAGIP(I)==0) CYCLE
        IF(CS_LOC(I)<=NEDGE) THEN
           IGP = IGP+2
        ELSE
           IGM = IGM+1
        ENDIF
      ENDDO        

#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIEL = NISKYFIE
      NISKYFIE = NISKYFIE + IGM
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
      IF (NISKYFIEL+IGM > NLSKYFIE(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
      DO I=1,JLT
        IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
C
        IF(CS_LOC(I)<=NEDGE) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX1(I)
          FSKYI(NISKYL,2)=FY1(I)
          FSKYI(NISKYL,3)=FZ1(I)
          FSKYI(NISKYL,4)=K1(I)
          FSKYI(NISKYL,5)=C1(I)
          ISKY(NISKYL) = N1(I)
C
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX2(I)
          FSKYI(NISKYL,2)=FY2(I)
          FSKYI(NISKYL,3)=FZ2(I)
          FSKYI(NISKYL,4)=K2(I)
          FSKYI(NISKYL,5)=C2(I)
          ISKY(NISKYL) = N2(I)
        ELSE
          NISKYFIEL = NISKYFIEL + 1
          FSKYFIE(NIN)%P(1,NISKYFIEL)=FX1(I)
          FSKYFIE(NIN)%P(2,NISKYFIEL)=FY1(I)
          FSKYFIE(NIN)%P(3,NISKYFIEL)=FZ1(I)
          FSKYFIE(NIN)%P(4,NISKYFIEL)=K1(I)
          FSKYFIE(NIN)%P(5,NISKYFIEL)=C1(I)
          FSKYFIE(NIN)%P(6,NISKYFIEL)=FX2(I)
          FSKYFIE(NIN)%P(7,NISKYFIEL)=FY2(I)
          FSKYFIE(NIN)%P(8,NISKYFIEL)=FZ2(I)
          FSKYFIE(NIN)%P(9,NISKYFIEL)=K2(I)
          FSKYFIE(NIN)%P(10,NISKYFIEL)=C2(I)
          ISKYFIE(NIN)%P(NISKYFIEL) = CS_LOC(I)-NEDGE
        END IF
      END DO
C
      DO I=1,JLT
        IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
C
        IF (HM1(I)/=ZERO.OR.TAGIP(I)==1) THEN
         NISKYL = NISKYL + 1
         FSKYI(NISKYL,1)=FX3(I)
         FSKYI(NISKYL,2)=FY3(I)
         FSKYI(NISKYL,3)=FZ3(I)
         FSKYI(NISKYL,4)=K3(I)
         FSKYI(NISKYL,5)=C3(I)
         ISKY(NISKYL) = M1(I)
        ENDIF
      ENDDO
      DO I=1,JLT
        IF(PENE(I)==ZERO.AND.TAGIP(I)==0) CYCLE
C
        IF (HM2(I)/=ZERO.OR.TAGIP(I)==1) THEN
         NISKYL = NISKYL + 1
         FSKYI(NISKYL,1)=FX4(I)
         FSKYI(NISKYL,2)=FY4(I)
         FSKYI(NISKYL,3)=FZ4(I)
         FSKYI(NISKYL,4)=K4(I)
         FSKYI(NISKYL,5)=C4(I)
         ISKY(NISKYL) = M2(I)
        ENDIF
      ENDDO
C
      RETURN
      END
