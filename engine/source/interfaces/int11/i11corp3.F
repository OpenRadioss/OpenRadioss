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
Chd|  I11CORP3                      source/interfaces/int11/i11corp3.F
Chd|-- called by -----------
Chd|        I11FORCF3                     source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11CORP3(JLT   ,X     ,IRECTS,IRECTM  ,CAND_E  ,
     1                   CAND_N,STIF  ,HS1   ,HS2     ,HM1     ,
     2                   HM2   ,NX    ,NY    ,NZ      ,N1      ,
     3                   N2    ,M1    ,M2    ,VXS1    ,VYS1    ,
     4                   VZS1  ,VXS2  ,VYS2  ,VZS2    ,VXM1    ,
     5                   VYM1  ,VZM1  ,VXM2  ,VYM2    ,VZM2    ,
     6                   DXS1  ,DYS1  ,DZS1  ,DXS2    ,DYS2    ,  
     7                   DZS2  ,DXM1  ,DYM1  ,DZM1    ,DXM2    ,  
     8                   DYM2  ,DZM2  ,MS1   ,MS2     ,MM1     ,
     9                   MM2   ,MS    ,D     ,V       ,NRTS    , 
     A                   NIN   ,JLT_NEW,LREM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECTS(2,*),IRECTM(2,*), CAND_E(*), CAND_N(*),
     .         JLT,JLT_NEW,NIN,NRTS,LREM
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ)
C     REAL
      my_real
     .   X(3,*),D(3,*),  V(3,*), MS(*),
     .   HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), STIF(MVSIZ),
     .   MS1(MVSIZ),MS2(MVSIZ),MM1(MVSIZ),MM2(MVSIZ),
     .   VXS1(MVSIZ),VYS1(MVSIZ),VZS1(MVSIZ),VXS2(MVSIZ),VYS2(MVSIZ),
     .   VZS2(MVSIZ),VXM1(MVSIZ),VYM1(MVSIZ),VZM1(MVSIZ),VXM2(MVSIZ),
     .   VYM2(MVSIZ),VZM2(MVSIZ),DXS1(MVSIZ),DYS1(MVSIZ),DZS1(MVSIZ),
     .   DXS2(MVSIZ),DYS2(MVSIZ),DZS2(MVSIZ),DXM1(MVSIZ),DYM1(MVSIZ),
     .   DZM1(MVSIZ),DXM2(MVSIZ),DYM2(MVSIZ),DZM2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,NI, L, NN, NS ,NE,NN1,NN2
C-----------------------------------------------
C
C
        DO I=1,JLT
         NI = CAND_N(I)
         L  = CAND_E(I)
         IF(NI>NRTS)THEN
           NN = NI - NRTS
           JLT_NEW = JLT_NEW + 1
           NN1=2*(NN-1)+1
           NN2=2*NN
           M1(JLT_NEW)=IRECTM(1,L)
           M2(JLT_NEW)=IRECTM(2,L)
           VXS1(JLT_NEW) = VFI(NIN)%P(1,NN1)
           VYS1(JLT_NEW) = VFI(NIN)%P(2,NN1)
           VZS1(JLT_NEW) = VFI(NIN)%P(3,NN1)
           VXS2(JLT_NEW) = VFI(NIN)%P(1,NN2)
           VYS2(JLT_NEW) = VFI(NIN)%P(2,NN2)
           VZS2(JLT_NEW) = VFI(NIN)%P(3,NN2)
           VXM1(JLT_NEW) = V(1,M1(JLT_NEW))
           VYM1(JLT_NEW) = V(2,M1(JLT_NEW))
           VZM1(JLT_NEW) = V(3,M1(JLT_NEW))
           VXM2(JLT_NEW) = V(1,M2(JLT_NEW))
           VYM2(JLT_NEW) = V(2,M2(JLT_NEW))
           VZM2(JLT_NEW) = V(3,M2(JLT_NEW))
           DXM1(JLT_NEW) = D(1,M1(JLT_NEW))
           DYM1(JLT_NEW) = D(2,M1(JLT_NEW))
           DZM1(JLT_NEW) = D(3,M1(JLT_NEW))
           DXM2(JLT_NEW) = D(1,M2(JLT_NEW))
           DYM2(JLT_NEW) = D(2,M2(JLT_NEW))
           DZM2(JLT_NEW) = D(3,M2(JLT_NEW))
           MS1(JLT_NEW) = MSFI(NIN)%P(NN1)
           MS2(JLT_NEW) = MSFI(NIN)%P(NN2)
           MM1(JLT_NEW) = MS(M1(JLT_NEW))
           MM2(JLT_NEW) = MS(M2(JLT_NEW))
C
             NS=IND_INT(NIN)%P(NN)
             NE=SHF_INT(NIN)+JLT_NEW+LREM
           HS1(JLT_NEW)  = H_E(1,NE)
           HS2(JLT_NEW)  = H_E(2,NE)
           HM1(JLT_NEW)  = H_E(3,NE)
           HM2(JLT_NEW)  = H_E(4,NE)
             STIF(JLT_NEW)=STIFS(NE)
             NX(JLT_NEW)=N_E(1,NE)
             NY(JLT_NEW)=N_E(2,NE)
             NZ(JLT_NEW)=N_E(3,NE)
C----------displacement
             DXS1(JLT_NEW)=DFI(1,NS)
             DYS1(JLT_NEW)=DFI(2,NS)
             DZS1(JLT_NEW)=DFI(3,NS)
             DXS2(JLT_NEW)=DFI(1,NS+1)
             DYS2(JLT_NEW)=DFI(2,NS+1)
             DZS2(JLT_NEW)=DFI(3,NS+1)
           N1(JLT_NEW)=NS
           N2(JLT_NEW)=NS+1
C
         END IF
C
        END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  I11CORKP3                     source/interfaces/int11/i11corp3.F
Chd|-- called by -----------
Chd|        I11FKU3                       source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE I11CORKP3(JLT   ,
     1                    XXS1   ,XXS2    ,XYS1    ,XYS2   ,XZS1   ,
     2                    XZS2   ,DXS1    ,DYS1    ,DZS1   ,DXS2   ,  
     3                    DYS2   ,DZS2    ,D       ,CAND_N ,NRTS   ,
     4                    N1     ,N2      ,NIN     ,IUPD   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_N(*),JLT,NIN,NRTS,IUPD
      INTEGER N1(MVSIZ), N2(MVSIZ)
C     REAL
      my_real
     .   DXS1(MVSIZ),DYS1(MVSIZ),DZS1(MVSIZ),
     .   DXS2(MVSIZ),DYS2(MVSIZ),DZS2(MVSIZ),
     .   XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .   XZS1(MVSIZ), XZS2(MVSIZ),D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,NI, L, NN, NS ,NE,NN1,NN2
C-----------------------------------------------
C
        DO I=1,JLT
         NI = CAND_N(I)
         IF(NI>NRTS)THEN
           NN = NI - NRTS
             NS=IND_INT(NIN)%P(NN)
C----------displacement
             DXS1(I)=DFI(1,NS)
             DYS1(I)=DFI(2,NS)
             DZS1(I)=DFI(3,NS)
             DXS2(I)=DFI(1,NS+1)
             DYS2(I)=DFI(2,NS+1)
             DZS2(I)=DFI(3,NS+1)
           ELSE
            NN1 = N1(I)
            NN2 = N2(I)
            DXS1(I) = D(1,NN1)
            DYS1(I) = D(2,NN1)
            DZS1(I) = D(3,NN1)
            DXS2(I) = D(1,NN2)
            DYS2(I) = D(2,NN2)
            DZS2(I) = D(3,NN2)
         END IF
C----------update
         IF(IUPD>0.AND.NI>NRTS)THEN
          XXS1(I) = XXS1(I) + DXS1(I)
          XYS1(I) = XYS1(I) + DYS1(I)
          XZS1(I) = XZS1(I) + DZS1(I)
          XXS2(I) = XXS2(I) + DXS2(I)
          XYS2(I) = XYS2(I) + DYS2(I)
          XZS2(I) = XZS2(I) + DZS2(I)
         END IF
C
        END DO
C
      RETURN
      END
