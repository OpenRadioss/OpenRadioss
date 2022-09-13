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
Chd|  SBOLTLAW                      source/elements/solid/solide/sboltlaw.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|====================================================================
      SUBROUTINE SBOLTLAW  (PM      ,SIG     ,MAT     ,D1     ,
     1                      D2      ,D3      ,D4      ,D5     ,
     2                      D6      ,NEL     ,RHO     ,BPRELD ,  
     3                      EINT    ,QOLD    ,VOL0    ,STIFN  ,
     4                      DT2T    ,NELTST  ,ITYPTST ,OFFG   ,
     5                      GEO     ,PID     ,
     7                      MUMAX   ,NGL     ,SSP     ,DVOL   ,
     8                      AIRE    ,VNEW    ,VD2     ,DELTAX ,
     9                      VIS     ,PNEW    ,PSH     ,QNEW   ,  
     A                      SSP_EQ  ,SOLD1   ,SOLD2   ,SOLD3  ,
     B                      SOLD4   ,SOLD5   ,SOLD6   ,MSSA   ,
     C                      DMELS   ,CONDE   ,AMU     ,VOL_AVG,
     D                      DTEL    ,G_DT    ,OFF     ,IPM    ,
     E                      RHOREF  ,RHOSP   ,VOL0DP  )
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL
      my_real
     .   PM(NPROPM,*), SIG(NEL,6), RHO(MVSIZ),
     .   D1(*), D2(*), D3(*), D4(*),D5(*), D6(*),
     .   BPRELD(NEL,*), OFF(*)
      INTEGER NELTST,ITYPTST,PID(*),G_DT
      INTEGER MAT(*),NGL(*),IPM(NPROPMI,*)
      my_real
     .   DT2T

      my_real
     .   EINT(*), QOLD(*),
     .   VOL0(*),STIFN(*), OFFG(*),GEO(NPROPG,*),MUMAX(*)
      my_real
     .   VNEW(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .   PSH(*), PNEW(*),QNEW(*) ,SSP_EQ(*), DVOL(*),
     .   SOLD1(*), SOLD2(*), SOLD3(*), SOLD4(*), SOLD5(*), SOLD6(*),
     .   MSSA(*), DMELS(*),CONDE(*),AMU(*),VOL_AVG(*),DTEL(*),
     .   RHOREF(*)  ,RHOSP(*) 
      DOUBLE PRECISION VOL0DP(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX,IBID !, J
      my_real
     .   REDUC ,REDUC1 , T1, T2 , EE, NU, GGT, TREPS, P, TP 
      my_real
     .   C1(MVSIZ), LMBD(MVSIZ), GG(MVSIZ),  
     .   RHO0(MVSIZ), RHO0_1,C1_1 
C-----------------------------------------------
      my_real
     .   DF,DPDM,FACQ0,
     .   E1, E2, E3, E4, E5, E6, EINC,
     .   BID1, BID2, BID3, DTA,  YM, DPDMP
C-----------------------------------------------
      FACQ0 = ONE
      REDUC1 = EM04 !EM03 !ONE 

      DO I=LFT,LLT
       T1 = BPRELD(I,1)+ZEP4*(BPRELD(I,2)-BPRELD(I,1))
       T2 = BPRELD(I,1)+ZEP7*(BPRELD(I,2)-BPRELD(I,1))
       REDUC = REDUC1
       TP = TT-T1
       IF(TP > ZERO ) THEN  
         REDUC = MIN(REDUC1*(ONE-TP/(T2-T1))+TP/(T2-T1), ONE)
       ENDIF
C       
       MX = MAT(I)
       RHO0_1 = PM(1, MX)      
       NU = PM(21, MX)
       EE = PM(20, MX)*REDUC
       GG(I) = PM(22, MX)*REDUC
       C1_1  = PM(32, MX)   
       LMBD(I) = DT1*EE*NU/((ONE+NU)*(ONE-TWO*NU))
       C1(I) = C1_1*REDUC 
       RHO0(I) =RHO0_1  
C
       !SSP(I)=SQRT((ONEP333*GG(I)+C1(I))/RHO0(I))
       SSP(I)=SQRT((ONEP333*PM(22, MX)+C1_1)/RHO0(I))
      ENDDO
C
      IF (JSPH==0)THEN
       CALL MQVISCB(PM,OFF     ,RHO    ,BID1  ,BID2    ,SSP    ,
     2        BID3    ,STIFN   ,DT2T   ,NELTST,ITYPTST ,AIRE   ,
     3        OFFG    ,GEO     ,PID    ,VNEW   ,VD2    ,DELTAX ,
     4        VIS     ,D1      ,D2     ,D3     ,PNEW  ,PSH     ,
     5        MAT     ,NGL     ,QNEW   ,SSP_EQ ,VOL0  ,MSSA    ,
     6        DMELS   ,IBID    ,FACQ0   ,CONDE  ,DTEL  ,G_DT   ,
     7        IPM     ,RHOREF  ,RHOSP   )
      ELSE
       CALL MDTSPH(PM ,OFF     ,RHO     ,BID1    ,BID2    ,
     2        BID3    ,STIFN   ,DT2T    ,NELTST  ,ITYPTST ,
     3        OFFG    ,GEO     ,PID     ,MUMAX   ,SSP     ,
     4        VNEW    ,VD2     ,DELTAX  ,VIS     ,D1      ,
     5        D2      ,D3      ,PNEW    ,PSH     ,MAT     ,
     6        NGL     ,QNEW    ,SSP_EQ  ,G_DT    ,DTEL)
      ENDIF

      DTA =HALF*DT1
C       
      DO I=LFT,LLT
        PNEW(I)  = C1(I)*AMU(I)                         !
        TREPS    = D1(I)+D2(I)+D3(I)
        GGT      = DT1*GG(I)
        SIG(I,1) = SIG(I,1)+TWO*GGT*D1(I)+LMBD(I)*TREPS
        SIG(I,2) = SIG(I,2)+TWO*GGT*D2(I)+LMBD(I)*TREPS
        SIG(I,3) = SIG(I,3)+TWO*GGT*D3(I)+LMBD(I)*TREPS
        SIG(I,4) = SIG(I,4)+GGT*D4(I)
        SIG(I,5) = SIG(I,5)+GGT*D5(I)
        SIG(I,6) = SIG(I,6)+GGT*D6(I)
        QOLD(I)  = QNEW(I)
      ENDDO
      !write(*,'("***** sboltlaw ")')
      !DO I=LFT,LLT
      !  write(*,'(i5,6e10.3)')i,sig(i,1:6)
      !enddo
C
C - To avoid discontinuous element pressure when switching material law 
C   cf Law1, Law2, etc <=> Pressure is computed using total formulation P = C1*(rho/rho0-1)
      IF(ISMSTR == 1)THEN
        IF(IRESP==0)THEN
          DO I=LFT,LLT
           IF(TT >= (BPRELD(I,2)-TWO*DT1)) THEN
             P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
             RHO(I)=RHO0_1*(ONE+P/C1_1) ! rho will be used in next cycles to compute 
                                       ! rho_new = rho_old - rho0 dV
                                       ! AMU = rho/rho0 - 1 in double precision
           ENDIF
          ENDDO
        ELSE ! Single precision
          DO I=LFT,LLT
           IF(TT >= (BPRELD(I,2)-TWO*DT1)) THEN
             P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
             RHO(I)=RHO0_1*(ONE+P/C1_1)
             VOL0DP(I) = VOL0(I)*(ONE+P/C1_1) ! VOL0DP will be used in next cycle to compute
                                             ! AMU = VOL0DP/VOLDP - 1 in single precision, VOLDP=V0 in small strain
           ENDIF
          ENDDO
        END IF
      ELSEIF(ISMSTR==2)THEN
        IF(IRESP==0)THEN
          DO I=LFT,LLT
            IF(OFFG(I) > ONE)THEN
              IF(TT >= (BPRELD(I,2)-TWO*DT1)) THEN
                P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
                RHO(I)=RHO0_1*(ONE+P/C1_1) ! rho will be used in next cycles to compute 
                                          ! rho_new = rho_old - rho0 dV
                                          ! AMU = rho/rho0 - 1 in double precision
              ENDIF
            ELSE
              P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
              VOL0(I)   = VNEW(I)*(ONE+P/C1_1)
              RHO(I)    = RHO0_1*VOL0(I)/VNEW(I) ! cf rho = rho0 * V0 / V at each time step
                                                 ! AMU = rho/rho0 - 1 in double precision

            END IF
          ENDDO
        ELSE ! Single precision
          DO I=LFT,LLT
            IF(OFFG(I) > ONE)THEN
              IF(TT >= (BPRELD(I,2)-TWO*DT1)) THEN
                P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
                RHO(I)    = RHO0_1*(ONE+P/C1_1)
                VOL0DP(I) = VOL0(I)*(ONE+P/C1_1) ! VOL0DP will be used in next cycle to compute
                                                ! AMU = VOL0DP/VOLDP - 1 in single precision, VOLDP=V0 in small strain
               ENDIF
            ELSE
              P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
              VOL0(I)   = VNEW(I)*(ONE+P/C1_1)
              VOL0DP(I) = VOL0(I)  ! cf rho = rho0 * VOL0DP / VOLDP
                                   ! AMU = VOL0DP/VOLDP - 1 in single precision, VOLDP = V in large strain
              RHO(I)    = RHO0_1*VOL0(I)/VNEW(I)
            END IF
          ENDDO
        END IF
      ELSEIF(ISMSTR<=4)THEN
        IF(IRESP==0)THEN
          DO I=LFT,LLT
            P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
            VOL0(I)=VNEW(I)*(ONE+P/C1_1)
            RHO(I)= RHO0_1*VOL0(I)/VNEW(I) ! cf rho = rho0 * V0 / V at each time step
                                           ! AMU = rho/rho0 - 1 in double precision
          ENDDO
        ELSE ! Single precision
          DO I=LFT,LLT
            P = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
            VOL0(I)   = VNEW(I)*(ONE+P/C1_1)
            RHO(I)    = RHO0_1*VOL0(I)/VNEW(I)
            VOL0DP(I) = VOL0(I)  ! cf rho = rho0 * VOL0DP / VOLDP
                                 ! AMU = VOL0DP/VOLDP - 1 in single precision, VOLDP = V in large strain
          ENDDO
        END IF
      END IF
C
      DO I=LFT,LLT
        E1=D1(I)*(SOLD1(I)+SIG(I,1))
        E2=D2(I)*(SOLD2(I)+SIG(I,2))
        E3=D3(I)*(SOLD3(I)+SIG(I,3))
        E4=D4(I)*(SOLD4(I)+SIG(I,4))
        E5=D5(I)*(SOLD5(I)+SIG(I,5))
        E6=D6(I)*(SOLD6(I)+SIG(I,6))
        EINC= ZERO !VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - HALF*DVOL(I)*(QOLD(I)+QNEW(I))
        EINT(I)=(EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL0(I))
      ENDDO
C
      RETURN
      END
