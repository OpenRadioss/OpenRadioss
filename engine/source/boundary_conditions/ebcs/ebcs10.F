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
Chd|  EBCS10                        source/boundary_conditions/ebcs/ebcs10.F
Chd|-- called by -----------
Chd|        EBCS_MAIN                     source/boundary_conditions/ebcs/ebcs_main.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE EBCS10(NSEG,ISEG,SEGVAR,
     .                 A,V,W,X,
     .                 LISTE,NOD,IRECT,IELEM,IFACE,
     .                 RO0,EN0,
     .                 LA,MS,STIFN,EBCS,IPARG,ELBUF_TAB,MULTI_FVM,IXQ,IXS,IXTG,
     .                 ITAB,NALE)
      USE EBCS_MOD
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE SEGVAR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "conv_c.inc"
#include      "scr11_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NSEG,NOD,ISEG(NSEG),LISTE(NOD),IRECT(4,NSEG),IELEM(NSEG),IFACE(NSEG)
      INTEGER,INTENT(IN) :: IXQ(NIXQ,NUMELQ),IXS(NIXS,NUMELS),IXTG(NIXTG,NUMELTG)
      INTEGER,INTENT(IN) :: ITAB(NUMNOD),NALE(NUMNOD)
      my_real,INTENT(INOUT) :: A(3,NUMNOD), MS(NUMNOD)
      my_real V(3,NUMNOD),W(3,NUMNOD),X(3,NUMNOD),RO0(NSEG),EN0(NSEG),LA(3,NOD),STIFN(NUMNOD)
      TYPE(t_ebcs_nrf), INTENT(INOUT) :: EBCS
      INTEGER :: IPARG(NPARG,NGROUP)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB      
      TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM
      TYPE(t_segvar),INTENT(INOUT) :: SEGVAR
c      my_real,INTENT(INOUT) :: UVAR(NEL,NUVAR)
c      INTEGER,INTENT(IN) :: NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      INTEGER II,IS,KK,K1,LL,KSEG,NN(4),NNG(4),NUM,KTY,KLT,MFT,NGRP,ILOC,NPT,IVOI,IDX(6),IX(4)
      INTEGER ICF_2d(2,4), ICF_3d(4,6), JJ, ISUBMAT, IPOS, NBMAT, MTN
      my_real
     .       ORIENT,RHO,C,FC,ROC,ALP,FAC1,FAC2,VOL,MASS,
     .       X13,Y13,Z13,X24,Y24,Z24,NX,NY,Z,S,
     .       ROOU,ENOU,VMX,VMY,VMZ,FLUXI,FLUXO,P,DVX,DVY,DVZ,
     .       ALPHA, BETA, V0(3,NOD),
     .       XSN,YSN,ZSN,
     .       XN, YN, ZN, TMP(3), Vold, Vnew, Pold, Pvois,
     .       DP0,MACH,PP,SSP,RHOC2,
     .       TCAR_P, TCAR_VF, SURF, TIME, EINT,PHASE_ALPHA(21),PHASE_RHO(21), PHASE_EINT(21)
      LOGICAL bFOUND
      TYPE(BUF_MAT_)  ,POINTER :: MBUF  
C-----------------------------------------------
      DATA ICF_2d  /1,2,2,3,3,4,4,1/
      DATA ICF_3d  /1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
C-----------------------------------------------
C   S o u r  c e   L i n e s
C-----------------------------------------------
      TCAR_P = EBCS%TCAR_P
      TCAR_VF = EBCS%TCAR_VF
      TIME = TT
      
      ALPHA = DT1/TCAR_P
      BETA = DT1/MAX(DT1,TCAR_VF)
      IF(TCAR_VF>=EP20)BETA=ZERO
      IF(DT1 == ZERO)THEN
        ALPHA = ONE !zero
        BETA = ONE
      ENDIF
      
      IF(IALE == 1)THEN 
        DO II=1,NOD
          NUM=LISTE(II)
          V0(1,II)=V(1,NUM)-W(1,NUM)
          V0(2,II)=V(2,NUM)-W(2,NUM)
          V0(3,II)=V(3,NUM)-W(3,NUM)
        ENDDO 
       ELSEIF(IEULER == 1)THEN
         DO II=1,NOD
           NUM=LISTE(II)
           V0(1,II)=V(1,NUM)
           V0(2,II)=V(2,NUM)
           V0(3,II)=V(3,NUM)
         ENDDO        
       ENDIF
        
        DO II=1,NOD
          NUM=LISTE(II)
          LA(1,II)=ZERO
          LA(2,II)=ZERO
          LA(3,II)=ZERO
        ENDDO     

        DO IS=1,NSEG
        
          KSEG=ABS(ISEG(IS))
          ORIENT=FLOAT(ISEG(IS)/KSEG)
                   
          !---OUTWARD NORMAL
          IF(N2D == 0)THEN
            JJ = IFACE(IS)  
            IX(1)=IXS(ICF_3d(1,JJ)+1,IELEM(IS))          
            IX(2)=IXS(ICF_3d(2,JJ)+1,IELEM(IS))          
            IX(3)=IXS(ICF_3d(3,JJ)+1,IELEM(IS))          
            IX(4)=IXS(ICF_3d(4,JJ)+1,IELEM(IS)) 
            X13=X(1,IX(3))-X(1,IX(1))           
            Y13=X(2,IX(3))-X(2,IX(1))           
            Z13=X(3,IX(3))-X(3,IX(1))           
            X24=X(1,IX(4))-X(1,IX(2))           
            Y24=X(2,IX(4))-X(2,IX(2))           
            Z24=X(3,IX(4))-X(3,IX(2))           
            XN=Y13*Z24-Z13*Y24             
            YN=Z13*X24-X13*Z24             
            ZN=X13*Y24-Y13*X24             
            FAC2=ONE/SQRT(XN**2+YN**2+ZN**2)  
            XN = XN*FAC2                     
            YN = YN*FAC2                     
            ZN = ZN*FAC2                               
            SURF = HALF/FAC2 
            IF(IX(4) == IX(3))THEN ; NPT=3;FAC1=THIRD; ELSE; NPT=4;FAC1=FOURTH; ENDIF
          ELSE
            FAC1=HALF
            NPT=2
            JJ = IFACE(IS)
            IF(NUMELTG>0)THEN
              IX(1)  = IXTG(ICF_2d(1,JJ)+1,IELEM(IS))
              IX(2)  = IXTG(ICF_2d(2,JJ)+1,IELEM(IS))
            ELSE
              IX(1)  = IXQ(ICF_2d(1,JJ)+1,IELEM(IS))
              IX(2)  = IXQ(ICF_2d(2,JJ)+1,IELEM(IS))
            ENDIF 
            XN = ZERO
            YN = -(-X(3,IX(2))+X(3,IX(1)))
            ZN =  (-X(2,IX(2))+X(2,IX(1)))                     
            FAC2 = ONE/SQRT(YN*YN+ZN*ZN) 
            YN=YN*FAC2
            ZN=ZN*FAC2                          
            SURF = ONE/FAC2               
          ENDIF

          !-- VELOCITY
          NN(1)=IRECT(1,IS)
          NN(2)=IRECT(2,IS)
          NN(3)=IRECT(3,IS)
          NN(4)=IRECT(4,IS)
          NNG(1)=LISTE(NN(1))
          NNG(2)=LISTE(NN(2))
          NNG(3)=LISTE(NN(3))
          NNG(4)=LISTE(NN(4))         
                    
          TMP(1:3) = ZERO
          DO KK=1,NPT
            TMP(1) = TMP(1) + V0(1,NN(KK))
            TMP(2) = TMP(2) + V0(2,NN(KK))
            TMP(3) = TMP(3) + V0(3,NN(KK))
          ENDDO
          Vold = EBCS%vold(IS)
          Vnew = FAC1 * (TMP(1)*XN + TMP(2)*YN + TMP(3)*ZN)
          IF(TIME == ZERO) Vold=Vnew
          !-- storage for next cycle
          EBCS%vold(IS) = Vnew  
          !Static-PRessure (increment)
          DP0 = EBCS%DP0(IS)
                    
          !-- ADJACENT STATE
          bFOUND = .FALSE.
          IVOI = IELEM(IS)                                  
          DO NGRP=1,NGROUP                                     
            KTY = IPARG(5,NGRP)                                
            KLT = IPARG(2,NGRP)                                
            MFT = IPARG(3,NGRP)
            IF(N2D==0)THEN
              IF(KTY /= 1)CYCLE
            ELSE
              IF(KTY /= 2 .AND. KTY /= 7)CYCLE
            ENDIF                                
             IF (IVOI <= KLT+MFT)THEN        
               bFOUND = .TRUE.                              
               EXIT                                         
             ENDIF                                          
          ENDDO                                             
          IF(.NOT.bFOUND)CYCLE !next I                      
          GBUF    => ELBUF_TAB(NGRP)%GBUF        
          LBUF    => ELBUF_TAB(NGRP)%BUFLY(1)%LBUF(1,1,1)   
          MTN = IPARG(1,NGRP)                     

          !PRESSURE                                         
          ILOC    =  IVOI-MFT-1                             
          DO KK=1,6                                          
            IDX(KK) = KLT*(KK-1)                               
          ENDDO                                             
          Pvois = -THIRD*(GBUF%SIG(IDX(1)+ILOC+1) + GBUF%SIG(IDX(2)+ILOC+1) + GBUF%SIG(IDX(3)+ILOC+1))           
          Pold = EBCS%pold(IS)
          IF(TIME == ZERO)Pold=Pvois+DP0 
          
          !DENSITY                                          
          RHO = GBUF%RHO(ILOC+1)

          !VOLUME
          VOL=GBUF%VOL(ILOC+1)
          MASS = RHO*VOL

          !SOUND SPEED 
          IF(MULTI_FVM%IS_USED)THEN
            SSP = MULTI_FVM%SOUND_SPEED(IVOI)
          ELSE
            SSP = LBUF%SSP(ILOC+1)
          ENDIF

          !ENERGY                                          
          EINT = GBUF%EINT(ILOC+1)
          
          !VOL FRAC          
          IF(MTN == 51)THEN
            !NUVAR = ELBUF_TAB(NGRP)%BUFLY(1)%NVAR_MAT 
            MBUF => ELBUF_TAB(NGRP)%BUFLY(1)%MAT(1,1,1)
            DO ISUBMAT=1,4
              IPOS = 1                                                                                                       
              KK = (N0PHAS + (ISUBMAT-1)*NVPHAS +IPOS-1) * KLT  +  ILOC+1                                                                                    
              PHASE_ALPHA(ISUBMAT) = MBUF%VAR(KK)                                     
              IPOS = 9                                                                                                       
              KK = (N0PHAS + (ISUBMAT-1)*NVPHAS +IPOS-1) * KLT  +  ILOC+1                                                                                    
              PHASE_RHO(ISUBMAT) = MBUF%VAR(KK)
              IPOS = 8                                                                                                       
              KK = (N0PHAS + (ISUBMAT-1)*NVPHAS +IPOS-1) * KLT  +  ILOC+1                                                                                    
              PHASE_EINT(ISUBMAT) = MBUF%VAR(KK)
            ENDDO!next ITRIMAT                                      
          ELSEIF(MTN == 151)THEN
            DO ISUBMAT=1,MULTI_FVM%NBMAT
              PHASE_ALPHA(ISUBMAT) = MULTI_FVM%PHASE_ALPHA(ISUBMAT,ILOC+1+KLT)
              PHASE_RHO(ISUBMAT) = MULTI_FVM%PHASE_RHO(ISUBMAT,ILOC+1+KLT)
              PHASE_EINT(ISUBMAT) = MULTI_FVM%PHASE_EINT(ISUBMAT,ILOC+1+KLT)                            
            ENDDO
          ENDIF
          NBMAT=EBCS%NBMAT
          !IF(TIME == ZERO)EBCS%PHASE_ALPHA(1:NBMAT,IS)=PHASE_ALPHA(1:NBMAT)
          
          !segment data for flux of volumes ( upwind/ACONVE() )
          SEGVAR%RHO(KSEG)=RHO
          SEGVAR%EINT(KSEG)=EINT          
          IF(SEGVAR%has_phase_alpha)SEGVAR%PHASE_ALPHA(1:4,KSEG)=PHASE_ALPHA(1:4)          
          IF(SEGVAR%has_phase_rho)SEGVAR%PHASE_RHO(1:4,KSEG)=PHASE_RHO(1:4)          
          IF(SEGVAR%has_phase_eint)SEGVAR%PHASE_EINT(1:4,KSEG)=PHASE_EINT(1:4)          

          !-- FORMULATION  
            MACH = ONE
            IF(SSP /= ZERO)MACH = ABS(Vnew / SSP)
          
            IF(MACH >= ONE .AND. Vnew > ZERO)THEN
              !vitesse sortante supersonique : etat = etat voisin
              PP          = Pvois
            ELSE
              RHOC2 = RHO*SSP*SSP 
              ROC   =SQRT(RHO*RHOC2)
              PP    = ONE/(ONE+ALPHA)*(Pold+ROC*(Vnew-Vold))+ALPHA*(Pvois+DP0)/(ALPHA + ONE)
            ENDIF
            EBCS%pold(IS) = PP   
            
            IF(SEGVAR%nbmat > 0)THEN
              !Volume Fractions update
              IF(Vnew < ZERO)THEN     !flux rentrant, relaxation du pourcentage volumique   
                IF(MTN == 51)THEN            
                  DO ISUBMAT=1,4 
                    ! (1-beta) VF_current + BETA VF_0
                    PHASE_ALPHA(ISUBMAT)=(ONE-BETA)*PHASE_ALPHA(ISUBMAT) 
     .                                  + BETA*MBUF%VAR((N0PHAS+(ISUBMAT-1)*NVPHAS +23-1)* KLT+ILOC+1)
                  ENDDO
                ELSEIF(MTN == 151)THEN
                
                ENDIF
              ENDIF 
              SEGVAR%PHASE_ALPHA(1:4,KSEG) = PHASE_ALPHA(1:4)
            ENDIF
            
           !expand pressure loading to segment nodes
           DO KK=1,NPT
             LA(1,NN(KK)) = LA(1,NN(KK)) -  PP*SURF*XN*FAC1
             LA(2,NN(KK)) = LA(2,NN(KK)) -  PP*SURF*YN*FAC1
             LA(3,NN(KK)) = LA(3,NN(KK)) -  PP*SURF*ZN*FAC1
           ENDDO         
          
        ENDDO      
        
      DO II=1,NOD
        NUM=LISTE(II)
        A(1,NUM)=A(1,NUM)+LA(1,II)
        A(2,NUM)=A(2,NUM)+LA(2,II)
        A(3,NUM)=A(3,NUM)+LA(3,II)
      ENDDO
      
         !DO II=1,NOD
         !  NUM=LISTE(II)
         !   FAC2 = SURF*ROC
         !   STIFN(NUM)=STIFN(NUM)+(TWO*FAC2*FAC2)/MS(NUM) 
         !ENDDO                           

      RETURN
      END



















