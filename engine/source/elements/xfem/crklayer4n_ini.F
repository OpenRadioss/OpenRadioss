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
Chd|  CRKLAYER4N_INI                source/elements/xfem/crklayer4n_ini.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        LSINT4                        source/elements/xfem/crklayer4n_adv.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CRKLAYER4N_INI(
     .           XFEM_STR ,NEL      ,NFT      ,IXC      ,ELCUTC   ,
     .           ILAY     ,NLAY     ,IEL_CRK  ,INOD_CRK ,
     .           IADC_CRK ,NODENR   ,ELCRKINI ,DIR1     ,DIR2     ,          
     .           NODEDGE  ,CRKNODIAD,KNOD2ELC ,CRKEDGE  ,A_I      ,      
     .           XL2      ,XL3      ,XL4      ,YL2      ,YL3      ,
     .           YL4      ,XEDGE4N  ,NGL      )
C-----------------------------------------------
C crack initialisation, shells 4N
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,ILAY,IXC(NIXC,*),NLAY,NGL(NEL),IEL_CRK(*),
     .  INOD_CRK(*),NODENR(*),IADC_CRK(4,*),ELCRKINI(NLAY,NEL),
     .  ELCUTC(2,*),NODEDGE(2,*),CRKNODIAD(*),KNOD2ELC(*),XEDGE4N(4,*)
C
      my_real, DIMENSION(NLAY,NEL) ::  DIR1,DIR2
      my_real, DIMENSION(NEL) ::  A_I
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL), TARGET :: XFEM_STR
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX)     :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,R,II,IR,P1,P2,PP1,PP2,PP3,NEWCRK,LAYCUT,FAC,
     .  IEDGE,ICUT,IED,IED1,IED2,ICRK,ELCRK,NOD1,NOD2,IE1,IE2,ITRI,
     .  IFI1,IFI2,IE10,IE20,NP,NX1,NX2,NX3,NX4
      INTEGER N(4),NN(4),dd(4),d(8),DX(8),
     .  ISIGN(4),IENR0(4),IENR(4),INV(2),ECUT(2,NEL),
     .  IADC(4),JCT(NEL),EDGEL(4,NEL),KPERM(8)
C
      my_real
     .   XM(NEL),YM(NEL),XL2(NEL),YL2(NEL),XL3(NEL),YL3(NEL),
     .   XL4(NEL),YL4(NEL),XIN(2,NEL),YIN(2,NEL),XMI(2),YMI(2),
     .   FIT(4,NEL),XXL(4,NEL),YYL(4,NEL),LEN(4,NEL),BETA0(4,NEL)
C
      my_real
     .   XINT,YINT,FI,XXX,YYY,ZZZ,D12,M12,MM,CROSS,ACD,BCD,DLX,DLY,
     .   XINT0,YINT0,DIR11,DIR22,BETA,BMIN,BMAX,
     .   X1,Y1,X2,Y2,X3,Y3,X4,Y4,AREA1,AREA2
c---
      DATA d/1,2,2,3,4,3,1,4/
      DATA dd/2,3,4,1/
      DATA DX/1,2,3,4,1,2,3,4/
      DATA KPERM/1,2,3,4,1,2,3,4/
      PARAMETER (BMIN = 0.01, BMAX = 0.99)
C=======================================================================
      NEWCRK = 0
      DO I=1,NEL
        JCT(I) = 0
        IF (ELCRKINI(ILAY,I) == -1) THEN   ! initialisation de fissure
          NEWCRK = NEWCRK + 1
          JCT(NEWCRK) = I
        ENDIF
      ENDDO
      IF (NEWCRK == 0) RETURN
c------------------
      II  = NXEL*(ILAY-1)
      PP1 = II + 1 
      PP2 = II + 2   
      PP3 = II + 3   
c------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        EDGEL(1:4,I) = 0
        ECUT(1:2,I)  = 0
        BETA0(1:4,I) = ZERO
        XIN(1,I)=ZERO  ! first inters point in local skew
        YIN(1,I)=ZERO
        XIN(2,I)=ZERO  ! second inters point in local skew
        YIN(2,I)=ZERO
c
        XXL(1,I) = ZERO
        YYL(1,I) = ZERO
        XXL(2,I) = XL2(I)
        YYL(2,I) = YL2(I)
        XXL(3,I) = XL3(I)
        YYL(3,I) = YL3(I)
        XXL(4,I) = XL4(I)
        YYL(4,I) = YL4(I)
        XM(I) = FOURTH*(XL2(I)+XL3(I)+XL4(I))
        YM(I) = FOURTH*(YL2(I)+YL3(I)+YL4(I))
c
        LEN(1,I) = XL2(I)*XL2(I) + YL2(I)*YL2(I)
        LEN(2,I) = (XL3(I)-XL2(I))*(XL3(I)-XL2(I))+
     .             (YL3(I)-YL2(I))*(YL3(I)-YL2(I))
        LEN(3,I) = (XL4(I)-XL3(I))*(XL4(I)-XL3(I))+
     .             (YL4(I)-YL3(I))*(YL4(I)-YL3(I))
        LEN(4,I) = XL4(I)*XL4(I) + YL4(I)*YL4(I)
      END DO
C------------------------------------------------
c     search for first intersected edge
C------------------------------------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)
C---
        DIR11 = -DIR2(ILAY,I)
        DIR22 =  DIR1(ILAY,I)
        FAC = 0
C---
        DO K=1,4
          IEDGE = XEDGE4N(K,ELCRK)                                      
          NOD1  = NODEDGE(1,IEDGE)                                      
          NOD2  = NODEDGE(2,IEDGE)                                      
          IF (NOD1 == IXC(K+1,I) .and. NOD2 == IXC(dd(K)+1,I)) THEN     
            p1 = K                                                      
            p2 = dd(K)                                                  
          ELSEIF (NOD2 == IXC(K+1,I).and.NOD1 == IXC(dd(K)+1,I)) THEN   
            p1 = dd(K)                                                  
            p2 = K                                                      
          ENDIF                                                         
c             
          IF (DIR11 == ZERO) THEN                                    
            DLX = XXL(p2,I) - XXL(p1,I)                              
            IF (DLX /= ZERO) THEN                                    
              DLY = YYL(p2,I) - YYL(p1,I)                              
              M12 = DLY / DLX                                          
              XINT = XM(I)                                             
              YINT = YYL(p1,I) + M12*(XINT-XXL(p1,I))                  
              IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO .and.      
     .            (YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN      
                FAC = 1                                                
                EXIT                                                              
              ENDIF                                                    
            ENDIF                                                    
c
          ELSEIF (DIR22 == ZERO) THEN                               
            DLY = YYL(p2,I) - YYL(p1,I)                              
            IF (DLY /= ZERO) THEN                                    
              DLX = XXL(p2,I) - XXL(p1,I)                              
              M12 = DLX / DLY                                          
              YINT = YM(I)                                             
              XINT = XXL(p1,I) + M12*(YINT-YYL(p1,I))                  
              IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO .and.      
     .            (YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN      
                FAC = 1                                                
                EXIT                                                              
              ENDIF                                                    
            ENDIF                                                    
c
          ELSEIF (DIR11 /= ZERO .AND. DIR22 /= ZERO) THEN
            DLX = XXL(p2,I) - XXL(p1,I)
            DLY = YYL(p2,I) - YYL(p1,I)
            MM = DIR22/DIR11   
            IF (DLX == ZERO) THEN
              XINT = XXL(p1,I)
              YINT = YM(I) + MM*(XINT-XM(I))
              IF ((YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN
                FAC = 1
              ENDIF
            ELSEIF (DLY == ZERO) THEN
              YINT = YYL(p1,I)
              XINT = XM(I) + (YM(I)-YYL(p1,I)) / MM
              IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO) THEN
                FAC = FAC + 1
                EXIT                                                              
              ENDIF
            ELSE
              M12 = DLY / DLX
              IF (MM /= M12) THEN
                XINT = (YM(I)-YYL(p1,I) + M12*XXL(p1,I) - MM*XM(I))/(M12-MM)
                YINT = YM(I) + MM*(XINT-XM(I))
                ACD = (YINT-YYL(p1,I))*(XM(I) - XXL(p1,I)) 
     .              - (XINT-XXL(p1,I))*(YM(I) - YYL(p1,I))  
                BCD = (YINT-YYL(p2,I))*(XM(I) - XXL(p2,I)) 
     .              - (XINT-XXL(p2,I))*(YM(I) - YYL(p2,I))  
                IF (ACD*BCD <= EM3) THEN
                  FAC = 1
                  EXIT                                                              
                ENDIF
              ENDIF
            ENDIF
          ENDIF
c
        ENDDO   ! K=1,4
c
        IF (FAC == 1) THEN                                                  
          CROSS = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2             
          BETA   = SQRT(CROSS / LEN(K,I))                                   
          IF (BETA > BMAX .OR. BETA < BMIN) THEN                            
            BETA = MAX(BETA, BMIN)                                          
            BETA = MIN(BETA, BMAX)                                          
            XINT = XXL(p1,I) + BETA*(XXL(p2,I)-XXL(p1,I))                   
            YINT = YYL(p1,I) + BETA*(YYL(p2,I)-YYL(p1,I))                   
          ENDIF                                                             
C---
          ECUT(FAC,I)= K                                                    
          XIN(FAC,I) = XINT                                                 
          YIN(FAC,I) = YINT                                                 
          EDGEL(K,I) = FAC                                                  
          BETA0(K,I) = BETA                                                 
        ELSE                                                               
          WRITE(IOUT,*) 'ERROR IN ADVANCING CRACK --- CHECK CRACK TIP'     
          CALL ARRET(2)                                                    
        ENDIF
c
      ENDDO     !  IR=1,NEWCRK    
C--------------------------------------------------
c     Search for second intersection (new cut edge)
C--------------------------------------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)
        XINT0 = XIN(1,I)
        YINT0 = YIN(1,I)
        DIR11 =-DIR2(ILAY,I)
        DIR22 = DIR1(ILAY,I)
C---
        K = ECUT(1,I)
        R = KPERM(K + 2)  ! second intersection must be on opposite edge                                             
        IEDGE = XEDGE4N(R,ELCRK)                                               
        NOD1  = NODEDGE(1,IEDGE)                                               
        NOD2  = NODEDGE(2,IEDGE)                                               
        IF (NOD1 == IXC(R+1,I) .and. NOD2 == IXC(dd(R)+1,I))THEN               
          p1 = R                                                               
          p2 = dd(R)                                                           
        ELSE IF (NOD2 == IXC(R+1,I).and.NOD1 == IXC(dd(R)+1,I))THEN            
          p1 = dd(R)                                                           
          p2 = R                                                               
        ENDIF                                                                  
c
        IF (DIR11 == ZERO) THEN                                             
          DLX = XXL(p2,I) - XXL(p1,I)                                       
          IF (DLX /= ZERO) THEN                                             
            DLY = YYL(p2,I) - YYL(p1,I)                                     
            M12 = DLY / DLX                                                 
            XINT = XM(I)                                                    
            YINT = YYL(p1,I) + M12*(XINT-XXL(p1,I))                         
            IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO .and.             
     .          (YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN             
              FAC = 2                                                      
            ENDIF                                                           
          ENDIF                                                             
c
        ELSEIF (DIR22 == ZERO) THEN                                         
          DLY = YYL(p2,I) - YYL(p1,I)                                       
          IF (DLY /= ZERO) THEN                                             
            DLX = XXL(p2,I) - XXL(p1,I)                                     
            M12 = DLX / DLY                                                 
            YINT = YM(I)                                                    
            XINT = XXL(p1,I) + M12*(YINT-YYL(p1,I))                         
            IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO .and.             
     .          (YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN             
              FAC = 2                                                       
            ENDIF                                                           
          ENDIF                                                             
c
        ELSEIF (DIR11 /= ZERO .AND. DIR22 /= ZERO) THEN                     
          DLX = XXL(p2,I) - XXL(p1,I)                                       
          DLY = YYL(p2,I) - YYL(p1,I)                                       
          MM = DIR22/DIR11                                                  
          IF (DLX == ZERO) THEN                                             
            XINT = XXL(p1,I)                                                
            YINT = YM(I) + MM*(XINT-XM(I))                                  
            IF ((YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN             
              FAC = 2                                                      
            ENDIF                                                           
          ELSEIF (DLY == ZERO) THEN                                         
            YINT = YYL(p1,I)                                                
            XINT = XM(I) + (YM(I)-YYL(p1,I)) / MM                           
            IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO) THEN             
              FAC = 2                                              
            ENDIF                                                           
          ELSE                                                              
            M12 = DLY / DLX                                                 
            IF (MM /= M12) THEN                                             
              XINT = (YM(I)-YYL(p1,I) + M12*XXL(p1,I) - MM*XM(I))/(M12-MM)  
              YINT = YM(I) + MM*(XINT-XM(I))                                
              ACD = (YINT-YYL(p1,I))*(XM(I) - XXL(p1,I))                    
     .            - (XINT-XXL(p1,I))*(YM(I) - YYL(p1,I))                    
              BCD = (YINT-YYL(p2,I))*(XM(I) - XXL(p2,I))                    
     .            - (XINT-XXL(p2,I))*(YM(I) - YYL(p2,I))                    
c              IF (ACD*BCD <= ZERO) THEN                                     
                FAC = 2                                                     
c              ENDIF                                                         
            ENDIF                                                           
          ENDIF                                                             
        ENDIF                                                               
                                                                               
        IF (FAC == 2) THEN                                                     
          CROSS = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2                
          BETA  = SQRT(CROSS / LEN(r,I))                                       
          IF (BETA > BMAX .OR. BETA < BMIN) THEN                               
            BETA = MAX(BETA, BMIN)                                             
            BETA = MIN(BETA, BMAX)                                             
            XINT = XXL(p1,I) + BETA*(XXL(p2,I)-XXL(p1,I))                   
            YINT = YYL(p1,I) + BETA*(YYL(p2,I)-YYL(p1,I))                      
          ENDIF                                                                
C
          ECUT(2,I) = R                                                        
          XIN(2,I)  = XINT                                                     
          YIN(2,I)  = YINT                                                     
          EDGEL(R,I)= 2                                                        
          BETA0(R,I)= BETA                                                     
        ENDIF                                                                  
      ENDDO
c----------------------------------------------------------------------
C    check for getting both intersections
c----------------------------------------------------------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        FAC = 0
        DO J=1,2
          K = ECUT(J,I)
          IF (EDGEL(K,I)==1 .or. EDGEL(K,I)==2) FAC=FAC+1
        ENDDO
        IF (FAC /= 2) THEN
          WRITE(IOUT,*) 'ERROR IN INITIATION CRACK.NO CUT EDGES'
          CALL ARRET(2)           
        ENDIF
      ENDDO
c----------------------------------------------------------------------
c     save cut edges numbers on each layer
c----------------------------------------------------------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)
        DO J=1,2
          K = ECUT(J,I)
          CRKEDGE(ILAY)%IEDGEC(K,ELCRK) = EDGEL(K,I)
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C    SIGN DISTANCE OF NEW CRACKED LAYER
C----------------------------------------------------------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        FIT(1,I)=ZERO
        FIT(2,I)=ZERO
        FIT(3,I)=ZERO
        FIT(4,I)=ZERO
        DO K=1,4
          p1 = K
          p2 = dd(K)
          IED = EDGEL(K,I)
          IF (IED > 0) THEN
            XMI(IED) = HALF*(XXL(p1,I)+XXL(p2,I))
            YMI(IED) = HALF*(YYL(p1,I)+YYL(p2,I))
          ENDIF
        ENDDO
C
        DO K=1,4
          FI = ZERO
          CALL LSINT4(XMI(1),YMI(1),XMI(2),YMI(2),XXL(K,I),YYL(K,I),FI )
          IF (FIT(K,I)==ZERO) FIT(K,I) = FI
        ENDDO
      ENDDO
C-------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)
        DO J=1,2
          K = ECUT(J,I)
          IEDGE = XEDGE4N(K,ELCRK)                                                            
          ICUT = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)                                                
          IF (ICUT > 0) THEN   ! edge connecting two cracks (for spmd                         
            CRKEDGE(ILAY)%ICUTEDGE(IEDGE) = 3   ! 2 cracks sur le meme edge
          ELSE                                                                                
            CRKEDGE(ILAY)%ICUTEDGE(IEDGE) = 2  ! edge cut                                     
            CRKEDGE(ILAY)%RATIO(IEDGE) = BETA0(K,I)                                           
          ENDIF                                                                               
        ENDDO
      ENDDO
C-----------------------
C     FILL new cut layer
C-----------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)
        ELCUTC(1,I) = 2
        NUMELCRK    = NUMELCRK + 1
C
        IADC(1) = IADC_CRK(1,ELCRK)
        IADC(2) = IADC_CRK(2,ELCRK)
        IADC(3) = IADC_CRK(3,ELCRK)
        IADC(4) = IADC_CRK(4,ELCRK)
C
        N(1)  = IXC(2,I)
        N(2)  = IXC(3,I)
        N(3)  = IXC(4,I)
        N(4)  = IXC(5,I)
        NN(1) = INOD_CRK(N(1))
        NN(2) = INOD_CRK(N(2))
        NN(3) = INOD_CRK(N(3))
        NN(4) = INOD_CRK(N(4))
C
        ISIGN(1) = INT(SIGN(ONE,FIT(1,I)))
        ISIGN(2) = INT(SIGN(ONE,FIT(2,I)))
        ISIGN(3) = INT(SIGN(ONE,FIT(3,I)))
        ISIGN(4) = INT(SIGN(ONE,FIT(4,I)))
C
        IF (FIT(1,I) == ZERO) ISIGN(1) = 0
        IF (FIT(2,I) == ZERO) ISIGN(2) = 0
        IF (FIT(3,I) == ZERO) ISIGN(3) = 0
        IF (FIT(4,I) == ZERO) ISIGN(4) = 0
c
        ICRK = CRKSHELL(PP1)%CRKSHELLID(ELCRK)
c
        IENR0(1) = CRKNODIAD(IADC(1))
        IENR0(2) = CRKNODIAD(IADC(2))
        IENR0(3) = CRKNODIAD(IADC(3))
        IENR0(4) = CRKNODIAD(IADC(4))
C
        IENR(1)  = IENR0(1) + KNOD2ELC(NN(1))*(ILAY-1)
        IENR(2)  = IENR0(2) + KNOD2ELC(NN(2))*(ILAY-1)
        IENR(3)  = IENR0(3) + KNOD2ELC(NN(3))*(ILAY-1)
        IENR(4)  = IENR0(4) + KNOD2ELC(NN(4))*(ILAY-1)
c--------------------------------------------
        DO J=1,2
          K = ECUT(J,I)
          IEDGE = XEDGE4N(K,ELCRK)                            
          NOD1 = NODEDGE(1,IEDGE)                             
          NOD2 = NODEDGE(2,IEDGE)                             
          IE10 = CRKEDGE(ILAY)%EDGEENR(1,IEDGE)               
          IE20 = CRKEDGE(ILAY)%EDGEENR(2,IEDGE)               
          IF (NOD1 == N(K) .and. NOD2 == N(dd(K))) THEN       
            IE1 = IENR(K)                                     
            IE2 = IENR(dd(K))                                 
            IFI1 = ISIGN(K)                                   
            IFI2 = ISIGN(dd(K))                               
          ELSE IF (NOD2 == N(K) .and. NOD1 == N(dd(K))) THEN  
            IE1 = IENR(dd(K))                                 
            IE2 = IENR(K)                                     
            IFI1 = ISIGN(dd(K))                               
            IFI2 = ISIGN(K)                                   
          END IF                                              
          CRKEDGE(ILAY)%EDGEENR(1,IEDGE) = MAX(IE1,IE10)      
          CRKEDGE(ILAY)%EDGEENR(2,IEDGE) = MAX(IE2,IE20)      
          IF (CRKEDGE(ILAY)%EDGEICRK(IEDGE) == 0)              
     .        CRKEDGE(ILAY)%EDGEICRK(IEDGE)  = ICRK            
        ENDDO
C------------------
        CRKEDGE(ILAY)%LAYCUT(ELCRK) = -1 ! layer cut
        XFEM_PHANTOM(ILAY)%ELCUT(ELCRK) = ICRK
C
        NP   = 0
        DO K=1,4
          IED = EDGEL(K,I)
          IEDGE = XEDGE4N(K,ELCRK)
          IF (IED > 0) THEN
            CRKEDGE(ILAY)%EDGETIP(1,IEDGE) = IED
            CRKEDGE(ILAY)%EDGETIP(2,IEDGE) = 
     .                  CRKEDGE(ILAY)%EDGETIP(2,IEDGE) + 1
          ENDIF
          IF (ISIGN(K) > 0) NP   = K
        ENDDO
c-------------------
        ITRI = 0
        NX1  = NP                                                     
        IF (NP > 0 .and. ISIGN(NP-1) > 0)  THEN                       
          NX1 = NP-1                                                  
        ELSE                                                          
          NX1 = NP                                                    
        ENDIF                                                         
        XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK) = ITRI                       
        XFEM_PHANTOM(ILAY)%ITRI(2,ELCRK) = NX1  ! first positive node  
        NX2 = DX(NX1+1)                                               
        NX3 = DX(NX1+2)                                               
        NX4 = DX(NX1+3)                                               
c       calculate first phantom area                                  
        X1 = XXL(NX1,I)                                               
        Y1 = YYL(NX1,I)                                               
        X2 = XXL(NX2,I)                                               
        Y2 = YYL(NX2,I)                                               
        IED = EDGEL(NX2,I)                                            
        IF (IED > 0) THEN                                             
          X3 = XIN(IED,I)                                             
          Y3 = YIN(IED,I)                                             
        ELSE                                                          
           print*,' ERROR: K,IED=',K,IED                             
        ENDIF                                                         
        IED = EDGEL(NX4,I)                                            
        IF (IED > 0) THEN                                             
          X4 = XIN(IED,I)                                             
          Y4 = YIN(IED,I)                                             
        ELSE                                                          
           print*,' ERROR: K,IED=',K,IED                             
        ENDIF                                                         
        AREA1 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))            
        AREA1 = AREA1 * A_I(I)                                       
        AREA2 = ONE - AREA1                                            
        CRKLVSET(PP1)%AREA(ELCRK) = AREA1                             
        CRKLVSET(PP2)%AREA(ELCRK) = AREA2                             
        CRKLVSET(PP3)%AREA(ELCRK) = ZERO                         
C
        XFEM_PHANTOM(ILAY)%IFI(IADC(1)) = ISIGN(1)
        XFEM_PHANTOM(ILAY)%IFI(IADC(2)) = ISIGN(2)
        XFEM_PHANTOM(ILAY)%IFI(IADC(3)) = ISIGN(3)
        XFEM_PHANTOM(ILAY)%IFI(IADC(4)) = ISIGN(4)
C------------------
C       IXEL = 1 => positif element within ILAY
C------------------
        CRKLVSET(PP1)%ENR0(1,IADC(1)) = -IENR(1)
        CRKLVSET(PP1)%ENR0(1,IADC(2)) = -IENR(2)
        CRKLVSET(PP1)%ENR0(1,IADC(3)) = -IENR(3)
        CRKLVSET(PP1)%ENR0(1,IADC(4)) = -IENR(4)
C
        IF (ISIGN(1) > 0) CRKLVSET(PP1)%ENR0(1,IADC(1)) = 0
        IF (ISIGN(2) > 0) CRKLVSET(PP1)%ENR0(1,IADC(2)) = 0
        IF (ISIGN(3) > 0) CRKLVSET(PP1)%ENR0(1,IADC(3)) = 0
        IF (ISIGN(4) > 0) CRKLVSET(PP1)%ENR0(1,IADC(4)) = 0
C------------------
C       IXEL = 2 => negatif element within ILAY    ! ILEV = PP2
C------------------
        CRKLVSET(PP2)%ENR0(1,IADC(1)) = -IENR(1)
        CRKLVSET(PP2)%ENR0(1,IADC(2)) = -IENR(2)
        CRKLVSET(PP2)%ENR0(1,IADC(3)) = -IENR(3)
        CRKLVSET(PP2)%ENR0(1,IADC(4)) = -IENR(4)
C
        IF (ISIGN(1) < 0) CRKLVSET(PP2)%ENR0(1,IADC(1)) = 0
        IF (ISIGN(2) < 0) CRKLVSET(PP2)%ENR0(1,IADC(2)) = 0
        IF (ISIGN(3) < 0) CRKLVSET(PP2)%ENR0(1,IADC(3)) = 0
        IF (ISIGN(4) < 0) CRKLVSET(PP2)%ENR0(1,IADC(4)) = 0
C------------------
c       IXEL = 3 => not actif
C------------------
        XFEM_STR(NXEL)%GBUF%OFF(I) = ZERO                           
        XFEM_STR(NXEL)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) = ZERO        
c        
      ENDDO !  IR=1,NEWCRK
C-------------------
      NLEVSET = NLEVSET + 1  ! update nb of cracks
C-------------------
      RETURN
      END
