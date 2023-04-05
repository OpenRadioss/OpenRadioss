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
Chd|  M51VOIS2                      source/materials/mat/mat051/m51vois2.F
Chd|-- called by -----------
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE M51VOIS2(PM     ,IPARG  ,IXQ    ,ALE_CONNECT ,ELBUF_TAB,V     ,
     2                    X      ,VN     ,W      ,VEL   ,VD2   ,
     3                    MAT    ,RHOV   ,PV     ,VDX   ,VDY      ,VDZ   ,
     4                    EIV    ,TV     ,BUFVOIS,IFLG  ,AVV      ,RHO0V ,    
     5                    IPM    ,BUFMAT ,UVAR  ,NEL      ,NUVAR ,
     6                    NV46   ,SSPv   ,EPSPv  ,P0_NRF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE ALE_CONNECTIVITY_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXQ(NIXQ,*),MAT(*),IFLG,ITRIMAT,
     .        IPM(NPROPMI,*), NUVAR,NEL,KK,
     .        ILAY, NV46
      my_real
     . PM(NPROPM,*), V(3,*),X(3,*),VN(*),W(3,*),P0_NRF(MVSIZ),
     .  VEL(*),BUFMAT(*),
     .   RHOV(0:4,MVSIZ), PV(0:4,MVSIZ), EIV(0:4,MVSIZ), AVV(0:4,MVSIZ), TV(0:4,MVSIZ), RHO0V(0:4,MVSIZ),
     .   BUFVOIS(M51_IFLG6_SIZE,*),UVAR(NEL,NUVAR),SSPv(0:4,MVSIZ),EPSPv(0:4,MVSIZ),
     .   VD2(NEL),VDX(NEL),VDY(NEL),VDZ(NEL)    , DVY, DVZ 
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,JJ, J, IVOI, ML, N, KTY, KLT, MFT, IS, 
     .   INOD, IX1, IX2, IX3, IX4, NELG,IJ(NV46),K,IAD2
      INTEGER   ICF(2,4), IFORM, IADBUF,ISUB_BIJ(4),ITMP
      my_real
     .   X13, Y13, Z13, X24, Y24, Z24, XN, YN, ZN, FAC, VN1, VN2,
     .   VN3, VN4, VX1, VX2, VX3, VX4, VY1, VY2, VY3, VY4,
     .   VZ1, VZ2, VZ3, VZ4  
      INTEGER NPH,IPRINT

      TYPE(G_BUFEL_)  ,POINTER :: GBUF 
      TYPE(BUF_MAT_)  ,POINTER :: MBUF 
      TYPE(L_BUFEL_)  ,POINTER :: LBUF 
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY       
C-----------------------------------------------
      !DATA ICF/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
      DATA ICF/1,2,2,3,3,4,4,1/
      ILAY = 1
C-----------------------------------------------
      DO I=1,NEL
       II     = I+NFT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
       
       DO  J=1,NV46
        IVOI  = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
        ML    = 51
        IFORM = 1000
        IF(IVOI/=0)THEN
          IF(IVOI<=NUMELQ)THEN
            ML     = NINT(PM(19,IXQ(1,IVOI))) 
            IADBUF = IPM(7,IXQ(1,IVOI))
            IF(ML==51)IFORM  = BUFMAT(IADBUF+31-1)     !si voisin est materiau 51 alors on recupere UPARAM(31)=IFLG (IFLG=0,1 pour IFROM=0,1ou10)
            ISUB_BIJ(1)=BUFMAT(IADBUF+276+1-1)
            ISUB_BIJ(2)=BUFMAT(IADBUF+276+2-1)
            ISUB_BIJ(3)=BUFMAT(IADBUF+276+3-1)
            ISUB_BIJ(4)=BUFMAT(IADBUF+276+4-1)                                    
          ELSE
            IS     = IVOI-NUMELQ
            IFORM  = BUFVOIS(36,IS)
            ITMP   = BUFVOIS(37,IS)
            ISUB_BIJ(1)=(ITMP/100000)
            ITMP=MOD(ITMP,100000)
            ISUB_BIJ(2)=(ITMP/10000)
            ITMP=MOD(ITMP,10000)            
            ISUB_BIJ(3)=(ITMP/1000)
            ITMP=MOD(ITMP,1000)            
            ISUB_BIJ(4)=(ITMP/100)
            ITMP=MOD(ITMP,100)             
            ML     = ITMP                    
          ENDIF
        ENDIF 
       IF(ML==51.AND.IFORM<=1)  EXIT                             ! si materiau voisin est loi 51 Iform=1 ou 10 alors on a trouve
       ENDDO

       IF(ML==51 .AND. IFORM<=1)THEN
         IX1 = IXQ(ICF(1,J)+1,II)
         IX2 = IXQ(ICF(2,J)+1,II)
         XN   = ZERO
         YN   = (-X(3,IX2)+X(3,IX1))
         ZN   = (-X(2,IX1)+X(2,IX2))
         FAC  = ONE/SQRT(YN**2+ZN**2)
         YN  = YN*FAC
         ZN  = ZN*FAC
         !
         ! VITESSES MOYENNES A LA FRONTIERE (SUPG)
         ! 
         VDX(I)=ZERO
         VDY(I)=HALF*(V(2,IX1)+V(2,IX2))
         VDZ(I)=HALF*(V(3,IX1)+V(3,IX2))         
         IF(JALE>0)THEN
             VDY(I)=VDY(I)-HALF*(W(2,IX1)+W(2,IX2))
             VDZ(I)=VDZ(I)-HALF*(W(3,IX1)+W(3,IX2))        
         ENDIF
         VD2(I)=VDY(I)**2+VDZ(I)**2
         IF(VDY(I)*ZN+VDZ(I)*YN<=ZERO)THEN
           VDY(I)=ZERO
           VDZ(I)=ZERO
         ENDIF
         !
         ! FRONTIERE NON REFLECHISSANTE
         !
         VN1=V(2,IX1)*YN+V(3,IX1)*ZN                                    
         VN2=V(2,IX2)*YN+V(3,IX2)*ZN
         VEL(I)=(MIN(VN1,VN2))**2                                        
         VN(I)=HALF*(VN1+VN2)                                      
         IF(VN(I)>=ZERO)VEL(I)=ZERO                                                                             

        IF(IVOI<=NUMELQ)THEN
          !element du processeur                           
 	  DO N=1,NGROUP                                    
 	     KTY = IPARG(5,N)                              
 	     KLT = IPARG(2,N)                              
 	     MFT = IPARG(3,N)                              
 	     IF (KTY==2 .AND. IVOI<=KLT+MFT) EXIT      
 	  ENDDO                                            

 	  IF (KTY/=2 .OR. IVOI>KLT+MFT) CYCLE
          GBUF  => ELBUF_TAB(N)%GBUF
          LBUF  => ELBUF_TAB(N)%BUFLY(1)%LBUF(1,1,1)
          MBUF  => ELBUF_TAB(N)%BUFLY(1)%MAT(1,1,1)
          BUFLY => ELBUF_TAB(N)%BUFLY(1) 
          NELG  = KLT
          IS    = IVOI-MFT   

          DO K=1,NV46
            IJ(K) = KLT*(K-1)
          ENDDO
          
          !Global Material data
          PV(0,I)   = -THIRD*(GBUF%SIG(IJ(1)+IS)
     .                      + GBUF%SIG(IJ(2)+IS)
     .                      + GBUF%SIG(IJ(3)+IS))
          AVV(0,I)     = ONE
          EIV(0,I)     = GBUF%EINT(IS)
          RHOV(0,I)    = GBUF%RHO(IS)         
          TV(0,I)      = GBUF%TEMP(IS)    !IF (JTHE>0)
          SSPv(0,I)    = LBUF%SSP(IS)
          IF(BUFLY%L_PLA>0)then
            EPSPv(0,I) = LBUF%PLA(IS)
          ELSE
            EPSPv(0,I) = ZERO
          ENDIF
          P0_NRF(I)    = MBUF%VAR(NELG*3+IS)  !UVAR(4,I)
 
          !Submaterial Data
          DO ITRIMAT=1,4
            KK                          = N0PHAS + (ITRIMAT-1)*NVPHAS 
            IADBUF=18 ; PV(ITRIMAT,I)   = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            !IADBUF=1  ; AVV(ITRIMAT,I)  = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            IADBUF=8  ; EIV(ITRIMAT,I)  = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            IADBUF=9  ; RHOV(ITRIMAT,I) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            IADBUF=16 ; TV(ITRIMAT,I)   = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            IADBUF=14 ; SSPv(ITRIMAT,I) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            IADBUF=15 ; EPSPv(ITRIMAT,I)= MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            !IADBUF=20 ; RHOFv(ITRIMAT,I)= MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            !IADBUF=21 ; EFv(ITRIMAT,I)  = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
            !IADBUF=22 ; PFv(ITRIMAT,I)  = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
          ENDDO 

          !volue fractions
          DO ITRIMAT=1,4
            KK                          = N0PHAS + (ISUB_BIJ(ITRIMAT)-1)*NVPHAS 
            IADBUF=1  ; AVV(ITRIMAT,I)  = MBUF%VAR(NELG*(IADBUF+KK-1)+IS)
          ENDDO           
                  
          IADBUF = IPM(7,IXQ(1,IVOI))

          RHO0V(1,I) = BUFMAT(IADBUF+09-1)  !UPARAM(9)  = RHO10
          RHO0V(2,I) = BUFMAT(IADBUF+10-1)  !UPARAM(10) = RHO20
          RHO0V(3,I) = BUFMAT(IADBUF+11-1)  !UPARAM(11) = RHO30
          RHO0V(4,I) = BUFMAT(IADBUF+47-1)  !UPARAM(47) = RHO40                  
          RHO0V(0,I) = BUFMAT(IADBUF+69-1)  !UPARAM(69) = RHO0
                                                 
        ELSE !(IVOI>NUMELQ)

          !cas SPMD et voisin remote : aller chercher dans BUFVOIS
          !BUFVOIS rempli dans ALEMAIN (spmf_cfd.F : SPMD_L51VOIS)

 	        IS               = IVOI-NUMELQ
 	        PV(0,I)          = BUFVOIS(01,IS)
 	        EIV(0,I)         = BUFVOIS(02,IS)
 	        RHOV(0,I)        = BUFVOIS(03,IS)
 	        TV(0,I)          = BUFVOIS(04,IS)
                SSPv(0,I)        = BUFVOIS(05,IS)
                EPSPv(0,I)       = BUFVOIS(06,IS)
                
                ITRIMAT          = 1
                PV(ITRIMAT,I)    = BUFVOIS(07,IS)
                !AVV(ITRIMAT,I)   = BUFVOIS(08,IS)
                EIV(ITRIMAT,I)   = BUFVOIS(09,IS)
                RHOV(ITRIMAT,I)  = BUFVOIS(10,IS)
                TV(ITRIMAT,I)    = BUFVOIS(11,IS)
                SSPV(ITRIMAT,I)  = BUFVOIS(12,IS)
                EPSPv(ITRIMAT,I) = BUFVOIS(13,IS)
                
                ITRIMAT          = 2
                PV(ITRIMAT,I)    = BUFVOIS(14,IS)
                !AVV(ITRIMAT,I)   = BUFVOIS(15,IS)
                EIV(ITRIMAT,I)   = BUFVOIS(16,IS)
                RHOV(ITRIMAT,I)  = BUFVOIS(17,IS)
                TV(ITRIMAT,I)    = BUFVOIS(18,IS)
                SSPV(ITRIMAT,I)  = BUFVOIS(19,IS)
                EPSPv(ITRIMAT,I) = BUFVOIS(20,IS)
                
                ITRIMAT          = 3
                PV(ITRIMAT,I)    = BUFVOIS(21,IS)
                !AVV(ITRIMAT,I)   = BUFVOIS(22,IS)
                EIV(ITRIMAT,I)   = BUFVOIS(23,IS)
                RHOV(ITRIMAT,I)  = BUFVOIS(24,IS)
                TV(ITRIMAT,I)    = BUFVOIS(25,IS)
                SSPV(ITRIMAT,I)  = BUFVOIS(26,IS)
                EPSPv(ITRIMAT,I) = BUFVOIS(27,IS)
                
                ITRIMAT          = 4
                PV(ITRIMAT,I)    = BUFVOIS(28,IS)
                !AVV(ITRIMAT,I)   = BUFVOIS(29,IS)
                EIV(ITRIMAT,I)   = BUFVOIS(30,IS)
                RHOV(ITRIMAT,I)  = BUFVOIS(31,IS)
                TV(ITRIMAT,I)    = BUFVOIS(32,IS)
                SSPV(ITRIMAT,I)  = BUFVOIS(33,IS)
                EPSPv(ITRIMAT,I) = BUFVOIS(34,IS)

                P0_NRF(I)        = BUFVOIS(35,IS)                                                                
                
                !ordering with bijection uparam(276+1:276+4)
                AVV(1,I) = BUFVOIS(1+ISUB_BIJ(1)*7,IS)
                AVV(2,I) = BUFVOIS(1+ISUB_BIJ(2)*7,IS)
                AVV(3,I) = BUFVOIS(1+ISUB_BIJ(3)*7,IS)
                AVV(4,I) = BUFVOIS(1+ISUB_BIJ(4)*7,IS)                                                
                
        ENDIF

       ELSE !(ML/=51.OR.IFORM>1)
        VN(I)       = ZERO
        PV(0:4,I)   = ZERO
        EIV(0:4,I)  = ZERO
        RHOV(0:4,I) = ZERO
        TV(0:4,I)   = ZERO
        AVV(0:4,I)  = ZERO
        SSPV(0:4,I) = ZERO
        EPSPv(0:4,I)= ZERO
       ENDIF
      ENDDO !next i
C-----------
      RETURN
      END
