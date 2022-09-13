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
Chd|  I18FOR3                       source/interfaces/int18/i18for3.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        I7ASS0                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS2                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS3                        source/interfaces/int07/i7ass3.F
Chd|        MULTI_I18_FORCE_POFF          source/interfaces/int18/multi_i18_force_poff.F
Chd|        MULTI_I18_FORCE_PON           source/interfaces/int18/multi_i18_force_pon.F
Chd|        ALEANIM_MOD                   share/modules/aleanim_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I18FOR3(JLT      ,A          ,V         ,IBCC      ,ICODT         ,
     2                  FSAV      ,GAP        ,FRIC      ,MS        ,VISC          ,
     3                  VISCF     ,NOINT      ,STFN      ,ITAB      ,CN_LOC        ,
     4                  STFVAL    ,STIFN      ,STIF      ,FSKYI     ,ISKY          ,
     6                  NX1       ,NX2        ,NX3       ,NX4       ,NY1           ,
     7                  NY2       ,NY3        ,NY4       ,NZ1       ,NZ2           ,
     8                  NZ3       ,NZ4        ,LB1       ,LB2       ,LB3           ,
     9                  LB4       ,LC1        ,LC2       ,LC3       ,LC4           ,
     A                  P1        ,P2         ,P3        ,P4        ,FCONT         ,
     B                  IX1       ,IX2        ,IX3       ,IX4       ,NSVG          ,
     C                  IVIS2     ,NELTST     ,ITYPTST   ,DT2T      ,IXS           ,
     D                  GAPV      ,INACTI     ,CAND_P    ,INDEX     ,NISKYFI       ,
     E                  KINET     ,NEWFRONT   ,ISECIN    ,NSTRF     ,SECFCUM       ,
     F                  X         ,IRECT      ,CE_LOC    ,MFROT     ,IFQ           ,
     G                  FROT_P    ,CAND_FX    ,CAND_FY   ,CAND_FZ   ,ALPHA0        ,
     H                  IFPEN     ,IPRES      ,ICONTACT  ,IGROUPS   ,IPARG         ,
     J                  VISCN     ,VXI        ,VYI       ,VZI       ,MSI           ,
     K                  KINI      ,NIN        ,NISUB     ,LISUB     ,ADDSUBS       ,
     L                  ADDSUBM   ,LISUBS     ,LISUBM    ,FSAVSUB   ,CAND_N        ,
     M                  ILAGM     ,ICURV      ,PRES      ,FNCONT    ,MS0           ,
     N                  JTASK     ,ISENSINT   ,FSAVPARIT ,NFT       ,MULTI_FVM     ,
     O                  H3D_DATA  ,ELBUF_TAB  ,IDIR      )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is computing reaction forces
C for fluid structure interaction /INTER/TYPE18
C It also ouputs contour and time histories.
C
C   I=1:JLT : local loop (JLT<=128)
C   INDEX(I) is Node id (internal)
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE ALEANIM_MOD !FANI_CELL   
      USE ELBUFDEF_MOD       
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "kincod_c.inc"
#include      "inter18.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IDIR
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER NELTST,ITYPTST,JLT,IBCC,IVIS2,INACTI,IPRES,NIN,
     .        ICODT(*), ITAB(*), ISKY(*), KINET(*),
     .        MFROT, IFQ, NOINT,NEWFRONT,ISECIN, NSTRF(*),
     .        IRECT(4,*),IFPEN(*) ,ICONTACT(*), CAND_N(*),
     .        KINI(*),IGROUPS(NUMELS),
     .        ISET, NISKYFI,INTTH,IFORM,JTASK,NFT,IPARG(NPARG)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        CN_LOC(MVSIZ),CE_LOC(MVSIZ),INDEX(MVSIZ),NSVG(MVSIZ),
     .        NISUB, LISUB(*), ADDSUBS(*), ADDSUBM(*), LISUBS(*),
     .        LISUBM(*),ILAGM,ICURV(3),ISENSINT(*),IXS(NIXS,NUMELS)
      my_real
     .   STFVAL,CAND_P(*),FROT_P(*), X(3,*),MS0(*),
     .   A(3,*), MS(*), V(3,*), FSAV(*),FCONT(3,*),
     .   CAND_FX(*),CAND_FY(*),CAND_FZ(*),ALPHA0,
     .   GAP, FRIC,VISC,VISCF,VIS,DT2T,STFN(*),STIFN(*),
     .   FSKYI(LSKYI,NFSKYI),FSAVSUB(NTHVKI,*),FNCONT(3,*),
     .   FSAVPARIT(NISUB+1,11,*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),
     .     SECFCUM(7,NUMNOD,NSECT), TMP(MVSIZ),
     .     STIFSAV(MVSIZ), VISCN(*),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(2*MVSIZ),
     .     PRES(*), RSTIF  
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,
     .        NA1,NA2
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ), XMU(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),DIST(MVSIZ), 
     .   VNX, VNY, VNZ, AA, CRIT,S2,RDIST,
     .   V2, FM2, DT1INV, VISCA, FAC,FF,ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,ECONTDT,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8, 
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15, FFO,
     .   E10, H0D, S2D, SUM,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,VISD,FACD,D1D,
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,TM,TS
      my_real
     .   SURFX,SURFY,SURFZ,SURF
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX,PHI1(MVSIZ), PHI2(MVSIZ), PHI3(MVSIZ),
     .   PHI4(MVSIZ),DX, DTI
      INTEGER JSUB,KSUB,JJ,KK,IN,NSUB,IBID,IDX,ILEN,IPOS,ITASK,NELFT,NELLT
      my_real
     .   FSAVSUB1(15,NISUB),IMPX,IMPY,IMPZ,PP1,PP2,PP3,PP4,BID
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
#include "sync_dir.inc"

      IBID   = 0
      BID    = ZERO      
      ECONTT = ZERO
      ECONTDT = ZERO     
      
      DT1INV =ZERO
      IF(DT1 > ZERO)DT1INV = ONE/DT1

      ITASK = JTASK-1
      
      !---------------------
      !   PENETRATION CALCULATION (PENE)
      !   & LOCAL COORDINATES (H1,H2,H3,H4) ON FACE FOR VELOCITY INTERPOLATION ON FACE
      !---------------------
      DO I=1,JLT
        IF(IX3(I) /= IX4(I))THEN
         !distance from triangles
         D1 = SQRT(P1(I))
         D2 = SQRT(P2(I))
         D3 = SQRT(P3(I))
         D4 = SQRT(P4(I))
         !penetration into each triangle gap
         PP1 = MAX(ZERO, GAP - D1)
         PP2 = MAX(ZERO, GAP - D2)
         PP3 = MAX(ZERO, GAP - D3)
         PP4 = MAX(ZERO, GAP - D4)
         !MAIN face penetration
         PENE(I) = MAX(PP1,PP2,PP3,PP4)
         !ratios
         A1 = PP1/MAX(EM20,D1)
         A2 = PP2/MAX(EM20,D2)
         A3 = PP3/MAX(EM20,D3)
         A4 = PP4/MAX(EM20,D4)
         !sum(ratio[k]*2Sn[k],k=1..4)
         N1(I) = A1*NX1(I) + A2*NX2(I) + A3*NX3(I) + A4*NX4(I)
         N2(I) = A1*NY1(I) + A2*NY2(I) + A3*NY3(I) + A4*NY4(I)
         N3(I) = A1*NZ1(I) + A2*NZ2(I) + A3*NZ3(I) + A4*NZ4(I)      
         LA1 = ONE - LB1(I) - LC1(I)
         LA2 = ONE - LB2(I) - LC2(I)
         LA3 = ONE - LB3(I) - LC3(I)
         LA4 = ONE - LB4(I) - LC4(I) 
         H0    = FOURTH * (PP1*LA1 + PP2*LA2 + PP3*LA3 + PP4*LA4)
         H1(I) = H0 + PP1 * LB1(I) + PP4 * LC4(I)
         H2(I) = H0 + PP2 * LB2(I) + PP1 * LC1(I)
         H3(I) = H0 + PP3 * LB3(I) + PP2 * LC2(I)
         H4(I) = H0 + PP4 * LB4(I) + PP3 * LC3(I)
         H00    = ONE/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
         H1(I) = H1(I) * H00
         H2(I) = H2(I) * H00
         H3(I) = H3(I) * H00
         H4(I) = H4(I) * H00
        ELSE
         D1 = SQRT(P1(I))
         PP1 = MAX(ZERO, GAP - D1)
         PENE(I) = PP1
         N1(I) = NX1(I)
         N2(I) = NY1(I)
         N3(I) = NZ1(I)
         H1(I) = LB1(I)
         H2(I) = LC1(I)
         H3(I) = ONE - LB1(I) - LC1(I)
         H4(I) = ZERO
        ENDIF
      ENDDO
      !---------------------
      !     UNITARY NORMAL VECTOR
      !---------------------
      DO I=1,JLT
         S2 = ONE/MAX(EM30,SQRT(N1(I)*N1(I) + N2(I)*N2(I) + N3(I)*N3(I)))
         N1(I) = N1(I)*S2
         N2(I) = N2(I)*S2
         N3(I) = N3(I)*S2
      ENDDO
      !---------------------
      !      RELATIVE VELOCITY (VX,VY,VZ)
      !      & ITS NORMAL COMPONENT (VN)
      !---------------------
      DO I=1,JLT
        VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I)) - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
        VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I)) - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
        VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I)) - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
        VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
        !hourglass correction
        H0 = -FOURTH*(H1(I) - H2(I) + H3(I) - H4(I))       
        H0 = MIN(H0,H2(I),H4(I))                        
        H0 = MAX(H0,-H1(I),-H3(I))                      
        IF(IX3(I) == IX4(I))H0 = ZERO                   
        H1(I) = H1(I) + H0                              
        H2(I) = H2(I) - H0                              
        H3(I) = H3(I) + H0                              
        H4(I) = H4(I) - H0 
      ENDDO
      
      !----------------------------------------------!
      !            CONSTANT STIFFNESS                !
      !----------------------------------------------!
      DO I=1,JLT
        IF(PENE(I) > ZERO )THEN
          DIST(I)=GAP-PENE(I)
          CAND_P(INDEX(I)) = CAND_P(INDEX(I)) + VN(I)*DT1 
         ! IF(CAND_P(INDEX(I)) > ZERO)CAND_P(INDEX(I))=ZERO  !contact lost
          STIF(I) = STFVAL * PENE(I)/GAP !STVAL:constant user param
        ELSE
          !remove old candidates
          CAND_P(INDEX(I)) = ZERO
          STIF(I) = ZERO
        ENDIF
      ENDDO    
          
      !----------------------------------------------!
      !               REACTION FORCE                 !
      !----------------------------------------------!
      DO I=1,JLT
        IF(PENE(I) > ZERO )THEN    
          FNI(I)  = STIF(I) * CAND_P(INDEX(I))                   
        ELSE                                                     
          FNI(I)=ZERO                                            
        ENDIF 
      ENDDO    
      
      !!KINEMATIC TIME STEP
      !DTI = EP20      
      !IF(INTER18_AUTOPARAM == 1)THEN
      !  DO I=1,JLT
      !      DX =GAP/TEN
      !      RDIST  = DX / MAX(EM20,ABS(VN(I)))
      !      DTI = MIN(RDIST,DTI)
      !  ENDDO
      !ENDIF
      !IF(DTI<DT2T)THEN  
      !  DT2T    = DTI      
      !  NELTST  = NOINT    
      !  ITYPTST = 10       
      !ENDIF                

      !---------------------------------
      ! EXPERIMENTAL SURFACE ORIENTATION
      !---------------------------------
      IF(IDIR == -1)THEN
        DO I=1,JLT
          FNI(I) = MIN(FNI(I),ZERO)
        ENDDO
      ELSEIF(IDIR == 1)THEN
        DO I=1,JLT
          FNI(I) = MAX(FNI(I),ZERO)
        ENDDO
      ENDIF
      
      !---------------------------------
      !     DAMPING
      !---------------------------------
      IF(VISC /= ZERO)THEN
         DO I=1,JLT
           IF(VN(I) > ZERO)THEN
            FAC = STIF(I) / MAX(EM30,STIF(I))
            FF  = FAC * VISC * PENE(I)/GAP
            STIF(I) = STIF(I) + TWO * FF * DT1INV
            FF = FF * VN(I)  
            ECONTDT = ECONTDT + FF * VN(I) * DT1   ! Damping Energy
            FNI(I)  = FNI(I) + FF
           ENDIF
         ENDDO
      ENDIF
  
      !---------------------------------
      !     NORMAL IMPULSE OUTPUT
      !---------------------------------
      FSAV1 = ZERO
      FSAV2 = ZERO
      FSAV3 = ZERO
      FSAV8 = ZERO
      FSAV9 = ZERO
      FSAV10= ZERO
      FSAV11= ZERO
      DO I=1,JLT
       ECONTT = ECONTT + DT1*VN(I)*FNI(I) !  Elastic energy : it is cumulated energy per cycle
       FXI(I)=N1(I)*FNI(I)
       FYI(I)=N2(I)*FNI(I)
       FZI(I)=N3(I)*FNI(I)
       IMPX=FXI(I)*DT12
       IMPY=FYI(I)*DT12
       IMPZ=FZI(I)*DT12
       FSAV1 =FSAV1 +IMPX
       FSAV2 =FSAV2 +IMPY
       FSAV3 =FSAV3 +IMPZ
       FSAV8 =FSAV8 +ABS(IMPX)
       FSAV9 =FSAV9 +ABS(IMPY)
       FSAV10=FSAV10+ABS(IMPZ)
       FSAV11=FSAV11+FNI(I)*DT12
      ENDDO
      IF (IPRES /= 0)THEN
#include "lockon.inc"
        DO I=1,JLT
          PRES(CE_LOC(I)) = PRES(CE_LOC(I)) + FNI(I)
        ENDDO
#include "lockoff.inc"
      END IF
#include "lockon.inc"
        FSAV(1)=FSAV(1)+FSAV1
        FSAV(2)=FSAV(2)+FSAV2
        FSAV(3)=FSAV(3)+FSAV3
        FSAV(8)=FSAV(8)+FSAV8
        FSAV(9)=FSAV(9)+FSAV9
        FSAV(10)=FSAV(10)+FSAV10
        FSAV(11)=FSAV(11)+FSAV11
#include "lockoff.inc"
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,1,I+NFT) = FSAVPARIT(1,1,I+NFT) + SQRT((FXI(I)**2)+(FYI(I)**2)+(FZI(I)**2))
        ENDDO
      ENDIF
      !---------------------------------
      !     TH OUTPUT FOR SUB INTERFACES
      !---------------------------------
      IF(NISUB /= 0)THEN
         DO JSUB=1,NISUB
           DO J=1,15
               FSAVSUB1(J,JSUB)=ZERO
           END DO
         END DO
         DO I=1,JLT
          NN = NSVG(I)
          IF(NN > 0)THEN
           IN=CN_LOC(I)
           IE=CE_LOC(I)
           JJ  =ADDSUBS(IN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ<ADDSUBS(IN+1))
             JSUB=LISUBS(JJ)
             DO WHILE(KK<ADDSUBM(IE+1))
               KSUB=LISUBM(KK)   
               IF(KSUB == JSUB)THEN
                 IMPX=FXI(I)*DT12
                 IMPY=FYI(I)*DT12
                 IMPZ=FZI(I)*DT12
                 !MAIN side
                 FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                 FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                 FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
                 IF(ISENSINT(JSUB)/=0) THEN
                   FSAVPARIT(JSUB+1,1,I+NFT) = FSAVPARIT(JSUB+1,1,I+NFT) + SQRT((FXI(I)**2)+(FYI(I)**2)+(FZI(I)**2))
                 ENDIF
                 FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                 FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                 FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
                 FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
                 KK=KK+1
                 EXIT             
               ELSE IF(KSUB<JSUB)THEN
                 KK=KK+1
               ELSE
                 EXIT
               END IF
             END DO
             JJ=JJ+1
           END DO !WHILE
          ELSE
           NN = -NN
           IE=CE_LOC(I)
           JJ  =ADDSUBSFI(NIN)%P(NN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ<ADDSUBSFI(NIN)%P(NN+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             DO WHILE(KK<ADDSUBM(IE+1))
               KSUB=LISUBM(KK)   
               IF(KSUB == JSUB)THEN
                 IMPX=FXI(I)*DT12
                 IMPY=FYI(I)*DT12
                 IMPZ=FZI(I)*DT12
                 ! MAIN side
                 FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                 FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                 FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
                 FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                 FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                 FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
                 FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
                 KK=KK+1
                 EXIT            
               ELSE IF(KSUB<JSUB)THEN
                 KK=KK+1
               ELSE
                 EXIT
               END IF
             END DO! WHILE (KK<ADDSUBM(IE+1))
             JJ=JJ+1
           END DO! WHILE (JJ<ADDSUBSFI(NIN)%P(NN+1))
          END IF
         END DO !NEXT I=1,JLT
      END IF
      !---------------------------------
      !     SORTIES TH PAR SOUS INTERFACE
      !---------------------------------
      IF(NISUB /= 0)THEN
         DO I=1,JLT
          NN = NSVG(I)
          IF(NN > 0)THEN
           IN=CN_LOC(I)
           IE=CE_LOC(I)
           JJ  =ADDSUBS(IN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ<ADDSUBS(IN+1))
             JSUB=LISUBS(JJ)
             DO WHILE(KK<ADDSUBM(IE+1))
               KSUB=LISUBM(KK)   
               IF(KSUB == JSUB)THEN
                 !CURRENTLY NO TANGENTIAL FORCE IN INTER18
                 !IMPX=FXT(I)*DT12
                 !IMPY=FYT(I)*DT12
                 !IMPZ=FZT(I)*DT12
                 !FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                 !FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                 !FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
                 IMPX=FXI(I)*DT12
                 IMPY=FYI(I)*DT12
                 IMPZ=FZI(I)*DT12
                 FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                 FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                 FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
                 FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB) + SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                 KK=KK+1
                 EXIT             
               ELSE IF(KSUB<JSUB)THEN
                 KK=KK+1
               ELSE
                 EXIT
               END IF
             END DO!WHILE (KK<ADDSUBM(IE+1))
           JJ=JJ+1
           END DO! WHILE (JJ<ADDSUBS(IN+1))
          ELSE
           NN = -NN
           IE=CE_LOC(I)
           JJ  =ADDSUBSFI(NIN)%P(NN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ<ADDSUBSFI(NIN)%P(NN+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             DO WHILE(KK<ADDSUBM(IE+1))
               KSUB=LISUBM(KK)   
               IF(KSUB == JSUB)THEN
                 !CURRENTLY NO TANGENTIAL FORCE IN INTER18
                 !IMPX=FXT(I)*DT12
                 !IMPY=FYT(I)*DT12
                 !IMPZ=FZT(I)*DT12
                 !!MAIN side :
                 !FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                 !FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                 !FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
                 IMPX=FXI(I)*DT12
                 IMPY=FYI(I)*DT12
                 IMPZ=FZI(I)*DT12
                 FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                 FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                 FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
                 FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)+SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                 KK=KK+1
                 EXIT             
               ELSE IF(KSUB<JSUB)THEN
                 KK=KK+1
               ELSE
                 EXIT
               END IF
             END DO!DO WHILE(KK<ADDSUBM(IE+1))
             JJ=JJ+1
           END DO!WHILE(JJ<ADDSUBSFI(NIN)%P(NN+1))
          END IF
         END DO !NEXT I=1,JLT
#include "lockon.inc"
         DO JSUB=1,NISUB
           NSUB=LISUB(JSUB)
           DO J=1,15
             FSAVSUB(J,NSUB)=FSAVSUB(J,NSUB)+FSAVSUB1(J,JSUB)
           END DO
         END DO
#include "lockoff.inc"
      END IF
C---------------------------------
#include "lockon.inc"
        ECONTD = ECONTD + ECONTDT ! Damping Energy 
        ECONT_CUMU  = ECONT_CUMU + ECONTT ! ELASTIC ENERGY
        FSAV(26) = FSAV(26) + ECONTT
        FSAV(28) = FSAV(28) + ECONTDT
#include "lockoff.inc"
C---------------------------------

      IF(IDTMIN(10) == 1.OR.IDTMIN(10) == 2.OR.IDTMIN(10) == 5.OR.IDTMIN(10) == 6)THEN
       DTMI0 = EP20  
       DO I=1,JLT                                 
         DTMI(I) = EP20                   
         MAS2  = TWO * MSI(I)   
         IF(MAS2>ZERO.AND.STIF(I)>ZERO .AND. IRB(KINI(I))==0.AND.IRB2(KINI(I))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/STIF(I)))
         ENDIF                                
         MAS2  = TWO* MS(IX1(I))       
         IF(MAS2>ZERO.AND.H1(I)*STIF(I)>ZERO .AND. IRB(KINET(IX1(I)))==0.AND.IRB2(KINET(IX1(I)))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H1(I)*STIF(I))))
         ENDIF                                                       
         MAS2  = TWO * MS(IX2(I))                            
         IF(MAS2>ZERO.AND.H2(I)*STIF(I)>ZERO .AND. IRB(KINET(IX2(I)))==0.AND.IRB2(KINET(IX2(I)))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H2(I)*STIF(I))))
         ENDIF                     
         MAS2  = TWO* MS(IX3(I))                  
         IF(MAS2 > ZERO.AND.H3(I)*STIF(I) > ZERO .AND. IRB(KINET(IX3(I))) == 0.AND.IRB2(KINET(IX3(I))) == 0)THEN 
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H3(I)*STIF(I))))
         ENDIF                                
         MAS2  = TWO * MS(IX4(I))                     
         IF(MAS2 > ZERO.AND.H4(I)*STIF(I) > ZERO .AND. IRB(KINET(IX4(I))) == 0.AND.IRB2(KINET(IX4(I))) == 0)THEN 
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H4(I)*STIF(I))))
         ENDIF                                
         DTMI0 = MIN(DTMI0,DTMI(I))
       ENDDO          
       IF(DTMI0<=DTMIN1(10))THEN
        DO I=1,JLT
         IF(DTMI(I)<=DTMIN1(10))THEN
           JG = NSVG(I)
           IF(JG > 0)THEN
             NI = ITAB(JG)
           ELSE
             NI = ITAFI(NIN)%P(-JG)
           ENDIF
           IF(IDTMIN(10) == 1)THEN
#include "lockon.inc"
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE ',NOINT
             WRITE(IOUT,'(A,I10)') '   SECONDARY NODE   : ',NI                
             WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .         ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
#include "lockoff.inc"
             TSTOP = TT
             IF ( ISTAMPING == 1) THEN
               WRITE(ISTDO,'(A)')'The run encountered a problem in an interface Type 7.'
               WRITE(ISTDO,'(A)')'You may need to check if there is enou gh clearance between the tools,'
               WRITE(ISTDO,'(A)')'and that they do not penetrate each other during their travel'
               WRITE(IOUT, '(A)')'The run encountered a problem in an interface Type 7.'
               WRITE(IOUT, '(A)')'You may need to check if there is enough clearance between the tools,'
               WRITE(IOUT, '(A)')'and that they do not penetrate each other during their travel'
             ENDIF
           ELSEIF(IDTMIN(10) == 2)THEN
#include "lockon.inc"
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')' **WARNING MINIMUM TIME STEP ',DTMI(I),' IN INTERFACE ',NOINT
             WRITE(IOUT,'(A,I10,A,I10)')'   DELETE SECONDARY NODE ',NI,' FROM INTERFACE ',NOINT                 
             WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
             IF(JG > 0) THEN
               STFN(CN_LOC(I)) = -ABS(STFN(CN_LOC(I)))
             ELSE
               STIFI(NIN)%P(-JG) = -ABS(STIFI(NIN)%P(-JG))
             ENDIF
#include "lockoff.inc"
             IF ( ISTAMPING == 1) THEN
               WRITE(ISTDO,'(A)')'The run encountered a problem in an interface Type 7.'
               WRITE(ISTDO,'(A)')'You may need to check if there is enou gh clearance between the tools,'
               WRITE(ISTDO,'(A)')'and that they do not penetrate each other during their travel'
               WRITE(IOUT, '(A)')'The run encountered a problem in an interface Type 7.'
               WRITE(IOUT, '(A)')'You may need to check if there is enough clearance between the tools,'
               WRITE(IOUT, '(A)')'and that they do not penetrate each other during their travel'
               ENDIF
             NEWFRONT = -1
           ELSEIF(IDTMIN(10) == 5)THEN
#include "lockon.inc"
             WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')' **WARNING MINIMUM TIME STEP ',DTMI(I),' IN INTERFACE ',NOINT
             WRITE(IOUT,'(A,I10)') '   SECONDARY NODE   : ',NI                 
             WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .         ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
#include "lockoff.inc"
             MSTOP = 2
             IF ( ISTAMPING == 1) THEN
               WRITE(ISTDO,'(A)')'The run encountered a problem in an interface Type 7.'
               WRITE(ISTDO,'(A)')'You may need to check if there is enou gh clearance between the tools,'
               WRITE(ISTDO,'(A)')'and that they do not penetrate each other during their travel'
               WRITE(IOUT, '(A)')'The run encountered a problem in an interface Type 7.'
               WRITE(IOUT, '(A)')'You may need to check if there is enough clearance between the tools,'
               WRITE(IOUT, '(A)')'and that they do not penetrate each other during their travel'
             ENDIF
           ELSEIF(IDTMIN(10) == 6.AND.ILAGM == 2)THEN
             IF(KINET(JG)+KINET(IX1(I))+KINET(IX2(I))+KINET(IX3(I))+KINET(IX4(I)) == 0 )THEN
               CAND_N(INDEX(I)) = -IABS(CAND_N(INDEX(I)))
               STIF(I) = ZERO
               FXI(I)  = ZERO
               FYI(I)  = ZERO
               FZI(I)  = ZERO
             ENDIF                
           ENDIF                
         ENDIF
        ENDDO
       ENDIF
      ENDIF
C---------------------------------
      DO I=1,JLT            
       FX1(I)=FXI(I)*H1(I)  
       FY1(I)=FYI(I)*H1(I)  
       FZ1(I)=FZI(I)*H1(I)  
       !
       FX2(I)=FXI(I)*H2(I)  
       FY2(I)=FYI(I)*H2(I)  
       FZ2(I)=FZI(I)*H2(I)  
       !
       FX3(I)=FXI(I)*H3(I)  
       FY3(I)=FYI(I)*H3(I)  
       FZ3(I)=FZI(I)*H3(I)  
       !
       FX4(I)=FXI(I)*H4(I)  
       FY4(I)=FYI(I)*H4(I)  
       FZ4(I)=FZI(I)*H4(I)
       !case law151 with single lagrangian fluid brick and constant velocity
       !FXI(I)=ZERO
       !FYI(I)=ZERO
       !FZI(I)=ZERO
      ENDDO    
                   
      !SPMD : identification des noeuds interf. utiles a envoyer
      IF (NSPMD > 1) THEN
        DO I = 1,JLT
          NN = NSVG(I)
          IF(NN<0)THEN
            ! tag temporaire de NSVFI a -
            NSVFI(NIN)%P(-NN) = -ABS(NSVFI(NIN)%P(-NN))
          ENDIF
        ENDDO
      ENDIF
      IF (MULTI_FVM%IS_USED) THEN
         IF (IPARIT  ==  0) THEN
            CALL MULTI_I18_FORCE_POFF(
     1           DT1      ,JLT        ,IX1  ,IX2  ,IX3    ,IX4  ,
     2           NSVG     ,H1         ,H2   ,H3   ,H4     ,STIF ,
     3           FX1      ,FY1        ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4           FX3      ,FY3        ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5           FXI      ,FYI        ,FZI  ,A    ,STIFN  ,NIN  ,
     7           JTASK    ,MULTI_FVM  ,X    ,IXS  ,V      , 
     8           ELBUF_TAB,IGROUPS    ,IPARG,MSI) 
         ELSE
            CALL MULTI_I18_FORCE_PON(
     1           JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2           NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3           FX1   ,FY1   ,FZ1  ,FX2  ,FY2  ,FZ2    ,
     4           FX3   ,FY3   ,FZ3  ,FX4  ,FY4  ,FZ4    ,
     5           FXI   ,FYI   ,FZI  ,FSKYI,ISKY ,NISKYFI,
     6           NIN   ,NOINT ,MULTI_FVM,DT1,JTASK)     
         ENDIF
      ELSE
         IF(IPARIT == 3)THEN
            CALL I7ASS3( JLT   ,IX1  ,IX2  ,IX3  ,IX4  ,
     2           NSVG  ,H1   ,H2   ,H3   ,H4   ,STIF ,
     3           FX1   ,FY1  ,FZ1  ,FX2  ,FY2  ,FZ2  ,
     4           FX3   ,FY3  ,FZ3  ,FX4  ,FY4  ,FZ4  ,
     5           FXI   ,FYI  ,FZI  ,A    ,STIFN)
            
         ELSEIF(IPARIT == 0)THEN
            CALL I7ASS0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2           NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3           FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4           FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5          FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6           IBID  ,BID  ,BID  ,BID  ,BID    ,BID  ,
     7           BID   ,BID  ,BID  ,JTASK,IBID   ) 
         ELSE
            CALL I7ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2           NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3           FX1   ,FY1   ,FZ1  ,FX2  ,FY2  ,FZ2    ,
     4           FX3   ,FY3   ,FZ3  ,FX4  ,FY4  ,FZ4    ,
     5           FXI   ,FYI   ,FZI  ,FSKYI,ISKY ,NISKYFI,
     6           NIN   ,NOINT ,IBID ,BID  ,BID  ,BID    ,
     7           BID   ,BID   ,BID  ,BID  ,BID  ,
     A           IBID )
         ENDIF
      ENDIF
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT  > 0)THEN
#include "lockon.inc"
            !---REACTION FORCE ON THE SOLID SURFACE            
            !   /H3D/NODA/CONT  (STAGGERED SCHEME) 
           DO I=1,JLT
             FCONT(1,IX1(I)) =FCONT(1,IX1(I)) + FX1(I)  
             FCONT(2,IX1(I)) =FCONT(2,IX1(I)) + FY1(I)  
             FCONT(3,IX1(I)) =FCONT(3,IX1(I)) + FZ1(I)  
             FCONT(1,IX2(I)) =FCONT(1,IX2(I)) + FX2(I)  
             FCONT(2,IX2(I)) =FCONT(2,IX2(I)) + FY2(I)  
             FCONT(3,IX2(I)) =FCONT(3,IX2(I)) + FZ2(I)  
             FCONT(1,IX3(I)) =FCONT(1,IX3(I)) + FX3(I)  
             FCONT(2,IX3(I)) =FCONT(2,IX3(I)) + FY3(I)  
             FCONT(3,IX3(I)) =FCONT(3,IX3(I)) + FZ3(I)  
             FCONT(1,IX4(I)) =FCONT(1,IX4(I)) + FX4(I)  
             FCONT(2,IX4(I)) =FCONT(2,IX4(I)) + FY4(I)  
             FCONT(3,IX4(I)) =FCONT(3,IX4(I)) + FZ4(I)  
           ENDDO
           !---REACTION FORCE ON THE FLUID SIDE                                         
           !   /H3D/ELEM/VECT/CONT WITH LAW151 (COLOCATED SCHEME)                       
           IF (MULTI_FVM%IS_USED) THEN 
             IF(H3D_DATA%N_VECT_CONT  > 0)THEN                                                 
               IDX=NUMELS*(ANIM_SE(10)+ANIM_SE(4960)+ANIM_SE(4961)) !skip vorticity data  
               ILEN=NUMELS+NUMELQ                                                         
               DO I=1,JLT                                                                 
                 IPOS=NSVG(I)
                 IF(IPOS>0) THEN ! local cell
                      IPOS=IPOS-NUMNOD                                                         
                      IPOS = IDX+3*(IPOS-1)                                                    
                      FANI_CELL(IPOS+1)=FANI_CELL(IPOS+1)-FXI(I)                               
                      FANI_CELL(IPOS+2)=FANI_CELL(IPOS+2)-FYI(I)                               
                      FANI_CELL(IPOS+3)=FANI_CELL(IPOS+3)-FZI(I)                               
                  ENDIF
               ENDDO     
             ENDIF                                                                 
           ELSE                                                                         
           !/H3D/NODA/CONT  (STAGGERED SCHEME)                                          
             DO I=1,JLT                                                               
              JG = NSVG(I)                                                              
              IF(JG > 0) THEN                                                          
                ! en SPMD : traitement a refaire apres reception noeud remote si JG < 0 
                FCONT(1,JG)=FCONT(1,JG)- FXI(I)                                         
                FCONT(2,JG)=FCONT(2,JG)- FYI(I)                                         
                FCONT(3,JG)=FCONT(3,JG)- FZI(I)
              ENDIF                                                                     
             ENDDO                                                                      
           ENDIF                                                                        
#include "lockoff.inc"
      ENDIF
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT > 0 .AND.
     .    (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D .OR.
     .    (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D /= 0))THEN
#include "lockon.inc"
           DO I=1,JLT
            FNCONT(1,IX1(I)) =FNCONT(1,IX1(I)) + FXI(I)*H1(I)
            FNCONT(2,IX1(I)) =FNCONT(2,IX1(I)) + FYI(I)*H1(I)
            FNCONT(3,IX1(I)) =FNCONT(3,IX1(I)) + FZI(I)*H1(I)
            FNCONT(1,IX2(I)) =FNCONT(1,IX2(I)) + FXI(I)*H2(I)
            FNCONT(2,IX2(I)) =FNCONT(2,IX2(I)) + FYI(I)*H2(I)
            FNCONT(3,IX2(I)) =FNCONT(3,IX2(I)) + FZI(I)*H2(I)
            FNCONT(1,IX3(I)) =FNCONT(1,IX3(I)) + FXI(I)*H3(I)
            FNCONT(2,IX3(I)) =FNCONT(2,IX3(I)) + FYI(I)*H3(I)
            FNCONT(3,IX3(I)) =FNCONT(3,IX3(I)) + FZI(I)*H3(I)
            FNCONT(1,IX4(I)) =FNCONT(1,IX4(I)) + FXI(I)*H4(I)
            FNCONT(2,IX4(I)) =FNCONT(2,IX4(I)) + FYI(I)*H4(I)
            FNCONT(3,IX4(I)) =FNCONT(3,IX4(I)) + FZI(I)*H4(I)
           ENDDO
#include "lockoff.inc"
      ENDIF
C-----------------------------------------------------
      IF(ISECIN > 0)THEN
        K0=NSTRF(25)
        IF(NSTRF(1)+NSTRF(2) /= 0)THEN
          DO I=1,NSECT
           NBINTER=NSTRF(K0+14)
           K1S=K0+30
           DO J=1,NBINTER
             IF(NSTRF(K1S) == NOINT)THEN
               IF(ISECUT /= 0)THEN
#include "lockon.inc"
                 DO K=1,JLT
                   ! attention aux signes pour le cumul des efforts
                   ! a rendre conforme avec CFORC3
                   IF(SECFCUM(4,IX1(K),I) == ONE)THEN
                    SECFCUM(1,IX1(K),I)=SECFCUM(1,IX1(K),I)-FX1(K)
                    SECFCUM(2,IX1(K),I)=SECFCUM(2,IX1(K),I)-FY1(K)
                    SECFCUM(3,IX1(K),I)=SECFCUM(3,IX1(K),I)-FZ1(K)
                   ENDIF
                   IF(SECFCUM(4,IX2(K),I) == ONE)THEN
                     SECFCUM(1,IX2(K),I)=SECFCUM(1,IX2(K),I)-FX2(K)
                     SECFCUM(2,IX2(K),I)=SECFCUM(2,IX2(K),I)-FY2(K)
                     SECFCUM(3,IX2(K),I)=SECFCUM(3,IX2(K),I)-FZ2(K)
                   ENDIF
                   IF(SECFCUM(4,IX3(K),I) == ONE)THEN
                     SECFCUM(1,IX3(K),I)=SECFCUM(1,IX3(K),I)-FX3(K)
                     SECFCUM(2,IX3(K),I)=SECFCUM(2,IX3(K),I)-FY3(K)
                     SECFCUM(3,IX3(K),I)=SECFCUM(3,IX3(K),I)-FZ3(K)
                   ENDIF
                   IF(SECFCUM(4,IX4(K),I) == ONE)THEN
                     SECFCUM(1,IX4(K),I)=SECFCUM(1,IX4(K),I)-FX4(K)
                     SECFCUM(2,IX4(K),I)=SECFCUM(2,IX4(K),I)-FY4(K)
                     SECFCUM(3,IX4(K),I)=SECFCUM(3,IX4(K),I)-FZ4(K)
                   ENDIF
                   JG = NSVG(K)
                   IF(JG > 0) THEN
                     ! en SPMD : traitement a refaire apres reception noeud remote si JG < 0
                     IF(SECFCUM(4,JG,I) == 1.)THEN
                       SECFCUM(1,JG,I)=SECFCUM(1,JG,I)+FXI(K)
                       SECFCUM(2,JG,I)=SECFCUM(2,JG,I)+FYI(K)
                       SECFCUM(3,JG,I)=SECFCUM(3,JG,I)+FZI(K)
                     ENDIF
                   ENDIF
                 ENDDO
#include "lockoff.inc"
               ENDIF
             ENDIF
             K1S=K1S+1
           ENDDO!NEXT J
           K0=NSTRF(K0+24)
          ENDDO!NEXT I
        ENDIF!(NSTRF(1)+NSTRF(2) /= 0)
      ENDIF!(ISECIN > 0)
C-----------------------------------------------------
      RETURN
      END
