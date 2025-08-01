Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    pforc3              ../engine/source/elements/beam/pforc3.F
!||--- called by ------------------------------------------------------
!||    forint              ../engine/source/elements/forint.F
!||--- calls      -----------------------------------------------------
!||    finter              ../engine/source/tools/curve/finter.F
!||    main_beam18         ../engine/source/elements/beam/main_beam18.F
!||    main_beam3          ../engine/source/elements/beam/main_beam3.F
!||    pbilan              ../engine/source/elements/beam/pbilan.F
!||    pcoor3              ../engine/source/elements/beam/pcoor3.F
!||    pcurv3              ../engine/source/elements/beam/pcurv3.F
!||    pdamp3              ../engine/source/elements/beam/pdamp3.F
!||    pdefo3              ../engine/source/elements/beam/pdefo3.F
!||    pdlen3              ../engine/source/elements/beam/pdlen3.F
!||    pevec3              ../engine/source/elements/beam/pevec3.F
!||    pfcum3              ../engine/source/elements/beam/pfcum3.F
!||    pfcum3p             ../engine/source/elements/beam/pfcum3p.F
!||    pfint3              ../engine/source/elements/beam/pfint3.F
!||    pmcum3              ../engine/source/elements/beam/pmcum3.F
!||    pmcum3p             ../engine/source/elements/beam/pmcum3p.F
!||    ppxpy3              ../engine/source/elements/beam/ppxpy3.F
!||    preload_axial       ../engine/source/elements/spring/preload_axial.F90
!||    thermexppg          ../engine/source/elements/beam/thermexpp.F
!||    thermexppi          ../engine/source/elements/beam/thermexpp.F
!||--- uses       -----------------------------------------------------
!||    dt_mod              ../engine/source/modules/dt_mod.F
!||    elbufdef_mod        ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    h3d_mod             ../engine/share/modules/h3d_mod.F
!||    mat_elem_mod        ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    preload_axial_mod   ../engine/source/elements/spring/preload_axial.F90
!||    sensor_mod          ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE PFORC3(
     1           ELBUF_STR,JFT      ,JLT      ,NEL      ,
     2           MTN      ,ISMSTR   ,PM       ,NCC      ,
     3           X        ,F        ,M        ,V        ,
     4           R        ,GEO      ,PARTSAV  ,DT2T     ,
     5           NELTST   ,ITYPTST  ,STIFN    ,STIFR    ,
     6           FSKY     ,IADP     ,OFFSET   ,IPARTP   ,
     7           TANI     ,FX1      ,FX2      ,FY1      ,
     8           FY2      ,FZ1      ,FZ2      ,MX1      ,
     9           MX2      ,MY1      ,MY2      ,MZ1      ,
     A           MZ2      ,IGEO     ,IPM      ,BUFMAT   ,
     B           NPT      ,NPF      ,TF       ,GRESAV   ,
     C           GRTH     ,IGRTH    ,MSP      ,DMELP    ,
     D           IOUTPRT  ,ITASK    ,JTHE     ,TEMP     ,
     E           FTHE     ,FTHESKY  ,IEXPAN   ,H3D_DATA ,
     F           JSMS     ,IGRE     ,NFT      ,IFAIL    ,
     G           SBUFMAT  ,SNPC     ,STF      ,NUMMAT   ,
     H           NUMGEO   ,IOUT     ,ISTDO    ,IDEL7NOK ,
     I           IDYNA    ,IMCONV   ,IMPL_S   ,MAT_PARAM,
     J           PRELD1   ,STF_F    ,DT       ,SENSORS  ,
     H           NTABLE   ,TABLE    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
      USE H3D_MOD
      USE PRELOAD_AXIAL_MOD
      USE ELBUFDEF_MOD
      USE DT_MOD
      USE SENSOR_MOD
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT,IGRE,JSMS,IFAIL
      INTEGER ,INTENT(IN) :: SBUFMAT
      INTEGER ,INTENT(IN) :: SNPC
      INTEGER ,INTENT(IN) :: STF
      INTEGER ,INTENT(IN) :: NUMMAT
      INTEGER ,INTENT(IN) :: NUMGEO
      INTEGER ,INTENT(IN) :: IOUT
      INTEGER ,INTENT(IN) :: ISTDO
      INTEGER ,INTENT(IN) :: IMPL_S
      INTEGER ,INTENT(IN) :: IDYNA
      INTEGER ,INTENT(IN) :: IMCONV
      INTEGER ,INTENT(INOUT) :: IDEL7NOK
      INTEGER,INTENT(IN) :: NTABLE
      TYPE(TTABLE), DIMENSION(NTABLE), INTENT(INOUT)    :: TABLE      ! TABLE DATA   
      INTEGER NCC(NIXP,*),IADP(2,*),IPARTP(*),IGEO(NPROPGI,*),
     .        IPM(NPROPMI,*),NPF(*),GRTH(*),IGRTH(*)
      INTEGER JFT,JLT,NELTST,ITYPTST,OFFSET,NEL,JTHE,
     .        MTN,ISMSTR,NPT,IOUTPRT,ITASK,IEXPAN
      my_real DT2T ,
     .   PM(NPROPM,*), X(*), F(*), M(*), V(*), R(*),GEO(NPROPG,*),TF(*),
     .   BUFMAT(*),PARTSAV(*),STIFN(*),STIFR(*),FSKY(*),TANI(15,*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .   GRESAV(*),MSP(*),DMELP(*),TEMP(*),FTHE(*),
     .   FTHESKY(*)
      my_real, INTENT(IN)          :: PRELD1,STF_F
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
      TYPE (DT_), INTENT(IN) :: DT
      type (sensors_),INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFLAG,IGTYP,NUPARAM,NUVAR,NFUNC,IFUNC_ALPHA,IMAT
      INTEGER MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),IFUNC(100),
     .   NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),IPT
      my_real 
     .   STI(MVSIZ),STIR(MVSIZ),OFF(MVSIZ),AL(MVSIZ),EXX(MVSIZ),
     .   EXY(MVSIZ),EXZ(MVSIZ),KXX(MVSIZ),KYY(MVSIZ),KZZ(MVSIZ),
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),M1(MVSIZ),M2(MVSIZ),M3(MVSIZ),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),
     .   Y3(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),RX1G(MVSIZ),RX2G(MVSIZ),
     .   RY1G(MVSIZ),RY2G(MVSIZ),RZ1G(MVSIZ),RZ2G(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .   E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),VX1G(MVSIZ),VX2G(MVSIZ),
     .   VY1G(MVSIZ),VY2G(MVSIZ),VZ1G(MVSIZ),VZ2G(MVSIZ),
     .   F11(MVSIZ), F12(MVSIZ), F21(MVSIZ),  
     .   F22(MVSIZ), F31(MVSIZ), F32(MVSIZ), 
     .   M11(MVSIZ), M12(MVSIZ), M21(MVSIZ),
     .   M22(MVSIZ), M31(MVSIZ), M32(MVSIZ),TEMPEL(MVSIZ),DTEMP(MVSIZ),
     .   FSCAL_ALPHA,ETH(MVSIZ),DEINTTH,ALPHA,DF,VL12(MVSIZ),DTINV
C
      my_real ::  KC,PHIX, CA,CB, AREA, FPHI(MVSIZ,2),DIE(MVSIZ)
      my_real ,DIMENSION(NEL) ::  SIGX,SAPT,VECNUL
      my_real ,DIMENSION(:) ,POINTER :: UVAR
      my_real ,DIMENSION(:) ,POINTER :: EL_TEMP
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TARGET :: TEMPEL,VECNUL
C-----------------------------------------------      
       my_real FINTER 
      EXTERNAL FINTER
C===============================================================================
      GBUF => ELBUF_STR%GBUF
          vecnul(:) = zero          
C-----------------------------------------------
      CALL PCOOR3(
     1   X,       NCC,     MAT,     PID,
     2   NGL,     NC1,     NC2,     NC3,
     3   X1,      X2,      X3,      Y1,
     4   Y2,      Y3,      Z1,      Z2,
     5   Z3,      NEL)
      CALL PEVEC3(
     1   GBUF%SKEW,R,        AL,       NC1,
     2   NC2,      NC3,      X1,       X2,
     3   X3,       Y1,       Y2,       Y3,
     4   Z1,       Z2,       Z3,       RX1G,
     5   RX2G,     RY1G,     RY2G,     RZ1G,
     6   RZ2G,     E1X,      E1Y,      E1Z,
     7   E2X,      E2Y,      E2Z,      E3X,
     8   E3Y,      E3Z,      NEL)
      IF (ISMSTR /= 0) CALL PPXPY3(
     1   GBUF%LENGTH,AL,         NEL)
      IGTYP = IGEO(11,PID(1))
      CALL PDLEN3(
     1   JFT,      JLT,      PM,       GEO,
     2   GBUF%OFF, DT2T,     NELTST,   ITYPTST,
     3   STI,      STIR,     MSP,      DMELP,
     4   GBUF%G_DT,GBUF%DT,  AL,       MAT,
     5   PID,      NGL,      NEL,      IGTYP,
     6   JSMS)
      CALL PDEFO3(
     1   V,       EXX,     EXY,     EXZ,
     2   AL,      NC1,     NC2,     NC3,
     3   E1X,     E1Y,     E1Z,     E2X,
     4   E2Y,     E2Z,     E3X,     E3Y,
     5   E3Z,     VX1G,    VX2G,    VY1G,
     6   VY2G,    VZ1G,    VZ2G,    NEL)
      CALL PCURV3(
     1   R,       GEO,     GBUF%OFF,OFF,
     2   EXX,     EXY,     EXZ,     KXX,
     3   KYY,     KZZ,     AL,      NC1,
     4   NC2,     NC3,     RX1G,    RX2G,
     5   RY1G,    RY2G,    RZ1G,    RZ2G,
     6   E1X,     E1Y,     E1Z,     E2X,
     7   E2Y,     E2Z,     E3X,     E3Y,
     8   E3Z,     PID,     NEL)
C---
      IMAT = MAT(1)
      NUPARAM = IPM(9,MAT(1))
C---
      DIE(1:NEL)   = ZERO
      IF (JTHE > 0) THEN 
         DO I=1,NEL 
           TEMPEL(I) = HALF *( TEMP(NC1(I)) + TEMP(NC2(I)))  
           DIE(I) = GBUF%EINT(I) + GBUF%EINT(NEL + I)
         ENDDO
      ENDIF
  
      IF (IEXPAN > 0 .AND. JTHE > 0) THEN
        IF (TT == ZERO) GBUF%TEMP(1:NEL) = TEMPEL(1:NEL)
        DTEMP(1:NEL) = TEMPEL(1:NEL) - GBUF%TEMP(1:NEL)
        GBUF%TEMP(1:NEL) = TEMPEL(1:NEL)
c      
        DO I=1,NEL 
          IFUNC_ALPHA = IPM(219, MAT(I))
          FSCAL_ALPHA = PM(191, MAT(I))
          ALPHA = FSCAL_ALPHA*FINTER(IFUNC_ALPHA,TEMPEL(I),NPF,TF,DF)
          ETH(I) = ALPHA*DTEMP(I)
          DEINTTH = - HALF*GBUF%FOR(I)*ETH(I)*AL(I)*OFF(I) 
          GBUF%EINTTH(I) = GBUF%EINTTH(I)  + DEINTTH
        ENDDO 
      ENDIF   
!
      if (jthe /= 0) then
        el_temp => tempel(1:nel)
      else if (igtyp == 3) then
        if (elbuf_str%gbuf%g_temp > 0) then
          el_temp => elbuf_str%gbuf%temp
        else
          el_temp => vecnul(1:nel)
        end if
      else if (igtyp == 18) then
        if (elbuf_str%bufly(1)%l_temp > 0) then
          el_temp => elbuf_str%bufly(1)%lbuf(1,1,1)%temp
        else
          el_temp => vecnul(1:nel)
        endif
      endif
C---
      IF (IGTYP == 3) THEN
        ! beams type 3 (global integration)
        NUVAR =  GBUF%G_NUVAR
        UVAR  => GBUF%VAR

        CALL MAIN_BEAM3(
     .       ELBUF_STR,NEL      ,MTN      ,JTHE     ,IFAIL    ,
     .       IPM      ,PM       ,GEO      ,EL_TEMP  ,OFF      ,
     .       MAT      ,PID      ,NGL      ,TT       ,DT1      ,
     .       AL       ,NPF      ,TF       ,EXX      ,EXY      ,
     .       EXZ      ,KXX      ,KYY      ,KZZ      ,F1       ,
     .       F2       ,F3       ,M1       ,M2       ,M3       ,
     .       BUFMAT   ,NPROPG   ,NPROPMI  ,NPROPM   ,NUMMAT   ,
     .       NUMGEO   ,SBUFMAT  ,SNPC     ,STF      ,IOUT     ,
     .       ISTDO    ,NUVAR    ,UVAR     ,GBUF%EPSD,IMAT   ,
     .       GBUF%FOR ,GBUF%MOM ,GBUF%EINT,ISMSTR   ,MAT_PARAM(IMAT),
     .       NTABLE   ,TABLE ) 
C----
      ELSEIF (IGTYP == 18) THEN  
        ! beams type 18 (integration points in the section)
        CALL MAIN_BEAM18(ELBUF_STR,
     1       NEL       ,NPT         ,MTN       ,IMAT   ,                
     2       PID       ,NGL         ,PM        ,IPM      ,                
     3       GEO       ,OFF         ,GBUF%FOR  ,GBUF%MOM ,                
     4       GBUF%EINT ,AL          ,GBUF%EPSD ,BUFMAT   ,NPF     ,       
     5       TF        ,EXX         ,EXY       ,EXZ      ,KXX     ,       
     6       KYY       ,KZZ         ,F1        ,F2       ,F3      ,       
     7       M1        ,M2          ,M3        ,JTHE     ,EL_TEMP ,       
     8       IFAIL     ,SBUFMAT     ,SNPC      ,STF      ,NUMMAT  ,       
     9       NUMGEO    ,IOUT        ,ISTDO     ,NPROPMI  ,NPROPM  ,
     A       NPROPG    ,TT          ,DT1       ,IDEL7NOK ,ISIGI   ,
     B       IMCONV    ,ISMSTR      ,MAT_PARAM(IMAT),
     C       NTABLE    ,TABLE )                                             
      ENDIF
c---------------------------
c     element damping
c---------------------------
      CALL PDAMP3(PM      ,GEO     ,OFF     ,IMAT  ,PID(1),
     .            NEL     ,NGL     ,EXX     ,EXY     ,EXZ  ,
     .            KXX     ,KYY     ,KZZ     ,AL      ,F1   ,
     .            F2      ,F3      ,M1      ,M2      ,M3   ,
     .            IMPL_S  ,IDYNA   ,DT1     )
c-----------------------------------------------------------------------  
c     Thermal expansion
c-----------------------------------------------------------------------  
      IF (JTHE > 0) THEN 
        IF (IEXPAN > 0) THEN
          IF (IGTYP == 3) THEN   
            CALL THERMEXPPG(NEL    ,MAT       ,PID          ,PM     ,GEO  ,
     .                      OFF    ,ETH       ,GBUF%FOR     ,GBUF%EINT ) 
          
          ELSEIF(IGTYP == 18) THEN
           CALL THERMEXPPI(ELBUF_STR,   
     .              NEL     ,NPT      ,MAT       ,PID       ,PM       , 
     .              GEO      ,AL      ,ETH       ,OFF      ,GBUF%FOR  ,
     .              GBUF%EINT)
          ENDIF
C          
          DO I=1,NEL
             DEINTTH = -HALF*GBUF%FOR(I)*ETH(I)*AL(I)*OFF(I) 
             GBUF%EINT(I) = GBUF%EINT(I) + DEINTTH
          ENDDO
        ENDIF
        DO I=1,NEL 
          DIE(I) = (GBUF%EINT(I) + GBUF%EINT(NEL + I) - DIE(I))*PM(90,MAT(I))
        ENDDO
      ENDIF            
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
      IFLAG = MOD(NCYCLE,NCPRI)
      IF (IOUTPRT > 0)     
     .  CALL PBILAN(
     1   PM,       V,        GBUF%EINT,GEO,
     2   PARTSAV,  IPARTP,   TANI,     GBUF%FOR,
     3   GBUF%MOM, GRESAV,   GRTH,     IGRTH,
     4   GBUF%OFF, NEL,      AL,       NC1,
     5   NC2,      NC3,      E1X,      E1Y,
     6   E1Z,      E2X,      E2Y,      E2Z,
     7   MAT,      PID,      VX1G,     VX2G,
     8   VY1G,     VY2G,     VZ1G,     VZ2G,
     9   X1,       X2,       Y1,       Y2,
     A   Z1,       Z2,       ITASK,    H3D_DATA,
     B   IGRE,     SENSORS,GBUF%G_WPLA,GBUF%WPLA)
C-------------------------
C     /PRELOAD/AXIAL
C-------------------------
        IF (PRELD1>ZERO) THEN
           DTINV = DT1/MAX(DT1**2,EM20)  
           DO I=1,NEL
              VL12(I) = EXX(I)*DTINV
           ENDDO
           CALL PRELOAD_AXIAL(NEL,PRELD1,GBUF%BPRELD,VL12,STF_F,F1 )
           IF (IGTYP == 18) THEN
             SAPT(1:NEL)=ZERO
             DO IPT = 1,NPT        
               DO I=1,NEL
                 SAPT(I) = SAPT(I) + GEO(400+IPT,PID(I))    
               ENDDO
             ENDDO
             SIGX(1:NEL) = F1(1:NEL)/SAPT(1:NEL)
             DO IPT = 1,NPT        
               ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%SIG(1:NEL) = SIGX(1:NEL) 
             ENDDO
           END IF
           GBUF%FOR(1:NEL) = F1(1:NEL) 
        END IF
C----------------------------
C     FORCES INTERNES
C----------------------------
      CALL PFINT3(GBUF%FOR ,GBUF%MOM ,GEO     ,GBUF%OFF   ,OFF,
     .            AL       ,F1       ,F2      ,F3         ,M1         ,
     .            M2       ,M3       ,STI     ,STIR       ,NEL,
     .            PID      ,F11      ,F12     ,F21        ,F22,
     .            F31      ,F32      ,M11     ,M12        ,M21,
     .            M22      ,M31      ,M32     )
C-------------------------
c     Thermal flux 
C--------------------------
C
       IF (JTHE > 0) THEN
        DO I=1,NEL
          CA = PM(75,MAT(I))
          CB = PM(76,MAT(I))
          AREA =GEO(1,PID(I)) 
          KC = (CA + CB*TEMPEL(I))*DT2T *AREA/AL(I)   
          PHIX = KC*(TEMP(NC2(I)) - TEMP(NC1(I)))
C
C force thermique nodale
C
          FPHI(I,1) = HALF * DIE(I) + PHIX ! 
          FPHI(I,2) = HALF * DIE(I) - PHIX
         ENDDO
      ENDIF     
C-------------------------
C     ASSEMBLAGE
C-------------------------
      IF (IPARIT == 0) THEN
        CALL PFCUM3(
     1   F,       STI,     STIFN,   FX1,
     2   FX2,     FY1,     FY2,     FZ1,
     3   FZ2,     NC1,     NC2,     NC3,
     4   E1X,     E1Y,     E1Z,     E2X,
     5   E2Y,     E2Z,     E3X,     E3Y,
     6   E3Z,     F11,     F12,     F21,
     7   F22,     F31,     F32,     FPHI,
     8   FTHE,    NEL,     JTHE)
        CALL PMCUM3(
     1   M,       STIR,    STIFR,   MX1,
     2   MX2,     MY1,     MY2,     MZ1,
     3   MZ2,     NC1,     NC2,     NC3,
     4   E1X,     E1Y,     E1Z,     E2X,
     5   E2Y,     E2Z,     E3X,     E3Y,
     6   E3Z,     M11,     M12,     M21,
     7   M22,     M31,     M32,     NEL)
      ELSE
        CALL PFCUM3P(
     1   STI,     FSKY,    FSKY,    IADP,
     2   FX1,     FX2,     FY1,     FY2,
     3   FZ1,     FZ2,     NC1,     NC2,
     4   NC3,     E1X,     E1Y,     E1Z,
     5   E2X,     E2Y,     E2Z,     E3X,
     6   E3Y,     E3Z,     F11,     F12,
     7   F21,     F22,     F31,     F32,
     8   FPHI,    FTHESKY, NEL,     NFT,
     9   JTHE)
     
        CALL PMCUM3P(
     1   STIR,    FSKY,    FSKY,    IADP,
     2   MX1,     MX2,     MY1,     MY2,
     3   MZ1,     MZ2,     NC1,     NC2,
     4   NC3,     E1X,     E1Y,     E1Z,
     5   E2X,     E2Y,     E2Z,     E3X,
     6   E3Y,     E3Z,     M11,     M12,
     7   M21,     M22,     M31,     M32,
     8   NEL,     NFT)
      ENDIF
C-----------------------------------------------
      RETURN
      END SUBROUTINE PFORC3
