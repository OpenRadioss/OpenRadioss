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
Chd|  MULTIFLUID_INIT2T             source/multifluid/multifluid_init2t.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        C3VEOK3                       source/elements/sh3n/coque3n/c3veok3.F
Chd|        M5IN2T                        source/initial_conditions/detonation/m5in2t.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTIFLUID_INIT2T(ELBUF_STR, NEL, NSIGS, NVC, IPARG, IXTG, ALE_CONNECTIVITY,
     .     IGEO, IPART, IPARTTG, IPM, PTSH3N, NPF, ILOADP, 
     .     XGRID, PM, GEO, SIGI, SKEW, TF, BUFMAT, FACLOAD, MULTI_FVM, 
     .     ERROR_THROWN, DETONATORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MULTI_FVM_MOD
      USE MESSAGE_MOD
      USE DETONATORS_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
! MVSIZ
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
! NIXTG, NPROPMI, NPROPM, LFACLOAD
#include      "param_c.inc"
! JEUL, LFT, LLT, NFT
#include      "vect01_c.inc"
! NUMSH3N
#include      "scry_c.inc"
! LIPART1
#include      "scr17_c.inc"
! SIZLOADP
#include      "com04_c.inc"
! N2D
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), INTENT(IN), TARGET :: ELBUF_STR
      INTEGER, INTENT(IN) :: NEL, NSIGS, IPARG(*), IXTG(NIXTG, *), 
     .     IGEO(*), PTSH3N(*), NPF(*), ILOADP(SIZLOADP, *),
     .     IPART(LIPART1, *), IPARTTG(*), IPM(NPROPMI, *)
      INTEGER, INTENT(OUT) :: NVC
      my_real, INTENT(IN) :: XGRID(3, *), FACLOAD(LFACLOAD, *)
      my_real,INTENT(INOUT) :: PM(NPROPM, *)
      my_real, INTENT(INOUT) :: GEO(*), SIGI(NSIGS, *), SKEW(LSKEW, *),
     .     TF(*), BUFMAT(*)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      LOGICAL :: ERROR_THROWN
      TYPE(DETONATOR_STRUCT_) DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF  
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      INTEGER :: NLAY, NGL(MVSIZ), MAT(MVSIZ), IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ)
      my_real ::
     .     Y1(MVSIZ), Z1(MVSIZ),
     .     Y2(MVSIZ), Z2(MVSIZ),
     .     Y3(MVSIZ), Z3(MVSIZ),
     .     NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .     LGTH1(MVSIZ), LGTH2(MVSIZ), LGTH3(MVSIZ),PRES,VFRAC
      INTEGER :: II, I, IP, IBID, ILAY, MATLAW
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
      IF(N2D>0 .AND. MTN /=151) THEN
         CALL ANCMSG(MSGID=1646,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1)
         RETURN
      ENDIF
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
C     Global buffer
      GBUF => ELBUF_STR%GBUF
C     Number of layers ( = number of material in law 151)
      NLAY = ELBUF_STR%NLAY
C     Gather nodale coordinates, compute surface (and volume)
      DO II = LFT, LLT
         I = II + NFT
         IX1(II) = IXTG(1 + 1, I)
         IX2(II) = IXTG(1 + 2, I)
         IX3(II) = IXTG(1 + 3, I)
         Y1(II) = XGRID(2, IXTG(1 + 1, I))
         Z1(II) = XGRID(3, IXTG(1 + 1, I))
         Y2(II) = XGRID(2, IXTG(1 + 2, I))
         Z2(II) = XGRID(3, IXTG(1 + 2, I))
         Y3(II) = XGRID(2, IXTG(1 + 3, I))
         Z3(II) = XGRID(3, IXTG(1 + 3, I))
         NGL(II) = IXTG(6, I)
         NX(II) = UNDEMI * ((Y2(II) - Y1(II)) * (Z3(II) - Z1(II)) - 
     .        (Z2(II) - Z1(II)) * (Y3(II) - Y1(II)))
         NY(II) = ZERO
         NZ(II) = ZERO
         GBUF%AREA(II) = SQRT(NX(II) * NX(II) + NY(II) * NY(II) + NZ(II) * NZ(II))
         IF (N2D .NE. 1) THEN
            GBUF%VOL(II) = GBUF%AREA(II)
         ELSE
            GBUF%VOL(II) = (Y1(II) + Y2(II) + Y3(II)) * (
     .           Y1(II) * (Z2(II) - Z3(II)) + 
     .           Y2(II) * (Z3(II) - Z1(II)) + 
     .           Y3(II) * (Z1(II) - Z2(II))) * UNSIX
         ENDIF
         ! TODO(DC) Calcul volume axi
         LGTH1(II) = SQRT((Y2(II) - Y1(II)) * (Y2(II) - Y1(II)) + 
     .        (Z2(II) - Z1(II)) * (Z2(II) - Z1(II)))
         LGTH2(II) = SQRT((Y3(II) - Y2(II)) * (Y3(II) - Y2(II)) + 
     .        (Z3(II) - Z2(II)) * (Z3(II) - Z2(II)))
         LGTH3(II) = SQRT((Y1(II) - Y3(II)) * (Y1(II) - Y3(II)) + 
     .        (Z1(II) - Z3(II)) * (Z1(II) - Z3(II)))
         GBUF%DELTAX(II) = GBUF%AREA(II) / MAX(LGTH1(II), LGTH2(II), LGTH3(II))
      ENDDO

      CALL C3VEOK3(NVC ,IX1 ,IX2 ,IX3 )
      
      PM(104,IXTG(1, 1 + NFT)) = ZERO  !global pressure
      
C     Loop over the materials
      DO ILAY = 1, NLAY
C     Layer buffer
         LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
         MBUF => ELBUF_STR%BUFLY(ILAY)%MAT(1,1,1)
         DO II = LFT, LLT
C     Material
            MAT(II) = IPM(20 + ILAY, IXTG(1, II + NFT))
C     Partial volumes
            LBUF%VOL(II) = PM(20 + ILAY, IXTG(1, II + NFT)) * GBUF%VOL(II)
         ENDDO
C     Material initialization
         IP = 1
         IBID = 0
         CALL MATINI(PM, IXTG, NIXTG, XGRID,
     .        GEO, ALE_CONNECTIVITY, DETONATORS, IPARG, 
     .        SIGI, NEL, SKEW, IGEO,
     .        IPART,IPARTTG,
     .        MAT, IPM, NSIGS, NUMSH3N, PTSH3N,
     .        IP, NGL, NPF, TF, BUFMAT,
     .        GBUF, LBUF, MBUF, ELBUF_STR, ILOADP,
     .        FACLOAD, GBUF%DELTAX)
     
         VFRAC = PM(20+ILAY,IXTG(1, 1 + NFT))
         PRES  = PM(104, IPM(20+ILAY,IXTG(1, 1 + NFT)))
         PM(104,IXTG(1, 1 + NFT)) = PM(104,IXTG(1, 1 + NFT)) + VFRAC * PRES !global pressure
     
         MATLAW = IPM(2, MAT(1))
         IF (MATLAW .EQ. 5) THEN
! JWL MAT
            IF (.NOT. ERROR_THROWN) THEN
               IF (PM(44, MAT(1)) .EQ. ZERO) THEN
                  CALL ANCMSG(MSGID = 1623, MSGTYPE = MSGERROR, ANMODE = ANINFO, 
     .                 I1 = IPM(1, IXTG(1, 1 + NFT)), I2 = IPM(1, MAT(1)))
               ENDIF
               ERROR_THROWN = .TRUE.
            ENDIF
            CALL M5IN2T(PM, MAT, DETONATORS, LBUF%TB, NGL, XGRID, IXTG, NIXTG)
         ENDIF
      ENDDO
      IF (NLAY .GT. 1) THEN
         IF (GBUF%G_TB .GT. 0) THEN
            GBUF%TB(LFT:LLT) = -EP30
         ENDIF
         DO ILAY = 1, NLAY
            MATLAW = IPM(2, IPM(20 + ILAY, IXTG(1, 1 +  NFT)))
            IF (MATLAW .EQ. 5) THEN
               LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
               DO II = LFT, LLT
                  GBUF%TB(II) = MAX(GBUF%TB(II), LBUF%TB(II))
               ENDDO
            ENDIF
         ENDDO
         
C      Mass globalization
         GBUF%RHO(LFT:LLT)=ZERO                 
         DO ILAY = 1, NLAY
            LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
            DO II = LFT, LLT
               GBUF%RHO(II) = GBUF%RHO(II) + LBUF%RHO(II) * PM(20 + ILAY, IXTG(1, II + NFT))
            ENDDO
         ENDDO
                  
C      Temperature globalization. We must solve later T such as e+p/rho=integral(Cp_global(T),dT)
         GBUF%TEMP(LFT:LLT)=ZERO
         DO ILAY = 1, NLAY
            LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
            DO II = LFT, LLT
               GBUF%TEMP(II) = GBUF%TEMP(II) + LBUF%TEMP(II) * PM(20 + ILAY, IXTG(1, II + NFT))*LBUF%RHO(II)/GBUF%RHO(II)   !volfrac*densfrac=massfrac
            ENDDO
         ENDDO    
                  
      ENDIF
      END SUBROUTINE MULTIFLUID_INIT2T
