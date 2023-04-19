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
Chd|  MULTIFLUID_INIT2              source/multifluid/multifluid_init2.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        M5IN2                         source/initial_conditions/detonation/m5in2.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|        QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|        QVOLI2                        source/elements/solid_2d/quad/qvoli2.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MULTIFLUID_INIT2(NEL, NSIGS, 
     .     IPARG, IXQ, IPM, ALE_CONNECTIVITY, IGEO, IPART, IPARTQ, NPF,
     .     PTQUAD, ILOADP, X, PM, 
     .     GEO, SIGI, SKEW, TF, BUFMAT, FACLOAD, ELBUF_STR, ERROR_THROWN, DETONATORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
! NIXQ, NPROPMI, NPROPM, LFACLOAD
#include      "param_c.inc"
! JEUL, LFT, LLT, NFT
#include      "vect01_c.inc"
! LIPART1
#include      "scr17_c.inc"
! NUMQUAD
#include      "scry_c.inc"
! SIZLOADP
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL, NSIGS, IPARG(*), IXQ(NIXQ, *), IPM(NPROPMI, *), 
     .     IGEO(*), IPART(LIPART1, *), IPARTQ(*), PTQUAD(*),
     .     NPF(*), ILOADP(SIZLOADP, *)
      TYPE(ELBUF_STRUCT_), INTENT(IN), TARGET :: ELBUF_STR
      my_real, INTENT(IN) :: X(*),  FACLOAD(LFACLOAD, *)
      my_real, INTENT(INOUT) :: PM(NPROPM, *)
      my_real, INTENT(INOUT) :: GEO(*), SIGI(NSIGS, *), 
     .     SKEW(LSKEW, *), TF(*), BUFMAT(*)
      LOGICAL :: ERROR_THROWN
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ILAY, NLAY, II, IP, IBID, MATLAW
      INTEGER :: NGL(MVSIZ), MAT(MVSIZ), PID(MVSIZ)
      INTEGER :: IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
      my_real :: 
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),Y4(MVSIZ),  
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),Z4(MVSIZ),
     .     SY(MVSIZ), SZ(MVSIZ), TY(MVSIZ),TZ(MVSIZ),PRES,VFRAC
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF  
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
C     Global buffer
      GBUF => ELBUF_STR%GBUF
C     Number of layers ( = number of material in law 151)
      NLAY = ELBUF_STR%NLAY
C     Gather coordinates, material Id and so on
      CALL QCOOR2(X, IXQ(1, NFT + 1), NGL, MAT, PID, 
     .     IX1, IX2, IX3, IX4, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4,
     .     SY, SZ, TY, TZ)
C     Volume, area computation
      CALL QVOLI2(GBUF%VOL, IXQ(1, NFT + 1), NGL, GBUF%AREA, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4)
C     Compute deltax
      IF (JEUL /= 0) THEN
         CALL QDLEN2(IPARG(63), 
     .        GBUF%AREA, GBUF%DELTAX, 
     .        Y1, Y2, Y3, Y4,
     .        Z1, Z2, Z3, Z4)
      ENDIF

      PM(104,IXQ(1, 1 + NFT)) = ZERO  !global pressure

C     Loop over the materials
      DO ILAY = 1, NLAY
C     Layer buffer
         LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
         MBUF => ELBUF_STR%BUFLY(ILAY)%MAT(1,1,1)
         DO II = LFT, LLT
C     Material
            MAT(II) = IPM(20 + ILAY, IXQ(1, II + NFT))
C     Partial volumes
            LBUF%VOL(II) = PM(20 + ILAY, IXQ(1, II + NFT)) * GBUF%VOL(II)
         ENDDO
C     Material initialization
         IP = 1
         IBID = 0
         CALL MATINI(PM, IXQ, NIXQ, X,
     .        GEO, ALE_CONNECTIVITY, DETONATORS, IPARG, 
     .        SIGI, NEL, SKEW, IGEO,
     .        IPART,IPARTQ,
     .        MAT, IPM, NSIGS, NUMQUAD, PTQUAD,
     .        IP, NGL,NPF, TF, BUFMAT,
     .        GBUF, LBUF, MBUF, ELBUF_STR, ILOADP,
     .        FACLOAD, GBUF%DELTAX)
     
         VFRAC = PM(20+ILAY,IXQ(1, 1 + NFT))
         PRES  = PM(104, IPM(20+ILAY,IXQ(1, 1 + NFT)))
         PM(104,IXQ(1, 1 + NFT)) = PM(104,IXQ(1, 1 + NFT)) + VFRAC * PRES !global pressure
     
         MATLAW = IPM(2, MAT(1))
         IF (MATLAW == 5) THEN
!     JWL MAT - layer detonation times
            IF (.NOT. ERROR_THROWN) THEN
               IF (PM(44, MAT(1)) == ZERO) THEN
                  CALL ANCMSG(MSGID = 1623, MSGTYPE = MSGERROR, ANMODE = ANINFO, 
     .                 I1 = IPM(1, IXQ(1, 1 + NFT)), I2 = IPM(1, MAT(1)))
               ENDIF
               ERROR_THROWN = .TRUE.
            ENDIF
            CALL M5IN2(PM, MAT, IPM(1, IXQ(1,LFT+NFT)), DETONATORS, LBUF%TB, NGL, X, IXQ, NIXQ)
         ENDIF
      ENDDO

      IF (NLAY > 1) THEN
         
C      Mass globalization
         GBUF%RHO(LFT:LLT)=ZERO                 
         DO ILAY = 1, NLAY
            LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
            DO II = LFT, LLT
               GBUF%RHO(II) = GBUF%RHO(II) + LBUF%RHO(II) * PM(20 + ILAY, IXQ(1, II + NFT))
            ENDDO
         ENDDO
                  
C      Temperature globalization. We must solve later T such as e+p/rho=integral(Cp_global(T),dT)
         GBUF%TEMP(LFT:LLT)=ZERO
         DO ILAY = 1, NLAY
            LBUF  => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
            DO II = LFT, LLT
               GBUF%TEMP(II) = GBUF%TEMP(II) + LBUF%TEMP(II) * PM(20 + ILAY, IXQ(1, II + NFT))*LBUF%RHO(II)/GBUF%RHO(II)   !volfrac*densfrac=massfrac
            ENDDO
         ENDDO    
     
      ENDIF
                        
      END SUBROUTINE MULTIFLUID_INIT2
