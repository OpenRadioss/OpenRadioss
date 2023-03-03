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
Chd|  NLOCAL_INIT_STA               source/materials/fail/nlocal_init_sta.F
Chd|-- called by -----------
Chd|        NLOC_DMG_INIT                 source/materials/fail/nloc_dmg_init.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        C3FINT_REG_INI                source/elements/sh3n/coque3n/c3fint_reg_ini.F
Chd|        CBAFINT_REG_INI               source/elements/shell/coqueba/cbafint_reg_ini.F
Chd|        CDKFINT_REG_INI               source/elements/sh3n/coquedk/cdkfint_reg_ini.F
Chd|        CFINT_REG_INI                 source/elements/shell/coque/cfint_reg_ini.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE NLOCAL_INIT_STA(ELBUF_TAB,NLOC_DMG ,IPARG    ,IXC      ,
     .                           IXS      ,IXTG     ,AREA     ,X        ,
     .                           XREFS    ,XREFC    ,XREFTG   ,IPM      ,
     .                           BUFMAT   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
      USE MESSAGE_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_) , TARGET                    :: NLOC_DMG 
      INTEGER IPARG(NPARG,NGROUP),IXS(NIXS,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IPM(NPROPMI,*)
      my_real ,DIMENSION(NUMELC+NUMELTG),INTENT(IN)        :: 
     .   AREA 
      my_real
     .   X(3,*),XREFC(4,3,*),XREFTG(3,3,*),XREFS(8,3,*),BUFMAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: CONVERGED,FAILURE
      INTEGER :: L_NLOC,NG,ILOC,NEL,NFT,ITY,FORMU
      my_real
     .   DT_NL,TIME,NORM_1,NORM
      my_real,
     .  PARAMETER :: TOL1 = 5.0D-4
      my_real,
     .  PARAMETER :: TOL2 = 1.0D-8
      my_real, 
     .  DIMENSION(:), POINTER :: FNL,UNL,VNL,DNL,MNL

        ! Initialization of the convergence flag
        CONVERGED = .FALSE.
        ! Initialization of the failure flag
        FAILURE   = .FALSE.
        ! Size of the non-local vectors
        L_NLOC = NLOC_DMG%L_NLOC
        ! Pointing the non-local forces vector
        FNL => NLOC_DMG%FNL(1:L_NLOC,1)
        VNL => NLOC_DMG%VNL(1:L_NLOC)
        DNL => NLOC_DMG%DNL(1:L_NLOC)
        UNL => NLOC_DMG%UNL(1:L_NLOC)
        MNL => NLOC_DMG%MASS(1:L_NLOC)
        ! Initialization of the non-local time step
        DT_NL = HUNDRED
        FNL   = ZERO
        VNL   = ZERO
        DNL   = ZERO
        UNL   = ZERO
        TIME  = ZERO
        ! Loop of the initialization
        DO WHILE (.NOT.CONVERGED)
          ! Loop over the element groups
          DO NG = 1,NGROUP
            ! Non-local flag
            ILOC = IPARG(78,NG)
            ! If the group is non-local
            IF (ILOC>0) THEN
              ! Number of elements in the group
              NEL   = IPARG(2,NG)
              ! NFT value
              NFT   = IPARG(3,NG) 
              ! Type of elements
              ITY   = IPARG(5,NG)
              ! Formulation of the element
              FORMU = IPARG(23,NG)
              ! If the elements are shells
              IF (ITY == 3) THEN 
                IF ((FORMU>10).AND.(FORMU<29)) THEN
                  CALL CBAFINT_REG_INI(ELBUF_TAB,NLOC_DMG ,AREA     ,IXC     ,
     .                                 DT_NL    ,X        ,XREFC    ,NFT     ,
     .                                 NEL      ,NG       ,IPM      ,BUFMAT  ,
     .                                 TIME     ,FAILURE  )
                ELSE
                  CALL CFINT_REG_INI(ELBUF_TAB,NLOC_DMG ,AREA     ,IXC     ,
     .                               DT_NL    ,X        ,XREFC    ,NFT     ,
     .                               NEL      ,NG       ,IPM      ,BUFMAT  ,
     .                               TIME     ,FAILURE  )
                ENDIF
              ELSEIF (ITY == 7) THEN 
                IF ((FORMU==1).OR.(FORMU==2)) THEN
                  CALL C3FINT_REG_INI(ELBUF_TAB,NLOC_DMG ,AREA     ,IXTG     ,
     .                                DT_NL    ,X        ,XREFTG   ,NFT      ,
     .                                NEL      ,NG       ,IPM      ,BUFMAT   ,
     .                                TIME     ,FAILURE  )
                ELSEIF ((FORMU==30).OR.(FORMU==31)) THEN
                  CALL CDKFINT_REG_INI(ELBUF_TAB,NLOC_DMG ,AREA     ,IXTG     ,
     .                                 DT_NL    ,X        ,XREFTG   ,NFT      ,
     .                                 NEL      ,NG       ,IPM      ,BUFMAT   ,
     .                                 TIME     ,FAILURE  )
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          ! Computation of the accelerations
          FNL = FNL / MNL
          ! Computation of the velocities
          VNL = VNL + DT_NL*FNL
          ! Resetting the forces
          FNL = ZERO
          ! Norm of the previous increment
          NORM_1 = SQRT(DOT_PRODUCT(DNL,DNL))
          ! Computation of the increments
          DNL = DT_NL*VNL
          ! Norm of the current increment
          NORM = SQRT(DOT_PRODUCT(DNL,DNL))
          ! Computation of the cumulated variable
          UNL = UNL + DNL
          ! Convergence criterion
          CONVERGED = ((NORM<TOL1).AND.(ABS(NORM-NORM_1)<TOL2).AND.(TIME/=ZERO))
          IF (ISNAN(SQRT(DOT_PRODUCT(DNL,DNL)))) THEN 
            CALL ANCMSG(MSGID=1765,MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1) 
            EXIT
          ENDIF
          ! Updating time 
          TIME = TIME + DT_NL
        ENDDO
        ! Resetting the non-local vectors
        FNL = ZERO
        VNL = ZERO
        DNL = ZERO
        ! Checking failure
        IF (FAILURE) THEN 
          CALL ANCMSG(MSGID=1767,MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1)         
        ENDIF
c-----------
      END
