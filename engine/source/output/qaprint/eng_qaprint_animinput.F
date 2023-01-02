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
Chd|  ENG_QAPRINT_ANIMINPUT         source/output/qaprint/eng_qaprint_animinput.F
Chd|-- called by -----------
Chd|        ENG_QAPRINT_DRIVER            source/output/qaprint/eng_qaprint_driver.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ENG_QAPRINT_ANIMINPUT()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr25_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
C-----------------------------------------------
C     QA PRINT FOR ANIM INPUTS
C-----------------------------------------------
      IF (MYQAKEY('/ANIM')) THEN

        WRITE(VARNAME,'(A)') 'ANIM_VERS'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_VERS,0.0_8)

        WRITE(VARNAME,'(A)') 'TANIM0'
        TEMP_DOUBLE = TANIM0
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'DTANIM0'
        TEMP_DOUBLE = DTANIM0
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

        WRITE(VARNAME,'(A)') 'IANISENS'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IANISENS,0.0_8)

        WRITE(VARNAME,'(A)') 'NLSENS'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NLSENS,0.0_8)

        WRITE(VARNAME,'(A)') 'MX_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),MX_ANI,0.0_8)

        DO I = 1,MX_ANI
          IF(ANIM_V(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_V_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_V(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_T(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_T_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_T(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_E(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_E_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_E(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_N(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_N_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_N(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_ST(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_ST_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_ST(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_SE(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_SE_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_SE(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_CT(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_CT_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_CT(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_CE(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_CE_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_CE(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_FT(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_FT_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_FT(I),0.0_8)
          ENDIF
        ENDDO
        DO I = 1,MX_ANI
          IF(ANIM_FE(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_FE_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_FE(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MX_ANI
          IF(ANIM_FE(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'LANISENS_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LANISENS(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MX_ANI
          IF(NTITLETAB(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'NTITLETAB_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NTITLETAB(I),0.0_8)
          ENDIF
        ENDDO

        WRITE(VARNAME,'(A)') 'NV_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NV_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NT_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NT_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NE_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NE_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NN_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NN_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NST_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NST_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NSE_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NSE_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NCT_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NCT_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NCE_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NCE_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NFT_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NFT_ANI,0.0_8)

        WRITE(VARNAME,'(A)') 'NFE_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NFE_ANI,0.0_8)
 
        DO I = 1,MAXLAY
          IF(ANIM_STRESS(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_STRESS_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_STRESS(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MAXLAY
          IF(ANIM_STRAIN(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_STRAIN_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_STRAIN(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MAXLAY
          IF(ANIM_EPSP(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_EPSP_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_EPSP(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MAXLAY
          IF(ANIM_DAMA(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_DAMA_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_DAMA(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MAXLAY
          IF(ANIM_WPLA(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_WPLA_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_WPLA(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MAXLAY
          IF(ANIM_EPSDOT(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'ANIM_EPSDOT_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_EPSDOT(I),0.0_8)
          ENDIF
        ENDDO

        WRITE(VARNAME,'(A)') 'IZIP'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IZIP,0.0_8)

        WRITE(VARNAME,'(A)') 'ANIM_MAT'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_MAT,0.0_8)

        WRITE(VARNAME,'(A)') 'ANIM_M'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_M,0.0_8)

        WRITE(VARNAME,'(A)') 'ANIM_K'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_K,0.0_8)

        WRITE(VARNAME,'(A)') 'ANIM_U'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_U,0.0_8)

        WRITE(VARNAME,'(A)') 'ANIM_CRK'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_CRK,.0_8)

        WRITE(VARNAME,'(A)') 'FMT_ANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),FMT_ANI,.0_8)

        WRITE(VARNAME,'(A)') 'IFVANI'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IFVANI,.0_8)

        WRITE(VARNAME,'(A)') 'IAD_GPS'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IAD_GPS,.0_8)

        WRITE(VARNAME,'(A)') 'IZIPSTRS'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IZIPSTRS,.0_8)

        WRITE(VARNAME,'(A)') 'ISTRAIALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISTRAIALL,.0_8)

        WRITE(VARNAME,'(A)') 'IEPSDOALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IEPSDOALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IEPSDOFULL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IEPSDOFULL,0.0_8)

        WRITE(VARNAME,'(A)') 'IEPSPALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IEPSPALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IEPSPFULL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IEPSPFULL,0.0_8)

        WRITE(VARNAME,'(A)') 'ISTRESALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISTRESALL,0.0_8)

        WRITE(VARNAME,'(A)') 'ISTRESFULL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISTRESFULL,0.0_8)

        WRITE(VARNAME,'(A)') 'ISTRAINFULL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISTRAINFULL,0.0_8)

        WRITE(VARNAME,'(A)') 'IEPSDOT'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IEPSDOT,0.0_8)

        WRITE(VARNAME,'(A)') 'IFAILA'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IFAILA,0.0_8)

        WRITE(VARNAME,'(A)') 'IPHIALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPHIALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IDAMAALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDAMAALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IDAMAFULL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDAMAFULL,0.0_8)

        WRITE(VARNAME,'(A)') 'INXTFALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INXTFALL,0.0_8)

        WRITE(VARNAME,'(A)') 'SIGH1ALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SIGH1ALL,0.0_8)

        WRITE(VARNAME,'(A)') 'SIGH2ALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SIGH2ALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IWPLAALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IWPLAALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IWPLAFULL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IWPLAFULL,0.0_8)

        WRITE(VARNAME,'(A)') 'IBRICK_STRESSALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBRICK_STRESSALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IBRICK_STRAINALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBRICK_STRAINALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IBRICK_EPSPALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBRICK_EPSPALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IORTHDALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IORTHDALL,0.0_8)

        WRITE(VARNAME,'(A)') 'IBEAM_EPSPALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBEAM_EPSPALL,0.0_8)

        WRITE(VARNAME,'(A)') 'ANIM_PLY'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ANIM_PLY,0.0_8)

        WRITE(VARNAME,'(A)') 'ISTRESALL_PLY'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISTRESALL_PLY,0.0_8)

        WRITE(VARNAME,'(A)') 'ISTRAINALL_PLY '
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISTRAINALL_PLY ,0.0_8)

        WRITE(VARNAME,'(A)') 'IEPSDOTALL_PLY'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IEPSDOTALL_PLY,0.0_8)

        WRITE(VARNAME,'(A)') 'IPHIALL_PLY'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPHIALL_PLY,0.0_8)

        WRITE(VARNAME,'(A)') 'IEPSPALL_PLY'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IEPSPALL_PLY,0.0_8)

        WRITE(VARNAME,'(A)') 'IDAMAALL_PLY'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IDAMAALL_PLY,0.0_8)

        WRITE(VARNAME,'(A)') 'IPLYALL'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPLYALL,0.0_8)

        DO I = 1,MX_PLY_ANIM
          IF(PLY_ANIM(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'PLY_ANIM_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PLY_ANIM(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MX_PLY_ANIM
          IF(PLY_ANIM_STRESS(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'PLY_ANIM_STRESS_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PLY_ANIM_STRESS(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MX_PLY_ANIM
          IF(PLY_ANIM_STRAIN(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'PLY_ANIM_STRAIN_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PLY_ANIM_STRAIN(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MX_PLY_ANIM
          IF(PLY_ANIM_EPSDOT(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'PLY_ANIM_EPSDOT_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PLY_ANIM_EPSDOT(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MX_PLY_ANIM
          IF(PLY_ANIM_PHI(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'PLY_ANIM_PHI_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PLY_ANIM_PHI(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MX_PLY_ANIM
          IF(PLY_ANIM_EPSP(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'PLY_ANIM_EPSP_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PLY_ANIM_EPSP(I),0.0_8)
          ENDIF
        ENDDO

        DO I = 1,MX_PLY_ANIM
          IF(PLY_ANIM_DAMA(I)/=0)THEN
            WRITE(VARNAME,'(A,I0)') 'PLY_ANIM_DAMA_',I
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PLY_ANIM_DAMA(I),0.0_8)
          ENDIF
        ENDDO

      ENDIF

      RETURN
      END
