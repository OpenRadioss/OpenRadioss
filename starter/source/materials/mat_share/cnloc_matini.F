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
!||    cnloc_matini       ../starter/source/materials/mat_share/cnloc_matini.F
!||--- called by ------------------------------------------------------
!||    c3fint_reg_ini     ../starter/source/elements/sh3n/coque3n/c3fint_reg_ini.F
!||    cbafint_reg_ini    ../starter/source/elements/shell/coqueba/cbafint_reg_ini.F
!||    cdkfint_reg_ini    ../starter/source/elements/sh3n/coquedk/cdkfint_reg_ini.F
!||    cfint_reg_ini      ../starter/source/elements/shell/coque/cfint_reg_ini.F
!||--- calls      -----------------------------------------------------
!||    cnloc_mat104_ini   ../starter/source/materials/mat/mat104/cnloc_mat104_ini.F
!||--- uses       -----------------------------------------------------
!||    message_mod        ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE CNLOC_MATINI(ELBUF_STR,NEL      ,IPM      ,
     .                        BUFMAT   ,TIME     ,VARNL    ,
     .                        FAILURE  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD      
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      my_real
     .        VARNL(NEL,*),BUFMAT(*),TIME
      TARGET :: BUFMAT
      INTEGER IPM(NPROPMI,*)
      LOGICAL :: FAILURE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IR,IS,IT,IADBUF,NPTT,NPTR,NPTS,
     .        IJ(5),IJ1,IJ2,IJ3,IJ4,IJ5,IPG,
     .        ILAW,IMAT,NUVAR,NUPARAM
      TYPE(BUF_LAY_), POINTER :: BUFLY
      TYPE(L_BUFEL_), POINTER :: LBUF     
      TYPE(BUF_MAT_), POINTER :: MBUF
      TYPE(G_BUFEL_), POINTER :: GBUF
      my_real, 
     .  DIMENSION(:), POINTER :: UPARAM,UVAR,THKN
C=======================================================================
      ! Buffer of the first layer
      BUFLY => ELBUF_STR%BUFLY(1)
      ! General buffer of the elements of the group
      GBUF  => ELBUF_STR%GBUF
      ! Global thickness of the shell
      THKN  => GBUF%THK(1:NEL)
C
      ! Number of integration points in the thickness
      NPTR  = ELBUF_STR%NPTR
      NPTS  = ELBUF_STR%NPTS
      NPTT  = BUFLY%NPTT
      ILAW  = BUFLY%ILAW
      IMAT  = BUFLY%IMAT
      NUVAR = BUFLY%NVAR_MAT
C
      ! Material properties and parameters
      IADBUF  = MAX(1,IPM(7,IMAT))
      NUPARAM = IPM(9,IMAT) 
      UPARAM  => BUFMAT(IADBUF:IADBUF+NUPARAM)       
C
      DO K=1,5
        IJ(K) = NEL*(K-1)
      ENDDO
      IJ1 = IJ(1) + 1
      IJ2 = IJ(2) + 1
      IJ3 = IJ(3) + 1
      IJ4 = IJ(4) + 1
      IJ5 = IJ(5) + 1
C
      ! Loop over integration points in the plane
      DO IR = 1,NPTR
        DO IS = 1,NPTS
          ! Current Gauss point
          IPG  = (IS-1)*ELBUF_STR%NPTR + IR
          ! Loop over thickness integration points
          DO IT = 1,NPTT       
            ! Integration point buffer
            LBUF => BUFLY%LBUF(IR,IS,IT)
            ! Interval variable buffer
            UVAR => BUFLY%MAT(IR,IS,IT)%VAR
            ! Non-local plastic strain update
            IF (GBUF%G_PLANL > 0) THEN 
              DO I = 1,NEL 
                VARNL(I,IT)   = MAX(VARNL(I,IT),ZERO)
                LBUF%PLANL(I) = LBUF%PLANL(I) + VARNL(I,IT)
              ENDDO 
            ENDIF
            ! Select the material initialization routine
            SELECT CASE (ILAW)
              CASE(104)
                CALL CNLOC_MAT104_INI(NEL      ,IPG      ,IT       ,NUPARAM  ,NUVAR    ,UPARAM   ,
     .                                UVAR     ,LBUF%PLA ,LBUF%OFF ,LBUF%THK ,GBUF%OFF ,LBUF%SIG(IJ1),
     .                                LBUF%SIG(IJ2),LBUF%SIG(IJ3)  ,LBUF%SIG(IJ4)      ,LBUF%SIG(IJ5),
     .                                THKN     ,LBUF%DMG ,NPTR     ,NPTS     ,NPTT     ,BUFLY        ,
     .                                TIME     ,VARNL(1,IT),FAILURE)                  
            END SELECT
            ! Plastic strain to regularize
            IF (GBUF%G_PLANL > 0) THEN 
              DO I = 1,NEL 
                VARNL(I,IT) = LBUF%PLA(I)
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C-----------
      END
