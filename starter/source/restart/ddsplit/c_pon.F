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
C
Chd|====================================================================
Chd|  C_PON                         source/restart/ddsplit/c_pon.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE C_PON(ADDCNE ,NUMNOD_L,NODGLOB ,LCNE_L,I2NSNT ,
     2                 ADDCNI2,LCNI2_L ,MONVOL  ,
     3                 CEP    ,PROC    ,NNMV_L  ,NNMVC_L,ADDCNE_PXFEM,
     4               LCNEPXFEM_L,INODPXFEM,ADDCNE_CRKXFEM,LCNECRKXFEM_L,
     5                 INODCRKXFEM,ADDCNCND,LCNCND_L,IGRSURF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com_xfem1.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMNOD_L, LCNE_L, LCNI2_L, I2NSNT, NNMV_L, PROC,NNMVC_L,
     .         ADDCNE(0:*), NODGLOB(*), ADDCNI2(0:*),
     .         MONVOL(*), CEP(*),
     .         ADDCNE_PXFEM(0:*),LCNEPXFEM_L,INODPXFEM(*),
     .         ADDCNE_CRKXFEM(0:*),LCNECRKXFEM_L,INODCRKXFEM(*),
     .         ADDCNCND(0:*),LCNCND_L
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,K0,K1,K2,K6,IS,NN,IAD,J,ITY,OFFC,OFFTG,NN_L,N1,
     .        ITYP,NVENT,IV,IADHOL,KIBHOL,KIBJET,NNC,KAD,NAV,IPVENT
C-----------------------------------------------
C
C taille Matrice connectivite locale
C
      LCNE_L = 0
      DO I = 1, NUMNOD_L
        N = NODGLOB(I)
        LCNE_L = LCNE_L + ADDCNE(N+1)-ADDCNE(N)
      ENDDO
C
C taille Matrice connectivite locale (interface type 2)
C
      LCNI2_L = 0
      IF(I2NSNT>0) THEN
        DO I = 1, NUMNOD_L
          N = NODGLOB(I)
          LCNI2_L = LCNI2_L + ADDCNI2(N+1)-ADDCNI2(N)
        END DO
      END IF
C
      LCNEPXFEM_L = 0
      IF(IPLYXFEM > 0) THEN
        DO I = 1, NUMNOD_L
          N = NODGLOB(I)
          N1 = INODPXFEM(N)
          IF(N1 > 0) THEN
            LCNEPXFEM_L = LCNEPXFEM_L
     .                  + ADDCNE_PXFEM(N1+1)-ADDCNE_PXFEM(N1)
          ENDIF
        ENDDO
      ENDIF
C-----Itet2 S10      
      LCNCND_L = 0
      IF(NS10E>0) THEN
        DO I = 1, NUMNOD_L
          N = NODGLOB(I)
          LCNCND_L = LCNCND_L + ADDCNCND(N+1)-ADDCNCND(N)
        END DO
      END IF
C
C crack xfem layered shell
C
      LCNECRKXFEM_L = 0
      IF(ICRACK3D > 0) THEN
        DO I = 1, NUMNOD_L
          N  = NODGLOB(I)
          N1 = INODCRKXFEM(N)   ! Num noeud systeme xfem global 
          IF (N1 > 0) THEN
            LCNECRKXFEM_L = LCNECRKXFEM_L 
     .                    + ADDCNE_CRKXFEM(N1+1) - ADDCNE_CRKXFEM(N1)
          ENDIF  
        ENDDO
      ENDIF
C
C Taille connectivite airbag
C
      OFFC = NUMELS+NUMELQ
      OFFTG =NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR
      K0 = 0
      K1 = 1
      K2 = 1 + NIMV*NVOLU
      KIBJET = K2  + LICBAG
      KIBHOL = KIBJET + LIBAGJET
      K6 = 0
      NN_L = 0
      DO N = 1, NVOLU
        ITYP  = MONVOL(K1+1)
        NAV   = MONVOL(K1+2)
        IS    = MONVOL(K1+3)
        NVENT = MONVOL(K1+10)
        NN    = IGRSURF(IS)%NSEG
        IADHOL= KIBHOL+MONVOL(K1+11)
        DO J = 1, NN
          ITY = IGRSURF(IS)%ELTYP(J)
          I   = IGRSURF(IS)%ELEM(J)
          IF (ITY==3) THEN
            IF(CEP(OFFC+I)==PROC) NN_L = NN_L + 1
          ELSEIF (ITY==7) THEN
            IF(CEP(OFFTG+I)==PROC) NN_L = NN_L + 1
          ELSE
          ENDIF
        ENDDO
C
        IF(ITYP==3.OR.ITYP==4.OR.ITYP==5.OR.ITYP==7.OR.ITYP==9) THEN
          DO IV = 1, NVENT
            IPVENT = MONVOL(IADHOL+NIBHOL*(IV-1)+2-1)
            IF(IPVENT/=0) THEN
              NNC=IGRSURF(IPVENT)%NSEG
              DO J = 1, NNC
                ITY = IGRSURF(IPVENT)%ELTYP(J)
                I   = IGRSURF(IPVENT)%ELEM(J)
                IF (ITY==3) THEN
                  IF(CEP(I+OFFC)==PROC) THEN
                    K0 = K0 + 1
                  END IF
                ELSEIF (ITY==7) THEN
                  IF(CEP(I+OFFTG)==PROC) THEN
                    K0 = K0 + 1
                  END IF
                END IF
              END DO
            END IF
          END DO
        END IF
        IF(ITYP==4.OR.ITYP==5.OR.ITYP==7.OR.ITYP==9)THEN
          DO IV = 1, NAV
            IPVENT = MONVOL(K2+NICBAG*(IV-1)+2-1)
            IF(IPVENT/=0) THEN
              NNC=IGRSURF(IPVENT)%NSEG
              DO J = 1, NNC
                ITY = IGRSURF(IPVENT)%ELTYP(J)
                I   = IGRSURF(IPVENT)%ELEM(J)
                IF (ITY==3) THEN
                  IF(CEP(I+OFFC)==PROC) THEN
                    K0 = K0 + 1
                  END IF
                ELSEIF (ITY==7) THEN
                  IF(CEP(I+OFFTG)==PROC) THEN
                    K0 = K0 + 1
                  END IF
                END IF
              END DO
            END IF
          END DO
        END IF
        K1 = K1 + NIMV
        K2 = K2 + NICBAG * NAV
        K6 = K6 + NN
      ENDDO
      NNMV_L  = NN_L
      NNMVC_L = K0
C
      RETURN
      END
