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
Chd|  I25STI_EDG                    source/interfaces/inter3d1/i25sti_edg.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE I25STI_EDG(
     1 NEDGE    ,LEDGE     ,STFE       ,STFM      ,IGAP      ,
     2 GAPE     ,GAP_E_L   ,GAP_M      ,GAP_M_L   ,GAP_S_L   ,
     3 BGAPEMX  ,INTFRIC   ,IPARTFRIC_E,IPARTFRICM,IPARTSM   ,
     4 BGAPEMX_L,NSN       ,NSV        )
C-----------------------------------------------
      USE INTBUF_FRIC_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEDGE,IGAP,INTFRIC,
     .   LEDGE(NLEDGE,*),IPARTFRIC_E(*) ,IPARTFRICM(*), IPARTSM(*)
      INTEGER , INTENT(IN) ::  NSN
      INTEGER , INTENT(IN) ::  NSV(NSN)
      my_real
     .   STFE(*), GAPE(*), GAP_E_L(*), STFM(*), GAP_M(*), GAP_M_L(*), GAP_S_L(*), BGAPEMX,
     .   BGAPEMX_L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, A, B, N1, N2, IPRTA, IPRTB, IPRTGA, IPRTGB
      my_real
     .   STFA,STFB,GAPA,GAPB
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGSLAV
C-----------------------------------------------
      DO I=1,NEDGE

        STFA=ZERO
        STFB=ZERO

        A=LEDGE(1,I)
        IF(A/=0) STFA=STFM(A)
        B=LEDGE(3,I)
        IF(B/=0) STFB=STFM(B)
        IF(STFA/=ZERO.AND.STFB/=ZERO)THEN
          STFE(I)=TWO*STFA*STFB/MAX(ZERO,STFA+STFB)
        ELSE
          STFE(I)=MAX(STFA,STFB)
        END IF
      END DO
C
      BGAPEMX=ZERO        
      DO I=1,NEDGE

        GAPA=ZERO
        GAPB=ZERO

        A=LEDGE(1,I)
        IF(A/=0) GAPA=GAP_M(A)
        B=LEDGE(3,I)
        IF(B/=0) GAPB=GAP_M(B)
        GAPE(I)=MAX(GAPA,GAPB)

        BGAPEMX   = MAX(BGAPEMX,GAPE(I)) ! BGAPEMX overall the model 

      END DO

      BGAPEMX_L=ZERO        
      IF(IGAP==3)THEN
        ALLOCATE(TAGSLAV(NUMNOD))
        TAGSLAV (1:NUMNOD)   = 0
        DO I=1,NSN
           TAGSLAV(NSV(I)) = I
        ENDDO
        DO I=1,NEDGE
          N1=LEDGE(5,I)
          N2=LEDGE(6,I)                             !      4-node segment
          GAP_E_L(I)=MIN(GAP_S_L(TAGSLAV(N1)),GAP_S_L(TAGSLAV(N2)))   !    O-----------------O
                                                    !   Edge1             Edge2

          BGAPEMX_L   = MAX(BGAPEMX_L,GAP_E_L(I)) ! BGAPEMX_L overall the model 
        END DO
        DEALLOCATE(TAGSLAV)
      END IF

      IF(INTFRIC > 0) THEN
        DO I=1,NEDGE
           IPRTA=0 
           IPRTB=0

           IPRTGA=0
           IPRTGB=0

           A=LEDGE(1,I)
           IF(A/=0) IPRTGA=IPARTSM(A)
           IF(A/=0) IPRTA=IPARTFRICM(A)

           B=LEDGE(3,I)
           IF(B/=0) IPRTGB=IPARTSM(B)
           IF(B/=0) IPRTB=IPARTFRICM(B)

           IF(IPRTA == IPRTB) THEN
              IPARTFRIC_E(I) = IPRTA
           ELSE
              IF(IPRTGA > IPRTGB ) THEN ! bigger part id is taken
                 IPARTFRIC_E(I) = IPRTA
              ELSE
                 IPARTFRIC_E(I) = IPRTB
              ENDIF
           ENDIF
        END DO
      ENDIF
C        
      RETURN
      END
