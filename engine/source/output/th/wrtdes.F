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
Chd|  WRTDES                        source/output/th/wrtdes.F     
Chd|-- called by -----------
Chd|        HIST1                         source/output/th/hist1.F      
Chd|        HIST13                        source/output/th/hist13.F     
Chd|        HIST2                         source/output/th/hist2.F      
Chd|        LECNOISE                      source/general_controls/computation/lecnoise.F
Chd|        NOISE                         source/general_controls/computation/noise.F
Chd|        THCLUSTER                     source/output/th/thcluster.F  
Chd|        THFXMOD                       source/output/th/thfxmod.F    
Chd|        THKIN                         source/output/th/thkin.F      
Chd|        THMONV                        source/output/th/thmonv.F     
Chd|        THSENS                        source/output/th/thsens.F     
Chd|        THSURF                        source/output/th/thsurf.F     
Chd|        WRITE_TH                      source/output/th/write_th.F   
Chd|        WRTDES0                       source/output/th/wrtdes0.F    
Chd|-- calls ---------------
Chd|        EOR_C                         source/output/tools/sortie_c.c
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        STRI                          source/tools/univ/stri.F      
Chd|        STRR                          source/tools/univ/strr.F      
Chd|====================================================================
      SUBROUTINE WRTDES(A,IA,L,IFORM,IR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER L, IFORM, IR
      INTEGER IA(*)
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr13_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L1, L2, II
      REAL R4
      CHARACTER EOR*8, CH8(1000)*8
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      CHARACTER STRR*8, STRI*8
C-----------------------------------------------
      EXTERNAL STRR,STRI
      DATA EOR/'ZZZZZEOR'/
      IF(L==0)RETURN
C
      IF (IFORM==0)THEN
       IF (IR==1)THEN
        WRITE(IUNIT) (A(I),I=1,L)
       ELSE
        WRITE(IUNIT) (IA(I),I=1,L)
       ENDIF
C
      ELSEIF (IFORM==1)THEN
       L1=1
       L2=1000
   1   L2=MIN(L,L2)
       II=0
       IF(IR==1)THEN
        DO 10 I=L1,L2
        II=II+1
 10     CH8(II)=STRR(A(I))
       ELSE
        DO 20 I=L1,L2
        II=II+1
 20     CH8(II)=STRI(IA(I))
       ENDIF
       IF(L2<L)THEN
        WRITE(IUNIT,'(10A8)') (CH8(I),I=1,II)
        L1=L1+1000
        L2=L2+1000
        GOTO 1
       ELSEIF(II<1000) THEN
        WRITE(IUNIT,'(10A8)') (CH8(I),I=1,II),EOR
       ELSE
        WRITE(IUNIT,'(10A8)') (CH8(I),I=1,II)
        WRITE(IUNIT,'(10A8)') EOR
       ENDIF
C
      ELSEIF (IFORM==2)THEN
       IF(IR==1)THEN
        WRITE(IUNIT,'(A,I5,A)')EOR,L,'R'
        WRITE(IUNIT,'((5(1X,1PE15.8)))')(A(I),I=1,L)
       ELSE
        WRITE(IUNIT,'(A,I5,A)')EOR,L,'I'
        WRITE(IUNIT,'(16I10)')          (IA(I),I=1,L)
       ENDIF
C
      ELSEIF (IFORM==3)THEN
       CALL EOR_C(4*L)
       IF(IR==1)THEN
        DO 100 I=1,L
        R4 = A(I)
 100    CALL WRITE_R_C(R4,1)
       ELSE
        CALL WRITE_I_C(IA,L)
       ENDIF
       CALL EOR_C(4*L)
      ENDIF
C
      RETURN
      END
