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
Chd|  AMULF2                        source/ale/bimat/amulf2.F     
Chd|-- called by -----------
Chd|        AFLUX2                        source/ale/ale2d/aflux2.F     
Chd|        EFLUX2                        source/ale/euler2d/eflux2.F   
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AMULF2(FILL,DFILL,FLUX,FLU1,VOL,ALE_CONNECT,ALPV,
     .     FLUX1, FLUX2, FLUX3, FLUX4, UPW, 
     .     NC1, NC2, NC3, NC4)
      USE ALE_CONNECTIVITY_MOD
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
#include      "vect01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   FILL(NUMNOD,*), DFILL(NUMNOD,*), FLUX(4,*), FLU1(*), VOL(*),
     .   ALPV(2,*), FLUX1(*), FLUX2(*), FLUX3(*), FLUX4(*), UPW(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JM, I, II, JJ, IAD2
      my_real
     .   FLUB1(MVSIZ), FLUB2(MVSIZ),
     .   FLUB3(MVSIZ), FLUB4(MVSIZ), ALPN1, ALPN2, ALPN3, ALPN4, ALPD1,
     .   ALPD2, ALPD3, ALPD4, ALP1, CALP1, ALPD, ALPH, ALP2, CALP2
C-----------------------------------------------
      DO JM=1,JMULT
C
        DO I=LFT,LLT
          II=I+NFT
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
          ALPN1= MAX(ZERO,FILL(NC1(I),JM))
     .         + MAX(ZERO,FILL(NC1(I),JM)-DFILL(NC1(I),JM))
          ALPN2= MAX(ZERO,FILL(NC2(I),JM))
     .         + MAX(ZERO,FILL(NC2(I),JM)-DFILL(NC2(I),JM))
          ALPN3= MAX(ZERO,FILL(NC3(I),JM))
     .         + MAX(ZERO,FILL(NC3(I),JM)-DFILL(NC3(I),JM))
          ALPN4= MAX(ZERO,FILL(NC4(I),JM))
     .         + MAX(ZERO,FILL(NC4(I),JM)-DFILL(NC4(I),JM))
          ALPD1=ABS(FILL(NC1(I),JM))
     .         +ABS(FILL(NC1(I),JM)-DFILL(NC1(I),JM))
          ALPD2=ABS(FILL(NC2(I),JM))
     .         +ABS(FILL(NC2(I),JM)-DFILL(NC2(I),JM))
          ALPD3=ABS(FILL(NC3(I),JM))
     .         +ABS(FILL(NC3(I),JM)-DFILL(NC3(I),JM))
          ALPD4=ABS(FILL(NC4(I),JM))
     .         +ABS(FILL(NC4(I),JM)-DFILL(NC4(I),JM))

          ALP1 =ALPV(JM,II)     *VOL(II)
          CALP1=(ONE -ALPV(JM,II))*VOL(II)

          ALPD=ALPD1+ALPD2
          IF(ALPD==ZERO)THEN
           ALPH=ONE
          ELSE
           ALPH=(ALPN1+ALPN2)/ALPD
          ENDIF
          FLUB1(I)=FLUX1(I)*ALPH
          IF(FLUX1(I)>=ZERO)THEN
           FLUB1(I)= MAX(FLUB1(I),FLUX1(I)-CALP1)
           FLUB1(I)= MIN(FLUB1(I),ALP1)
          ELSE
           JJ=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
           IF(JJ==0)JJ=II
           ALP2 =ALPV(JM,JJ)       *VOL(JJ)
           CALP2=(ONE - ALPV(JM,JJ))*VOL(JJ)
           FLUB1(I)=-MAX(-FLUB1(I),-FLUX1(I)-CALP2)
           FLUB1(I)=-MIN(-FLUB1(I),ALP2)
          ENDIF

          ALPD=ALPD2+ALPD3
          IF(ALPD==ZERO)THEN
           ALPH=ONE
          ELSE
           ALPH=(ALPN2+ALPN3)/ALPD
          ENDIF
          FLUB2(I)=FLUX2(I)*ALPH
          IF(FLUX2(I)>=ZERO)THEN
           FLUB2(I)= MAX(FLUB2(I),FLUX2(I)-CALP1)
           FLUB2(I)= MIN(FLUB2(I),ALP1)
          ELSE
           JJ=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
           IF(JJ==0)JJ=II
           ALP2 =ALPV(JM,JJ)     *VOL(JJ)
           CALP2=(ONE-ALPV(JM,JJ))*VOL(JJ)
           FLUB2(I)=-MAX(-FLUB2(I),-FLUX2(I)-CALP2)
           FLUB2(I)=-MIN(-FLUB2(I),ALP2)
          ENDIF

          ALPD=ALPD3+ALPD4
          IF(ALPD==ZERO)THEN
           ALPH=ONE
          ELSE
           ALPH=(ALPN3+ALPN4)/ALPD
          ENDIF
          FLUB3(I)=FLUX3(I)*ALPH
          IF(FLUX3(I)>=ZERO)THEN
           FLUB3(I)= MAX(FLUB3(I),FLUX3(I)-CALP1)
           FLUB3(I)= MIN(FLUB3(I),ALP1)
          ELSE
           JJ=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
           IF(JJ==0)JJ=II
           ALP2 =ALPV(JM,JJ)     *VOL(JJ)
           CALP2=(ONE-ALPV(JM,JJ))*VOL(JJ)
           FLUB3(I)=-MAX(-FLUB3(I),-FLUX3(I)-CALP2)
           FLUB3(I)=-MIN(-FLUB3(I),ALP2)
          ENDIF

          ALPD=ALPD4+ALPD1
          IF(ALPD==ZERO)THEN
           ALPH=ONE
          ELSE
           ALPH=(ALPN4+ALPN1)/ALPD
          ENDIF
          FLUB4(I)=FLUX4(I)*ALPH
          IF(FLUX4(I)>=ZERO)THEN
           FLUB4(I)= MAX(FLUB4(I),FLUX4(I)-CALP1)
           FLUB4(I)= MIN(FLUB4(I),ALP1)
          ELSE
           JJ=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
           IF(JJ==0)JJ=II
           ALP2 =ALPV(JM,JJ)     *VOL(JJ)
           CALP2=(ONE-ALPV(JM,JJ))*VOL(JJ)
           FLUB4(I)=-MAX(-FLUB4(I),-FLUX4(I)-CALP2)
           FLUB4(I)=-MIN(-FLUB4(I),ALP2)
          ENDIF

       ENDDO !next I

       DO I=LFT,LLT
         II=I+(JM-1)*NUMELQ
         FLUX(1,II)=FLUB1(I)-UPW(I)*ABS(FLUB1(I))
         FLUX(2,II)=FLUB2(I)-UPW(I)*ABS(FLUB2(I))
         FLUX(3,II)=FLUB3(I)-UPW(I)*ABS(FLUB3(I))
         FLUX(4,II)=FLUB4(I)-UPW(I)*ABS(FLUB4(I))

         FLU1(II) =FLUB1(I)+UPW(I)*ABS(FLUB1(I))
     .            +FLUB2(I)+UPW(I)*ABS(FLUB2(I))
     .            +FLUB3(I)+UPW(I)*ABS(FLUB3(I))
     .            +FLUB4(I)+UPW(I)*ABS(FLUB4(I))
       ENDDO

      ENDDO !next JM

      RETURN
      END
