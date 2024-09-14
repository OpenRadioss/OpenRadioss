Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    sum_6_float             ../engine/source/system/parit.F
      !||--- called by ------------------------------------------------------
      !||    airbagb                 ../engine/source/airbag/airbag2.F
      !||    airbagb1                ../engine/source/airbag/airbagb1.F
      !||    ass18                   ../engine/source/interfaces/int18/ass18.F
      !||    damping_vref_sum6_rby   ../engine/source/assembly/damping_vref_sum6_rby.F90
      !||    dampvref_sum6           ../engine/source/assembly/dampvref_sum6.F
      !||    get_volume_area         ../engine/source/airbag/get_volume_area.F90
      !||    poro                    ../engine/source/ale/porous/poro.F
      !||    rbyact                  ../engine/source/constraints/general/rbody/rbyact.F
      !||    rbypid                  ../engine/source/constraints/general/rbody/rbypid.F
      !||    rgbodfp                 ../engine/source/constraints/general/rbody/rgbodfp.F
      !||    rgwalc                  ../engine/source/constraints/general/rwall/rgwalc.F
      !||    rgwall                  ../engine/source/constraints/general/rwall/rgwall.F
      !||    rgwalp                  ../engine/source/constraints/general/rwall/rgwalp.F
      !||    rgwals                  ../engine/source/constraints/general/rwall/rgwals.F
      !||    rgwath                  ../engine/source/interfaces/int09/rgwath.F
      !||    rlink0                  ../engine/source/constraints/general/rlink/rlink0.F
      !||    rlink1                  ../engine/source/constraints/general/rlink/rlink1.F
      !||    rlink2                  ../engine/source/constraints/general/rlink/rlink2.F
      !||    rlink3                  ../engine/source/constraints/general/rlink/rlink10.F
      !||    rmatpon                 ../engine/source/materials/mat/mat013/rmatpon.F
      !||    sensor_energy_bilan     ../engine/source/tools/sensor/sensor_energy_bilan.F
      !||    sensor_temp0            ../engine/source/tools/sensor/sensor_temp0.F
      !||    sms_pcg                 ../engine/source/ams/sms_pcg.F
      !||    sms_produt_h            ../engine/source/ams/sms_proj.F
      !||    sms_rbe_1               ../engine/source/ams/sms_rbe2.F
      !||    sms_rbe_5               ../engine/source/ams/sms_rbe2.F
      !||    sms_rgwalc_bilan        ../engine/source/ams/sms_rgwalc.F
      !||    sms_rgwall_bilan        ../engine/source/ams/sms_rgwall.F
      !||    sms_rgwalp_bilan        ../engine/source/ams/sms_rgwalp.F
      !||    sms_rgwals_bilan        ../engine/source/ams/sms_rgwals.F
      !||    sms_rlink1              ../engine/source/ams/sms_rlink.F
      !||    sms_rlink2              ../engine/source/ams/sms_rlink.F
      !||    sms_rlink3              ../engine/source/ams/sms_rlink.F
      !||    spgauge                 ../engine/source/elements/sph/spgauge.F
      !||    telesc                  ../engine/source/constraints/general/cyl_joint/telesc.F
      !||    volpvgb                 ../engine/source/airbag/volpvg.F
      !||    volum0                  ../engine/source/airbag/volum0.F
      !||====================================================================
      SUBROUTINE SUM_6_FLOAT(JFT  ,JLT  ,F, F6, N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, N
      my_real
     .   F(*)
      DOUBLE PRECISION F6(N,6)
C-----------------------------------------------
c
c      r=26 (foat_to_6_float): nombre de bits pour la retenu
c         autorise  faire 2^r + ~= 67,000,000.
c
c      m1: masque 1
c      ...
c      m6: masque 6
c
c      m2 = m1 - 53 + r
c      m3 = m2 - 53 + r
c      m4 = m3 - 53 + r
c      m5 = m4 - 53 + r
c      m6 = m5 - 53 + r
c
c      si r= 26 : mi = mi-1 - 27
c
c      f1 = (f + 2^m1) - 2^m1
c       b = f - f1
c      f1 = (b + 2^m2) - 2^m2
c       d = b - f2
c      f3 = (d + 2^m3) - 2^m3
c      f4 = ((d - f3) + 2^m4) - 2^m4
c      ...
c--------- calcul de fmax, fmin 
c      fmax avec r bits  zero = 2^(m1-r)
c
c      fmin avec 53 bits significatif = 2^m6
c      fmin avec 1 bits significatif  = 2^(m6-53)
c
c--------- fmax, fmin aprs exprimentation
c      fmax avec r bits  zero ~= 2^(m1-2r)
c
c      fmin avec 53 bits significatif ~= 2^(m6-r)
c      fmin avec 1 bits significatif  ~= 2^(m6-53-r)
c
c 6 float r=26 m1=89 m6=-46
c 
c      fmax avec r bits  zero ~= 2^37 = 
c      fmin avec 53 bits significatif ~= 2^(m6-r)
c      fmin avec 1 bits significatif  ~= 2^(m6-53-r)
c-------------------------------------------------------
c       a = f + deuxp63
c       f4(1) = a - deuxp63
c       b = f - f4(1)
c       c = b + deuxp30
c       f4(2) = c - deuxp30
c       d = b - f4(2)
c       e = d + deuxpm3
c       f4(3) = e - deuxpm3
c       g = d - f4(3)
c       h = g + deuxpm36
c       f4(4) = h - deuxpm36
C-----------------------------------------------
C   L o c a l   C o m m o n
C-----------------------------------------------
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C Commun pour casser l optimisation et thread private pour multithread
C
      COMMON /PARIT_VAR/TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                  TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                  TEMP17,RESTE
!$OMP THREADPRIVATE(/PARIT_VAR/)
      DOUBLE PRECISION TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                 TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                 TEMP17,RESTE
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION R8DEUXP89,R8DEUXP62,R8DEUXP35,R8TWOP8
      DOUBLE PRECISION R8DEUXPM19,R8DEUXPM46
      DATA R8DEUXP89 /'4580000000000000'x/
      DATA R8DEUXP62 /'43D0000000000000'x/
      DATA R8DEUXP35 /'4220000000000000'x/
      DATA R8TWOP8  /'4070000000000000'x/
      DATA R8DEUXPM19/'3EC0000000000000'x/
      DATA R8DEUXPM46/'3D10000000000000'x/
    
      DO I=JFT,JLT

       RESTE  = F(I)

       TEMP1  = RESTE + R8DEUXP89
       TEMP11 = TEMP1 - R8DEUXP89
       RESTE  = RESTE - TEMP11

       TEMP2  = RESTE + R8DEUXP62
       TEMP12 = TEMP2 - R8DEUXP62
       RESTE  = RESTE - TEMP12

       TEMP3  = RESTE + R8DEUXP35
       TEMP13 = TEMP3 - R8DEUXP35
       RESTE  = RESTE - TEMP13

       TEMP4  = RESTE + R8TWOP8
       TEMP14 = TEMP4 - R8TWOP8
       RESTE  = RESTE - TEMP14

       TEMP5  = RESTE + R8DEUXPM19
       TEMP15 = TEMP5 - R8DEUXPM19
       RESTE  = RESTE - TEMP15

       TEMP6  = RESTE + R8DEUXPM46
       TEMP16 = TEMP6 - R8DEUXPM46
       
       F6(1,1)  = F6(1,1) + TEMP11
       F6(1,2)  = F6(1,2) + TEMP12
       F6(1,3)  = F6(1,3) + TEMP13
       F6(1,4)  = F6(1,4) + TEMP14
       F6(1,5)  = F6(1,5) + TEMP15
       F6(1,6)  = F6(1,6) + TEMP16

      ENDDO

      RETURN
      END

      !||====================================================================
      !||    foat_to_6_float       ../engine/source/system/parit.F
      !||--- called by ------------------------------------------------------
      !||    i20for3               ../engine/source/interfaces/int20/i20for3.F
      !||    i20for3e              ../engine/source/interfaces/int20/i20for3.F
      !||    i21ass3               ../engine/source/interfaces/int21/i21ass3.F
      !||    inter_sh_offset_ini   ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
      !||    multi_i18_force_pon   ../engine/source/interfaces/int18/multi_i18_force_pon.F
      !||    offset_nproj          ../engine/source/interfaces/shell_offset/offset_nproj.F90
      !||    rbe2f                 ../engine/source/constraints/general/rbe2/rbe2f.F
      !||    rbe2fl                ../engine/source/constraints/general/rbe2/rbe2f.F
      !||    rbe3t1                ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    s10volnod3            ../engine/source/elements/solid/solide4_sfem/s10volnod3.F
      !||    s10volnodt3           ../engine/source/elements/solid/solide4_sfem/s10volnodt3.F
      !||    s4alesfem             ../engine/source/elements/solid/solide4_sfem/s4alesfem.F
      !||    s4lagsfem             ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
      !||    s4volnod3             ../engine/source/elements/solid/solide4_sfem/s4volnod3.F
      !||    s4volnod_sm           ../engine/source/elements/solid/solide4_sfem/s4volnod_sm.F
      !||    sms_build_diag        ../engine/source/ams/sms_build_diag.F
      !||    sms_mav_lt            ../engine/source/ams/sms_pcg.F
      !||    sms_mav_lt1           ../engine/source/ams/sms_pcg.F
      !||    sms_mav_lt2           ../engine/source/ams/sms_pcg.F
      !||    sms_produt3           ../engine/source/ams/sms_proj.F
      !||    sms_rbe3_prec         ../engine/source/ams/sms_rbe3.F
      !||    sms_rbe3t1            ../engine/source/ams/sms_rbe3.F
      !||    splissv               ../engine/source/elements/sph/splissv.F
      !||====================================================================
      SUBROUTINE FOAT_TO_6_FLOAT(JFT  ,JLT  ,F, F6)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   F(*)
      DOUBLE PRECISION F6(6,*)
C-----------------------------------------------
c
c      r=26 (foat_to_6_float): nombre de bits pour la retenu
c         autorise  faire 2^r + ~= 67,000,000.
c
c      m1: masque 1
c      ...
c      m6: masque 6
c
c      m2 = m1 - 53 + r
c      m3 = m2 - 53 + r
c      m4 = m3 - 53 + r
c      m5 = m4 - 53 + r
c      m6 = m5 - 53 + r
c
c      si r= 26 : mi = mi-1 - 27
c
c      f1 = (f + 2^m1) - 2^m1
c       b = f - f1
c      f1 = (b + 2^m2) - 2^m2
c       d = b - f2
c      f3 = (d + 2^m3) - 2^m3
c      f4 = ((d - f3) + 2^m4) - 2^m4
c      ...
c--------- calcul de fmax, fmin 
c      fmax avec r bits  zero = 2^(m1-r)
c
c      fmin avec 53 bits significatif = 2^m6
c      fmin avec 1 bits significatif  = 2^(m6-53)
c
c--------- fmax, fmin aprs exprimentation
c      fmax avec r bits  zero ~= 2^(m1-2r)
c
c      fmin avec 53 bits significatif ~= 2^(m6-r)
c      fmin avec 1 bits significatif  ~= 2^(m6-53-r)
c
c 6 float r=26 m1=89 m6=-46
c 
c      fmax avec r bits  zero ~= 2^37 = 
c      fmin avec 53 bits significatif ~= 2^(m6-r)
c      fmin avec 1 bits significatif  ~= 2^(m6-53-r)
c-------------------------------------------------------
c       a = f + deuxp63
c       f4(1) = a - deuxp63
c       b = f - f4(1)
c       c = b + deuxp30
c       f4(2) = c - deuxp30
c       d = b - f4(2)
c       e = d + deuxpm3
c       f4(3) = e - deuxpm3
c       g = d - f4(3)
c       h = g + deuxpm36
c       f4(4) = h - deuxpm36
C-----------------------------------------------
C   L o c a l   C o m m o n
C-----------------------------------------------
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C Commun pour casser l optimisation et thread private pour multithread
C
      COMMON /PARIT_VAR/TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                  TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                  TEMP17,RESTE
!$OMP THREADPRIVATE(/PARIT_VAR/)
      DOUBLE PRECISION TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                 TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                 TEMP17,RESTE
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION R8DEUXP89,R8DEUXP62,R8DEUXP35,R8TWOP8
      DOUBLE PRECISION R8DEUXPM19,R8DEUXPM46
      DATA R8DEUXP89 /'4580000000000000'x/
      DATA R8DEUXP62 /'43D0000000000000'x/
      DATA R8DEUXP35 /'4220000000000000'x/
      DATA R8TWOP8  /'4070000000000000'x/
      DATA R8DEUXPM19/'3EC0000000000000'x/
      DATA R8DEUXPM46/'3D10000000000000'x/
    
      DO I=JFT,JLT

       RESTE   = F(I)

       TEMP1   = RESTE + R8DEUXP89
       F6(1,I) = TEMP1 - R8DEUXP89
       RESTE   = RESTE - F6(1,I)

       TEMP2   = RESTE + R8DEUXP62
       F6(2,I) = TEMP2 - R8DEUXP62
       RESTE   = RESTE - F6(2,I)

       TEMP3   = RESTE + R8DEUXP35
       F6(3,I) = TEMP3 - R8DEUXP35
       RESTE   = RESTE - F6(3,I)

       TEMP4   = RESTE + R8TWOP8
       F6(4,I) = TEMP4 - R8TWOP8
       RESTE   = RESTE - F6(4,I)

       TEMP5   = RESTE + R8DEUXPM19
       F6(5,I) = TEMP5 - R8DEUXPM19
       RESTE   = RESTE - F6(5,I)

       TEMP6   = RESTE + R8DEUXPM46
       F6(6,I) = TEMP6 - R8DEUXPM46

      ENDDO

      RETURN
      END

      !||====================================================================
      !||    foat_to_7_float   ../engine/source/system/parit.F
      !||====================================================================
       SUBROUTINE FOAT_TO_7_FLOAT(F,F7)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       DOUBLE PRECISION F,F7(7)
C-----------------------------------------------
c
c      r=29 (foat_to_7_float): nombre de bits pour la retenu
c         autorise  faire 2^r + ~= 537,000,000.
c
c      m1: masque 1
c      ...
c      m6: masque 6
c
c      m2 = m1 - 53 + r
c      m3 = m2 - 53 + r
c      m4 = m3 - 53 + r
c      m5 = m4 - 53 + r
c      m6 = m5 - 53 + r
c      m7 = m6 - 53 + r
c
c      si r= 29 : mi = mi-1 - 24
c
c      f1 = (f + 2^m1) - 2^m1
c       b = f - f1
c      f1 = (b + 2^m2) - 2^m2
c       d = b - f2
c      f3 = (d + 2^m3) - 2^m3
c      f4 = ((d - f3) + 2^m4) - 2^m4
c      ...
c--------- calcul de fmax, fmin 
c          calcul 
c      fmax avec 0 bits  zero = 2^m1
c      fmax avec r bits  zero = 2^(m1-r)
c
c      fmin avec 53 bits significatif = 2^m7
c      fmin avec 1 bits significatif  = 2^(m7+53)
c
c      fmax avec 0 bits  zero = 5. 10^27
c      fmax avec r bits  zero = 9. 10^18
c      fmin avec 53 bits significatif ~= 2.2 10^-16
c      fmin avec 1 bits significatif  ~= 2.4 10^-32
c-------------------------------------------------------
c       a = f + deuxp63
c       f4(1) = a - deuxp63
c       b = f - f4(1)
c       c = b + deuxp30
c       f4(2) = c - deuxp30
c       d = b - f4(2)
c       e = d + deuxpm3
c       f4(3) = e - deuxpm3
c       g = d - f4(3)
c       h = g + deuxpm36
c       f4(4) = h - deuxpm36
C-----------------------------------------------
C   L o c a l   C o m m o n
C-----------------------------------------------
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C Commun pour casser l optimisation et thread private pour multithread
C
      COMMON /PARIT_VAR/TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                  TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                  TEMP17,RESTE
!$OMP THREADPRIVATE(/PARIT_VAR/)
      DOUBLE PRECISION TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                 TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                 TEMP17,RESTE
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       DOUBLE PRECISION PE,PS
       DOUBLE PRECISION DEUXP92,DEUXP68,DEUXP44 ,DEUXP20,DEUXPM4,
     .          DEUXPM28,DEUXPM52
       DATA DEUXP92 /'45B0000000000000'X/
       DATA DEUXP68 /'4430000000000000'X/
       DATA DEUXP44 /'42B0000000000000'X/
       DATA DEUXP20 /'4130000000000000'X/
       DATA DEUXPM4 /'3FB0000000000000'X/
       DATA DEUXPM28/'3E30000000000000'X/
       DATA DEUXPM52/'3CB0000000000000'X/

       TEMP1 = F     + DEUXP92
       F7(1) = TEMP1 - DEUXP92
       RESTE = F     - F7(1)

       TEMP2 = RESTE + DEUXP68
       F7(2) = TEMP2 - DEUXP68
       RESTE = RESTE - F7(2)

       TEMP3 = RESTE + DEUXP44
       F7(3) = TEMP3 - DEUXP44
       RESTE = RESTE - F7(3)

       TEMP4 = RESTE + DEUXP20
       F7(4) = TEMP4 - DEUXP20
       RESTE = RESTE - F7(4)

       TEMP5 = RESTE + DEUXPM4
       F7(5) = TEMP5 - DEUXPM4
       RESTE = RESTE - F7(5)

       TEMP6 = RESTE + DEUXPM28
       F7(6) = TEMP6 - DEUXPM28
       RESTE = RESTE - F7(6)

       TEMP7 = RESTE + DEUXPM52
       F7(7) = TEMP7 - DEUXPM52

       RETURN
       END


      !||====================================================================
      !||    double_flot_ieee   ../engine/source/system/parit.F
      !||--- called by ------------------------------------------------------
      !||    cupdt3f            ../engine/source/elements/shell/coque/cupdt3.F
      !||    i7ass3             ../engine/source/interfaces/int07/i7ass3.F
      !||    i7ass35            ../engine/source/interfaces/int07/i7ass3.F
      !||====================================================================
      SUBROUTINE DOUBLE_FLOT_IEEE(JFT  ,JLT  ,I8 ,R8, I8F)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      integer*8 I8(*),I8F(3,*)
      my_real
     .   R8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c___________________________________________________          
      double precision
     .    R8_LOCAL,R8_DEUXP43,AA
      INTEGER*8 I8_DEUXP43
      DATA I8_DEUXP43 /'80000000000'x/
      DATA R8_DEUXP43 /'42A0000000000000'x/
      INTEGER I
c___________________________________________________          
C-----------------------------------------------
C
      DO I=JFT,JLT
c___________________________________________________          
          I8F(1,I)   = R8(I)
          AA         = I8F(1,I)
          R8_LOCAL   = (R8(I)    - AA) * R8_DEUXP43
          I8F(2,I)   = R8_LOCAL
          AA         = I8F(2,I)
          R8_LOCAL   = (R8_LOCAL - AA) * R8_DEUXP43
          I8F(3,I)   = R8_LOCAL + 0.5
      ENDDO
c___________________________________________________          
      RETURN
      END
      !||====================================================================
      !||    sum_6_float_sens   ../engine/source/system/parit.F
      !||--- called by ------------------------------------------------------
      !||    i10mainf           ../engine/source/interfaces/int10/i10mainf.F
      !||    i11mainf           ../engine/source/interfaces/int11/i11mainf.F
      !||    i20mainf           ../engine/source/interfaces/int20/i20mainf.F
      !||    i21mainf           ../engine/source/interfaces/int21/i21mainf.F
      !||    i22mainf           ../engine/source/interfaces/int22/i22mainf.F
      !||    i23mainf           ../engine/source/interfaces/int23/i23mainf.F
      !||    i24mainf           ../engine/source/interfaces/int24/i24main.F
      !||    i25mainf           ../engine/source/interfaces/int25/i25mainf.F
      !||    i7mainf            ../engine/source/interfaces/int07/i7mainf.F
      !||====================================================================
      SUBROUTINE SUM_6_FLOAT_SENS(F, A, B, C, JFT  ,JLT  , F6, D, E, G, ISENSINT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, A, B, C, D, E, G, ISENSINT(*)
C     REAL
      my_real
     .   F(A,B,C)
      DOUBLE PRECISION F6(D,E,G)
C-----------------------------------------------
c
c      r=26 (foat_to_6_float): nombre de bits pour la retenu
c         autorise  faire 2^r + ~= 67,000,000.
c
c      m1: masque 1
c      ...
c      m6: masque 6
c
c      m2 = m1 - 53 + r
c      m3 = m2 - 53 + r
c      m4 = m3 - 53 + r
c      m5 = m4 - 53 + r
c      m6 = m5 - 53 + r
c
c      si r= 26 : mi = mi-1 - 27
c
c      f1 = (f + 2^m1) - 2^m1
c       b = f - f1
c      f1 = (b + 2^m2) - 2^m2
c       d = b - f2
c      f3 = (d + 2^m3) - 2^m3
c      f4 = ((d - f3) + 2^m4) - 2^m4
c      ...
c--------- calcul de fmax, fmin 
c      fmax avec r bits  zero = 2^(m1-r)
c
c      fmin avec 53 bits significatif = 2^m6
c      fmin avec 1 bits significatif  = 2^(m6-53)
c
c--------- fmax, fmin aprs exprimentation
c      fmax avec r bits  zero ~= 2^(m1-2r)
c
c      fmin avec 53 bits significatif ~= 2^(m6-r)
c      fmin avec 1 bits significatif  ~= 2^(m6-53-r)
c
c 6 float r=26 m1=89 m6=-46
c 
c      fmax avec r bits  zero ~= 2^37 = 
c      fmin avec 53 bits significatif ~= 2^(m6-r)
c      fmin avec 1 bits significatif  ~= 2^(m6-53-r)
c-------------------------------------------------------
c       a = f + deuxp63
c       f4(1) = a - deuxp63
c       b = f - f4(1)
c       c = b + deuxp30
c       f4(2) = c - deuxp30
c       d = b - f4(2)
c       e = d + deuxpm3
c       f4(3) = e - deuxpm3
c       g = d - f4(3)
c       h = g + deuxpm36
c       f4(4) = h - deuxpm36
C-----------------------------------------------
C   L o c a l   C o m m o n
C-----------------------------------------------
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C Commun pour casser l optimisation et thread private pour multithread
C
      COMMON /PARIT_VAR/TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                  TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                  TEMP17,RESTE
!$OMP THREADPRIVATE(/PARIT_VAR/)
      DOUBLE PRECISION TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                 TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                 TEMP17,RESTE
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      DOUBLE PRECISION R8DEUXP89,R8DEUXP62,R8DEUXP35,R8TWOP8
      DOUBLE PRECISION R8DEUXPM19,R8DEUXPM46
      DATA R8DEUXP89 /'4580000000000000'x/
      DATA R8DEUXP62 /'43D0000000000000'x/
      DATA R8DEUXP35 /'4220000000000000'x/
      DATA R8TWOP8  /'4070000000000000'x/
      DATA R8DEUXPM19/'3EC0000000000000'x/
      DATA R8DEUXPM46/'3D10000000000000'x/

      DO I= 1,A
        IF(ISENSINT(I) /=0)THEN
         DO J= 1,B
              DO K=JFT,JLT

               RESTE  = F(I,J,K)

               TEMP1  = RESTE + R8DEUXP89
               TEMP11 = TEMP1 - R8DEUXP89
               RESTE  = RESTE - TEMP11

               TEMP2  = RESTE + R8DEUXP62
               TEMP12 = TEMP2 - R8DEUXP62
               RESTE  = RESTE - TEMP12

               TEMP3  = RESTE + R8DEUXP35
               TEMP13 = TEMP3 - R8DEUXP35
               RESTE  = RESTE - TEMP13

               TEMP4  = RESTE + R8TWOP8
               TEMP14 = TEMP4 - R8TWOP8
               RESTE  = RESTE - TEMP14

               TEMP5  = RESTE + R8DEUXPM19
               TEMP15 = TEMP5 - R8DEUXPM19
               RESTE  = RESTE - TEMP15

               TEMP6  = RESTE + R8DEUXPM46
               TEMP16 = TEMP6 - R8DEUXPM46

#include "lockon.inc"
               F6(J,1,ISENSINT(I))  = F6(J,1,ISENSINT(I)) + TEMP11
               F6(J,2,ISENSINT(I))  = F6(J,2,ISENSINT(I)) + TEMP12
               F6(J,3,ISENSINT(I))  = F6(J,3,ISENSINT(I)) + TEMP13
               F6(J,4,ISENSINT(I))  = F6(J,4,ISENSINT(I)) + TEMP14
               F6(J,5,ISENSINT(I))  = F6(J,5,ISENSINT(I)) + TEMP15
               F6(J,6,ISENSINT(I))  = F6(J,6,ISENSINT(I)) + TEMP16 
#include "lockoff.inc"

              ENDDO
         ENDDO
        ENDIF
      ENDDO

      RETURN
      END
      !||====================================================================
      !||    sum_6_float_sect   ../engine/source/system/parit.F
      !||--- called by ------------------------------------------------------
      !||    section_3n         ../engine/source/tools/sect/section_3n.F
      !||    section_c          ../engine/source/tools/sect/section_c.F
      !||    section_p          ../engine/source/tools/sect/section_p.F
      !||    section_r          ../engine/source/tools/sect/section_r.F
      !||    section_s          ../engine/source/tools/sect/section_s.F
      !||    section_s4         ../engine/source/tools/sect/section_s4.F
      !||    section_s6         ../engine/source/tools/sect/section_s6.F
      !||    section_t          ../engine/source/tools/sect/section_t.F
      !||====================================================================
      SUBROUTINE SUM_6_FLOAT_SECT(F, A, B, JFT  ,JLT  , F6, D, E)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, A, B, D, E
      my_real F(A,B)
      DOUBLE PRECISION F6(D,E)
C-----------------------------------------------
c
c      r=26 (foat_to_6_float): nombre de bits pour la retenu
c         autorise  faire 2^r + ~= 67,000,000.
c
c      m1: masque 1
c      ...
c      m6: masque 6
c
c      m2 = m1 - 53 + r
c      m3 = m2 - 53 + r
c      m4 = m3 - 53 + r
c      m5 = m4 - 53 + r
c      m6 = m5 - 53 + r
c
c      si r= 26 : mi = mi-1 - 27
c
c      f1 = (f + 2^m1) - 2^m1
c       b = f - f1
c      f1 = (b + 2^m2) - 2^m2
c       d = b - f2
c      f3 = (d + 2^m3) - 2^m3
c      f4 = ((d - f3) + 2^m4) - 2^m4
c      ...
c--------- calcul de fmax, fmin 
c      fmax avec r bits  zero = 2^(m1-r)
c
c      fmin avec 53 bits significatif = 2^m6
c      fmin avec 1 bits significatif  = 2^(m6-53)
c
c--------- fmax, fmin aprs exprimentation
c      fmax avec r bits  zero ~= 2^(m1-2r)
c
c      fmin avec 53 bits significatif ~= 2^(m6-r)
c      fmin avec 1 bits significatif  ~= 2^(m6-53-r)
c
c 6 float r=26 m1=89 m6=-46
c 
c      fmax avec r bits  zero ~= 2^37 = 
c      fmin avec 53 bits significatif ~= 2^(m6-r)
c      fmin avec 1 bits significatif  ~= 2^(m6-53-r)
c-------------------------------------------------------
c       a = f + deuxp63
c       f4(1) = a - deuxp63
c       b = f - f4(1)
c       c = b + deuxp30
c       f4(2) = c - deuxp30
c       d = b - f4(2)
c       e = d + deuxpm3
c       f4(3) = e - deuxpm3
c       g = d - f4(3)
c       h = g + deuxpm36
c       f4(4) = h - deuxpm36
C-----------------------------------------------
C   L o c a l   C o m m o n
C-----------------------------------------------
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C Commun pour casser l optimisation et thread private pour multithread
C
      COMMON /PARIT_VAR/TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                  TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                  TEMP17,RESTE
!$OMP THREADPRIVATE(/PARIT_VAR/)
      DOUBLE PRECISION TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,
     .                 TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,TEMP16,
     .                 TEMP17,RESTE
C
C Attention - Attention - Attention - Attention - Attention - Attention
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      DOUBLE PRECISION R8DEUXP89,R8DEUXP62,R8DEUXP35,R8TWOP8
      DOUBLE PRECISION R8DEUXPM19,R8DEUXPM46
      DATA R8DEUXP89 /'4580000000000000'x/
      DATA R8DEUXP62 /'43D0000000000000'x/
      DATA R8DEUXP35 /'4220000000000000'x/
      DATA R8TWOP8  /'4070000000000000'x/
      DATA R8DEUXPM19/'3EC0000000000000'x/
      DATA R8DEUXPM46/'3D10000000000000'x/
      DO I= 1,A
          DO J=JFT,JLT

           RESTE  = F(I,J)

           TEMP1  = RESTE + R8DEUXP89
           TEMP11 = TEMP1 - R8DEUXP89
           RESTE  = RESTE - TEMP11

           TEMP2  = RESTE + R8DEUXP62
           TEMP12 = TEMP2 - R8DEUXP62
           RESTE  = RESTE - TEMP12

           TEMP3  = RESTE + R8DEUXP35
           TEMP13 = TEMP3 - R8DEUXP35
           RESTE  = RESTE - TEMP13

           TEMP4  = RESTE + R8TWOP8
           TEMP14 = TEMP4 - R8TWOP8
           RESTE  = RESTE - TEMP14

           TEMP5  = RESTE + R8DEUXPM19
           TEMP15 = TEMP5 - R8DEUXPM19
           RESTE  = RESTE - TEMP15

           TEMP6  = RESTE + R8DEUXPM46
           TEMP16 = TEMP6 - R8DEUXPM46

#include "lockon.inc"      
           F6(I,1)  = F6(I,1) + TEMP11
           F6(I,2)  = F6(I,2) + TEMP12
           F6(I,3)  = F6(I,3) + TEMP13
           F6(I,4)  = F6(I,4) + TEMP14
           F6(I,5)  = F6(I,5) + TEMP15
           F6(I,6)  = F6(I,6) + TEMP16
#include "lockoff.inc"

          ENDDO
      ENDDO

      RETURN
      END
