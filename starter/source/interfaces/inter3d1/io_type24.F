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
Chd|  IO_TYPE24                     source/interfaces/inter3d1/io_type24.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IO_IARRAY                     source/interfaces/inter3d1/io_type24.F
Chd|        IO_RARRAY                     source/interfaces/inter3d1/io_type24.F
Chd|====================================================================
      SUBROUTINE IO_TYPE24(IDO,IOUT,
     .                     IGAP,
     .                     IPEN0,
     .                     I_STOK, ! output
     .                     INACTI,
     .                     MULTIMP,
     .                     NMN,
     .                     NRTM,
     .                     NRTM0,
     .                     NRTM_SH,
     .                     NRTSE,
     .                     NSN,
     .                     NSN0,
     .                     NSNE,
     .                     NUMELS,
     .                     NUMELS8,
     .                     NUMELS10,
     .                     NUMELS16,
     .                     NUMELS20,
     .                     NUMNOD ,
     .                     B_I_STOK,
     .                     ICONT_I,
     .                     IPARTNS,
     .                     IRECTM,
     .                     IRTLM,
     .                     IRTSE,
     .                     IS2SE,
     .                     IS2PT,
     .                     ITAB ,
     .                     MBINFLG,
     .                     MSEGTYP24,
     .                     MSR,
     .                     MWA,
     .                     NBINFLG,
     .                     NOD2ELS,
     .                     KNOD2ELS,
     .                     NSEG,
     .                     NSV,
     .                     IXS,
     .                     IXS10,
     .                     IXS16,
     .                     IXS20,
     .                     MVOISIN,
     .                     VARIABLES,
     .                     X,
     .                     CAND_E,
     .                     CAND_N,
     .                     GAP_M,
     .                     GAP_NM,
     .                     GAP_S,
     .                     PENE_OLD,
     .                     PENMIN,
     .                     STFM ,
     .                     STFNS,
     .                     STIF_OLD,
     .                     XFIC,
     .                     RWA)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IDO ! 0 => WRITE
                     ! 1 => READ SIZES
                     ! 2 => READ VALUES
C=======================================================================
      INTEGER :: IOUT
      INTEGER :: IGAP
      INTEGER :: IPEN0
      INTEGER :: I_STOK
      INTEGER :: INACTI
      INTEGER :: MULTIMP
      INTEGER :: NMN
      INTEGER :: NRTM
      INTEGER :: NRTM0
      INTEGER :: NRTM_SH
      INTEGER :: NRTSE
      INTEGER :: NSN
      INTEGER :: NSN0
      INTEGER :: NSNE
      INTEGER :: NUMELS
      INTEGER :: NUMELS8
      INTEGER :: NUMELS10
      INTEGER :: NUMELS16
      INTEGER :: NUMELS20
      INTEGER :: NUMNOD
C ============================================
      INTEGER :: B_I_STOK(*)
      INTEGER :: ICONT_I(*)
      INTEGER :: IPARTNS(*)
      INTEGER :: IRECTM(*)
      INTEGER :: IRTLM(*)
      INTEGER :: IRTSE(*)
      INTEGER :: IS2SE(*)
      INTEGER :: IS2PT(*)
      INTEGER :: ITAB (*)
      INTEGER :: MBINFLG(*)
      INTEGER :: MSEGTYP24(*)
      INTEGER :: MSR(*)
      INTEGER :: MWA(*)
      INTEGER :: NBINFLG(*)
      INTEGER :: NOD2ELS,KNOD2ELS(*)
      INTEGER :: NSEG(*)
      INTEGER :: NSV(*)
      INTEGER :: IXS(*)
      INTEGER :: IXS10(*)
      INTEGER :: IXS16(*)
      INTEGER :: IXS20 (*)
      INTEGER :: MVOISIN(*)
      REAL(8) :: VARIABLES(38)
      REAL(8) :: X(*)
      INTEGER :: CAND_E(*)
      INTEGER :: CAND_N(*)
      REAL(8) :: GAP_M(*)
      REAL(8) :: GAP_NM(*)
      REAL(8) :: GAP_S(*)
      REAL(8) :: PENE_OLD(*)
      REAL(8) :: PENMIN(*)
      REAL(8) :: STFM (*)
      REAL(8) :: STFNS(*)
      REAL(8) :: STIF_OLD(*)
      REAL(8) :: XFIC(*)
      REAL(8) :: RWA (*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: SIZ_B_I_STOK
      INTEGER :: SIZ_ICONT_I
      INTEGER :: SIZ_IPARTNS
      INTEGER :: SIZ_IRECTM
      INTEGER :: SIZ_IRTLM
      INTEGER :: SIZ_IRTSE
      INTEGER :: SIZ_IS2SE
      INTEGER :: SIZ_IS2PT
      INTEGER :: SIZ_ITAB
      INTEGER :: SIZ_MBINFLG
      INTEGER :: SIZ_MSEGTYP24
      INTEGER :: SIZ_MSR
      INTEGER :: SIZ_MWA
      INTEGER :: SIZ_NBINFLG
      INTEGER :: SIZ_NOD2ELS
      INTEGER :: SIZ_KNOD2ELS
      INTEGER :: SIZ_NSEG
      INTEGER :: SIZ_NSV
      INTEGER :: SIZ_IXS
      INTEGER :: SIZ_IXS10
      INTEGER :: SIZ_IXS16
      INTEGER :: SIZ_IXS20
      INTEGER :: SIZ_MVOISIN
      INTEGER :: SIZ_X
      INTEGER :: SIZ_CAND_E
      INTEGER :: SIZ_CAND_N
      INTEGER :: SIZ_GAP_M
      INTEGER :: SIZ_GAP_NM
      INTEGER :: SIZ_GAP_S
      INTEGER :: SIZ_PENE_OLD
      INTEGER :: SIZ_PENMIN
      INTEGER :: SIZ_STFM
      INTEGER :: SIZ_STFNS
      INTEGER :: SIZ_STIF_OLD
      INTEGER :: SIZ_XFIC
      INTEGER :: SIZ_RWA
      LOGICAL :: FLAG
      INTEGER :: IEDGE
      INTEGER :: IEDGE4
      INTEGER :: ILEV

C=======================================================================
      IEDGE = 0
      IEDGE4= 0
      ILEV = 1
      FLAG = .FALSE.           !write

      IF(IDO==0) FLAG = .TRUE. !read

C============================ SIZES
       IF(IDO == 0 .OR. IDO == 1) THEN
       WRITE(6,*)"IGAP    " ; CALL IO_INT(IGAP      ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"IPEN0   " ; CALL IO_INT(IPEN0     ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"I_STOK  " ; CALL IO_INT(I_STOK    ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"INACTI  " ; CALL IO_INT(INACTI    ,IOUT ,FLAG); CALL FLUSH(6)
c      WRITE(6,*)"MULTIMP " ; CALL IO_INT(MULTIMP   ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NMN     " ; CALL IO_INT(NMN       ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NRTM    " ; CALL IO_INT(NRTM      ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NRTM0   " ; CALL IO_INT(NRTM0     ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NRTM_SH " ; CALL IO_INT(NRTM_SH   ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NRTSE   " ; CALL IO_INT(NRTSE     ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NSN     " ; CALL IO_INT(NSN       ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NSN0    " ; CALL IO_INT(NSN0      ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NSNE    " ; CALL IO_INT(NSNE      ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NUMELS  " ; CALL IO_INT(NUMELS    ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NUMELS8 " ; CALL IO_INT(NUMELS8    ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NUMELS10" ; CALL IO_INT(NUMELS10  ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NUMELS16" ; CALL IO_INT(NUMELS16  ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NUMELS20" ; CALL IO_INT(NUMELS20  ,IOUT ,FLAG); CALL FLUSH(6)
       WRITE(6,*)"NUMNOD  " ; CALL IO_INT(NUMNOD    ,IOUT ,FLAG); CALL FLUSH(6)
       ENDIF


C============================= lengths
       IF(IDO == 0 .OR. IDO ==2) THEN
       SIZ_B_I_STOK      =1
       SIZ_ICONT_I       =NSN
       SIZ_IPARTNS       =NSN
       SIZ_IRECTM        =4 * NRTM
       SIZ_IRTLM         =2 * NSN
       SIZ_IRTSE = 0
       SIZ_IS2SE = 0
       IF(IEDGE4>0)  SIZ_IRTSE         =5 * NSNE
       IF(IEDGE4>0)  SIZ_IS2SE         =2 * NSNE
       SIZ_IS2PT         =NSNE
       SIZ_ITAB          =NUMNOD
       SIZ_MBINFLG = 0
       IF(IEDGE >0 .OR. ILEV == 2) SIZ_MBINFLG       =NRTM
       SIZ_MSEGTYP24     =NRTM
       SIZ_MSR           =NMN
       SIZ_MWA           =6*MAX(NUMNOD,NRTM+100)
       SIZ_NBINFLG = 0
       IF(IEDGE >0 .OR. ILEV == 2)  SIZ_NBINFLG       =NMN
       SIZ_NOD2ELS       =8*NUMELS+6*NUMELS10+12*NUMELS20+8*NUMELS16
       SIZ_KNOD2ELS      =NUMNOD + 1
       SIZ_NSEG          =1+NMN
       SIZ_NSV           =NSN
       SIZ_IXS           =NUMELS*11
       SIZ_IXS10         =NUMELS10*6
       SIZ_IXS16         =NUMELS20*12
       SIZ_IXS20         =NUMELS16*8
       SIZ_MVOISIN       =NRTM*4
       SIZ_X             =NUMNOD*3
       SIZ_CAND_E        =(NSN * MULTIMP)
       SIZ_CAND_N        =(NSN * MULTIMP )
       SIZ_GAP_M =0
       IF(IGAP > 0 ) SIZ_GAP_M         =NRTM
       SIZ_GAP_NM        =12*NRTM
       SIZ_GAP_S = 0
       IF(IGAP > 0 ) SIZ_GAP_S         =NSN
       SIZ_PENE_OLD      =5 *NSN
       SIZ_PENMIN        =NSN
       SIZ_STFM          =NRTM
       SIZ_STFNS         =NSN
       SIZ_STIF_OLD      =2*NSN
       SIZ_XFIC          =3*NSNE
       SIZ_RWA           =6*NUMNOD

C======================== INTEGERS
      WRITE(6,*) " IPARTNS   ";CALL IO_IARRAY (IPARTNS  ,SIZ_IPARTNS  ,IOUT ,FLAG)
      WRITE(6,*) " IRECTM    ";CALL IO_IARRAY (IRECTM   ,SIZ_IRECTM   ,IOUT ,FLAG)
c     WRITE(6,*) " IRTLM     ";CALL IO_IARRAY (IRTLM    ,SIZ_IRTLM    ,IOUT ,FLAG)
      WRITE(6,*) " IRTSE     ";CALL IO_IARRAY (IRTSE    ,SIZ_IRTSE    ,IOUT ,FLAG)
      WRITE(6,*) " IS2SE     ";CALL IO_IARRAY (IS2SE    ,SIZ_IS2SE    ,IOUT ,FLAG)
      WRITE(6,*) " IS2PT     ";CALL IO_IARRAY (IS2PT    ,SIZ_IS2PT    ,IOUT ,FLAG)
      WRITE(6,*) " ITAB      ";CALL IO_IARRAY (ITAB     ,SIZ_ITAB     ,IOUT ,FLAG)
      WRITE(6,*) " MBINFLG   ";CALL IO_IARRAY (MBINFLG  ,SIZ_MBINFLG  ,IOUT ,FLAG)
      WRITE(6,*) " MSEGTYP24 ";CALL IO_IARRAY (MSEGTYP24,SIZ_MSEGTYP24,IOUT ,FLAG)
      WRITE(6,*) " MSR       ";CALL IO_IARRAY (MSR      ,SIZ_MSR      ,IOUT ,FLAG)
c     WRITE(6,*) " MWA       ";CALL IO_IARRAY (MWA      ,SIZ_MWA      ,IOUT ,FLAG)
      WRITE(6,*) " NBINFLG   ";CALL IO_IARRAY (NBINFLG  ,SIZ_NBINFLG  ,IOUT ,FLAG)
      WRITE(6,*) " NOD2ELS   ";CALL IO_IARRAY (NOD2ELS  ,SIZ_NOD2ELS  ,IOUT ,FLAG)
      WRITE(6,*) " KNOD2ELS  ";CALL IO_IARRAY (KNOD2ELS ,SIZ_KNOD2ELS ,IOUT ,FLAG)
c     WRITE(6,*) " NSEG      ";CALL IO_IARRAY (NSEG     ,SIZ_NSEG     ,IOUT ,FLAG)
      WRITE(6,*) " NSV       ";CALL IO_IARRAY (NSV      ,SIZ_NSV      ,IOUT ,FLAG)
      WRITE(6,*) " ICONT_I   ";CALL IO_IARRAY (ICONT_I    ,SIZ_ICONT_I    ,IOUT ,FLAG)
      WRITE(6,*) " IXS       ";CALL IO_IARRAY (IXS      ,SIZ_IXS      ,IOUT ,FLAG)
      WRITE(6,*) " IXS10     ";CALL IO_IARRAY (IXS10    ,SIZ_IXS10    ,IOUT ,FLAG)
      WRITE(6,*) " IXS16     ";CALL IO_IARRAY (IXS16    ,SIZ_IXS16    ,IOUT ,FLAG)
      WRITE(6,*) " IXS20     ";CALL IO_IARRAY (IXS20    ,SIZ_IXS20    ,IOUT ,FLAG)
      WRITE(6,*) " MVOISIN   ";CALL IO_IARRAY (MVOISIN  ,SIZ_MVOISIN  ,IOUT ,FLAG)
      WRITE(6,*) " CAND_E    ";CALL IO_IARRAY (CAND_E   ,SIZ_CAND_E   ,IOUT ,FLAG)
      WRITE(6,*) " CAND_N    ";CALL IO_IARRAY (CAND_N   ,SIZ_CAND_N   ,IOUT ,FLAG)
C=============================;=========================
      WRITE(6,*) " VARIABLES ";CALL IO_RARRAY (VARIABLES,38           ,IOUT ,FLAG)
      WRITE(6,*) " X         ";CALL IO_RARRAY (X        ,SIZ_X        ,IOUT ,FLAG)
      WRITE(6,*) " GAP_M     ";CALL IO_RARRAY (GAP_M    ,SIZ_GAP_M    ,IOUT ,FLAG)
      WRITE(6,*) " GAP_NM    ";CALL IO_RARRAY (GAP_NM   ,SIZ_GAP_NM   ,IOUT ,FLAG)
      WRITE(6,*) " GAP_S     ";CALL IO_RARRAY (GAP_S    ,SIZ_GAP_S    ,IOUT ,FLAG)
c     WRITE(6,*) " PENE_OLD  ";CALL IO_RARRAY (PENE_OLD ,SIZ_PENE_OLD ,IOUT ,FLAG)
c     WRITE(6,*) " STFM      ";CALL IO_RARRAY (STFM     ,SIZ_STFM     ,IOUT ,FLAG)
c     WRITE(6,*) " STFNS     ";CALL IO_RARRAY (STFNS    ,SIZ_STFNS    ,IOUT ,FLAG)
c     WRITE(6,*) " STIF_OLD  ";CALL IO_RARRAY (STIF_OLD ,SIZ_STIF_OLD ,IOUT ,FLAG)
      WRITE(6,*) " XFIC      ";CALL IO_RARRAY (XFIC     ,SIZ_XFIC     ,IOUT ,FLAG)
c     WRITE(6,*) " RWA       ";CALL IO_RARRAY (RWA      ,SIZ_RWA      ,IOUT ,FLAG)

       ENDIF
      CALL FLUSH(6)
      CALL FLUSH(IOUT)

      RETURN
      END
Chd|====================================================================
Chd|  IO_INT                        source/interfaces/inter3d1/io_type24.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IO_INT(SCALAR,IOUT,FLAG)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"

      INTEGER, INTENT(IN)   :: IOUT
      INTEGER, INTENT(INOUT):: SCALAR
      LOGICAL, INTENT(IN)   :: FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ! Integer
       IF(FLAG) THEN
          WRITE(IOUT,'(I20.11)' ) SCALAR
c         WRITE(700,*) "W",SCALAR,LOC(SCALAR)
       ELSE
          READ(IOUT,'(I20.11)' ) SCALAR
c         WRITE(6,*) SCALAR
       ENDIF
c     CALL FLUSH(700)
      RETURN
      END

Chd|====================================================================
Chd|  IO_IARRAY                     source/interfaces/inter3d1/io_type24.F
Chd|-- called by -----------
Chd|        IO_TYPE24                     source/interfaces/inter3d1/io_type24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IO_IARRAY(ARRAY,LENGTH,IOUT,FLAG)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"

      INTEGER, INTENT(IN)   :: LENGTH,IOUT
      INTEGER, INTENT(INOUT):: ARRAY(LENGTH)
      LOGICAL, INTENT(IN)   :: FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ! Integer
       IF(FLAG) THEN
         DO I=1,LENGTH
          WRITE(IOUT,'(I20.11)' ) ARRAY(I)
c          WRITE(700,*) "W",I,ARRAY(I)
         ENDDO
       ELSE
         DO I=1,LENGTH
          READ(IOUT,'(I20.11)' ) ARRAY(I)
c          WRITE(6,*) I,ARRAY(I)
         ENDDO
       ENDIF
      RETURN
      END
C============================================================================
Chd|====================================================================
Chd|  IO_RARRAY                     source/interfaces/inter3d1/io_type24.F
Chd|-- called by -----------
Chd|        IO_TYPE24                     source/interfaces/inter3d1/io_type24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IO_RARRAY(ARRAY,LENGTH,IOUT,FLAG)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"

      INTEGER, INTENT(IN)   :: LENGTH,IOUT
      my_real, INTENT(INOUT):: ARRAY(LENGTH)
      LOGICAL, INTENT(IN)   :: FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
       IF(FLAG) THEN
         DO I=1,LENGTH
            WRITE(IOUT,'(Z20)' ) ARRAY(I)
C            WRITE(700,*) "W",I,ARRAY(I)
         ENDDO
       ELSE
         DO I=1,LENGTH
          READ(IOUT,'(Z20)' ) ARRAY(I)
c         WRITE(6,*) I,ARRAY(I)
         ENDDO
       ENDIF
      RETURN
      END
