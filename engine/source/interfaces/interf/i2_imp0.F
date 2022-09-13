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
Chd|  I2_IMP0                       source/interfaces/interf/i2_imp0.F
Chd|-- called by -----------
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        I2_IMP1                       source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPM                       source/interfaces/interf/i2_imp1.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2_IMP0(NINT2 ,IINT2 ,IPARI ,
     .                   INTBUF_TAB,X   ,MS   ,IN   ,
     .                   NMC2  ,IMIJ2 ,ITAB  ,
     .                   NSC2  ,ISIJ2 ,NSS2  ,ISS2,
     .                   WEIGHT,IKC   ,NDOF  ,NDDL,IDDL  ,
     .                   IADK  ,JDIK  ,DIAG_K,LT_K,B    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), WEIGHT(*),
     .        NINT2 ,IINT2(*),NSC2(*),ISIJ2(*),NSS2(*),ISS2(*),
     .        NMC2,IMIJ2(*),ITAB(*)
      INTEGER NDDL,IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*)
C     REAL
      my_real
     .   X(*), MS(*),IN(*),DIAG_K(*),LT_K(*),B(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY, JI, JB, NSN, I, K,NK,NN,J
C-----------------------------------------------
        K=1
        NK=1
        NN=1
        DO I=1,NINT2
          N=IINT2(I)
         JI=IPARI(1,N)
         JB=IPARI(2,N)
         NSN =IPARI(5,N)
         CALL I2_IMP1(IPARI(1,N),INTBUF_TAB(N),ITAB   ,
     .                NSC2(I),ISIJ2(NK),NSS2(K),ISS2(NN),
     .                X  ,MS  ,  IN,WEIGHT,
     .                IKC ,NDOF ,NDDL,IDDL    ,IADK  ,
     .                JDIK ,DIAG_K ,LT_K  ,B  )
          DO J=1,NSN
           NN  = NN  + NSS2(K+J-1)
          ENDDO 
          K  = K  + NSN
          NK  = NK  + 2*NSC2(I)
        ENDDO 
        IF (NMC2>0) 
     .  CALL I2_IMPM(IPARI     ,INTBUF_TAB,NMC2   ,IMIJ2  , 
     .                X        ,MS     ,     IN,WEIGHT,
     .                NDOF     ,NDDL     ,IDDL   ,IADK   ,JDIK  ,
     .                LT_K     ,DIAG_K   )
C
      RETURN
      END
Chd|====================================================================
Chd|  I2_IMPI                       source/interfaces/interf/i2_imp0.F
Chd|-- called by -----------
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        I2_IMP1                       source/interfaces/interf/i2_imp1.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2_IMPI(NINT2 ,IINT2 ,IPARI ,
     .                   INTBUF_TAB,X   ,MS   ,IN   ,
     .                   NSS2  ,ISS2,WEIGHT,IKC   ,NDOF  ,
     .                   NDDL  ,IDDL ,IADK  ,JDIK  ,DIAG_K,
     .                   LT_K  ,IAINT2,B    ,ITAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), WEIGHT(*),
     .        NINT2 ,IINT2(*),NSS2(*),ISS2(*),IAINT2(*),ITAB(*)
      INTEGER NDDL,IADK(*),JDIK(*),NDOF(*),IDDL(*),IKC(*)
C     REAL
      my_real
     .   X(*), MS(*),IN(*),DIAG_K(*),LT_K(*),B(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, JI, JB, NSN, I, K,J,NN,NSC2,ISIJ2
C-----------------------------------------------
        K=1
        NN=1
        NSC2=0
        DO I=1,NINT2
         IF (IAINT2(I)==1) THEN
          N=IINT2(I)
          JI=IPARI(1,N)
          JB=IPARI(2,N)
          NSN =IPARI(5,N)
          CALL I2_IMP1(IPARI(1,N),INTBUF_TAB(N) ,ITAB  ,
     .                NSC2,ISIJ2,NSS2(K),ISS2(NN),
     .                X  ,MS  ,  IN,WEIGHT,
     .                IKC ,NDOF ,NDDL,IDDL    ,IADK  ,
     .                JDIK ,DIAG_K ,LT_K  ,B  )
         DO J=1,NSN
           NN  = NN  + NSS2(K+J-1)
         ENDDO 
         K  = K  + NSN
         ENDIF 
        ENDDO 
C
      RETURN
      END
