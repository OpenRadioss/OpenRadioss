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
Chd|====================================================================
Chd|  ASSEM_INT                     source/implicit/assem_int.F   
Chd|-- called by -----------
Chd|        I10KE3                        source/interfaces/int10/i10ke3.F
Chd|        I24KE3                        source/interfaces/int24/i24ke3.F
Chd|        I5KE3                         source/interfaces/inter3d/i5ke3.F
Chd|        I7KE3                         source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_INT(ND      ,NS        ,N1    ,N2    ,N3    ,
     1         N4       ,NEL       ,IDDL      ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,KI11      ,KI12      ,KJ11     ,
     3         KJ12     ,KK11      ,KK12      ,KL11      ,KL12     ,
     4         OFF      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND  ,NS(*)  ,N1(*)    ,N2(*)  ,N3(*),N4(*)   
      INTEGER 
     .        NEL   ,IDDL(*)   ,IADK(*)  ,JDIK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,OFF(*),
     .   KI11(ND,ND,*),KI12(ND,ND,*),KJ11(ND,ND,*),KJ12(ND,ND,*),
     .   KK11(ND,ND,*),KK12(ND,ND,*),KL11(ND,ND,*),KL12(ND,ND,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C-------NS,N1----
       CALL ASSEM_KII(NS,NEL,IDDL,IADK,K_DIAG,K_LT ,KI11,ND,OFF) 
       CALL ASSEM_KII(N1,NEL,IDDL,IADK,K_DIAG,K_LT ,KI11,ND,OFF) 
       CALL ASSEM_KIJ(NS,N1,NEL,IDDL,IADK,JDIK,
     .                K_DIAG,K_LT,KI12,ND,OFF) 
C-------NS,N2----
       CALL ASSEM_KII(NS,NEL,IDDL,IADK,K_DIAG,K_LT ,KJ11,ND,OFF) 
       CALL ASSEM_KII(N2,NEL,IDDL,IADK,K_DIAG,K_LT ,KJ11,ND,OFF) 
       CALL ASSEM_KIJ(NS,N2,NEL,IDDL,IADK,JDIK,
     .                K_DIAG,K_LT,KJ12,ND,OFF) 
C-------NS,N3----
       CALL ASSEM_KII(NS,NEL,IDDL,IADK,K_DIAG,K_LT ,KK11,ND,OFF) 
       CALL ASSEM_KII(N3,NEL,IDDL,IADK,K_DIAG,K_LT ,KK11,ND,OFF) 
       CALL ASSEM_KIJ(NS,N3,NEL,IDDL,IADK,JDIK,
     .                K_DIAG,K_LT,KK12,ND,OFF) 
C-------NS,N4----
       CALL ASSEM_KII(NS,NEL,IDDL,IADK,K_DIAG,K_LT ,KL11,ND,OFF) 
       CALL ASSEM_KII(N4,NEL,IDDL,IADK,K_DIAG,K_LT ,KL11,ND,OFF) 
       CALL ASSEM_KIJ(NS,N4,NEL,IDDL,IADK,JDIK,
     .                K_DIAG,K_LT,KL12,ND,OFF) 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ASS_SPMD                      source/implicit/assem_int.F   
Chd|-- called by -----------
Chd|        I10KE3                        source/interfaces/int10/i10ke3.F
Chd|        I24KE3                        source/interfaces/int24/i24ke3.F
Chd|        I7KE3                         source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ASS_SPMD(ND       ,NS        ,N1    ,N2    ,N3    ,
     1         N4       ,NEL       ,IDDL      ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,KI11      ,KI12      ,KJ11     ,
     3         KJ12     ,KK11      ,KK12      ,KL11      ,KL12     ,
     4         OFF      ,NIN       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND  ,NS(*)  ,N1(*)    ,N2(*)  ,N3(*),N4(*)   
      INTEGER 
     .        NEL   ,IDDL(*)   ,IADK(*)  ,JDIK(*),NIN
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,OFF(*),
     .   KI11(ND,ND,*),KI12(ND,ND,*),KJ11(ND,ND,*),KJ12(ND,ND,*),
     .   KK11(ND,ND,*),KK12(ND,ND,*),KL11(ND,ND,*),KL12(ND,ND,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I,NINDEX,INDEX(NEL),J,K,NN,IDI,IDJ
C
       NINDEX=0
       DO I=1,NEL
        IF (NS(I)>0) THEN
         NINDEX=NINDEX+1
         INDEX(NINDEX)=I
        ELSE
C--------spmd--secnd remote--Kss->(tempo)FFI,DFI, Diag_Kmm->Diag_I[KI 0]
c------------------------------------------------------------------[0 Dm]-
         CALL ASSEM_KII(N1(I),1,IDDL,IADK,K_DIAG,K_LT ,KI11(1,1,I),
     .                  ND   ,OFF(I)) 
         CALL ASSEM_KII(N2(I),1,IDDL,IADK,K_DIAG,K_LT ,KJ11(1,1,I), 
     .                  ND   ,OFF(I)) 
         CALL ASSEM_KII(N3(I),1,IDDL,IADK,K_DIAG,K_LT ,KK11(1,1,I), 
     .                  ND   ,OFF(I)) 
         CALL ASSEM_KII(N4(I),1,IDDL,IADK,K_DIAG,K_LT ,KL11(1,1,I), 
     .                  ND   ,OFF(I)) 
           NN=IND_INT(NIN)%P(-NS(I))
         DO J=1,ND
            FFI(J,NN)=FFI(J,NN) +
     .              KI11(J,J,I)+KJ11(J,J,I)+KK11(J,J,I)+KL11(J,J,I)
         ENDDO
           DFI(1,NN)=DFI(1,NN) +
     .             KI11(1,2,I)+KJ11(1,2,I)+KK11(1,2,I)+KL11(1,2,I)
           DFI(2,NN)=DFI(2,NN) +
     .             KI11(1,3,I)+KJ11(1,3,I)+KK11(1,3,I)+KL11(1,3,I)
           DFI(3,NN)=DFI(3,NN) +
     .             KI11(2,3,I)+KJ11(2,3,I)+KK11(2,3,I)+KL11(2,3,I)
         IF (INTP_D>0) THEN
C-------NS,N1----
          IDI = IDDL_SI(NN)
          IDJ = IDDL(N1(I))
          CALL ASSFR_KIJ(IDI    ,IDJ   ,IAD_SI  ,JDI_SI,LT_SI  ,
     1                   KI12(1,1,I),ND    )
          IDJ = IDDL(N2(I))
          CALL ASSFR_KIJ(IDI    ,IDJ   ,IAD_SI  ,JDI_SI,LT_SI  ,
     1                   KJ12(1,1,I),ND    )
          IDJ = IDDL(N3(I))
          CALL ASSFR_KIJ(IDI    ,IDJ   ,IAD_SI  ,JDI_SI,LT_SI  ,
     1                   KK12(1,1,I),ND    )
          IDJ = IDDL(N4(I))
          CALL ASSFR_KIJ(IDI    ,IDJ   ,IAD_SI  ,JDI_SI,LT_SI  ,
     1                   KL12(1,1,I),ND    )
        ENDIF 
        ENDIF
       ENDDO
       IF (NINDEX<NEL) THEN
        DO I=1,NINDEX
         N1(I)= N1(INDEX(I))
         N2(I)= N2(INDEX(I))
         N3(I)= N3(INDEX(I))
         N4(I)= N4(INDEX(I))
         NS(I)= NS(INDEX(I))
         OFF(I)= OFF(INDEX(I))
         DO J=1,ND
          DO K=J,ND
           KI11(J,K,I)=KI11(J,K,INDEX(I))
           KJ11(J,K,I)=KJ11(J,K,INDEX(I))
           KK11(J,K,I)=KK11(J,K,INDEX(I))
           KL11(J,K,I)=KL11(J,K,INDEX(I))
          ENDDO
          DO K=1,ND
           KI12(J,K,I)=KI12(J,K,INDEX(I))
           KJ12(J,K,I)=KJ12(J,K,INDEX(I))
           KK12(J,K,I)=KK12(J,K,INDEX(I))
           KL12(J,K,I)=KL12(J,K,INDEX(I))
          ENDDO
         ENDDO
        ENDDO
       ENDIF 
       NEL=NINDEX
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ASSEM_INT11                   source/implicit/assem_int.F   
Chd|-- called by -----------
Chd|        I11KE3                        source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_INT11(ND      ,N1        ,N2     ,M1        ,
     1         M2       ,NEL       ,IDDL      ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,K1I11    ,K1I12     ,K1J11    ,
     9         K1J12    ,K2I11     ,K2I12    ,K2J11     ,K2J12    ,
     4         OFF      )
C----6------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND  ,N1(*)  ,N2(*)    ,M1(*) ,M2(*) 
      INTEGER 
     .        NEL   ,IDDL(*)   ,IADK(*)  ,JDIK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,OFF(*),
     .   K1I11(ND,ND,*),K1I12(ND,ND,*),K1J11(ND,ND,*),K1J12(ND,ND,*),
     .   K2I11(ND,ND,*),K2I12(ND,ND,*),K2J11(ND,ND,*),K2J12(ND,ND,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
C-------N1,M1----
       CALL ASSEM_KII(N1,NEL,IDDL,IADK,K_DIAG,K_LT ,K1I11,ND,OFF) 
       CALL ASSEM_KII(M1,NEL,IDDL,IADK,K_DIAG,K_LT ,K1I11,ND,OFF) 
       CALL ASSEM_KIJ(N1,M1,NEL,IDDL,IADK,JDIK,
     .                K_DIAG,K_LT,K1I12,ND,OFF) 
C-------N1,M2----
       CALL ASSEM_KII(N1,NEL,IDDL,IADK,K_DIAG,K_LT ,K1J11,ND,OFF) 
       CALL ASSEM_KII(M2,NEL,IDDL,IADK,K_DIAG,K_LT ,K1J11,ND,OFF) 
       CALL ASSEM_KIJ(N1,M2,NEL,IDDL,IADK,JDIK,
     .                K_DIAG,K_LT,K1J12,ND,OFF) 
C-------N2,M1----
       CALL ASSEM_KII(N2,NEL,IDDL,IADK,K_DIAG,K_LT ,K2I11,ND,OFF) 
       CALL ASSEM_KII(M1,NEL,IDDL,IADK,K_DIAG,K_LT ,K2I11,ND,OFF) 
       CALL ASSEM_KIJ(N2,M1,NEL,IDDL,IADK,JDIK,
     .                K_DIAG,K_LT,K2I12,ND,OFF) 
C-------N2,M2----
       CALL ASSEM_KII(N2,NEL,IDDL,IADK,K_DIAG,K_LT ,K2J11,ND,OFF) 
       CALL ASSEM_KII(M2,NEL,IDDL,IADK,K_DIAG,K_LT ,K2J11,ND,OFF) 
       CALL ASSEM_KIJ(N2,M2,NEL,IDDL,IADK,JDIK,
     .                K_DIAG,K_LT,K2J12,ND,OFF) 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ASS_SPMD11                    source/implicit/assem_int.F   
Chd|-- called by -----------
Chd|        I11KE3                        source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ASS_SPMD11(ND     ,N1        ,N2        ,M1       ,
     1         M2       ,NEL       ,IDDL      ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,K1I11     ,K1I12     ,K1J11    ,
     9         K1J12    ,K2I11     ,K2I12     ,K2J11     ,K2J12    ,
     4         OFF      ,NIN       ,NS        ,NRTS      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND  ,N1(*)    ,N2(*)  ,M1(*),M2(*)   ,NS(*)    ,NRTS
      INTEGER 
     .        NEL   ,IDDL(*)   ,IADK(*)  ,JDIK(*),NIN
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,OFF(*),
     .   K1I11(ND,ND,*),K1I12(ND,ND,*),K1J11(ND,ND,*),K1J12(ND,ND,*),
     .   K2I11(ND,ND,*),K2I12(ND,ND,*),K2J11(ND,ND,*),K2J12(ND,ND,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I,NINDEX,INDEX(NEL),J,K,NN1,NN2,NN,IDI,IDJ
C
       NINDEX=0
       DO I=1,NEL
        IF (NS(I)<=NRTS) THEN
         NINDEX=NINDEX+1
         INDEX(NINDEX)=I
        ELSE
C--------spmd--secnd remote--Kss->(tempo)FFI,DFI, Diag_Kmm->Diag_I[KI 0]
c------------------------------------------------------------------[0 Dm]-
         CALL ASSEM_KII(M1(I),1,IDDL,IADK,K_DIAG,K_LT ,K1I11(1,1,I),
     .                  ND   ,OFF(I)) 
         CALL ASSEM_KII(M2(I),1,IDDL,IADK,K_DIAG,K_LT ,K1J11(1,1,I), 
     .                  ND   ,OFF(I)) 
         CALL ASSEM_KII(M1(I),1,IDDL,IADK,K_DIAG,K_LT ,K2I11(1,1,I),
     .                  ND   ,OFF(I)) 
         CALL ASSEM_KII(M2(I),1,IDDL,IADK,K_DIAG,K_LT ,K2J11(1,1,I), 
     .                  ND   ,OFF(I)) 
         NN = NS(I)-NRTS
           NN1=IND_INT(NIN)%P(NN)
           NN2=NN1+1
         DO J=1,ND
            FFI(J,NN1)=FFI(J,NN1) + K1I11(J,J,I)+K1J11(J,J,I)
            FFI(J,NN2)=FFI(J,NN2) + K2I11(J,J,I)+K2J11(J,J,I)
         ENDDO
           DFI(1,NN1)=DFI(1,NN1) +K1I11(1,2,I)+K1J11(1,2,I)
           DFI(2,NN1)=DFI(2,NN1) +K1I11(1,3,I)+K1J11(1,3,I)
           DFI(3,NN1)=DFI(3,NN1) +K1I11(2,3,I)+K1J11(2,3,I)
           DFI(1,NN2)=DFI(1,NN2) +K2I11(1,2,I)+K2J11(1,2,I)
           DFI(2,NN2)=DFI(2,NN2) +K2I11(1,3,I)+K2J11(1,3,I)
           DFI(3,NN2)=DFI(3,NN2) +K2I11(2,3,I)+K2J11(2,3,I)
         IF (INTP_D>0) THEN
          IDI = IDDL_SI(NN1)
          IDJ = IDDL(M1(I))
          CALL ASSFR_KIJ(IDI    ,IDJ   ,IAD_SI  ,JDI_SI,LT_SI  ,
     1                   K1I12(1,1,I),ND    )
          IDJ = IDDL(M2(I))
          CALL ASSFR_KIJ(IDI    ,IDJ   ,IAD_SI  ,JDI_SI,LT_SI  ,
     1                   K1J12(1,1,I),ND    )
          IDI = IDDL_SI(NN2)
          CALL ASSFR_KIJ(IDI    ,IDJ   ,IAD_SI  ,JDI_SI,LT_SI  ,
     1                   K2J12(1,1,I),ND    )
          IDJ = IDDL(M1(I))
          CALL ASSFR_KIJ(IDI    ,IDJ   ,IAD_SI  ,JDI_SI,LT_SI  ,
     1                   K2I12(1,1,I),ND    )
        ENDIF 
        ENDIF
       ENDDO
       IF (NINDEX<NEL) THEN
        DO I=1,NINDEX
         N1(I)= N1(INDEX(I))
         N2(I)= N2(INDEX(I))
         M1(I)= M1(INDEX(I))
         M2(I)= M2(INDEX(I))
         NS(I)= NS(INDEX(I))
         OFF(I)= OFF(INDEX(I))
         DO J=1,ND
          DO K=J,ND
           K1I11(J,K,I)=K1I11(J,K,INDEX(I))
           K1J11(J,K,I)=K1J11(J,K,INDEX(I))
           K2I11(J,K,I)=K2I11(J,K,INDEX(I))
           K2J11(J,K,I)=K2J11(J,K,INDEX(I))
          ENDDO
          DO K=1,ND
           K1I12(J,K,I)=K1I12(J,K,INDEX(I))
           K1J12(J,K,I)=K1J12(J,K,INDEX(I))
           K2I12(J,K,I)=K2I12(J,K,INDEX(I))
           K2J12(J,K,I)=K2J12(J,K,INDEX(I))
          ENDDO
         ENDDO
        ENDDO
       ENDIF 
       NEL=NINDEX
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
