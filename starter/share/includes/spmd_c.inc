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
C.../SPMDV5/
      INTEGER LSPMDV5
      PARAMETER (LSPMDV5 = 50)
      COMMON /SPMDV5/ NSKYRW0 ,NSKYRBK0,NBDDNOD,NBDDNRB ,MULTIMAX,
     .                NBDDI2M ,NISKYI2 ,LCNI2  ,I2NSNT  ,NSKYMV0 ,
     .                NSKYMVC0,NSKYLL0 ,NBDDNCJ,NBDDNRBM,NSKYRBM0,
     .                NNODTCUT,NNODLCUT,NUMNODG,NUMNODM, NUMELQG, 
     .                NUMELCG, NUMELTGG,NUMELURG,NUMELSG,NUMELPG, 
     .                NUMELRG, NUMELTRG,NB1DG,  NSKEWAG, SECTIOM,
     .                NUMELS16G,NUM16SHIFT,SPHSHIFT,NUMSPHG,NUMELXG,
     .                NBI18   ,NSKYI18 ,SKIPPON,NSPMDN,NBDDRBE3M,
     .                NSKYRBE30,NBDDNRBE2,NRBE2G,NRBE3G,NBDDEDGE,
     .                NBDDNORT,NBDDNOR_MAX,NBCCFR25, NBCCNOR, NSNT25,
     .                NUMELS10G,NUMELS20G,NBDDEDGT,NBDDEDG_MAX,NRTMX25

      INTEGER NSKYRW0 ,NSKYRBK0,NBDDNOD,NBDDNRB ,MULTIMAX,
     .        NBDDI2M ,NISKYI2 ,LCNI2  ,I2NSNT  ,NSKYMV0 ,
     .        NSKYMVC0,NSKYLL0 ,NBDDNCJ,NBDDNRBM,NSKYRBM0,
     .        NNODTCUT,NNODLCUT,NUMNODG,NUMNODM, NUMELQG,
     .        NUMELCG, NUMELTGG,NUMELURG,NUMELSG,NUMELPG,
     .        NUMELRG, NUMELTRG,NB1DG,  NSKEWAG, SECTIOM,
     .        NUMELS16G,NUM16SHIFT,SPHSHIFT,NUMSPHG,NUMELXG,
     .        NBI18   ,NSKYI18,SKIPPON,NSPMDN,NBDDRBE3M,
     .        NSKYRBE30,NBDDNRBE2,NRBE2G,NRBE3G,NBDDEDGE,
     .        NBDDNORT,NBDDNOR_MAX,NBCCFR25, NBCCNOR, NSNT25,
     .        NUMELS10G,NUMELS20G,NBDDEDGT,NBDDEDG_MAX,NRTMX25

C variables specifiques CFD/SPMD
C.../COMCFD/
      INTEGER LCOMCFD
      PARAMETER (LCOMCFD = 10)
C
      COMMON /COMCFD/NSVOIS ,NQVOIS , NTGVOIS, NRCVVOIS,NSNDVOIS,NERVOIS,
     .               NESVOIS,ILAW11 ,NLWALE  ,NSEGCOM, NUMPORL
      INTEGER NSVOIS ,NQVOIS , NTGVOIS, NRCVVOIS,NSNDVOIS,NERVOIS,
     .        NESVOIS,ILAW11 ,NLWALE  ,NSEGCOM, NUMPORL 

