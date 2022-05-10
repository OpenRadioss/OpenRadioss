#ifndef f90
#define f90 1990
#endif
#ifndef f77
#define f77 1977
#endif
#ifndef CPP_sgi3
#define CPP_sgi3 103
#endif
#ifndef CPP_sgi4
#define CPP_sgi4 104
#endif
#ifndef CPP_sgi5
#define CPP_sgi5 105
#endif
#ifndef CPP_sgi6
#define CPP_sgi6 106
#endif
#ifndef CPP_sgi6_spmd
#define CPP_sgi6_spmd 126
#endif
#ifndef CPP_ymp
#define CPP_ymp 201
#endif
#ifndef CPP_c90
#define CPP_c90 202
#endif
#ifndef CPP_t90
#define CPP_t90 203
#endif
#ifndef CPP_t90_i3e
#define CPP_t90_i3e 204
#endif
#ifndef CPP_ymp_spmd
#define CPP_ymp_spmd 221
#endif
#ifndef CPP_cx1_spmd
#define CPP_cx1_spmd 231
#endif
#ifndef CPP_cx1
#define CPP_cx1 232
#endif
#ifndef CPP_hp9
#define CPP_hp9 301
#endif
#ifndef CPP_hp10
#define CPP_hp10 302
#endif
#ifndef CPP_hp11
#define CPP_hp11 303
#endif
#ifndef CPP_hp9_spmd
#define CPP_hp9_spmd 321
#endif
#ifndef CPP_hp11_spmd
#define CPP_hp11_spmd 322
#endif
#ifndef CPP_cvx
#define CPP_cvx 311
#endif
#ifndef CPP_sun
#define CPP_sun 401
#endif
#ifndef CPP_sun25
#define CPP_sun25 402
#endif
#ifndef CPP_sol10x64_spmd
#define CPP_sol10x64_spmd 412
#endif
#ifndef CPP_rs7
#define CPP_rs7 501
#endif
#ifndef CPP_sp2
#define CPP_sp2 502
#endif
#ifndef CPP_rs9
#define CPP_rs9 503
#endif
#ifndef CPP_ppc
#define CPP_ppc 505
#endif
#ifndef CPP_pwr4
#define CPP_pwr4 506
#endif
#ifndef CPP_pwr4_spmd
#define CPP_pwr4_spmd 507
#endif
#ifndef CPP_dec
#define CPP_dec 601
#endif
#ifndef CPP_deca
#define CPP_deca 602
#endif
#ifndef CPP_deca_mp
#define CPP_deca_mp 603
#endif
#ifndef CPP_deca_spmd
#define CPP_deca_spmd 611
#endif
#ifndef CPP_sx4
#define CPP_sx4 703
#endif
#ifndef CPP_sx5_spmd
#define CPP_sx5_spmd 705
#endif
#ifndef CPP_sx4_i3e
#define CPP_sx4_i3e 704
#endif
#ifndef CPP_vpp
#define CPP_vpp 801
#endif
#ifndef CPP_vpp_spmd
#define CPP_vpp_spmd 821
#endif
#ifndef CPP_ppw_spmd
#define CPP_ppw_spmd 831
#endif
#ifndef CPP_ppw
#define CPP_ppw 841
#endif
#ifndef CPP_w95
#define CPP_w95 901
#endif
#ifndef CPP_wnt
#define CPP_wnt 911
#endif
#ifndef CPP_win64_spmd
#define CPP_win64_spmd 921
#endif
#ifndef CPP_p4win64_spmd
#define CPP_p4win64_spmd 922
#endif
#ifndef CPP_p4win32
#define CPP_p4win32 923
#endif
#ifndef CPP_linux
#define CPP_linux 931
#endif
#ifndef CPP_linux_spmd
#define CPP_linux_spmd 941
#endif
#ifndef CPP_alinux
#define CPP_alinux 961
#endif
#ifndef CPP_alinux_spmd
#define CPP_alinux_spmd 971
#endif
#ifndef CPP_linux64_spmd
#define CPP_linux64_spmd 981
#endif
#ifndef CPP_il_spmd
#define CPP_il_spmd 1000
#endif
#ifndef CPP_il
#define CPP_il 1010
#endif
#ifndef CPP_p4linux964_spmd
#define CPP_p4linux964_spmd 1100
#endif
#ifndef CPP_lmr
#define CPP_lmr 1200
#endif
#ifndef CPP_wmr
#define CPP_wmr 1210
#endif
#ifndef CPP_linux964
#define CPP_linux964 1310
#endif
#ifndef CPP_p4win64
#define CPP_p4win64 1410
#endif
#ifndef CPP_p4linux964
#define CPP_p4linux964 1510
#endif
#ifndef CPP_p4linux932
#define CPP_p4linux932 1610
#endif
#ifndef CPP_macosx64
#define CPP_macosx64 1710
#endif

#ifndef CPP_mach
/* CPP_unknow doit etre egal a 1 (valeur par defaut des variables cpp non definies)*/
#endif

#ifndef CPP_unknow
#define CPP_unknow 1
#endif

#ifndef CPP_mach
 on ne doit pas passer par ici(spe/hardware.inc); si on passe ici verifiez:
   - que le f77 est utilise dans Make_head
   - si f77 ne supporte pas cpp verifiez que f777 est a jour 
     (transfert de -Dvar=def a cpp; voir /radiossb/v31/util/f777/f777.c)
   - en remote pour machine_c.c ne pas oublier de compiler avec 
     cc -DCPP_mach=CPP_xxx -c machine_c.c -o obj/machine_c.o 
     ou xxx est le nom de la machine (c90 ymp cvx nec ...)
#endif
