## Set wo_linalg flag when linear algebra (MUMPS) is not being used.
if (NOT DEFINED WITH_LINEAR_ALGEBRA)
  set(wo_linalg "-DWITHOUT_LINALG")
endif()
