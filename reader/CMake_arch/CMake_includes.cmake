##########################################################
# hm_reader has problematic include statements for CMAKE
# need to add Manually include list

list (APPEND include_dir_list ${open_reader_root_dir}/source )
list (APPEND include_dir_list ${open_reader_root_dir}/source/sdi/converter/ )
list (APPEND include_dir_list ${open_reader_root_dir}/source/sdi/interface/ )
list (APPEND include_dir_list ${open_reader_root_dir}/source/sdi/utils/ )
list (APPEND include_dir_list ${open_reader_root_dir}/source/cfgkernel/ )
list (APPEND include_dir_list ${open_reader_root_dir}/source/cfgio/ )
list (APPEND include_dir_list ${open_reader_root_dir}/source/sdi/tools/  )
list (APPEND include_dir_list ${open_reader_root_dir}/source/dyna2rad)
list (APPEND include_dir_list ${open_reader_root_dir}/../extlib/boost/boost_1_70_0/  )
list (APPEND include_dir_list ${open_reader_root_dir}/../extlib/zlib/win64_mt/include/ )
list (APPEND include_dir_list ${open_reader_root_dir}/../extlib/exprtk )
