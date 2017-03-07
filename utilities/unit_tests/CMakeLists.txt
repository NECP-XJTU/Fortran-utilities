################################################################################
#
# CMake File for Utilities unit tests
#
# Description: Adds the subdirectories for the unit tests
#
# Author: Zhouyu Liu
#   Date: 03/18/2015
#
################################################################################
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/../)
# List the names of all the tests (same as subdirectory)
SET(test_names
    test_string
    test_file_manager
    test_file_hdf5
    test_error_warning
    test_math
    test_linkedList
    test_quadrature_set
    test_random_number
    test_hash_table
    test_parallel_env
    test_petsc_matrix
    test_petsc_sweep
    test_timer
   )

FOREACH(test ${test_names})
  FILE(GLOB srcFiles ${CMAKE_CURRENT_SOURCE_DIR}/${test}/*.f90)
  ADD_EXECUTABLE(${test} ${srcFiles})
  TARGET_LINK_LIBRARIES(${test} utilities)
  ADD_TEST(${test} ${test})
ENDFOREACH()


