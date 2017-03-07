################################################################################
#
# CMake File for NECP_X
#
# Description: CMake build script for NECP_X.
#
# Author: Liu Zhouyu
#   Date: 03/17/2015
#
################################################################################

# Project configuration types
SET(CONFIG_TYPES Debug Release)

# Set the new configurations
FOREACH(ctyp ${CONFIG_TYPES})
    LIST(FIND CMAKE_CONFIGURATION_TYPES ${ctyp} CONFIG_EXIST)
    IF(${CONFIG_EXIST} LESS 0)
        SET(CMAKE_CONFIGURATION_TYPES
            ${CMAKE_CONFIGURATION_TYPES}
            ${ctyp} CACHE STRING "" FORCE
           )
    ENDIF()
ENDFOREACH()

# Set the default build type
SET(${PROJECT_NAME}_DEFAULT_BUILD_TYPE Debug)

# Determine symbol for command line compiler option
IF(MSVC_IDE)
    SET(CSYM "/")
ELSE()
    SET(CSYM "-")
ENDIF()

IF( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR
   (MSVC_IDE AND "${CMAKE_Fortran_COMPILER}" STREQUAL "ifort"))

    IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        SET(s ":")
    ELSE()
        SET(s " ")
    ENDIF()

    SET(Fortran_FLAGS
        ${CSYM}nologo
        ${CSYM}fpp
        ${CSYM}stand${s}f03
        ${CSYM}threads
        ${CSYM}convert${s}little_endian
        ${CSYM}assume${s}byterecl
       )
    SET(Fortran_FLAGS_DEBUG
        ${CSYM}debug${s}full
        ${CSYM}debug-parameters${s}all
        ${CSYM}warn${s}declarations
        ${CSYM}warn${s}unused
        ${CSYM}warn${s}interfaces
        ${CSYM}check${s}pointer
        ${CSYM}check${s}bounds
        ${CSYM}check${s}uninit
        ${CSYM}check${s}format
        ${CSYM}check${s}arg_temp_created
        ${CSYM}traceback
       )
    SET(Fortran_FLAGS_RELEASE
        ${CSYM}O3
        ${CSYM}Os
       )
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    SET(Fortran_FLAGS
        ${CSYM}cpp
        ${CSYM}std=f2008
        ${CSYM}static-libgfortran
        ${CSYM}fall-intrinsics
        ${CSYM}fconvert=little-endian
        ${CSYM}ffree-line-length-none
       )
     # The following flags are to be added.
     # ${CSYM}ffpe-trap=invalid,overflow,underflow,zero,denormal
    SET(Fortran_FLAGS_DEBUG
        ${CSYM}O0
        ${CSYM}Wall
        ${CSYM}fcheck=bounds,do,mem,pointer
        ${CSYM}fbacktrace
        ${CSYM}g
       )
    IF(${WITH_COVERAGE})
      SET(Fortran_FLAGS_DEBUG ${Fortran_FLAGS_DEBUG} ${CSYM}fprofile-arcs)
      SET(Fortran_FLAGS_DEBUG ${Fortran_FLAGS_DEBUG} ${CSYM}ftest-coverage)
      SET(CMAKE_EXE_LINKER_FLAGS_DEBUG ${CMAKE_EXE_LINKER_FLAGS_DEBUG} ${CSYM}lgcov)
    ENDIF()
    SET(Fortran_FLAGS_RELEASE
        ${CSYM}Ofast
       )
ELSE()
    MESSAGE(ERROR " Fortran compiler: ${CMAKE_Fortran_COMPILER_ID} not supported!")
ENDIF()

IF(MSVC_IDE)
  SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES}
      SYS_WIN32)
ENDIF()

# Include developers' configuration files
file(GLOB config_files ${CMAKE_SOURCE_DIR}/cmake/developer_config/*.cmake)
foreach(config_file ${config_files})
  message(STATUS "include developers' config file \"${config_file}\"")
  include(${config_file})
endforeach()

# Include hdf5 files
file(GLOB config_files ${CMAKE_SOURCE_DIR}/cmake/developer_config/hdf5_config/*.cmake)
foreach(config_file ${config_files})
  message(STATUS "include developers' config file \"${config_file}\"")
  include(${config_file})
endforeach()

IF(${WITH_PETSC})
  #file(GLOB config_files ${CMAKE_SOURCE_DIR}/cmake/developer_config/petsc_config/*.cmake)
  #foreach(config_file ${config_files})
  #  message(STATUS "include developers' config file \"${config_file}\"")
  #  include(${config_file})
  #endforeach()
ENDIF()

# Add define symbols to Fortran_FLAGS
FOREACH(def ${${PACKAGE_NAME}_DEFINES})
    SET(Fortran_FLAGS ${Fortran_FLAGS} ${CSYM}D${def})
ENDFOREACH()

# Add OpenMP comilation flags
IF(${WITH_OMP})
  SET(Fortran_FLAGS ${Fortran_FLAGS} ${OpenMP_C_FLAGS})
ENDIF()

# Convert the lists to strings with spaces
STRING(REGEX REPLACE ";" " " Fortran_FLAGS_STRING         "${Fortran_FLAGS}")
STRING(REGEX REPLACE ";" " " Fortran_FLAGS_DEBUG_STRING   "${Fortran_FLAGS_DEBUG}")
STRING(REGEX REPLACE ";" " " Fortran_FLAGS_RELEASE_STRING "${Fortran_FLAGS_RELEASE}")
STRING(REGEX REPLACE ";" " " EXE_LINKER_FLAGS_DEBUG_STRING "${CMAKE_EXE_LINKER_FLAGS_DEBUG}")

# Set the all the flags
SET(CMAKE_Fortran_FLAGS ${Fortran_FLAGS_STRING} CACHE STRING "" FORCE)
SET(CMAKE_Fortran_FLAGS_DEBUG ${Fortran_FLAGS_DEBUG_STRING} CACHE STRING "" FORCE)
SET(CMAKE_Fortran_FLAGS_RELEASE ${Fortran_FLAGS_RELEASE_STRING} CACHE STRING "" FORCE )
SET(CMAKE_EXE_LINKER_FLAGS_DEBUG ${EXE_LINKER_FLAGS_DEBUG_STRING} CACHE STRING "" FORCE )

MESSAGE(STATUS "CMAKE_Fortran_FLAGS " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG " ${CMAKE_Fortran_FLAGS_DEBUG})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE " ${CMAKE_Fortran_FLAGS_RELEASE})
