
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.10 FATAL_ERROR)

if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_CURRENT_BINARY_DIR)
    MESSAGE(FATAL ": In-source builds are not allowed!")
endif()

#Include the cmake modules and self-defined modules
SET(CMAKE_MODULE_PATH  ${CMAKE_ROOT}/Modules)
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/)
#message("CMAKE_BINARY_DIR:"  ${CMAKE_BINARY_DIR})

# Set the project name
SET(PROJECT_NAME utilites)
SET(${PROJECT_NAME}_ROOT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "")
SET(${PROJECT_NAME}_ROOT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE INTERNAL "")

# Set Version numbering
execute_process(
  COMMAND git log -1 --format=%H
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
#message(${GIT_COMMIT_HASH})
SET(${PROJECT_NAME}_VERSION_MAJOR 0)
SET(${PROJECT_NAME}_VERSION_MINOR 0)
SET(${PROJECT_NAME}_VERSION_PATCH 0)
SET(${PROJECT_NAME}_VERSION
     ${${PROJECT_NAME}_VERSION_MAJOR}.${${PROJECT_NAME}_VERSION_MINOR}.${${PROJECT_NAME}_VERSION_PATCH}
      CACHE INTERNAL "")

SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} GIT_COMMIT_HASH)
SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} ${PROJECT_NAME}_VERSION)

if(DEFINED CMAKE_Fortran_COMPILER AND CMAKE_Fortran_COMPILER MATCHES "^$")
    set(CMAKE_Fortran_COMPILER CMAKE_Fortran_COMPILER-NOTFOUND)
endif()
if(DEFINED CMAKE_C_COMPILER AND CMAKE_C_COMPILER MATCHES "^$")
    set(CMAKE_C_COMPILER CMAKE_C_COMPILER-NOTFOUND)
endif()
if(DEFINED CMAKE_CXX_COMPILER AND CMAKE_CXX_COMPILER MATCHES "^$")
    set(CMAKE_CXX_COMPILER CMAKE_CXX_COMPILER-NOTFOUND)
endif()

PROJECT(${PROJECT_NAME})
ENABLE_LANGUAGE(Fortran)
ENABLE_LANGUAGE(C OPTIONAL)
ENABLE_LANGUAGE(CXX OPTIONAL)
ENABLE_TESTING()
INCLUDE(CTest)

################################################################################
####################### configure some options #################################
################################################################################
#-------------------------------------------------------------------------------
# Set up DBL_REAL option
IF(NOT DEFINED DBL_REAL)
  SET(DBL_REAL ON CACHE BOOL "")
ENDIF()

IF(${DBL_REAL})
  SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} DBL_REAL)
ENDIF()

#-------------------------------------------------------------------------------
# Set up WITH_HDF5 option
IF(NOT DEFINED WITH_HDF5)
  SET(WITH_HDF5 OFF CACHE BOOL "" FORCE)
ENDIF()

IF(${WITH_HDF5})
  SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} WITH_HDF5)
  FIND_PACKAGE(HDF5 COMPONENTS Fortran)
  message("====== HDF5 MESSAGE ======  HDF5_INCLUDE_DIRS:  ${HDF5_INCLUDE_DIRS}")
  message("====== HDF5 MESSAGE ======  HDF5_LIBRARY_DIRS:  ${HDF5_LIBRARY_DIRS}")
  IF(MSVC)
    INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIRS}/shared)
    LINK_LIBRARIES(${HDF5_INCLUDE_DIRS}/../lib/hdf5.lib)
    LINK_LIBRARIES(${HDF5_INCLUDE_DIRS}/../lib/hdf5_fortran.lib)
  ELSE()
    IF(NOT ${HDF5_FOUND})
      MESSAGE(FATAL_ERROR "HDF5 package not found")
    ELSE()
      INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIRS})
      LINK_LIBRARIES(${HDF5_Fortran_LIBRARIES})
    ENDIF()
  ENDIF()
ENDIF()

#-------------------------------------------------------------------------------
# Set up WITH_OMP option
IF(NOT DEFINED WITH_OMP)
  SET(WITH_OMP OFF CACHE BOOL "" FORCE)
ENDIF()

IF(${WITH_OMP})
  FIND_PACKAGE(OpenMP)
  IF(NOT ${OPENMP_FOUND})
    MESSAGE(FATAL_ERROR "OpenMP package not found")
  ELSE()
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} WITH_OMP)
  ENDIF()
ENDIF()

#-------------------------------------------------------------------------------
# Set up WITH_MPI option
IF(NOT DEFINED WITH_MPI)
  SET(WITH_MPI OFF CACHE BOOL "" FORCE)
ENDIF()

IF(${WITH_MPI})
  FIND_PACKAGE(MPI)
  IF(NOT ${MPI_FOUND})
      MESSAGE(FATAL_ERROR "MPI package not found")
  ELSE()
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} WITH_MPI)
    INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
    IF(MSVC)
      FIND_LIBRARY(found_lib NAMES fmpich2.lib PATHS ${MPI_INCLUDE_PATH}/../lib)
      IF({${found_lib}-NOTFOUND)
        MESSAGE(FATAL_ERROR "mpi Fortran library not found, disabled mpi compiling.")
      ELSE()
        LINK_LIBRARIES(${found_lib})
      ENDIF()
    ELSE()
      #message("${MPI_CXX_LIBRARIES}")
      #message("${MPI_C_LIBRARIES}")
      #message("${MPI_Fortran_LIBRARIES}")

      STRING(FIND ${CMAKE_Fortran_COMPILER} "mpi" POS_STR)

      IF(${POS_STR} LESS 0)
        STRING(FIND ${CMAKE_Fortran_COMPILER} "tau" POS_STR)

        IF(${POS_STR} LESS 0)
            MESSAGE(FATAL_ERROR "Fortran compiler is <" ${CMAKE_Fortran_COMPILER}
                            ">, MPI fortran compiler should be specified.")
        ENDIF()

      ENDIF()

      STRING(FIND ${CMAKE_C_COMPILER} "mpi" POS_STR)
      IF(${POS_STR} LESS 0)
        STRING(FIND ${CMAKE_Fortran_COMPILER} "tau" POS_STR)

        IF(${POS_STR} LESS 0)
          MESSAGE(FATAL_ERROR "C compiler is <" ${CMAKE_C_COMPILER}
                            ">, MPI C compiler should be specified.")
        ENDIF()

      ENDIF()

      FIND_PROGRAM(MPI_COMPILER ${CMAKE_Fortran_COMPILER})
      IF({${MPI_COMPILER}-NOTFOUND})
        MESSAGE(FATAL_ERROR "mpi fortran compiler not found, disabled mpi compiling.")
      ENDIF()

      FIND_PROGRAM(MPI_COMPILER ${CMAKE_C_COMPILER})
      IF({${MPI_COMPILER}-NOTFOUND})
        MESSAGE(FATAL_ERROR "mpi C compiler not found, disabled mpi compiling.")
      ENDIF()

    ENDIF()
  ENDIF()
ENDIF(${WITH_MPI})

#-------------------------------------------------------------------------------
# Set up WITH_PETSC option
IF(NOT DEFINED WITH_PETSC)
  SET(WITH_PETSC OFF CACHE BOOL "" FORCE)
ENDIF()

IF(${WITH_PETSC})
  FIND_PACKAGE(PETSc)

  IF(NOT ${PETSC_FOUND})
    MESSAGE(FATAL_ERROR "PETSC package not found")
  ELSE()
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} WITH_PETSC)
    INCLUDE_DIRECTORIES(${PETSC_INCLUDES})

    LINK_LIBRARIES(${PETSC_LIBRARIES})

    message("====== PETSC MESSAGE ======  PETSC_LIBRARY_DIR:  ${PETSC_LIBRARY_DIR}")
    message("====== PETSC MESSAGE ======  PETSC_INCLUDE_DIR:  ${PETSC_INCLUDE_DIR}")

  ENDIF()
ENDIF()


################################################################################
######################### build source #########################################
################################################################################

INCLUDE(${PROJECT_NAME}_Configurations)
IF(NOT DEFINED CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE ${${PROJECT_NAME}_DEFAULT_BUILD_TYPE} CACHE STRING "" FORCE)
ENDIF()

ADD_SUBDIRECTORY(utilities)

################################################################################
#########################  Edit status parameters     ##########################
################################################################################
MESSAGE(STATUS "====== NECP_X MESSAGE ======   DBL_REAL:       ${DBL_REAL}")
MESSAGE(STATUS "====== NECP_X MESSAGE ======   WITH_HDF5:      ${WITH_HDF5}")
MESSAGE(STATUS "====== NECP_X MESSAGE ======   WITH_MPI:       ${WITH_MPI}")
MESSAGE(STATUS "====== NECP_X MESSAGE ======   WITH_OMP:       ${WITH_OMP}")
MESSAGE(STATUS "====== NECP_X MESSAGE ======   WITH_PETSC:     ${WITH_PETSC}")
MESSAGE(STATUS "====== NECP_X MESSAGE ======   WITH_MEMCHECK:  ${WITH_MEMCHECK}")
MESSAGE(STATUS "====== NECP_X MESSAGE ======   WITH_COVERAGE:  ${WITH_COVERAGE}")
MESSAGE(STATUS "====== NECP_X MESSAGE ======   WITH_VALIDATION:${WITH_VALIDATION}")
MESSAGE(STATUS "====== NECP_X MESSAGE ======   DOXYGEN_FOUND:  ${DOXYGEN_FOUND}")


