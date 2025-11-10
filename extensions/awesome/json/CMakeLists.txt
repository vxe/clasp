# CMakeLists.txt for json clbind bindings
cmake_minimum_required(VERSION 3.15)
project(json_bindings CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find clasp
find_package(Clasp REQUIRED)

# Include directories
include_directories(
  ${CLASP_INCLUDE_DIRS}
  /tmp/clbind-gen/json-install/include
)

# Source files
set(SOURCES
  src/json_bindings.cc
)

# Build shared library
add_library(json_bindings SHARED ${SOURCES})

target_link_libraries(json_bindings
  ${CLASP_LIBRARIES}
)

# Installation
install(TARGETS json_bindings
  LIBRARY DESTINATION lib/clasp/extensions
)

install(DIRECTORY lisp/
  DESTINATION lib/clasp/extensions/json
  FILES_MATCHING PATTERN "*.lisp"
)
