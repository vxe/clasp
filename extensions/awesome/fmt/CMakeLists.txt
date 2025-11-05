# CMakeLists.txt for fmt clbind bindings
cmake_minimum_required(VERSION 3.15)
project(fmt_bindings CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find clasp
find_package(Clasp REQUIRED)

# Include directories
include_directories(
  ${CLASP_INCLUDE_DIRS}
  /tmp/clbind-gen/fmt-install/include
)

# Source files
set(SOURCES
  src/fmt_bindings.cc
)

# Build shared library
add_library(fmt_bindings SHARED ${SOURCES})

target_link_libraries(fmt_bindings
  ${CLASP_LIBRARIES}
)

# Installation
install(TARGETS fmt_bindings
  LIBRARY DESTINATION lib/clasp/extensions
)

install(DIRECTORY lisp/
  DESTINATION lib/clasp/extensions/fmt
  FILES_MATCHING PATTERN "*.lisp"
)
