# CMakeLists.txt for spdlog clbind bindings
cmake_minimum_required(VERSION 3.15)
project(spdlog_bindings CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find clasp
find_package(Clasp REQUIRED)

# Include directories
include_directories(
  ${CLASP_INCLUDE_DIRS}
  /tmp/clbind-gen/spdlog-install/include
)

# Source files
set(SOURCES
  src/spdlog_bindings.cc
)

# Build shared library
add_library(spdlog_bindings SHARED ${SOURCES})

target_link_libraries(spdlog_bindings
  ${CLASP_LIBRARIES}
)

# Installation
install(TARGETS spdlog_bindings
  LIBRARY DESTINATION lib/clasp/extensions
)

install(DIRECTORY lisp/
  DESTINATION lib/clasp/extensions/spdlog
  FILES_MATCHING PATTERN "*.lisp"
)
