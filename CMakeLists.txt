cmake_minimum_required(VERSION 3.13)
project(temp-opt LANGUAGES C CXX)


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake: ${LLVM_DIR}")

# Find Clang
find_package(Clang REQUIRED CONFIG)
message(STATUS "Found Clang ${Clang_PACKAGE_VERSION}")
message(STATUS "Using ClangConfig.cmake: ${Clang_DIR}")

add_executable(temp-opt TempOpt.cpp)

# Include LLVM/Clang headers
target_include_directories(temp-opt PRIVATE ${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
target_compile_definitions(temp-opt PRIVATE ${LLVM_DEFINITIONS})

target_compile_options(temp-opt PRIVATE -fno-rtti)

# Link required Clang/LLVM libraries
target_link_libraries(temp-opt
  PRIVATE
    clangTooling
    clangFrontend
    clangAST
    clangBasic
)

# Make lit test usable via "lit test" from the build directory
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/test)
configure_file(
  ${CMAKE_SOURCE_DIR}/test/lit.cfg.py
  ${CMAKE_BINARY_DIR}/test/lit.cfg.py
  COPYONLY
)
