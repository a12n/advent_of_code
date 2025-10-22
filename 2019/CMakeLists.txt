cmake_minimum_required(VERSION 3.14)

project(aoc2019 LANGUAGES CXX)

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_library(aoc
  ansi.cpp
  grid.cpp
  intcode.cpp
  )

file(GLOB SRCS [0-2][0-9].cpp)
foreach(src ${SRCS})
  get_filename_component(day ${src} NAME_WE)
  if(${day} EQUAL 25)
    add_executable(${day} ${src})
    target_link_libraries(${day} PRIVATE aoc)
  else()
    foreach(part RANGE 1 2)
      add_executable(${day}-${part} ${src})
      target_compile_definitions(${day}-${part} PRIVATE PART=${part})
      target_link_libraries(${day}-${part} PRIVATE aoc)
    endforeach()
  endif()
endforeach()
