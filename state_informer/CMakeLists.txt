cmake_minimum_required(VERSION 2.8.3)
project(state_informer)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

find_package(catkin REQUIRED COMPONENTS
  geometry_msgs
  roscpp
  std_msgs
  visualization_msgs
  sensor_msgs
  designator_integration_cpp
  designator_integration_msgs
  image_transport)

catkin_package(
  INCLUDE_DIRS include
  LIBRARIES state_informer
  CATKIN_DEPENDS geometry_msgs roscpp std_msgs visualization_msgs sensor_msgs designator_integration_cpp designator_integration_msgs image_transport)

include_directories(
  include
  ${catkin_INCLUDE_DIRS})

add_executable(informer
  src/main.cpp
  src/ControlCenter.cpp)

target_link_libraries(informer
  ${catkin_LIBRARIES})
