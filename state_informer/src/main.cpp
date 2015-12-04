// System
#include <iostream>
#include <cstdlib>
#include <memory>

// ROS
#include <ros/ros.h>

// State Informer
#include <state_informer/ControlCenter.h>


int main(int argc, char** argv) {
  ros::init(argc, argv, "state_informer");
  ros::NodeHandle nhHandle("~");
  
  state_informer::ControlCenter ccControl(nhHandle);
  
  while(ros::ok()) {
    ros::spinOnce();
    
    ros::Duration(0.01).sleep();
  }
  
  return EXIT_SUCCESS;
}
