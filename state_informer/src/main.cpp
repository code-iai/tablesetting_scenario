// System
#include <iostream>
#include <cstdlib>
#include <memory>
#include <signal.h>

// ROS
#include <ros/ros.h>

// State Informer
#include <state_informer/ControlCenter.h>


std::shared_ptr<state_informer::ControlCenter> g_ccControlCenter;


void catchHandler(int nSignum) {
  switch(nSignum) {
  case SIGTERM:
  case SIGINT: {
    if(g_ccControlCenter) {
      g_ccControlCenter->shutdown();
    }
  } break;
    
  default:
    break;
  }
}


int main(int argc, char** argv) {
  ros::init(argc, argv, "state_informer");
  ros::NodeHandle nhHandle("~");
  
  struct sigaction action;
  memset(&action, 0, sizeof(struct sigaction));
  action.sa_handler = catchHandler;
  sigaction(SIGTERM, &action, NULL);
  sigaction(SIGINT, &action, NULL);
  
  g_ccControlCenter = std::make_shared<state_informer::ControlCenter>(nhHandle);
  
  while(g_ccControlCenter->ok()) {
    ros::spinOnce();
    ros::Duration(0.01).sleep();
  }
  
  g_ccControlCenter.reset();
  
  std::cout << "\rExiting gracefully." << std::endl;
  
  return EXIT_SUCCESS;
}
