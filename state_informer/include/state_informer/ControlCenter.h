#ifndef __CONTROLCENTER_H__
#define __CONTROLCENTER_H__


// System
#include <memory>

// ROS
#include <ros/ros.h>

// Image Transport
#include <image_transport/image_transport.h>
#include <image_transport/subscriber_filter.h>

// Designators
#include <designators/Designator.h>
#include <designator_integration_msgs/DesignatorCommunication.h>


namespace state_informer {
  class ControlCenter {
  private:
    ros::NodeHandle m_nhHandle;
    
    image_transport::ImageTransport m_itTransport;
    std::shared_ptr<image_transport::Subscriber> m_isCompressed;
    std::shared_ptr<image_transport::Subscriber> m_isRaw;
    
    ros::ServiceServer m_srvControl;
    
  protected:
  public:
    ControlCenter(ros::NodeHandle nhHandle);
    ~ControlCenter();
    
    bool controlCallback(designator_integration_msgs::DesignatorCommunication::Request &req,
			 designator_integration_msgs::DesignatorCommunication::Response &res);
    void imageCallback(const sensor_msgs::Image::ConstPtr imgImage);
  };
}


#endif /* __CONTROLCENTER__ */
