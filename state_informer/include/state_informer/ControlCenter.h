#ifndef __CONTROLCENTER_H__
#define __CONTROLCENTER_H__


// System
#include <memory>

// ROS
#include <ros/ros.h>

// Visualization
#include <visualization_msgs/Marker.h>
#include <visualization_msgs/MarkerArray.h>

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
    ros::Publisher m_pubImageOut;
    
    ros::Publisher m_pubMarkers;
    
    ros::ServiceServer m_srvControl;
    
    std::map<std::string, visualization_msgs::Marker> m_mapMarkers;
    
    bool m_bShouldRun;
    
  protected:
  public:
    ControlCenter(ros::NodeHandle nhHandle);
    ~ControlCenter();
    
    void shutdown();
    bool ok();
    
    bool controlCallback(designator_integration_msgs::DesignatorCommunication::Request &req,
			 designator_integration_msgs::DesignatorCommunication::Response &res);
    void imageCallback(const sensor_msgs::Image::ConstPtr imgImage);
    
    void addBox(std::string strID, geometry_msgs::Pose psPose, float fWidth, float fHeight, float fDepth, float fR, float fG, float fB, float fA);
    void addMesh(std::string strID, geometry_msgs::Pose psPose, std::string strPath);
    
    void addMarker(std::string strID, visualization_msgs::Marker mkrAdd);
    
    void displayAllMarkers();
    void removeAllMarkers();
  };
}


#endif /* __CONTROLCENTER__ */
