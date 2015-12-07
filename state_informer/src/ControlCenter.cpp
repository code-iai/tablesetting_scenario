#include <state_informer/ControlCenter.h>


namespace state_informer {
  ControlCenter::ControlCenter(ros::NodeHandle nhHandle) : m_nhHandle(nhHandle), m_itTransport(nhHandle) {
    m_srvControl = m_nhHandle.advertiseService<ControlCenter>("control", &ControlCenter::controlCallback, this);
    
    image_transport::TransportHints thHintsCompressed("compressed");
    m_isCompressed = std::make_shared<image_transport::Subscriber>(m_itTransport.subscribe("image_in", 1, boost::bind(&ControlCenter::imageCallback, this, _1), ros::VoidPtr(), thHintsCompressed));

    image_transport::TransportHints thHintsRaw("raw");
    m_isRaw = std::make_shared<image_transport::Subscriber>(m_itTransport.subscribe("image_in", 1, boost::bind(&ControlCenter::imageCallback, this, _1), ros::VoidPtr(), thHintsRaw));
    
    m_pubImageOut = m_nhHandle.advertise<sensor_msgs::Image>("image_out", 10);
  }
  
  ControlCenter::~ControlCenter() {
  }
  
  bool ControlCenter::controlCallback(designator_integration_msgs::DesignatorCommunication::Request &req,
				      designator_integration_msgs::DesignatorCommunication::Response &res) {
    std::shared_ptr<designator_integration::Designator> desigControl = std::make_shared<designator_integration::Designator>(req.request.designator);
    
    // TODO(winkler): Do whatever the control request says here.
    
    return true;
  }
  
  void ControlCenter::imageCallback(const sensor_msgs::Image::ConstPtr imgImage) {
    m_pubImageOut.publish(imgImage);
  }
}
