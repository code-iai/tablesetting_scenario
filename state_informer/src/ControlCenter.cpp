#include <state_informer/ControlCenter.h>


namespace state_informer {
  ControlCenter::ControlCenter(ros::NodeHandle nhHandle) : m_nhHandle(nhHandle), m_itTransport(nhHandle), m_bShouldRun(true) {
    m_srvControl = m_nhHandle.advertiseService<ControlCenter>("control", &ControlCenter::controlCallback, this);
    
    image_transport::TransportHints thHintsCompressed("compressed");
    m_isCompressed = std::make_shared<image_transport::Subscriber>(m_itTransport.subscribe("image_in", 1, boost::bind(&ControlCenter::imageCallback, this, _1), ros::VoidPtr(), thHintsCompressed));

    image_transport::TransportHints thHintsRaw("raw");
    m_isRaw = std::make_shared<image_transport::Subscriber>(m_itTransport.subscribe("image_in", 1, boost::bind(&ControlCenter::imageCallback, this, _1), ros::VoidPtr(), thHintsRaw));
    
    m_pubImageOut = m_nhHandle.advertise<sensor_msgs::Image>("image_out", 10);
    
    m_pubMarkers = m_nhHandle.advertise<visualization_msgs::MarkerArray>("markers", 10);
  }
  
  ControlCenter::~ControlCenter() {
    this->removeAllMarkers();
  }
  
  void ControlCenter::shutdown() {
    m_bShouldRun = false;
  }
  
  bool ControlCenter::ok() {
    return (m_bShouldRun && ros::ok());
  }
  
  bool ControlCenter::controlCallback(designator_integration_msgs::DesignatorCommunication::Request &req,
				      designator_integration_msgs::DesignatorCommunication::Response &res) {
    std::shared_ptr<designator_integration::Designator> desigControl = std::make_shared<designator_integration::Designator>(req.request.designator);
    
    std::string strCommand = desigControl->stringValue("command");
    if(strCommand == "add") {
      std::string strWhat = desigControl->stringValue("what");
      std::string strID = desigControl->stringValue("id");
      geometry_msgs::Pose psPose = desigControl->poseValue("where");
      
      if(strWhat == "box") {
	designator_integration::KeyValuePair* kvpDimensions = desigControl->childForKey("dimensions");
	
	if(kvpDimensions) {
	  float fWidth = kvpDimensions->floatValue("width");
	  float fHeight = kvpDimensions->floatValue("height");
	  float fDepth = kvpDimensions->floatValue("depth");
	  
	  float fR = 1.0;
	  float fG = 0.0;
	  float fB = 0.0;
	  float fA = 1.0;
	  
	  designator_integration::KeyValuePair* kvpColors = desigControl->childForKey("colors");
	  
	  if(kvpColors) {
	    fR = kvpColors->floatValue("r");
	    fG = kvpColors->floatValue("g");
	    fB = kvpColors->floatValue("b");
	    fA = kvpColors->floatValue("a");
	  }
	  
	  this->addBox(strID, psPose, fWidth, fHeight, fDepth, fR, fG, fB, fA);
	}
      } else if(strWhat == "mesh") {
	designator_integration::KeyValuePair* kvpDetails = desigControl->childForKey("details");
	
	if(kvpDetails) {
	  std::string strPath = kvpDetails->stringValue("path");
	  
	  this->addMesh(strID, psPose, strPath);
	}
      }
    } else if(strCommand == "remove") {
      std::string strID = desigControl->stringValue("id");
      
      ROS_INFO("Removing id '%s'", strID.c_str());
      
      removeAllMarkers();
      m_mapMarkers.erase(strID);
      displayAllMarkers();
    }
    
    return true;
  }
  
  void ControlCenter::imageCallback(const sensor_msgs::Image::ConstPtr imgImage) {
    m_pubImageOut.publish(imgImage);
  }
  
  void ControlCenter::addBox(std::string strID, geometry_msgs::Pose psPose, float fWidth, float fHeight, float fDepth, float fR, float fG, float fB, float fA) {
    ROS_INFO("Adding box '%s' at x=%f, y=%f, z=%f", strID.c_str(), psPose.position.x, psPose.position.y, psPose.position.z);
    ROS_INFO(" - Dimensions: w=%f, h=%f, d=%f", fWidth, fHeight, fDepth);
    
    visualization_msgs::Marker mkrBox;
    mkrBox.type = visualization_msgs::Marker::CUBE;
    mkrBox.pose = psPose;
    mkrBox.header.frame_id = "map";
    
    mkrBox.scale.x = fWidth;
    mkrBox.scale.y = fDepth;
    mkrBox.scale.z = fHeight;
    
    mkrBox.color.r = fR;
    mkrBox.color.g = fG;
    mkrBox.color.b = fB;
    mkrBox.color.a = fA;
    
    this->addMarker(strID, mkrBox);
  }
  
  void ControlCenter::addMesh(std::string strID, geometry_msgs::Pose psPose, std::string strPath) {
    ROS_INFO("Adding mesh '%s' at x=%f, y=%f, z=%f", strID.c_str(), psPose.position.x, psPose.position.y, psPose.position.z);
    
    visualization_msgs::Marker mkrMesh;
    mkrMesh.type = visualization_msgs::Marker::MESH_RESOURCE;
    mkrMesh.pose = psPose;
    mkrMesh.header.frame_id = "map";
    
    mkrMesh.scale.x = 1.0;
    mkrMesh.scale.y = 1.0;
    mkrMesh.scale.z = 1.0;
    
    mkrMesh.color.r = 1.0;
    mkrMesh.color.g = 1.0;
    mkrMesh.color.b = 1.0;
    mkrMesh.color.a = 1.0;
    
    mkrMesh.mesh_resource = "package://tablesetting_scenario_models/models/" + strPath;
    mkrMesh.mesh_use_embedded_materials = true;
    
    this->addMarker(strID, mkrMesh);
  }
  
  void ControlCenter::addMarker(std::string strID, visualization_msgs::Marker mkrAdd) {
    mkrAdd.action = visualization_msgs::Marker::ADD;
    
    this->removeAllMarkers();
    m_mapMarkers[strID] = mkrAdd;
    this->displayAllMarkers();
  }
  
  void ControlCenter::displayAllMarkers() {
    visualization_msgs::MarkerArray maMarkers;
    
    int nIndex = 0;
    for(std::pair<std::string, visualization_msgs::Marker> prMarker : m_mapMarkers) {
      visualization_msgs::Marker mkrAdd = prMarker.second;
      mkrAdd.id = nIndex;
      
      maMarkers.markers.push_back(mkrAdd);
      
      nIndex++;
    }
    
    m_pubMarkers.publish(maMarkers);
  }
  
  void ControlCenter::removeAllMarkers() {
    visualization_msgs::MarkerArray maMarkers;
    
    for(std::pair<std::string, visualization_msgs::Marker> prMarker : m_mapMarkers) {
      visualization_msgs::Marker mkrRemove = prMarker.second;
      mkrRemove.action = visualization_msgs::Marker::DELETE;
      
      maMarkers.markers.push_back(mkrRemove);
    }
    
    m_pubMarkers.publish(maMarkers);
  }
}
