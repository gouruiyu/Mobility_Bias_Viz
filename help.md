## Unbiased Mobility Project
**Hosted by:** The Data Science for Social Good (DSSG) program at the University of British Columbia

**Sponsor:** Cedar Academy Society - VanCom Project

Public sector and academic communities have been using mobility and traffic data as a proxy measurement for a variety of social topics, from GDP prediction and economic development to greenhouse gas emissions and environmental impact.

One method to measure mobility and acquire traffic data is through the analysis of pictures and footage from traffic cameras installed at fixed locations (in urban and rural areas). 


## Data Pipeline
Traffic camera snapshots are taken at each camera locations. Information of vehicle counts of different categories are extracted using computer vision method. 

Specifically, this pipeline uses the object detection system You Only Look Once, namely YOLO. It is one of the state-of-the-art detection model, known for being both accurate and fast. It detects different vehicles in the image with bounding boxes at the locations, then translate them into counts of objects of interest.


### Undercounting Correction

Traffic camera images, especially at busy intersections, suffer from high occlusion between vehicles and low resolution due to the hardware constraint. Current pipeline often fails to detect the semantic features when they interweave, thus is missing count when there is a long line-up. On the other hand, it is actually able to detect the vehicles correctly at far distance when traffic is free-of-flow since the cars are more apart. This leads to two consequences:

- **saturation** of counts, the maximum vehicle count at the example intersection is capped at around 15 where one can easily find snapshot with 20+ cars under visual check. 

- **random noise**: the data shows fluctuations not due to the change in actual number of vehicles, but due to the failure of object detection, as the line-up resulting from red light almost always appear out-of-sync from the snapshot frequency. It introduces inconsistent biases when the images used for statistical inference are sampled with a lower frequency.

Our `Correct for Undercount` feature uses a model learned from manually labeled traffic images (including number fo vehicle count and Queue/Free-of-Flow category) to add correction for this detection bias. It is expected to generate more reliable traffic data for cameras.

Notice that the current used one is a preliminary version only effective to car counts. It is to be improved and generalized to other vehicle types in the future.


### Rush Hours Comparison


## UI Features 

### Bicyle Routes
An overlay of bicycle routes is included in this application along with the overlay panels. To view the bicycle routes, ensure that the label "Bike Routes " is checked. The bicycle routes are coloured in 7 types. This is shown in the legend listed in the top left corner.


### Neighbourhood Filters 
A selection of neighbourhoods is available for selection. The option panel is on the left hand side and it is titled "Select a Neighbourhood". The default selected is SURREY, where it displays all the neighbourhoods within the city of Surrey. When a neighbourhood is selected, the map will shift its view to the selected neighbourhood.

### Business Overlays
There are 6 business overlays included in this application. All of them are clustered into groups. Those are: Stores, Foods and Restaurants, Liquor Stores, Health and Medicine, Businesses and Finance, and Services. As the map is zoomed in, each individual business is shown in more detail.


## Acknowledgement
