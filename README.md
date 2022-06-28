# Large-scale Energy Anomaly Detection (LEAD) Dataset

Modern buildings are densely equipped with smart energy meters, which periodically generate a massive amount of time-series data yielding few million data points every day. This data can be leveraged to discover the underlying loads, infer their energy consumption patterns, inter-dependencies on environmental factors, and the building's operational properties.  Furthermore, it allows us to simultaneously identify anomalies present in the electricity consumption profiles, which is a big step towards saving energy and achieving global sustainability.  However, to date, the lack of large-scale annotated energy consumption datasets hinders the ongoing research in anomaly detection.  We contribute to this effort by releasing a well-annotated version of a publicly available [ASHRAE Great Energy Predictor III](https://www.kaggle.com/c/ashrae-energy-prediction/) data set containing 1,413 smart meter time series spanning over one year. In addition, we benchmark the performance of eight state-of-the-art anomaly detection methods on our dataset and compare their performance.

## Kaggle Community Prediction Competition
A subset of LEAD1.0 dataset is being used in the [Large-scale Energy Anomaly Detection (LEAD) competition](https://www.kaggle.com/competitions/energy-anomaly-detection/overview). In this competition, we provide a large training dataset consisting of year-long hourly electricity meter readings with anomaly annotations from 200 buildings. We challenge the participants to develop accurate anomaly detection models using this training dataset and then predict whether each each meter reading in the test dataset (from another 206 buildings) is anomalous (1) or normal (0).

### Competition Timeline

  - April 27, 2022: Competition start date.
  - July 24, 2022: Entry deadline. You must accept the competition rules before this date in order to compete.
  - July 24, 2022: Team merger deadline. This is the last day participants may join or merge teams.
  - July 31, 2022: Final submission deadline.
  
 
**Note:** The final and complete LEAD1.0 dataset with annotated meter readings from 1000+ buildings will be relseased after the completion of this competition.
