PMSM Rotor Temperature Prediction

The rotor temperature of any motor is difficult to measure as it is a rotating part. Placing any sensors to measure this difficult to measure temperature would result in increase in costs and also increase the weight of the motor. In the era of electric vehicles, electric drives have become common in automotives and a lot of research is ongoing to reduce the weight of the motors in order to increase the efficiency of electric cars.

Measurement of quantities like temperature, torque of the rotor is important in order to design control systems to effectively control the motor. Many statistical based approaches have been studied in estimating the values of temperatures and torque, but these approaches require domain knowledge and often are different for different motors and different operating conditions. There is no universal approach towards estimating these values.

The goal of the project is to efficiently predict the rotor temperature of a permanent magnet synchronous motor (PMSM), as it is usually difficult to measure the rotor temperature. This kind of prediction helps to reduce the amount of equipment that is to be mounted on to the motor to measure the temperature.

1. Problem Statement


Predicting rotor temperature of the rotor of a Permanent Magnet Synchronous Motor(PMSM) given other sensor measurements during operation.


2. Data Description


This project uses the electric-motor-temperature dataset to predict the rotor temperature of a motor.

A brief description of data attributes:

Ambient - Ambient temperature
Coolant - Coolant temperature
u_d - Voltage d-component (Active component)
u_q - Voltage q-component (Reactive component)
Motor_speed - Speed of the motor
Torque - Torque induced by current
i_d - Current d-component (Active component)
i_q - Current q-component (Reactive component)
pm - Permanent Magnet Surface temperature
Stator_yoke - Stator yoke temperature
Stator_tooth - Stator tooth temperature
Stator_winding - Stator Stator_winding temperature
Profile_id - Measurement Session id

Measuring the temperature of motor is complex task in real time scenario. The cost of sensor is high and also lifetime of those are less.But also testing every motor’s temperature by placing sensors inside the Stator is not possible.
By using Machine learning algorithms , the task is made easier. The accuracy by my model is 94.35% and also, I deployed the model using Rshiny where prediction without dataset is also possible by imputing values in their respective fields


I prepared a shinyapp of my project.
I made predicitions for the dataset permanent magnet synchronus motor dataset to get the best accuracy model.
Prepared models with required algorithms both in R and Python.
I'm not able to add csv file for your reference because of the file size. The file size is 120 mb. Github is not allowing the files contains with higher size.
