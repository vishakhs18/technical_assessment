# Shopping Basket
This repository contains a Scala application called "Shopping Basket"

# Prerequisites (These needs to be installed in your machine before running the program)
- Java 8:
  Java can be installed from the following link:
  https://www.oracle.com/java/technologies/javase-jdk8-downloads.html
- sbt:
  The following link can be used to install sbt:
  https://www.scala-sbt.org/download.html

# How to run the program?
- Clone this repository to your machine
- From the root of this repository, execute the following command:
  scalac src/main/scala/PriceBasket.scala
  
<img width="279" alt="Screenshot 2023-10-20 at 4 17 17 PM" src="https://github.com/vishakhs18/technical_assessment/assets/96525131/4529d799-e3a1-45a8-ac60-0dd9dfc46c02">

  This command will compile the program
- Next, execute the following command:
  scala PriceBasket item1 item2 item3 ... itemn
  
<img width="702" alt="Screenshot 2023-10-20 at 4 18 42 PM" src="https://github.com/vishakhs18/technical_assessment/assets/96525131/1c128cd2-1277-43d9-9ff4-c52e6bb2e95d">

  This command will execute the compile the executed binary and provide the required output to the console
  
# Unit-tests
This project uses sbt to implement unit-tests.
To execute unit-tests:
- From the root of this repository, execute the following command:
  sbt test

  <img width="899" alt="Screenshot 2023-10-20 at 4 23 21 PM" src="https://github.com/vishakhs18/technical_assessment/assets/96525131/bd46312f-ea53-4383-be17-7f4b934660b0">
