# Echo bot (no frameworks)
Test project for Metalamp. Description of requirements aviable [here](https://coda.io/@metalamp/education/3-14) and [here](https://coda.io/@metalamp/education/5-16).

## Getting started
All you need to run bot is write your tokens into configuration file (or just do nothing on this step if you want to work in console mode). After that just run main.hs file with --conf ./configTemplate.dhall command line argument.


## Important notes
VK API version - 5.81.

## Architecture
Bot relies heavily on MTL-pattern. At current moment it can work in console mode and with vkontakte and telegram messangers.
The bot is just an endless loop of receiving updates, processing them, and sending the result back to the user. The embodiment of the business logic of this loop can be found at Bot folder and it is represented mainly through 2 type classes - IsFrontEnd and FrontEndIO. Also here is App folder where you can find the code that is responsible for launching the bot and Console/Telegram/Vkontakte folders where you can find concrete FrontEnd class instantations and some auxiliary code to parse messangers updates correctly.
