let Verbosity  = < Debug | Info    | Warning | Error >
let Mode       = < None  | Display | Write   | Both >
let LoggerConf = { cVerbosity : Verbosity, cMode : Mode}
let FrontEnd   = < Vkontakte | Telegram | Console >
let VKConf     = { token : Text
                 , groupID : Integer
                 }
let TGConf     = { token : Text
                 }
let Conf       = { loggerConf : LoggerConf
                 , cDefaultRepeats : Integer
                 , cHelpMessage : Text
                 , cRepeatMessage : Text
                 , cFrontEnd : FrontEnd
                 , cPollingTime : Optional Integer
                 , cVKConfig : Optional VKConf
                 , cTGConfig : Optional TGConf
                 } 
in
{ cLogger = 
  { cVerbosity = Verbosity.Debug
  , cMode      = Mode.Both
  , cFilePath  = "log.txt"
  }
, cDefaultRepeats = +5
, cHelpMessage    = "This is a simple bot that sends your messages back. The following commands are also available to you:\n/help - see this message;\n/repeat - enter a new number of repetitions"
, cRepeatMessage  = "Enter a new number of repetitions."
, cFrontEnd = FrontEnd.Telegram
, cPollingTime = Some +45
, cVKConfig = Some
    { cToken = ""
    , cGroupID = +0
    }
, cTGConfig = Some
   { cToken = ""
   }
}
