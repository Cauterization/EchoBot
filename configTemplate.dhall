let Verbosity  = < Debug | Info    | Warning | Error >
let Mode       = < None  | Display | Write   | Both >
let LoggerConf = { cVerbosity : Verbosity, cMode : Mode}
let FrontEnd   = < Vkontakte | Telegram | Console >
let VKConf     = { token : Text
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
, cHelpMessage    = "_"
, cRepeatMessage  = "_"
, cFrontEnd = FrontEnd.Telegram
, cPollingTime = Some +45
, cVKConfig = Some
    { cToken = ""
    }
, cTGConfig = Some
   { cToken = ""
   }
}
