module App

open Elmish
open Elmish.React

open Fable.Core.JsInterop

importSideEffects "./index.css"

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkSimple 
    CoreSkillList.init 
    CoreSkillList.update 
    CoreSkillList.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run