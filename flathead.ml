open Type;;
let story = Story.load_story "output.z8" ;;
let screen = Screen.make (Character_height 50) (Character_width 80) ;;
let interpreter = Interpreter.make story screen ;;
let debugger = Debugger.make interpreter ;;
Debugger.run debugger ;;
