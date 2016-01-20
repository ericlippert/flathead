let story = Story.load_story "TRINITY.DAT" in
let screen = Screen.make 50 80 in
let interpreter = Interpreter.make story screen in
let debugger = Debugger.make interpreter in
Debugger.run debugger
