module Jt.Command.All(allCommands) where

import Jt(Command)
import Jt.Command.Show(showCommand)
import Jt.Command.Jobs(jobCommand)

allCommands :: [Command]
allCommands = [showCommand, jobCommand]
