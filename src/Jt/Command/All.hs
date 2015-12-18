module Jt.Command.All(allCommands) where

import Jt(Command)
import Jt.Command.Show(showCommand)
import Jt.Command.Jobs(jobsCommand)
import Jt.Command.Job(jobCommand)

allCommands :: [Command]
allCommands = [showCommand, jobsCommand, jobCommand]
