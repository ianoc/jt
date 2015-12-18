module Jt.Command.All(allCommands) where

import Jt(Command)
import Jt.Command.Show(showCommand)
import Jt.Command.Jobs(jobsCommand)
import Jt.Command.Details(detailsCommand)
import Jt.Command.Counters(countersCommand)

allCommands :: [Command]
allCommands = [showCommand, jobsCommand, detailsCommand, countersCommand]
