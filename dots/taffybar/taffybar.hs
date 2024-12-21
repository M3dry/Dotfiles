import Data.Default (def)

import System.Taffybar

import System.Taffybar.SimpleConfig

import System.Taffybar.Widget
import System.Taffybar.Widget.Layout

main = do
  let layout = layoutNew def
      workspaces =
        workspacesNew
          def
            { maxIcons = Just 3
            , widgetGap = 0
            , showWorkspaceFn      = \name -> hideEmpty name && workspaceName name /= "NSP"
            , urgentWorkspaceState = True
            }
      mpris = mpris2New
      simpleConfig = def
                       { monitorsAction = usePrimaryMonitor
                       , barHeight = ExactSize 44
                       , barPosition = Bottom
                       , startWidgets = [ layout, workspaces, sniTrayNew ]
                       }
  simpleTaffybar simpleConfig




