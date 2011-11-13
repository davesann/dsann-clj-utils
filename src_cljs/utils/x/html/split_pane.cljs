(ns utils.x.html.split-pane
  (:require
    [goog.ui.Component :as gcomp]
    [goog.ui.SplitPane :as gsplitpane]
    )
  )


(def orientation 
  {
   :h goog.ui.SplitPane.Orientation.HORIZONTAL
   :horizontal goog.ui.SplitPane.Orientation.HORIZONTAL
   :v goog.ui.SplitPane.Orientation.VERTICAL
   :vertical goog.ui.SplitPane.Orientation.VERTICAL}
  )

(defn split-pane [parent id l-content r-content]
  (let [c (ph/html
            [:div {:class "goog-splitpane" :id id}
             [:div
              {:class "goog-splitpane-first-container"}
              l-content
              ]
             [:div 
              {:class "goog-splitpane-second-container"}
              r-content
              ]
             [:div {:class "goog-splitpane-handle"}]
             ]
            )
        ]
    (pdom/append parent c)
    (let
      [c1 (goog.ui.Component.)
       c2 (goog.ui.Component.)
       sp (goog.ui.SplitPane. c1 c2 (:vertical orientation))
       sp-dom (gdom/getElement id)
       ]
      (. sp (decorate sp-dom))
      sp)))
  