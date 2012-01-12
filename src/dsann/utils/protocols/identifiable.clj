(ns dsann.utils.protocols.identifiable)

(defprotocol Identifiable
  (id [this] "return a unique identifier"))