{:user {:local-repo #=(eval (str (System/getenv "XDG_CACHE_HOME") "/m2/repository"))
        :repositories  {"local" {:url #=(eval (str "file://" (System/getenv "XDG_CACHE_HOME") "/m2/repository"))
                                 :releases {:checksum :ignore}}}}}
