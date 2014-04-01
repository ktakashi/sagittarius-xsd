(formula
 (description "XSD marshal/unmarshal for Sagittarius")
 (version "HEAD")
 (homepage :url "https://github.com/ktakashi/sagittarius-xsd")
 (author :name "Takashi Kato" :email "ktakashi@ymail.com")
 (source 
  ;;:type tar :compression gzip
  :type zip
  :url "https://github.com/ktakashi/sagittarius-xsd/archive/master.zip")

 (install (directories ("src" :excludes ("CMakeLists")))))