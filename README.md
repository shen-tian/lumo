# lumo

A Clojure app for controlling some LEDs via OPC

## Usage

For dev, `cider-jack-in`, switch to `lumo.core`

    (def opc (init-client))
    (run opc)
    
or just show a single frame with

    (tick opc)
    
## Build

    lein uberjar
    
    java -jar target/lumo-XXXX-standalong.jar

## License

Copyright © 2017 Shen Tian

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
