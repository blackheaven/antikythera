# antikythera

Named after [Antikythera mechanism](https://en.wikipedia.org/wiki/Antikythera_mechanism),
an analogue computer used to track various recurring events (such as ancient
Olympics Games).

## Example

```haskell
import Control.Antikythera

runPeriodicityZonedTime (inclusiveRange (Min 8) (Max 23) hour .&& every 30 minute) $
  putStrLn "Don't forget to hydrate"
```

