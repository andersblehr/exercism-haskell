module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

solAge :: Float -> Float -> Float
solAge seconds daySolFactor = seconds / (daySolFactor * daySeconds)
    where daySeconds = 31557600

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = solAge seconds 0.2408467
ageOn Venus seconds = solAge seconds 0.61519726
ageOn Earth seconds = solAge seconds 1.0
ageOn Mars seconds = solAge seconds 1.8808158
ageOn Jupiter seconds = solAge seconds 11.862615
ageOn Saturn seconds = solAge seconds 29.447498
ageOn Uranus seconds = solAge seconds 84.016846
ageOn Neptune seconds = solAge seconds 164.79132