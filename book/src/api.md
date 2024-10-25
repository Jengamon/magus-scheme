# API

The Scheme API for declaring abilities

Keyword abilities are implemented in their own files, where the ability
is given a `(keyword <sym>)` argument in order to specify what *actual* keyword the ability will use.

Examples:
- Flying
```scheme
(ability 'static
    (keyword 'flying)
    (for 'creature)
    (logic 'valid-blockers
        ((not (has blocker 'flying)) (not (has blocker 'reach)))
        #f
    )
)
```
- Haunt
```scheme
; a prelude could define something like (define (dies? card) (move? card 'battlefield 'graveyard))
(ability 'triggered
    (keyword 'haunt)
    (for 'card) ; TODO Should this just be the implicit default?
    (trigger (move? this 'battlefield 'graveyard))
    (let
    ; could be (target 'creature 'spell ... for "target creature or spell"
    (
        (tc (target 'creature (lambda (c) #t)))
    )
        (action
            ; 'graveyard means that on resolution the card must be in the graveyard, or this fails to resolve
            (move this 'graveyard 'exile)
            ; the last argument should control how long the designation is valid for, with the default
            ; being that it is only such until either card changes zones
            (designate 'haunts this 'haunted tc)
        )
    )
)
```
- Kicker
- Bushido
```scheme
; todo rework for parameters!!!
(ability 'triggered
    (keyword 'bushido)
    (for 'creature)
    ; trigger checks can accept a second parameter, which is a lambda to check properties
    ; of what satisfied the check
    (trigger (block? this))
    (trigger (blocked? this))
    (action
        (gain-power )
    )
)
```

## Static

## Triggered

- [Absolver Thrull](https://scryfall.com/card/gpt/1/absolver-thrull)
```scheme
(ability 'triggered
    ; this could be card or w/e, but generally, rule-of-thumb should be to make the "for" of abilities
    ; as specific as possible so that copying the ability is valid if the copy would be valid (todo explain better)
    (for 'creature)
    (trigger (move? this 'any 'battlefield))
    (trigger (move? (relation this 'haunts) 'battlefield 'graveyard))
    ; short version of "target" is if there is no condition on the enchantment
    (let ((te (target 'enchantment))) (action (destroy te)))
)
```

## Activated

## Spell
