# purescript-hylograph-transitions

Animation and transition support for Hylograph visualizations.

## Overview

Provides interpolation, easing functions, and animation coordination for smooth transitions in visualizations. Supports requestAnimationFrame-based updates and complex multi-property animations.

## Installation

```bash
spago install hylograph-transitions
```

## Modules

- `Hylograph.Transition.Engine` - Animation engine
- `Hylograph.Transition.Interpolate` - Value interpolation
- `Hylograph.Transition.Easing` - Easing functions (linear, ease-in-out, etc.)
- `Hylograph.Transition.RAF` - requestAnimationFrame utilities
- `Hylograph.Transition.Tick` - Tick-based updates
- `Hylograph.Transition.Coordinator` - Multi-transition coordination

## Part of Hylograph

- **hylograph-transitions** - Animation support (this package)
- **hylograph-selection** - DOM rendering with transitions
- **hylograph-simulation** - Force simulation (uses transitions)

## License

MIT
