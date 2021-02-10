# Manual tests dataset

Each test is running on `guard-property.imiprop`.

|File|Expected|Actual|Success|
|---|---|---|---|
| linear-d-reachable | `True` | `True` | Y |
| linear-xd-reachable | `True` | `True` | Y |
| linear-pd-reachable | `0 < p1 < 10` | `0 < p1 < 10` | Y |
| linear-pxd-reachable | under-approximation `p1 > 0` | under-approximation `p1 > 0` | Y |
| nonlinear-d-reachable | `True` | `True` | Y |
| nonlinear-xd-reachable | `True` | `True` | Y |
| nonlinear-pd-reachable | `0 < p1 < 10` |  `0 < p1 < 10` | Y |
| nonlinear-pxd-reachable | `p1 > 0` | `p1 > 0` | Y |
| nonlinear-d-semantic-error | `Error` | `Error` | Y
| nonlinear-xd-semantic-error | `Error` | `Error` | Y
| nonlinear-pd-semantic-error | `Error` | `Error` | Y
| nonlinear-pxd-semantic-error | `Error` | `Error` | Y
