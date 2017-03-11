# monerodo GUI



## Installation

Copy the binary distribution for your architecture into `/usr/local/bin/`, or
include it in the `$PATH` directory list, then run

```bash
monerodo-backend
```

If you don't see any text come up, that's great! It means the web server is running
and can be viewed at `http://localhost:3000`.



## TODO

- Use electron to open a browser window when the daemon starts, so nobody needs to
  look at `http://localhost:3000`
- Finish encoding the logic from the original monerodo scripts into the backend, and
  stop using dummy data
