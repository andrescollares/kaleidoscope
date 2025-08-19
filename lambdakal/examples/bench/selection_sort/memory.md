### Overwrite this file with the content:

nano /etc/apt/sources.list

```
deb http://archive.debian.org/debian buster main
deb http://archive.debian.org/debian-security buster/updates main
```

### Run command

echo 'Acquire::Check-Valid-Until "false";' > /etc/apt/apt.conf.d/99no-check-valid-until

### Install

apt-get update
apt-get install -y time

### Measure memory consumption

/usr/bin/time -v ./examples/bench/selection_sort/selection_sort
